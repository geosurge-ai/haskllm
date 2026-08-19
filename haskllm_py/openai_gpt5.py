"""
OpenAI GPT-5 client using the Responses API, with structured-output enforcement.

Credentials can be provided via the Credentials map or environment variables:
- openai_api_key: OPENAI_API_KEY (e.g., "sk-proj-...")

The provider checks the Credentials map first, then falls back to the OPENAI_API_KEY
environment variable if the key is missing. This enables flexible configuration.
"""

from typing import List, Optional, Any, Dict
import os
import json
import time
import requests
from . import Credentials, ChatMessage, JSONSchemaSpec, RequestConfig, default_request_config


class OpenAI:
    """Provider for OpenAI GPT-5 (Responses API)."""

    def _get_credential(self, creds: Credentials, key: str, env_var: str) -> str:
        """
        Get a required credential, with environment variable fallback.
        Checks the credentials map first, then falls back to environment variables.
        """
        if key in creds.data:
            return creds.data[key]

        env_value = os.environ.get(env_var)
        if env_value:
            return env_value

        raise ValueError(
            f"Missing credential key: {key} (not in Credentials map, "
            f"and environment variable {env_var} is not set)"
        )

    def _retry_with_backoff(self, action, max_retries: int):
        """Retry an action with exponential backoff."""
        delay = 1
        for attempt in range(max_retries):
            try:
                return action()
            except Exception as e:
                if attempt == max_retries - 1:
                    # Last attempt, re-raise the exception
                    raise
                # Exponential backoff with cap at 8 seconds
                time.sleep(min(delay, 8))
                delay *= 2

    def _make_text_request(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        """Make a text request with configurable timeout and retries."""
        api_key = self._get_credential(credentials, "openai_api_key", "OPENAI_API_KEY")

        input_messages = [msg.to_dict() for msg in messages]
        max_output_tokens = max_tokens if max_tokens is not None else 8192

        body = {
            "model": model_name,
            "input": input_messages,
            "max_output_tokens": max_output_tokens,
        }

        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        timeout = config.timeout_seconds if config.timeout_seconds is not None else None

        response = requests.post(
            "https://api.openai.com/v1/responses",
            headers=headers,
            json=body,
            timeout=timeout,
        )
        response.raise_for_status()

        js = response.json()
        return self._extract_responses_text(js)

    def _make_json_request(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> Any:
        """Make a JSON request with configurable timeout and retries."""
        api_key = self._get_credential(credentials, "openai_api_key", "OPENAI_API_KEY")

        input_messages = [msg.to_dict() for msg in messages]
        max_output_tokens = max_tokens if max_tokens is not None else 8192

        text_format = {
            "type": "json_schema",
            "name": schema.schema_name,
            "schema": schema.schema,
            "strict": schema.strict,
        }

        body = {
            "model": model_name,
            "input": input_messages,
            "text": {"format": text_format},
            "max_output_tokens": max_output_tokens,
        }

        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        timeout = config.timeout_seconds if config.timeout_seconds is not None else None

        response = requests.post(
            "https://api.openai.com/v1/responses",
            headers=headers,
            json=body,
            timeout=timeout,
        )
        response.raise_for_status()

        js = response.json()
        txt = self._extract_responses_text(js)

        try:
            return json.loads(txt)
        except json.JSONDecodeError as e:
            raise ValueError(
                f"OpenAI: schema-enforced output was not valid JSON: {e}\n"
                f"Raw response text: {txt}"
            )

    def _extract_responses_text(self, js: Dict[str, Any]) -> str:
        """
        OpenAI "Responses API" extraction:
        Prefer output_text; else concatenate output[].content[].text.
        """
        if "output_text" in js:
            return js["output_text"]

        if "output" in js and isinstance(js["output"], list):
            texts = []
            for item in js["output"]:
                if isinstance(item, dict) and "content" in item:
                    content = item["content"]
                    if isinstance(content, list):
                        for content_item in content:
                            if isinstance(content_item, dict) and "text" in content_item:
                                texts.append(content_item["text"])
            return "\n".join(texts)

        return ""

    # LLMFormatChat interface implementation

    def respond_text(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
    ) -> str:
        """Responses API text output (no schema)."""
        return self._make_text_request(
            credentials, model_name, messages, None, default_request_config()
        )

    def respond_json(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
    ) -> Any:
        """Responses API structured output with JSON schema."""
        return self._make_json_request(
            credentials, model_name, messages, schema, None, default_request_config()
        )

    def respond_text_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int] = None,
    ) -> str:
        """Responses API text output with configurable max tokens."""
        return self._make_text_request(
            credentials, model_name, messages, max_tokens, default_request_config()
        )

    def respond_json_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int] = None,
    ) -> Any:
        """Responses API structured output with JSON schema and configurable max tokens."""
        return self._make_json_request(
            credentials, model_name, messages, schema, max_tokens, default_request_config()
        )

    def respond_text_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        config: RequestConfig,
    ) -> str:
        """Plain chat with configurable timeout and retries."""
        return self._retry_with_backoff(
            lambda: self._make_text_request(credentials, model_name, messages, None, config),
            config.max_retries
        )

    def respond_json_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        config: RequestConfig,
    ) -> Any:
        """Chat with enforced JSON schema and configurable timeout and retries."""
        return self._retry_with_backoff(
            lambda: self._make_json_request(credentials, model_name, messages, schema, None, config),
            config.max_retries
        )

    def respond_text_with_tokens_and_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        """Plain chat with configurable max tokens, timeout and retries."""
        return self._retry_with_backoff(
            lambda: self._make_text_request(credentials, model_name, messages, max_tokens, config),
            config.max_retries
        )

    def respond_json_with_tokens_and_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> Any:
        """Chat with enforced JSON schema, configurable max tokens, timeout and retries."""
        return self._retry_with_backoff(
            lambda: self._make_json_request(credentials, model_name, messages, schema, max_tokens, config),
            config.max_retries
        )


__all__ = ["OpenAI"]

