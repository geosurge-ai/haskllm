"""
vLLM (Qwen) client via OpenAI-compatible Chat Completions API.
Supports strict JSON schema enforcement using response_format.json_schema.

Credentials can be provided via the Credentials map or environment variables:
- base_url: BLOOD_MONEY_BASE_URL (default: "https://outland-dev-1.doubling-season.geosurge.ai")
- api_key: BLOOD_MONEY_API_KEY (required)

The provider checks the Credentials map first, then falls back to environment
variables if a key is missing. If base_url is not provided, uses the default
production URL. The /02/ route for QweN2.5 is automatically appended if needed.
"""

from typing import List, Optional, Any, Dict
import os
import json
import time
import requests
from . import Credentials, ChatMessage, JSONSchemaSpec, RequestConfig, default_request_config


class Qwen:
    """Provider for vLLM/Qwen (OpenAI-compatible server)."""

    def _get_credential(self, creds: Credentials, key: str, env_var: str, default: Optional[str] = None) -> str:
        """
        Get a required credential, with environment variable fallback.
        Checks the credentials map first, then falls back to environment variables.
        For base_url, uses a default if neither is provided.
        """
        if key in creds.data:
            return creds.data[key]

        env_value = os.environ.get(env_var)
        if env_value:
            return env_value

        if default is not None:
            return default

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

    def _concretize_chat_endpoint(self, base_url: str) -> str:
        """
        Normalize a base URL into a concrete Chat Completions endpoint.
        Handles the /02/ routing for QweN2.5 in the blood-money infrastructure.

        Accepts:
        - Full endpoint ending with "/chat/completions" → used verbatim
        - Base URL with "/02" → appends "/v1/chat/completions"
        - Base URL without "/02" → appends "/02/v1/chat/completions"
        """
        base = base_url.rstrip('/')

        if base.endswith("/chat/completions"):
            return base  # Already a full endpoint
        elif base.endswith("/02"):
            return base + "/v1/chat/completions"  # Has /02, just add the rest
        else:
            return base + "/02/v1/chat/completions"  # Needs /02/ route

    def _extract_chat_content(self, js: Dict[str, Any]) -> str:
        """Extract assistant content from OpenAI-compatible Chat Completions."""
        if "choices" in js and isinstance(js["choices"], list) and len(js["choices"]) > 0:
            first_choice = js["choices"][0]
            if isinstance(first_choice, dict) and "message" in first_choice:
                message = first_choice["message"]
                if isinstance(message, dict) and "content" in message:
                    return message["content"]
        return ""

    def _make_text_request(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        """Make a text request with configurable timeout and retries."""
        base_url = self._get_credential(
            credentials,
            "base_url",
            "BLOOD_MONEY_BASE_URL",
            "https://outland-dev-1.doubling-season.geosurge.ai"
        )
        api_key = self._get_credential(credentials, "api_key", "BLOOD_MONEY_API_KEY")

        url = self._concretize_chat_endpoint(base_url)

        body = {
            "model": model_name,
            "messages": [msg.to_dict() for msg in messages],
            "temperature": 0.7,
        }

        if max_tokens is not None:
            body["max_tokens"] = max_tokens

        headers = {
            "Content-Type": "application/json",
            "x-api-key": api_key,
        }

        timeout = config.timeout_seconds if config.timeout_seconds is not None else None

        response = requests.post(url, headers=headers, json=body, timeout=timeout)
        response.raise_for_status()

        js = response.json()
        return self._extract_chat_content(js)

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
        base_url = self._get_credential(
            credentials,
            "base_url",
            "BLOOD_MONEY_BASE_URL",
            "https://outland-dev-1.doubling-season.geosurge.ai"
        )
        api_key = self._get_credential(credentials, "api_key", "BLOOD_MONEY_API_KEY")

        url = self._concretize_chat_endpoint(base_url)

        response_format = {
            "type": "json_schema",
            "json_schema": {
                "name": schema.schema_name,
                "schema": schema.schema,
                "strict": schema.strict,
            }
        }

        body = {
            "model": model_name,
            "messages": [msg.to_dict() for msg in messages],
            "temperature": 0.7,
            "response_format": response_format,
        }

        if max_tokens is not None:
            body["max_tokens"] = max_tokens

        headers = {
            "Content-Type": "application/json",
            "x-api-key": api_key,
        }

        timeout = config.timeout_seconds if config.timeout_seconds is not None else None

        response = requests.post(url, headers=headers, json=body, timeout=timeout)
        response.raise_for_status()

        js = response.json()
        txt = self._extract_chat_content(js)

        try:
            return json.loads(txt)
        except json.JSONDecodeError as e:
            raise ValueError(f"vLLM: schema-enforced output was not valid JSON: {e}")

    # LLMFormatChat interface implementation

    def respond_text(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
    ) -> str:
        """Plain chat (no schema)."""
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
        """Enforced JSON schema via response_format (vLLM / chat.completions)."""
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
        """Plain chat with configurable max tokens."""
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
        """Enforced JSON schema with configurable max tokens via response_format."""
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


__all__ = ["Qwen"]

