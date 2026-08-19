"""
Phony LLM provider that always fails.
Useful for testing fallback mechanisms and error handling.

Example usage:
    # This will always raise an exception
    provider = Phony()
    result = provider.respond_text(credentials, "phony-model", messages)

    # But in a fallback chain, it will skip to the next provider
    from haskllm_py.fallback import FallbackProvider, ProviderConfig
    from haskllm_py.vllm_qwen import Qwen

    fallback = FallbackProvider(
        primary=ProviderConfig(Phony(), credentials, "phony"),
        secondary=ProviderConfig(Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct")
    )
    result = fallback.respond_text(credentials, model_name, messages)  # Uses Qwen
"""

from typing import List, Optional, Any
from . import Credentials, ChatMessage, JSONSchemaSpec, RequestConfig


class Phony:
    """
    Provider for a phony/mock LLM that always fails.
    Raises an error on every method call.
    """

    def _phony_fail(self, method: str) -> None:
        """Helper that always fails with a descriptive error."""
        raise RuntimeError(f"Phony LLM provider: {method} intentionally failed")

    def respond_text(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
    ) -> str:
        self._phony_fail("respond_text")

    def respond_json(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
    ) -> Any:
        self._phony_fail("respond_json")

    def respond_text_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int] = None,
    ) -> str:
        self._phony_fail("respond_text_with_tokens")

    def respond_json_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int] = None,
    ) -> Any:
        self._phony_fail("respond_json_with_tokens")

    def respond_text_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        config: RequestConfig,
    ) -> str:
        self._phony_fail("respond_text_with_config")

    def respond_json_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        config: RequestConfig,
    ) -> Any:
        self._phony_fail("respond_json_with_config")

    def respond_text_with_tokens_and_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        self._phony_fail("respond_text_with_tokens_and_config")

    def respond_json_with_tokens_and_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> Any:
        self._phony_fail("respond_json_with_tokens_and_config")


__all__ = ["Phony"]

