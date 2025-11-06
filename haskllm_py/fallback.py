"""
Type-safe LLM provider fallback mechanism.
Attempts to use a primary provider, and falls back to a secondary provider on failure.

Basic Usage (Two Providers):

    from haskllm_py import Credentials
    from haskllm_py.fallback import FallbackProvider, ProviderConfig
    from haskllm_py.vllm_qwen import Qwen
    from haskllm_py.openai_gpt5 import OpenAI

    qwen_creds = Credentials({"base_url": "https://...", "api_key": "..."})
    gpt5_creds = Credentials({"openai_api_key": "sk-..."})

    fallback = FallbackProvider(
        primary=ProviderConfig(Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct"),
        secondary=ProviderConfig(OpenAI(), gpt5_creds, "gpt-5-preview")
    )
    result = fallback.respond_text(Credentials({}), "", messages)

Chaining Multiple Providers (Three or More):

    # Option 1: Explicit nesting
    three_way_fallback = FallbackProvider(
        primary=ProviderConfig(Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct"),
        secondary=ProviderConfig(
            FallbackProvider(
                primary=ProviderConfig(Claude(), claude_creds, "claude-3-opus"),
                secondary=ProviderConfig(OpenAI(), gpt5_creds, "gpt-5-preview")
            ),
            Credentials({}),  # unused for FallbackProvider
            ""  # unused for FallbackProvider
        )
    )

    # Option 2: Using the helper (recommended)
    three_way_fallback = chain3(
        Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct",
        Claude(), claude_creds, "claude-3-opus",
        OpenAI(), gpt5_creds, "gpt-5-preview"
    )

    result = three_way_fallback.respond_json(Credentials({}), "", messages, schema)
    # Tries: Qwen → Claude → GPT-5 (stops at first success)
"""

from typing import List, Optional, Any, TypeVar, Callable
from dataclasses import dataclass
from . import Credentials, ChatMessage, JSONSchemaSpec, RequestConfig


T = TypeVar('T')


@dataclass
class ProviderConfig:
    """Configuration for a single provider with its credentials and model name."""
    provider: Any  # Any LLMFormatChat implementation
    credentials: Credentials
    model_name: str


class FallbackProvider:
    """
    A fallback provider that tries a primary provider first, then falls back to a secondary.
    This class itself implements the LLMFormatChat interface, allowing it to be used anywhere
    a regular provider can be used.
    """

    def __init__(self, primary: ProviderConfig, secondary: ProviderConfig):
        """
        Initialize a fallback provider.

        Args:
            primary: Primary provider (tried first)
            secondary: Secondary provider (fallback)
        """
        self.primary = primary
        self.secondary = secondary

    def _with_fallback(self, action: Callable[[ProviderConfig], T]) -> T:
        """
        Execute fallback logic: try primary, fall back to secondary on failure.

        Args:
            action: Function that takes a ProviderConfig and returns a result

        Returns:
            Result from either primary or secondary provider
        """
        try:
            return action(self.primary)
        except Exception:
            # On any exception from primary, try secondary
            return action(self.secondary)

    def respond_text(
        self,
        credentials: Credentials,  # Ignored - uses config credentials
        model_name: str,  # Ignored - uses config model names
        messages: List[ChatMessage],
    ) -> str:
        def action(config: ProviderConfig) -> str:
            return config.provider.respond_text(
                config.credentials, config.model_name, messages
            )
        return self._with_fallback(action)

    def respond_json(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
    ) -> Any:
        def action(config: ProviderConfig) -> Any:
            return config.provider.respond_json(
                config.credentials, config.model_name, messages, schema
            )
        return self._with_fallback(action)

    def respond_text_with_tokens(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        max_tokens: Optional[int] = None,
    ) -> str:
        def action(config: ProviderConfig) -> str:
            return config.provider.respond_text_with_tokens(
                config.credentials, config.model_name, messages, max_tokens
            )
        return self._with_fallback(action)

    def respond_json_with_tokens(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int] = None,
    ) -> Any:
        def action(config: ProviderConfig) -> Any:
            return config.provider.respond_json_with_tokens(
                config.credentials, config.model_name, messages, schema, max_tokens
            )
        return self._with_fallback(action)

    def respond_text_with_config(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        config: RequestConfig,
    ) -> str:
        def action(prov_config: ProviderConfig) -> str:
            return prov_config.provider.respond_text_with_config(
                prov_config.credentials, prov_config.model_name, messages, config
            )
        return self._with_fallback(action)

    def respond_json_with_config(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        config: RequestConfig,
    ) -> Any:
        def action(prov_config: ProviderConfig) -> Any:
            return prov_config.provider.respond_json_with_config(
                prov_config.credentials, prov_config.model_name, messages, schema, config
            )
        return self._with_fallback(action)

    def respond_text_with_tokens_and_config(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        def action(prov_config: ProviderConfig) -> str:
            return prov_config.provider.respond_text_with_tokens_and_config(
                prov_config.credentials, prov_config.model_name, messages, max_tokens, config
            )
        return self._with_fallback(action)

    def respond_json_with_tokens_and_config(
        self,
        credentials: Credentials,  # Ignored
        model_name: str,  # Ignored
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> Any:
        def action(prov_config: ProviderConfig) -> Any:
            return prov_config.provider.respond_json_with_tokens_and_config(
                prov_config.credentials, prov_config.model_name, messages, schema, max_tokens, config
            )
        return self._with_fallback(action)


# Ergonomic helpers for multi-provider chains

def chain3(
    p1: Any, c1: Credentials, m1: str,
    p2: Any, c2: Credentials, m2: str,
    p3: Any, c3: Credentials, m3: str,
) -> FallbackProvider:
    """
    Chain three providers into a fallback sequence: p1 → p2 → p3

    Example:
        from haskllm_py.phony import Phony
        from haskllm_py.vllm_qwen import Qwen
        from haskllm_py.openai_gpt5 import OpenAI

        triple_backup = chain3(
            Phony(), Credentials({}), "phony",           # Will fail
            Qwen(), qwen_creds, "Qwen/...",              # First real attempt
            OpenAI(), gpt5_creds, "gpt-5-preview"        # Final fallback
        )

        result = triple_backup.respond_text(Credentials({}), "", messages)
    """
    inner_fallback = FallbackProvider(
        ProviderConfig(p2, c2, m2),
        ProviderConfig(p3, c3, m3)
    )
    return FallbackProvider(
        ProviderConfig(p1, c1, m1),
        ProviderConfig(inner_fallback, Credentials({}), "")
    )


__all__ = ["FallbackProvider", "ProviderConfig", "chain3"]

