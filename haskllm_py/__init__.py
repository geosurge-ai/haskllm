"""
HaskLLM Python Port

A Python port of the HaskLLM library for querying LLMs with fallback support.
Supports GPT-5, Qwen 2.5, and other providers through a common interface.
"""

from typing import Protocol, Dict, Optional, Any, List
from dataclasses import dataclass
import json


@dataclass
class Credentials:
    """
    Simple credential bag.
    Required keys:
    - OpenAI GPT-5: "openai_api_key"
    - vLLM/Qwen: "base_url", "api_key"
    """
    data: Dict[str, str]


@dataclass
class RequestConfig:
    """Configuration for HTTP requests."""
    timeout_seconds: Optional[int] = None  # None means no timeout
    max_retries: int = 3


def default_request_config() -> RequestConfig:
    """Default request configuration: no timeout, 3 retries."""
    return RequestConfig(timeout_seconds=None, max_retries=3)


@dataclass
class ChatMessage:
    """Minimal chat message (OpenAI/vLLM compatible)."""
    role: str  # "system" | "user" | "assistant"
    content: str

    def to_dict(self) -> Dict[str, str]:
        """Convert to dictionary for JSON serialization."""
        return {"role": self.role, "content": self.content}


@dataclass
class JSONSchemaSpec:
    """
    JSON Schema spec (portable across providers).
    For OpenAI Responses API: becomes text.format payload.
    For vLLM Chat Completions: becomes response_format.json_schema.
    """
    schema_name: str
    schema: Dict[str, Any]  # A JSON Schema (subset supported by providers)
    strict: bool = True  # Enforce exact conformance when supported


class LLMFormatChat(Protocol):
    """
    Generic interface for conversational generation with optional JSON format enforcement.
    This is a Protocol (similar to Haskell typeclass) that defines the interface all providers must implement.
    """

    def respond_text(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
    ) -> str:
        """Plain chat; returns assistant text."""
        ...

    def respond_json(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
    ) -> Any:
        """Chat with enforced JSON schema; returns parsed JSON (raises on invalid JSON)."""
        ...

    def respond_text_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int] = None,
    ) -> str:
        """Plain chat with configurable max tokens; returns assistant text."""
        ...

    def respond_json_with_tokens(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        max_tokens: Optional[int] = None,
    ) -> Any:
        """Chat with enforced JSON schema and configurable max tokens."""
        ...

    def respond_text_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        config: RequestConfig,
    ) -> str:
        """Plain chat with configurable timeout and retries; returns assistant text."""
        ...

    def respond_json_with_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        schema: JSONSchemaSpec,
        config: RequestConfig,
    ) -> Any:
        """Chat with enforced JSON schema and configurable timeout and retries."""
        ...

    def respond_text_with_tokens_and_config(
        self,
        credentials: Credentials,
        model_name: str,
        messages: List[ChatMessage],
        max_tokens: Optional[int],
        config: RequestConfig,
    ) -> str:
        """Plain chat with configurable max tokens, timeout and retries."""
        ...

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
        ...


__all__ = [
    "Credentials",
    "RequestConfig",
    "default_request_config",
    "ChatMessage",
    "JSONSchemaSpec",
    "LLMFormatChat",
]

