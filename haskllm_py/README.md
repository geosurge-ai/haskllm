# HaskLLM Python Port

A Python port of the HaskLLM library for querying LLMs with fallback support.

## Features

- **Unified Interface**: A single `LLMFormatChat` protocol for all providers
- **Multiple Providers**: Support for OpenAI GPT-5, Qwen 2.5, and custom providers
- **Fallback Support**: Automatically fall back to secondary providers on failure
- **JSON Schema Enforcement**: Structured output with JSON schema validation
- **Flexible Credentials**: Support for credential maps or environment variables

## Installation

```bash
pip install requests
```

Or install from requirements:

```bash
pip install -r requirements.txt
```

## Quick Start

### Basic Usage (Single Provider)

```python
from haskllm_py import Credentials, ChatMessage
from haskllm_py.vllm_qwen import Qwen

# Credentials can be provided directly or via environment variables
creds = Credentials({
    "api_key": "your-api-key",
    "base_url": "https://your-server.com"  # optional, has default
})

# Or use empty credentials to rely on environment variables
creds = Credentials({})  # Will use BLOOD_MONEY_API_KEY and BLOOD_MONEY_BASE_URL

messages = [
    ChatMessage("system", "You are a helpful assistant."),
    ChatMessage("user", "Hello!")
]

provider = Qwen()
response = provider.respond_text(creds, "Qwen/Qwen2.5-32B-Instruct", messages)
print(response)
```

### Fallback Provider

```python
from haskllm_py import Credentials, ChatMessage
from haskllm_py.fallback import FallbackProvider, ProviderConfig
from haskllm_py.vllm_qwen import Qwen
from haskllm_py.openai_gpt5 import OpenAI

qwen_creds = Credentials({"api_key": "..."})
gpt5_creds = Credentials({"openai_api_key": "sk-..."})

# Create a fallback provider: tries Qwen first, then GPT-5
fallback = FallbackProvider(
    primary=ProviderConfig(Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct"),
    secondary=ProviderConfig(OpenAI(), gpt5_creds, "gpt-5-preview")
)

messages = [ChatMessage("user", "Hello!")]

# Will try Qwen first, fall back to GPT-5 on any error
response = fallback.respond_text(Credentials({}), "", messages)
```

### Three-Way Fallback

```python
from haskllm_py.fallback import chain3
from haskllm_py.phony import Phony
from haskllm_py.vllm_qwen import Qwen
from haskllm_py.openai_gpt5 import OpenAI

# Chain three providers: Phony (always fails) → Qwen → GPT-5
triple_fallback = chain3(
    Phony(), Credentials({}), "phony",
    Qwen(), qwen_creds, "Qwen/Qwen2.5-32B-Instruct",
    OpenAI(), gpt5_creds, "gpt-5-preview"
)

response = triple_fallback.respond_text(Credentials({}), "", messages)
```

### JSON Schema Enforcement

```python
from haskllm_py import JSONSchemaSpec

schema = JSONSchemaSpec(
    schema_name="greeting_response",
    schema={
        "type": "object",
        "properties": {
            "greeting": {"type": "string"},
            "language": {"type": "string"}
        },
        "required": ["greeting", "language"]
    },
    strict=True
)

response = provider.respond_json(creds, "Qwen/Qwen2.5-32B-Instruct", messages, schema)
print(response)  # Dictionary with 'greeting' and 'language' keys
```

## Environment Variables

The library supports the following environment variables:

### OpenAI GPT-5
- `OPENAI_API_KEY`: Your OpenAI API key

### vLLM/Qwen (blood-money server)
- `BLOOD_MONEY_API_KEY`: API key for the blood-money server (required)
- `BLOOD_MONEY_BASE_URL`: Base URL (default: `https://outland-dev-1.doubling-season.geosurge.ai`)

## Module Structure

```
haskllm_py/
├── __init__.py          # Core types and protocols
├── phony.py            # Phony provider (always fails, for testing)
├── fallback.py         # Fallback provider implementation
├── openai_gpt5.py      # OpenAI GPT-5 provider
├── vllm_qwen.py        # vLLM/Qwen provider
├── requirements.txt    # Python dependencies
└── README.md          # This file
```

## Testing

Run the Qwen integration test:

```bash
./run-qwen-test.py
```

This will fetch credentials from `passveil` and run a simple connectivity test.

