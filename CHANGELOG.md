# Changelog for haskllm

## 0.3.0.0 -- 2026-09-15

* **BREAKING:** Added `costUsd` to `TokenUsage`; code that constructs the record must set it
* Added `HaskLLM.OpenRouter`, a Chat Completions provider that pins hosts via routing preferences and reports `costUsd`
* **BREAKING:** Added `cacheWriteTokens :: Maybe Int` to `TokenUsage`.
  Update direct constructors, using `Nothing` when the provider doesn't report it.
* Preserve OpenAI cache-write counts in text, JSON and tool responses, including
  aggregation across tool rounds. Input totals already include cache reads/writes.
* Added `OpenAIWithObserver` for best-effort per-HTTP-attempt accounting, including
  responses discarded by parsing, retry or tool-loop failures. Observations expose
  available provider identifiers, status and usage before application processing.
* Observer failures are logged to stderr without triggering a retry or fallback.
  Cancellation propagates, including through provider fallback chains.

## 0.2.0.0 -- 2025-01-01

* **BREAKING:** Added new methods to `LLMFormatChat` typeclass requiring implementation in custom providers
* Added `RequestConfig` data type for configuring HTTP requests with timeout and retry settings
* Added `defaultRequestConfig` function providing sensible defaults (no timeout, 3 retries)
* Added new configurable methods in `LLMFormatChat` typeclass:
  * `respondTextWithConfig` - plain chat with configurable timeout and retries
  * `respondJSONWithConfig` - JSON schema chat with configurable timeout and retries
  * `respondTextWithTokensAndConfig` - plain chat with max tokens, timeout and retries
  * `respondJSONWithTokensAndConfig` - JSON schema chat with max tokens, timeout and retries
* Added retry functionality with exponential backoff for all HTTP requests
* Added configurable timeout support (defaults to no timeout instead of hardcoded timeouts)
* Changed default timeout from hardcoded 15 minutes (OpenAI) / 2 minutes (vLLM) to no timeout
* Changed HTTP requests to retry up to 3 times by default on failure
* Refactored request logic into reusable helper functions for better maintainability
* All existing method signatures remain unchanged for backward compatibility

## 0.1.0.0 -- 2024-12-01

* Initial release
* OpenAI GPT-5 Responses API support
* vLLM/Qwen Chat Completions API support
* JSON schema enforcement for structured outputs
* Basic timeout and token configuration
