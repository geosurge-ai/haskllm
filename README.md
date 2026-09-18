## `haskllm` ✨

A dumb (read: bottom-up) Haskell library for making simple structured requests to frontier models and other models that aren't too dumb.

## What it’s good for

 - **Repeatable generation**.
 - **Language task benchmarking**.

## Setup

You will need to set the following environment variables to run tests:

```
OPENAI_API_KEY=$(passveil show platform.openai.com/api | head -n 1)
OPENROUTER_API_KEY=$(passveil show openrouter.ai/api | head -n 1)
```

The OpenRouter integration tests are skipped unless `OPENROUTER_API_KEY` is set.

## Usage accounting

Detailed responses include `TokenUsage`. For OpenAI, `inputTokens` includes
`cachedInputTokens` (cache reads) and `cacheWriteTokens` (cache writes). These
categories can have different prices. Ordinary input is therefore
`inputTokens - cachedInputTokens - cacheWriteTokens`, when all three are known.
HaskLLM reports counts and provider-billed `costUsd` when available; callers own
estimated pricing. `Nothing` means **unreported**, not zero. vLLM and OpenRouter
currently leave cache writes unreported.

To account for received responses even when parsing or a later tool round fails,
attach an observer to the OpenAI provider:

```haskell
import HaskLLM
import HaskLLM.OpenAI.GPT5 (OpenAI (..))

let provider = OpenAIWithObserver recordAttempt
-- recordAttempt :: AttemptObservation -> IO ()
response <- respondTextDetailed provider credentials model messages
  Nothing defaultRequestConfig
```

The same provider works with JSON, native tools, PandocChat and `ProviderConfig`
in a fallback chain. No request-configuration or call-signature changes are needed.
An observer can capture the caller's review/run identifier in its closure.

* Each received HTTP response is observed before status checks, assistant-output
  parsing or tool execution. Synchronous transport failures also produce an
  observation, with unknown usage. Neither HTTP status nor a timeout alone tells
  you whether the provider charged for the attempt.
* Observations include the returned model (falling back to the requested model),
  available request/response IDs, provider response status, HTTP/transport outcome
  and optional usage. Undecodable envelopes retain the HTTP status/request ID.
* Callbacks run synchronously and should be short. Their synchronous exceptions
  are logged to stderr and ignored; they never cause another model request or
  replay a tool. Cancellation propagates. This is best-effort delivery, not a
  durable ledger, and delivery isn't guaranteed after cancellation/process death.
* Account from **observations or final response usage**, never both: they overlap.
  Observations cover each attempt; final response usage covers the returned
  response (or successful tool rounds), not discarded retry/fallback attempts.
  Tool summaries sum known counts per field; they can be partial when a round
  omits usage. Keep the observations to distinguish missing counts from zero.

Cache-write field documentation:
https://developers.openai.com/api/docs/guides/prompt-caching#monitor-cache-performance

## Maintainers

Written and maintained with ❤️ by the team at [geoSurge.ai](https://geosurge.ai).

## License

WTFPL+
