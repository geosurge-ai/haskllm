#!/usr/bin/env bash

# Script to run the HaskLLM test suite with API key from passveil
set -e

echo "Setting up OpenAI API key..."
export OPENAI_API_KEY="$(passveil show platform.openai.com/api | head -n 1)"

if [ -z "$OPENAI_API_KEY" ]; then
    echo "Error: Could not retrieve API key from passveil"
    exit 1
fi

echo "Running HaskLLM test suite..."
cabal test haskllm-test --test-show-details=direct
