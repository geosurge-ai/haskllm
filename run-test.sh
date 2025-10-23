#!/usr/bin/env bash

# Script to run the HaskLLM test suite with API keys from passveil
set -e

echo "Setting up OpenAI API key..."
export OPENAI_API_KEY="$(passveil show platform.openai.com/api | head -n 1)"

if [ -z "$OPENAI_API_KEY" ]; then
    echo "Error: Could not retrieve OpenAI API key from passveil"
    exit 1
fi

echo "Setting up blood-money API key..."
export BLOOD_MONEY_API_KEY="$(passveil show geosurge.ai/blood-money/api | head -n 1)"

if [ -z "$BLOOD_MONEY_API_KEY" ]; then
    echo "Error: Could not retrieve blood-money API key from passveil"
    exit 1
fi

echo "Running HaskLLM test suite..."
cabal test haskllm-test --test-show-details=direct
