#!/bin/bash

# Quick test script for PandocChat functionality only
# Skips the slow card generation tests

set -e

echo "Setting up OpenAI API key..."
export OPENAI_API_KEY="$(passveil show platform.openai.com/api | head -n 1)"

if [ -z "$OPENAI_API_KEY" ]; then
    echo "Error: Could not retrieve API key from passveil"
    exit 1
fi

echo "Running PandocChat test only (skipping slow card generation tests)..."

# Build the test suite
cabal build haskllm-test

# Run only the PandocChat test by filtering with pattern matching
cabal test haskllm-test --test-options="-m PandocChat"

echo "PandocChat test completed!"
