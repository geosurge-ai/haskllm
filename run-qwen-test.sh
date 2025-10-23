#!/usr/bin/env bash

# Quick test script for Qwen integration only
set -e

echo "Setting up blood-money API key..."
export BLOOD_MONEY_API_KEY="$(passveil show geosurge.ai/blood-money/api | head -n 1)"

if [ -z "$BLOOD_MONEY_API_KEY" ]; then
    echo "Error: Could not retrieve blood-money API key from passveil"
    exit 1
fi

echo "Running Qwen integration test only..."
cabal test haskllm-test --test-show-details=direct --test-options='--match "Qwen Integration"'

