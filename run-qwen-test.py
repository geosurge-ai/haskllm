#!/usr/bin/env python3
"""
Quick test script for Qwen integration only.
Provisions credentials with passveil and runs a simple hello world test.
"""

import subprocess
import sys
import os


def get_passveil_secret(path: str) -> str:
    """Get a secret from passveil."""
    try:
        result = subprocess.run(
            ["passveil", "show", path],
            capture_output=True,
            text=True,
            check=True
        )
        # Get the first line of output
        lines = result.stdout.strip().split('\n')
        if lines:
            return lines[0]
        else:
            raise ValueError(f"Empty output from passveil for {path}")
    except subprocess.CalledProcessError as e:
        print(f"Error: Could not retrieve secret from passveil: {path}", file=sys.stderr)
        print(f"stderr: {e.stderr}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError:
        print("Error: passveil not found. Please ensure passveil is installed and in PATH.", file=sys.stderr)
        sys.exit(1)


def main():
    print("Setting up blood-money API key...")
    api_key = get_passveil_secret("geosurge.ai/blood-money/api")

    if not api_key:
        print("Error: Could not retrieve blood-money API key from passveil", file=sys.stderr)
        sys.exit(1)

    # Set environment variable
    os.environ["BLOOD_MONEY_API_KEY"] = api_key

    print("Running Qwen integration test...")
    print()

    # Import and run the test
    from haskllm_py import Credentials, ChatMessage
    from haskllm_py.vllm_qwen import Qwen

    # Use empty credentials - will fall back to env vars and default URL
    creds = Credentials({})
    messages = [ChatMessage("user", "Hello! Please respond with a brief greeting.")]
    model_name = "Qwen/Qwen2.5-32B-Instruct"

    try:
        # Make actual request to production
        provider = Qwen()
        response = provider.respond_text(creds, model_name, messages)

        print("Response:")
        print(response)
        print()

        # Verify we got a non-empty response
        if not response:
            print("ERROR: Received empty response from Qwen", file=sys.stderr)
            sys.exit(1)

        print("✓ Test passed! Qwen integration is working.")

    except Exception as e:
        print(f"ERROR: Test failed with exception: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()

