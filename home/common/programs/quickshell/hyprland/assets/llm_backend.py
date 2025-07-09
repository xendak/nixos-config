# caelestia/chatbox/llm_backend.py
import os
import sys
import argparse
import requests
import google.generativeai as genai

# --- Configuration ---
# Choose your backend: "gemini" or "local"
LLM_BACKEND = "gemini"  # or "local"

# Gemini API Key
# It's recommended to set this as an environment variable
GEMINI_API_KEY = os.environ.get("GEMINI_API_KEY", "YOUR_API_KEY")

# Local LLM API endpoint (example for Ollama)
LOCAL_LLM_URL = "http://localhost:11434/api/generate"
LOCAL_LLM_MODEL = "llama2" # The model you are serving with Ollama

# --- Main Logic ---

def query_gemini(prompt):
    """Queries the Gemini API."""
    try:
        genai.configure(api_key=GEMINI_API_KEY)
        model = genai.GenerativeModel('gemini-pro')
        response = model.generate_content(prompt)
        return response.text
    except Exception as e:
        return f"Error communicating with Gemini: {e}"

def query_local_llm(prompt):
    """Queries a local LLM API."""
    try:
        response = requests.post(
            LOCAL_LLM_URL,
            json={"model": LOCAL_LLM_MODEL, "prompt": prompt, "stream": False},
            headers={"Content-Type": "application/json"}
        )
        response.raise_for_status()
        return response.json()["response"]
    except requests.exceptions.RequestException as e:
        return f"Error communicating with local LLM: {e}"

if __name__ == "__main__":
    if len(sys.argv) > 1:
        user_prompt = " ".join(sys.argv[1:])

        if LLM_BACKEND == "gemini":
            if GEMINI_API_KEY == "YOUR_API_KEY":
                print("Please set your Gemini API key in the script or as an environment variable.")
            else:
                response = query_gemini(user_prompt)
                print(response)
        elif LLM_BACKEND == "local":
            response = query_local_llm(user_prompt)
            print(response)
        else:
            print(f"Invalid LLM_BACKEND configured: {LLM_BACKEND}")

    else:
        print("No prompt provided.")
