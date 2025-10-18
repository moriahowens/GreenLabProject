import requests
import pandas as pd
import time
import os

BASE_URL = "http://<RASBERRY-PI-IP-:5000/generate"

MODELS = ["gemma","qwen","llama2" ]

SMALL_PROMPT = "Write a short poem about AI."
LARGE_PROMPT = "Write a detailed essay on the impact of artificial intelligence. Include examples and references."

SMALL_MAX_TOKENS = 20
LARGE_MAX_TOKENS = 200

RUNS = 30
CSV_FILE = "model_metrics.csv"

if os.path.exists(CSV_FILE):
    df = pd.read_csv(CSV_FILE)
    results = df.to_dict("records")
else:
    results = []

for model in MODELS:
    for size, prompt, max_tokens in [
        ("large", LARGE_PROMPT, LARGE_MAX_TOKENS),
        ("small", SMALL_PROMPT, SMALL_MAX_TOKENS)
    ]:
        for i in range(1, RUNS + 1):
            if any(r["model"] == model and r["prompt_size"] == size and r["run"] == i for r in results):
                continue

            payload = {"prompt": prompt, "max_tokens": max_tokens}
            start = time.time()
            try:
                response = requests.post(f"{BASE_URL}/{model}", json=payload, timeout=600)
                data = response.json()
                metrics = data.get("metrics", {})
            except Exception as e:
                metrics = {
                    "execution_time_sec": None,
                    "cpu_percent": None,
                    "memory_usage_mb": None,
                    "energy_usage_joules": None,
                    "input_size_kb": len(prompt.encode("utf-8")) / 1024,
                    "output_size_kb": None
                }

            metrics.update({
                "model": model,
                "prompt_size": size,
                "run": i
            })
            results.append(metrics)

            pd.DataFrame(results).to_csv(CSV_FILE, index=False)

            print(f"Completed: Model={model}, Size={size}, Run={i}")
            time.sleep(1)

print("All runs completed!")
