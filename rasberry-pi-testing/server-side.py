import psutil
import time
from flask import Flask, request, jsonify
from langchain_ollama import OllamaLLM

app = Flask(__name__)

MODELS = {
    "qwen": "qwen3:1.7b",
    "gemma": "gemma3:1b",
    "llama2": "llama3.2:1b",
}

IDLE_POWER_W = 2.5
MAX_POWER_W = 6.5

def estimate_energy(cpu_percent, duration_sec):
    avg_power = IDLE_POWER_W + (cpu_percent / 100.0) * (MAX_POWER_W - IDLE_POWER_W)
    return avg_power * duration_sec

@app.route("/generate/<model_name>", methods=["POST"])
def generate(model_name):
    model_id = MODELS.get(model_name.lower())
    if not model_id:
        return jsonify({"error": f"Model '{model_name}' not available"}), 404

    data = request.get_json()
    prompt = data.get("prompt")
    max_tokens = data.get("max_tokens")
    if not prompt:
        return jsonify({"error": "Missing 'prompt'"}), 400

    start_time = time.time()
    try:
        #num predict is used to set the max nuber of tokens, however the model can stop earlier if it feels like it should
        num_predict = max_tokens if max_tokens else None
        llm = OllamaLLM(model=model_id, num_predict=num_predict)

        stdout = llm.invoke(prompt)


        # memoery usage tracking
        ps_proc = psutil.Process()
        peak_mem_mb = ps_proc.memory_info().rss / (1024*1024) #to convert to mb

    except Exception as e:
        return jsonify({"error": str(e)}), 500

    end_time = time.time()
    execution_time = end_time - start_time
    cpu_percent = psutil.cpu_percent(interval=None)
    energy_joules = estimate_energy(cpu_percent, execution_time)

    metrics = {
        "execution_time_sec": execution_time,
        "cpu_percent": cpu_percent,
        "memory_usage_mb": peak_mem_mb,
        "energy_usage_joules": energy_joules,
        "input_size_kb": len(prompt.encode("utf-8")) / 1024,
        "output_size_kb": len(stdout.encode("utf-8")) / 1024 if stdout else 0
    }

    return jsonify({"output": stdout, "metrics": metrics})

import socket

if __name__ == "__main__":
    socket.setdefaulttimeout(600)

    app.run(host="0.0.0.0", port=5000, threaded=True)
