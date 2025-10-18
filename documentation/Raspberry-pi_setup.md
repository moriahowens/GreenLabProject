# Raspberry Pi LLM Server

This project sets up a Flask-based API on a Raspberry Pi to run and monitor LLMs through the Ollama framework. It consists of two python components:

- **Client-side script** – sends requests to the Raspberry Pi and stores the generated responses.  
- **Server-side script** – hosts the API endpoint, generates responses using one of several available models, and reports performance metrics such as CPU usage, memory consumption, and estimated energy usage.

---

## Raspberry Pi Setup 

Follow these steps to prepare your Raspberry Pi to run the LLM server.


### 1. Install Raspberry Pi OS

1. Download and install Raspberry Pi Imager from the [official website](https://www.raspberrypi.com/software/).
2. Insert the microSD card that will be on the pi into your computer.
3. Open Raspberry Pi Imager and:
   - **Choose OS:** Select Raspberry Pi OS (64-bit).
   - **Choose Storage:** Select your microSD card.
   - **Edit Settings** before writing the image:
     - **Set hostname:** e.g. `raspberrypi.local`
     - **Enable SSH:** Check “Enable SSH” and choose password authentication.
     - **Configure Wi-Fi** by entering your SSID and password.

4. Once the write completes, insert the sd card into your Raspberry Pi.


### 2. Boot the Raspberry Pi

1. Connect the Raspberry Pi to power.  
2. Wait about 1–2 minutes for the system to boot.


### 3. Find Pi on the Network

If the hostname was set to `raspberrypi.local`, you can connect directly using that name.  
Alternatively, find the Pi’s IP address through a network scanner.


### 4. Connect via SSH

From your PC, open a terminal and run:

```bash
ssh <username>@raspberrypi.local

```
---
## Server Side Script

1. Once logged onto the Raspberry Pi, install the Ollama framework using the following command:

```bash
curl -fsSL https://ollama.com/install.sh | sh

```
2. Install the Required models

After Ollama is installed, pull the three models used:

```bash
ollama pull qwen3:1.7b
ollama pull gemma3:1b
ollama pull llama3.2:1b

```

3. Install Python and Dependencies

4. Copy over the Server side python file onto the raspberry pi and start the Flask server with the following command:


```bash
python3 server-side.py

```
---

## Client Side Script
1. Ensure you replace **RASPBERRY-PI-IP** with the actual IP address of the Raspberry Pi. Then run the script:
```bash
python3 client-side.py

```

This will create a csv file where it will continuously save the run table