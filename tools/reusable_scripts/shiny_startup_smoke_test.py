#!/usr/bin/env python3
"""
Startup smoke test for BIOSZEN Shiny app.

What it verifies:
1) The app process starts and serves HTML on localhost within timeout.
2) The first response contains expected UI markers.
3) A second request succeeds to confirm it remains responsive.

Exit code:
- 0 on success
- 1 on failure
"""

from __future__ import annotations

import argparse
import json
import os
import random
import signal
import socket
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Optional, Tuple


def find_free_port(start: int = 20000, end: int = 40000) -> int:
    for _ in range(60):
        cand = random.randint(start, end)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                s.bind(("127.0.0.1", cand))
                return cand
            except OSError:
                continue
    raise RuntimeError("Could not find a free localhost port.")


def build_r_expr(app_dir: str, host: str, port: int) -> str:
    app_dir_esc = app_dir.replace("\\", "/").replace("'", "\\'")
    return (
        f"shiny::runApp('{app_dir_esc}', launch.browser=FALSE, "
        f"host='{host}', port={port})"
    )


def http_get(url: str, timeout: float) -> Tuple[int, str]:
    req = urllib.request.Request(url, method="GET")
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        body = resp.read().decode("utf-8", errors="replace")
        return int(resp.status), body


def terminate_process(proc: subprocess.Popen) -> None:
    if proc.poll() is not None:
        return
    try:
        if os.name == "nt":
            proc.send_signal(signal.CTRL_BREAK_EVENT)
            time.sleep(0.8)
        else:
            proc.terminate()
            time.sleep(0.5)
    except Exception:
        pass
    if proc.poll() is None:
        try:
            proc.terminate()
            proc.wait(timeout=5)
        except Exception:
            try:
                proc.kill()
            except Exception:
                pass


def run_smoke_test(
    app_dir: Path,
    rscript: str,
    host: str,
    port: Optional[int],
    startup_timeout: float,
    request_timeout: float,
) -> dict:
    resolved_app_dir = str(app_dir.resolve())
    chosen_port = int(port) if port else find_free_port()
    r_expr = build_r_expr(resolved_app_dir, host, chosen_port)
    cmd = [rscript, "-e", r_expr]

    creationflags = 0
    if os.name == "nt":
        creationflags = subprocess.CREATE_NEW_PROCESS_GROUP  # type: ignore[attr-defined]

    start_t = time.time()
    proc = subprocess.Popen(
        cmd,
        cwd=str(app_dir),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        creationflags=creationflags,
    )

    url = f"http://{host}:{chosen_port}/"
    markers = ["plotInteractivo", "Centro de Gr", "BIOSZEN"]

    first_ok = False
    second_ok = False
    first_status = None
    first_elapsed = None
    first_body = ""
    error_msg = None

    try:
        deadline = start_t + startup_timeout
        while time.time() < deadline:
            if proc.poll() is not None:
                stderr_preview = ""
                try:
                    stderr_preview = (proc.stderr.read() or "")[-1200:]
                except Exception:
                    pass
                error_msg = f"App process exited early with code {proc.returncode}. {stderr_preview}"
                break
            try:
                status, body = http_get(url, timeout=request_timeout)
                first_status = status
                first_elapsed = time.time() - start_t
                first_body = body
                first_ok = status == 200
                if first_ok:
                    break
            except urllib.error.URLError:
                time.sleep(0.4)
            except Exception:
                time.sleep(0.4)

        if first_ok:
            time.sleep(1.0)
            try:
                status2, body2 = http_get(url, timeout=request_timeout)
                second_ok = status2 == 200 and len(body2) > 2000
            except Exception:
                second_ok = False

    finally:
        terminate_process(proc)
        stdout_preview = ""
        stderr_preview = ""
        try:
            stdout_preview = (proc.stdout.read() or "")[-1200:]
        except Exception:
            pass
        try:
            stderr_preview = (proc.stderr.read() or "")[-1200:]
        except Exception:
            pass

    marker_hits = {m: (m in first_body) for m in markers}
    marker_ok = any(marker_hits.values())
    success = bool(first_ok and second_ok and marker_ok)

    return {
        "success": success,
        "url": url,
        "app_dir": resolved_app_dir,
        "rscript": rscript,
        "startup_timeout_seconds": startup_timeout,
        "request_timeout_seconds": request_timeout,
        "first_response_ok": first_ok,
        "second_response_ok": second_ok,
        "first_status_code": first_status,
        "time_to_first_response_seconds": first_elapsed,
        "marker_hits": marker_hits,
        "error": error_msg,
        "stdout_tail": stdout_preview,
        "stderr_tail": stderr_preview,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Smoke test BIOSZEN Shiny startup and responsiveness.")
    parser.add_argument("--app-dir", default=".", help="Path to Shiny app directory (default: current directory).")
    parser.add_argument("--rscript", default="C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe", help="Path to Rscript executable.")
    parser.add_argument("--host", default="127.0.0.1", help="Host to bind and test (default: 127.0.0.1).")
    parser.add_argument("--port", type=int, default=None, help="Port to use; auto-select if omitted.")
    parser.add_argument("--startup-timeout", type=float, default=90.0, help="Seconds to wait for first successful response.")
    parser.add_argument("--request-timeout", type=float, default=8.0, help="Per-request timeout in seconds.")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    app_dir = Path(args.app_dir)
    if not app_dir.exists():
        print(json.dumps({"success": False, "error": f"App directory not found: {app_dir}"}, ensure_ascii=False))
        return 1
    if not Path(args.rscript).exists():
        print(json.dumps({"success": False, "error": f"Rscript not found: {args.rscript}"}, ensure_ascii=False))
        return 1

    result = run_smoke_test(
        app_dir=app_dir,
        rscript=args.rscript,
        host=args.host,
        port=args.port,
        startup_timeout=float(args.startup_timeout),
        request_timeout=float(args.request_timeout),
    )
    print(json.dumps(result, ensure_ascii=False, indent=2))
    return 0 if result.get("success") else 1


if __name__ == "__main__":
    sys.exit(main())

