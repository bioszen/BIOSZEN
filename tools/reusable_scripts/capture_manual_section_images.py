#!/usr/bin/env python3
"""
Capture high-resolution, section-focused screenshots for BIOSZEN manuals.

Outputs (PNG):
  - 01_app_home_overview.png
  - 02_plot_setup_layers.png
  - 03_filter_media_conditions.png
  - 04_filter_biological_replicates.png
  - 10_significance_annotations.png
  - 13_growth_parameters_workflow.png
"""

from __future__ import annotations

import argparse
import random
import signal
import socket
import subprocess
import time
import urllib.error
import urllib.request
from io import BytesIO
from pathlib import Path
from typing import Iterable, Optional, Tuple

from PIL import Image
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.edge.options import Options
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait


def find_free_port(start: int = 22000, end: int = 45000) -> int:
    for _ in range(100):
        port = random.randint(start, end)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            try:
                s.bind(("127.0.0.1", port))
                return port
            except OSError:
                continue
    raise RuntimeError("Could not find a free port.")


def wait_for_http_ok(url: str, timeout_sec: float = 180.0) -> None:
    deadline = time.time() + timeout_sec
    while time.time() < deadline:
        try:
            with urllib.request.urlopen(url, timeout=2.0) as resp:
                if int(resp.status) == 200:
                    return
        except (urllib.error.URLError, TimeoutError):
            time.sleep(0.4)
    raise TimeoutError(f"App did not become ready at {url} within {timeout_sec:.0f}s")


def stop_process(proc: subprocess.Popen) -> None:
    if proc.poll() is not None:
        return
    try:
        proc.send_signal(signal.CTRL_BREAK_EVENT)
        time.sleep(1.0)
    except Exception:
        pass
    if proc.poll() is None:
        try:
            proc.terminate()
            proc.wait(timeout=8)
        except Exception:
            try:
                proc.kill()
            except Exception:
                pass


def start_shiny_app(app_dir: Path, rscript: Path) -> Tuple[subprocess.Popen, str]:
    port = find_free_port()
    url = f"http://127.0.0.1:{port}/"
    app_dir_esc = str(app_dir.resolve()).replace("\\", "/").replace("'", "\\'")
    expr = (
        f"shiny::runApp('{app_dir_esc}', launch.browser=FALSE, "
        f"host='127.0.0.1', port={port})"
    )
    proc = subprocess.Popen(
        [str(rscript), "-e", expr],
        cwd=str(app_dir),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        creationflags=subprocess.CREATE_NEW_PROCESS_GROUP,
    )
    wait_for_http_ok(url, timeout_sec=180.0)
    return proc, url


def make_driver(window_size: str) -> webdriver.Edge:
    opts = Options()
    opts.add_argument("--headless=new")
    opts.add_argument("--disable-gpu")
    opts.add_argument(f"--window-size={window_size}")
    # Keep rendering crisp and deterministic.
    opts.add_argument("--force-color-profile=srgb")
    return webdriver.Edge(options=opts)


def wait_for_id(driver: webdriver.Edge, elem_id: str, timeout: float = 120.0) -> None:
    WebDriverWait(driver, timeout).until(EC.presence_of_element_located((By.ID, elem_id)))


def click_accordion(driver: webdriver.Edge, text_contains: str) -> bool:
    xpath = (
        "//button[contains(@class,'accordion-button')][contains(normalize-space(.),"
        f" \"{text_contains}\")]"
    )
    elems = driver.find_elements(By.XPATH, xpath)
    if not elems:
        return False
    btn = elems[0]
    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", btn)
    time.sleep(0.4)
    driver.execute_script("arguments[0].click();", btn)
    time.sleep(0.9)
    return True


def click_growth_tab(driver: webdriver.Edge) -> None:
    xpaths = [
        "//a[contains(normalize-space(.),'Growth Parameter Extraction')]",
        "//*[contains(@role,'tab')][contains(normalize-space(.),'Growth Parameter Extraction')]",
    ]
    for xp in xpaths:
        elems = driver.find_elements(By.XPATH, xp)
        if elems:
            driver.execute_script("arguments[0].click();", elems[0])
            time.sleep(1.2)
            return
    raise NoSuchElementException("Could not find Growth Parameter Extraction tab.")


def save_element_png(driver: webdriver.Edge, by: str, locator: str, out_path: Path) -> None:
    elem = driver.find_element(by, locator)
    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", elem)
    time.sleep(0.6)
    elem.screenshot(str(out_path))


def get_element_rect(driver: webdriver.Edge, element) -> Optional[dict]:
    r = driver.execute_script(
        "const b = arguments[0].getBoundingClientRect();"
        "return {x: b.x, y: b.y, w: b.width, h: b.height};",
        element,
    )
    if r and r["w"] > 0 and r["h"] > 0:
        return r
    return None


def get_rect_by_id(driver: webdriver.Edge, elem_id: str) -> Optional[dict]:
    elems = driver.find_elements(By.ID, elem_id)
    if not elems:
        return None
    return get_element_rect(driver, elems[0])


def save_rects_crop(
    driver: webdriver.Edge,
    rects: Iterable[dict],
    out_path: Path,
    margin: int = 20,
    target_aspect: Optional[float] = None,
) -> None:
    png = driver.get_screenshot_as_png()
    img = Image.open(BytesIO(png)).convert("RGB")

    rects = list(rects)
    if not rects:
        raise RuntimeError("No rectangles provided for crop.")

    visible_rects = []
    for r in rects:
        x0, y0 = r["x"], r["y"]
        x1, y1 = r["x"] + r["w"], r["y"] + r["h"]
        if x1 <= 0 or y1 <= 0 or x0 >= img.width or y0 >= img.height:
            continue
        visible_rects.append(r)
    if not visible_rects:
        raise RuntimeError("Target rectangles were found but not visible in viewport.")

    min_x = max(0, int(min(r["x"] for r in visible_rects)) - margin)
    min_y = max(0, int(min(r["y"] for r in visible_rects)) - margin)
    max_x = min(img.width, int(max(r["x"] + r["w"] for r in visible_rects)) + margin)
    max_y = min(img.height, int(max(r["y"] + r["h"] for r in visible_rects)) + margin)
    if max_x <= min_x or max_y <= min_y:
        raise RuntimeError(
            f"Invalid crop bounds after clamping -> ({min_x}, {min_y}, {max_x}, {max_y})"
        )

    if target_aspect is not None and target_aspect > 0:
        width = max_x - min_x
        height = max_y - min_y
        current_aspect = width / max(1, height)
        cx = (min_x + max_x) // 2
        cy = (min_y + max_y) // 2

        if current_aspect > target_aspect:
            # Too wide: crop width.
            new_w = int(height * target_aspect)
            half_w = max(1, new_w // 2)
            min_x = max(0, cx - half_w)
            max_x = min(img.width, min_x + new_w)
            min_x = max(0, max_x - new_w)
        elif current_aspect < target_aspect:
            # Too tall: crop height.
            new_h = int(width / target_aspect)
            half_h = max(1, new_h // 2)
            min_y = max(0, cy - half_h)
            max_y = min(img.height, min_y + new_h)
            min_y = max(0, max_y - new_h)

    cropped = img.crop((min_x, min_y, max_x, max_y))
    cropped.save(out_path, format="PNG", optimize=True)


def save_union_crop(
    driver: webdriver.Edge,
    ids: Iterable[str],
    out_path: Path,
    margin: int = 20,
    target_aspect: Optional[float] = None,
) -> None:
    rects = []
    for eid in ids:
        r = get_rect_by_id(driver, eid)
        if r is not None:
            rects.append(r)
    if not rects:
        raise RuntimeError(f"No visible rectangles found for ids: {list(ids)}")
    save_rects_crop(
        driver=driver,
        rects=rects,
        out_path=out_path,
        margin=margin,
        target_aspect=target_aspect,
    )


def capture_manual_images(
    app_dir: Path,
    output_dir: Path,
    rscript: Path,
    data_file: Path,
    curve_file: Optional[Path],
    window_size: str,
) -> None:
    if not data_file.exists():
        raise FileNotFoundError(f"Data file not found: {data_file}")
    if curve_file is not None and not curve_file.exists():
        raise FileNotFoundError(f"Curve file not found: {curve_file}")

    output_dir.mkdir(parents=True, exist_ok=True)

    proc: Optional[subprocess.Popen] = None
    driver: Optional[webdriver.Edge] = None
    try:
        proc, url = start_shiny_app(app_dir=app_dir, rscript=rscript)
        driver = make_driver(window_size=window_size)
        driver.get(url)

        wait_for_id(driver, "dataFile", timeout=120.0)
        driver.find_element(By.ID, "dataFile").send_keys(str(data_file.resolve()))
        if curve_file is not None:
            driver.find_element(By.ID, "curveFile").send_keys(str(curve_file.resolve()))
        wait_for_id(driver, "strain-selectized", timeout=120.0)
        time.sleep(4.0)

        # 01: Home overview (tabs + setup + main plot), with realistic 16:9 framing.
        driver.execute_script("window.scrollTo(0, 0);")
        time.sleep(0.8)
        home_rects = []
        for tab_text in ("Plots and Stats", "Growth Parameter Extraction"):
            tab_elems = driver.find_elements(
                By.XPATH,
                f"//a[contains(normalize-space(.), '{tab_text}')]",
            )
            if tab_elems:
                r = get_element_rect(driver, tab_elems[0])
                if r is not None:
                    home_rects.append(r)

        for eid in ("plot_setup_core", "plotInteractivo"):
            r = get_rect_by_id(driver, eid)
            if r is not None:
                home_rects.append(r)

        # Fallback in case tab links are unavailable for any reason.
        if not home_rects:
            save_union_crop(
                driver,
                ids=("mainTabs", "plot_setup_core", "plotInteractivo"),
                out_path=output_dir / "01_app_home_overview.png",
                margin=28,
                target_aspect=16 / 9,
            )
        else:
            save_rects_crop(
                driver=driver,
                rects=home_rects,
                out_path=output_dir / "01_app_home_overview.png",
                margin=42,
                target_aspect=16 / 9,
            )

        # 02: Plot setup/layers section.
        save_element_png(
            driver,
            by=By.ID,
            locator="plot_setup_core",
            out_path=output_dir / "02_plot_setup_layers.png",
        )

        # 03: Media conditions/filter section.
        show_medios = driver.find_element(By.ID, "showMedios")
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", show_medios)
        time.sleep(0.8)
        save_union_crop(
            driver,
            ids=("showMedios", "repsGlobalPanel"),
            out_path=output_dir / "03_filter_media_conditions.png",
            margin=60,
        )

        # 04: Biological replicate filtering section.
        click_accordion(driver, "Replicates by Media")
        save_element_png(
            driver,
            by=By.ID,
            locator="repsPanel",
            out_path=output_dir / "04_filter_biological_replicates.png",
        )

        # 10: Significance/annotation controls section.
        click_accordion(driver, "Statistical Analysis")
        sig_label = driver.find_element(By.ID, "sig_label")
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", sig_label)
        time.sleep(0.8)
        save_union_crop(
            driver,
            ids=(
                "sig_label",
                "add_sig",
                "sig_update_label",
                "remove_sig",
                "sig_move_up",
                "sig_textsize",
            ),
            out_path=output_dir / "10_significance_annotations.png",
            margin=50,
        )

        # 13: Growth workflow section.
        click_growth_tab(driver)
        time.sleep(1.2)
        save_union_crop(
            driver,
            ids=("runGrowth", "stopGrowth", "downloadGrowthZip", "growthStatus"),
            out_path=output_dir / "13_growth_parameters_workflow.png",
            margin=240,
        )

    finally:
        if driver is not None:
            try:
                driver.quit()
            except Exception:
                pass
        if proc is not None:
            stop_process(proc)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Capture high-resolution section screenshots for BIOSZEN manuals."
    )
    parser.add_argument(
        "--app-dir",
        default=".",
        help="BIOSZEN app root directory (default: current directory).",
    )
    parser.add_argument(
        "--output-dir",
        default="inst/app/www/manual_images",
        help="Output directory for manual PNG files.",
    )
    parser.add_argument(
        "--rscript",
        default=r"C:\Program Files\R\R-4.5.2\bin\Rscript.exe",
        help="Path to Rscript executable.",
    )
    parser.add_argument(
        "--data-file",
        default="inst/app/www/reference_files/Ejemplo_platemap_parametros.xlsx",
        help="Reference data workbook used to initialize app state.",
    )
    parser.add_argument(
        "--curve-file",
        default="inst/app/www/reference_files/Ejemplo_curvas.xlsx",
        help="Reference curves workbook used to initialize app state.",
    )
    parser.add_argument(
        "--window-size",
        default="4600,2800",
        help="Headless browser window size as WIDTH,HEIGHT (default: 4600,2800).",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    app_dir = Path(args.app_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    rscript = Path(args.rscript).resolve()
    data_file = Path(args.data_file).resolve()
    curve_file = Path(args.curve_file).resolve() if args.curve_file else None

    capture_manual_images(
        app_dir=app_dir,
        output_dir=output_dir,
        rscript=rscript,
        data_file=data_file,
        curve_file=curve_file,
        window_size=args.window_size,
    )
    print(f"Manual images updated in: {output_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
