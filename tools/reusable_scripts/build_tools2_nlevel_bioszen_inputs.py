#!/usr/bin/env python3
"""Build BIOSZEN-compatible n-level curves + platemap from tools2 workbooks."""

from __future__ import annotations

import argparse
import math
import re
import shutil
import sys
from datetime import datetime
from pathlib import Path
from typing import Any

import pandas as pd

STAIN_LABEL_MAP = {
    "FILIPIN": "Filipin",
    "MITO": "Mitotracker",
    "LYSOR": "Lysotracker Red",
    "LYSOG": "Lysotracker Green",
}

CONDITION_ORDER = [
    ("FILIPIN", "-"),
    ("FILIPIN", "siNEG"),
    ("FILIPIN", "siSOR"),
    ("MITO", "-"),
    ("MITO", "siNEG"),
    ("MITO", "siSOR"),
    ("LYSOR", "-"),
    ("LYSOR", "siNEG"),
    ("LYSOR", "siSOR"),
    ("LYSOG", "-"),
    ("LYSOG", "siNEG"),
    ("LYSOG", "siSOR"),
]

REQUIRED_PARAMS = [
    "intensity_mean_raw",
    "intensity_integrated_raw",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create BIOSZEN n-level curves + platemap from tools2 outputs."
    )
    parser.add_argument("--advanced", required=True, help="organized_results_advanced.xlsx")
    parser.add_argument("--output-dir", required=True, help="Output directory")
    parser.add_argument("--curves-name", default="Curvas_Int_n.xlsx")
    parser.add_argument("--platemap-name", default="platemap_Int_n.xlsx")
    return parser.parse_args()


def round_up_sig(value: float, digits: int = 2) -> float:
    if pd.isna(value):
        return float("nan")
    if value <= 0:
        return 0.0
    power = math.floor(math.log10(abs(value)))
    scale = 10 ** (power - (digits - 1))
    return math.ceil(value / scale) * scale


def backup_if_exists(path: Path) -> Path | None:
    if not path.exists():
        return None
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup = path.with_suffix(path.suffix + f".bak_{stamp}")
    shutil.copy2(path, backup)
    return backup


def gen_wells(n: int) -> list[str]:
    base_letters = [letter for letter in "ABCDEFGH" for _ in range(12)]
    base_numbers = [num for _ in range(8) for num in range(1, 13)]
    base = [f"{letter}{num}" for letter, num in zip(base_letters, base_numbers)]
    if n <= 96:
        return base[:n]
    extra = [f"H{num}" for num in range(13, 13 + (n - 96))]
    return base + extra


def normalize_n_rows(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["stain"] = out["stain"].astype(str).str.upper()
    out["condition"] = out["condition"].astype(str)
    out["n_label"] = out["n_label"].astype(str)
    out["n_numeric"] = pd.to_numeric(out["n_numeric"], errors="coerce")
    out = out[out["n_label"].str.match(r"^n\d+$", na=False)].copy()
    out = out[out["n_numeric"].notna()].copy()
    out["n_numeric"] = out["n_numeric"].astype(int)
    return out


def normalize_n_rows_no_stain(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["condition"] = out["condition"].astype(str)
    out["n_label"] = out["n_label"].astype(str)
    out["n_numeric"] = pd.to_numeric(out["n_numeric"], errors="coerce")
    out = out[out["n_label"].str.match(r"^n\d+$", na=False)].copy()
    out = out[out["n_numeric"].notna()].copy()
    out["n_numeric"] = out["n_numeric"].astype(int)
    return out


def build_params(advanced_path: Path) -> pd.DataFrame:
    key_cols = ["stain", "condition", "n_label", "n_numeric"]

    # Cell-level aggregation (as requested): one n = mean across all cells in that n.
    c_int = normalize_n_rows(pd.read_excel(advanced_path, sheet_name="cell_intensity"))
    c_spa = normalize_n_rows(pd.read_excel(advanced_path, sheet_name="cell_spatial"))
    c_mito = normalize_n_rows(pd.read_excel(advanced_path, sheet_name="cell_mito"))

    int_cols = [
        c
        for c in c_int.columns
        if c
        in [
            "area_pixels",
            "background_intensity",
            "intensity_mean_raw",
            "intensity_integrated_raw",
            "intensity_mean_corrected",
            "intensity_integrated_corrected",
        ]
    ]
    spa_cols = [
        c
        for c in c_spa.columns
        if c
        in [
            "perinuclear_mean",
            "peripheral_mean",
            "edge_enrichment_index",
            "radial_bin_00",
            "radial_bin_01",
            "radial_bin_02",
            "radial_bin_03",
            "radial_bin_04",
            "radial_bin_05",
            "radial_bin_06",
            "radial_bin_07",
            "radial_bin_08",
            "radial_bin_09",
        ]
    ]
    mito_cols = [
        c
        for c in c_mito.columns
        if c
        in [
            "mito_threshold",
            "mito_pixels",
            "mito_area_fraction",
            "n_mito_components",
            "fragmentation_index",
            "skeleton_length",
            "branch_points",
        ]
    ]

    p_int_agg = c_int.groupby(key_cols, as_index=False)[int_cols].mean(numeric_only=True)
    p_spa_agg = c_spa.groupby(key_cols, as_index=False)[spa_cols].mean(numeric_only=True)
    p_mito_agg = c_mito.groupby(key_cols, as_index=False)[mito_cols].mean(numeric_only=True)

    params = p_int_agg.merge(p_spa_agg, on=key_cols, how="outer")
    params = params.merge(p_mito_agg, on=key_cols, how="left")

    return params


def build_curves(advanced_path: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    key_cols = ["stain", "condition", "n_label", "n_numeric"]
    hist = normalize_n_rows(pd.read_excel(advanced_path, sheet_name="histogram_n"))

    hist["intensity"] = pd.to_numeric(hist["intensity"], errors="coerce")
    hist["count"] = pd.to_numeric(hist["count"], errors="coerce").fillna(0)
    hist = hist[hist["intensity"].notna()].copy()
    hist["intensity"] = hist["intensity"].astype(int)

    # Sum counts across all photos/analyses for each n-level key and intensity.
    agg = (
        hist.groupby(key_cols + ["intensity"], as_index=False)["count"]
        .sum()
        .sort_values(key_cols + ["intensity"])
    )

    # Build cumulative % vectors.
    curve_rows: list[dict[str, Any]] = []
    for key, sub in agg.groupby(key_cols, sort=False):
        sub = sub.set_index("intensity")["count"].reindex(range(256), fill_value=0)
        total = float(sub.sum())
        if total <= 0:
            cum_pct = [0.0] * 256
        else:
            cum_pct = (sub.cumsum() / total * 100.0).tolist()
        row = dict(zip(key_cols, key))
        for i in range(256):
            row[f"I{i:03d}"] = cum_pct[i]
        curve_rows.append(row)

    curve_long = pd.DataFrame(curve_rows)
    sheet2 = pd.DataFrame(
        [
            {
                "X_Max": 255,
                "Interval_X": 5,
                "Y_Max": 100,
                "Interval_Y": 20,
                "X_Title": "Intensity Levels",
                "Y_Title": "Percentage of Pixels per N",
            }
        ]
    )
    return curve_long, sheet2


def main() -> int:
    args = parse_args()
    advanced_path = Path(args.advanced)
    output_dir = Path(args.output_dir)
    curves_out = output_dir / args.curves_name
    platemap_out = output_dir / args.platemap_name

    if not advanced_path.exists():
        raise FileNotFoundError(f"Advanced workbook not found: {advanced_path}")

    params = build_params(advanced_path)
    curves_long, curves_sheet2 = build_curves(advanced_path)

    key_cols = ["stain", "condition", "n_label", "n_numeric"]
    merged = curves_long[key_cols].merge(params, on=key_cols, how="left")

    # Keep only keys that have required intensity params.
    for req in REQUIRED_PARAMS:
        if req not in merged.columns:
            raise ValueError(f"Missing required parameter column: {req}")
    valid = merged.dropna(subset=REQUIRED_PARAMS).copy()

    # Restrict curves to valid keys to keep exact alignment.
    valid_keys = valid[key_cols].drop_duplicates()
    curves_long = curves_long.merge(valid_keys, on=key_cols, how="inner")
    merged = curves_long[key_cols].merge(params, on=key_cols, how="left")

    order_rank = {k: i for i, k in enumerate(CONDITION_ORDER, start=1)}
    merged["_rank"] = merged.apply(
        lambda r: order_rank.get((str(r["stain"]).upper(), str(r["condition"])), 999), axis=1
    )
    merged["_n"] = pd.to_numeric(merged["n_numeric"], errors="coerce")
    merged = merged.sort_values(["_rank", "_n", "n_label"]).reset_index(drop=True)

    wells = gen_wells(len(merged))
    merged["Well"] = wells
    merged["Strain"] = merged["stain"].astype(str).str.upper().map(STAIN_LABEL_MAP)
    merged["Media"] = merged["condition"].astype(str)
    merged["Orden"] = merged.apply(
        lambda r: order_rank[(str(r["stain"]).upper(), str(r["condition"]))], axis=1
    )
    merged["BiologicalReplicate"] = merged["n_numeric"].astype(int)
    merged["Replicate"] = merged["BiologicalReplicate"]
    merged["TechnicalReplicate"] = "A"

    curve_bins = [f"I{i:03d}" for i in range(256)]
    curves_aligned = merged[key_cols + ["Well"]].merge(curves_long, on=key_cols, how="left")
    curves_sheet1: dict[str, Any] = {"Time": list(range(256))}
    for _, row in curves_aligned.iterrows():
        curves_sheet1[str(row["Well"])] = [row[b] for b in curve_bins]
    curves_sheet1_df = pd.DataFrame(curves_sheet1)

    # Platemap parameters
    id_cols = [
        "Well",
        "Strain",
        "Media",
        "Orden",
        "Replicate",
        "BiologicalReplicate",
        "TechnicalReplicate",
    ]
    exclude_cols = set(key_cols + ["_rank", "_n"] + id_cols)
    param_cols = [c for c in merged.columns if c not in exclude_cols]
    for c in param_cols:
        merged[c] = pd.to_numeric(merged[c], errors="coerce")

    datos_df = merged[id_cols + param_cols].copy()

    y_max = []
    for p in param_cols:
        mx = pd.to_numeric(datos_df[p], errors="coerce").max(skipna=True)
        y_max.append(round_up_sig(float(mx), digits=2) if pd.notna(mx) else float("nan"))
    interval = [v / 5 if pd.notna(v) and v > 0 else float("nan") for v in y_max]
    plot_settings = pd.DataFrame(
        {"Parameter": param_cols, "Y_Max": y_max, "Interval": interval, "Y_Title": param_cols}
    )

    # Validation
    curve_wells = [c for c in curves_sheet1_df.columns if c != "Time"]
    plate_wells = datos_df["Well"].astype(str).tolist()
    if curve_wells != plate_wells:
        raise ValueError("Well order mismatch between curves and platemap.")
    if set(datos_df["Media"].dropna().astype(str).unique()) != {"-", "siNEG", "siSOR"}:
        raise ValueError("Unexpected media labels in n-level platemap.")
    if float(curves_sheet1_df.iloc[-1, 1:].min()) < 99.0:
        raise ValueError("Some n-level curves do not end near 100%.")

    output_dir.mkdir(parents=True, exist_ok=True)
    curves_backup = backup_if_exists(curves_out)
    plate_backup = backup_if_exists(platemap_out)

    with pd.ExcelWriter(curves_out, engine="openpyxl") as writer:
        curves_sheet1_df.to_excel(writer, sheet_name="Sheet1", index=False)
        curves_sheet2.to_excel(writer, sheet_name="Sheet2", index=False)

    with pd.ExcelWriter(platemap_out, engine="openpyxl") as writer:
        datos_df.to_excel(writer, sheet_name="Datos", index=False)
        plot_settings.to_excel(writer, sheet_name="PlotSettings", index=False)

    summary = (
        datos_df.groupby(["Strain", "Media"], dropna=False)
        .size()
        .reset_index(name="n_rows")
        .sort_values(["Strain", "Media"])
    )
    print("Build completed successfully.")
    print(f"Advanced source:   {advanced_path}")
    print(f"Curves output:     {curves_out}")
    print(f"Platemap output:   {platemap_out}")
    if curves_backup:
        print(f"Curves backup:     {curves_backup}")
    if plate_backup:
        print(f"Platemap backup:   {plate_backup}")
    print(f"Total n-level rows: {len(datos_df)}")
    print(summary.to_string(index=False))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
