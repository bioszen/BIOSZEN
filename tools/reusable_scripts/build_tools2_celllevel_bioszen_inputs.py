#!/usr/bin/env python3
"""Build BIOSZEN-compatible per-cell curves + platemap from tools2 workbooks."""

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

BIO_REP_MAP = {
    "rep1": 6,
    "rep2": 7,
}

REQUIRED_INTENSITY_PARAMS = [
    "intensity_mean_raw",
    "intensity_integrated_raw",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]

INTENSITY_PARAM_CANDIDATES = [
    "area_pixels",
    "background_intensity",
    "intensity_mean_raw",
    "intensity_integrated_raw",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]

SPATIAL_PARAM_CANDIDATES = [
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

MITO_PARAM_CANDIDATES = [
    "mito_threshold",
    "mito_pixels",
    "mito_area_fraction",
    "n_mito_components",
    "fragmentation_index",
    "skeleton_length",
    "branch_points",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create BIOSZEN per-cell curves + platemap from tools2 outputs."
    )
    parser.add_argument("--advanced", required=True, help="organized_results_advanced.xlsx")
    parser.add_argument(
        "--comparative",
        required=True,
        help="informe_comparativo_tools2_selector.xlsx",
    )
    parser.add_argument(
        "--output-dir",
        required=True,
        help="Directory for final Curvas_Int.xlsx and platemap_Int.xlsx",
    )
    parser.add_argument("--curves-name", default="Curvas_Int.xlsx")
    parser.add_argument("--platemap-name", default="platemap_Int.xlsx")
    return parser.parse_args()


def normalize_path_token(value: Any) -> str:
    out = str(value).upper().strip()
    out = out.replace(".TIF", "").replace(".ND2", "")
    out = re.sub(r"\s+", "", out)
    out = re.sub(r"[^A-Z0-9]", "", out)
    return out


def key_base(
    stain: Any, group: Any, rep: Any, archivo: Any, cell_id: Any
) -> tuple[Any, ...]:
    cell = pd.to_numeric(pd.Series([cell_id]), errors="coerce").iloc[0]
    if pd.isna(cell):
        return ("", "", "", "", pd.NA)
    return (
        str(stain).strip().upper(),
        str(group).strip(),
        str(rep).strip().lower(),
        normalize_path_token(archivo),
        int(cell),
    )


def add_occurrence_index(df: pd.DataFrame, key_col: str) -> pd.DataFrame:
    out = df.copy()
    out["_occ"] = out.groupby(key_col, dropna=False).cumcount() + 1
    return out


def gen_wells(n: int) -> list[str]:
    base_letters = [letter for letter in "ABCDEFGH" for _ in range(12)]
    base_numbers = [num for _ in range(8) for num in range(1, 13)]
    base = [f"{letter}{num}" for letter, num in zip(base_letters, base_numbers)]
    if n <= 96:
        return base[:n]
    extra = [f"H{num}" for num in range(13, 13 + (n - 96))]
    return base + extra


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


def load_curves_df(comparative_path: Path) -> tuple[pd.DataFrame, list[str]]:
    curves = pd.read_excel(comparative_path, sheet_name="histograma_celula_cum_bins")
    bin_cols = [c for c in curves.columns if re.fullmatch(r"I\d{3}", str(c))]
    if len(bin_cols) != 256:
        raise ValueError(
            f"Expected 256 cumulative bins (I000-I255), found {len(bin_cols)}."
        )
    bin_cols = sorted(bin_cols, key=lambda x: int(str(x)[1:]))

    if "photo_include" in curves.columns:
        include_flag = pd.to_numeric(curves["photo_include"], errors="coerce")
        if (include_flag == 1).any():
            curves = curves[include_flag == 1].copy()

    curves["key_base"] = curves.apply(
        lambda r: key_base(
            r.get("stain"), r.get("group"), r.get("rep"), r.get("archivo"), r.get("cell_id")
        ),
        axis=1,
    )
    curves = add_occurrence_index(curves, "key_base")
    curves["join_key"] = list(zip(curves["key_base"], curves["_occ"]))
    return curves, bin_cols


def select_existing(df: pd.DataFrame, cols: list[str]) -> list[str]:
    return [c for c in cols if c in df.columns]


def load_param_table(advanced_path: Path, sheet_name: str) -> pd.DataFrame:
    df = pd.read_excel(advanced_path, sheet_name=sheet_name)
    group_col = "condition" if "condition" in df.columns else "group"
    df["key_base"] = df.apply(
        lambda r: key_base(
            r.get("stain"), r.get(group_col), r.get("rep"), r.get("archivo"), r.get("cell_id")
        ),
        axis=1,
    )
    df = add_occurrence_index(df, "key_base")
    df["join_key"] = list(zip(df["key_base"], df["_occ"]))
    return df


def main() -> int:
    args = parse_args()
    advanced_path = Path(args.advanced)
    comparative_path = Path(args.comparative)
    output_dir = Path(args.output_dir)
    curves_out = output_dir / args.curves_name
    platemap_out = output_dir / args.platemap_name

    if not advanced_path.exists():
        raise FileNotFoundError(f"Advanced workbook not found: {advanced_path}")
    if not comparative_path.exists():
        raise FileNotFoundError(f"Comparative workbook not found: {comparative_path}")

    curves_df, bin_cols = load_curves_df(comparative_path)

    # Build per-cell cumulative percentage curves
    bin_values = curves_df[bin_cols].apply(pd.to_numeric, errors="coerce").fillna(0)
    final_values = pd.to_numeric(curves_df[bin_cols[-1]], errors="coerce").fillna(0)
    denom = final_values.where(final_values > 0, 1.0)
    curves_pct = bin_values.div(denom, axis=0) * 100.0

    # Parameter sources (cell-level)
    cell_intensity = load_param_table(advanced_path, "cell_intensity")
    cell_spatial = load_param_table(advanced_path, "cell_spatial")
    cell_mito = load_param_table(advanced_path, "cell_mito")

    param_master = curves_df[
        ["join_key", "key_base", "analysis", "archivo", "group", "rep", "stain", "cell_id"]
    ].copy()
    param_master = param_master.rename(columns={"group": "condition"})

    intensity_cols = select_existing(cell_intensity, INTENSITY_PARAM_CANDIDATES)
    spatial_cols = select_existing(cell_spatial, SPATIAL_PARAM_CANDIDATES)
    mito_cols = select_existing(cell_mito, MITO_PARAM_CANDIDATES)

    param_master = param_master.merge(
        cell_intensity[["join_key"] + intensity_cols], on="join_key", how="left"
    )
    param_master = param_master.merge(
        cell_spatial[["join_key"] + spatial_cols], on="join_key", how="left"
    )
    param_master = param_master.merge(
        cell_mito[["join_key"] + mito_cols], on="join_key", how="left"
    )

    for req_col in REQUIRED_INTENSITY_PARAMS:
        if req_col not in param_master.columns:
            raise ValueError(f"Required parameter column missing: {req_col}")
        if param_master[req_col].isna().any():
            missing = int(param_master[req_col].isna().sum())
            raise ValueError(
                f"Required parameter '{req_col}' has {missing} missing values after join."
            )

    # Ordering and metadata mapping
    order_rank = {k: i for i, k in enumerate(CONDITION_ORDER, start=1)}
    param_master["_rank"] = param_master.apply(
        lambda r: order_rank.get((str(r["stain"]).upper(), str(r["condition"])), 999),
        axis=1,
    )
    rep_rank = {"rep1": 1, "rep2": 2}
    param_master["_rep_rank"] = param_master["rep"].map(rep_rank).fillna(9)
    param_master["_row_id"] = range(len(param_master))
    param_master = param_master.sort_values(["_rank", "_rep_rank", "_row_id"]).reset_index(
        drop=True
    )

    wells = gen_wells(len(param_master))
    param_master["Well"] = wells
    param_master["Strain"] = param_master["stain"].astype(str).str.upper().map(STAIN_LABEL_MAP)
    param_master["Media"] = param_master["condition"].astype(str)
    param_master["Orden"] = param_master.apply(
        lambda r: order_rank[(str(r["stain"]).upper(), str(r["condition"]))], axis=1
    )
    param_master["BiologicalReplicate"] = (
        param_master["rep"].astype(str).str.lower().map(BIO_REP_MAP)
    )
    param_master["Replicate"] = param_master["BiologicalReplicate"]
    param_master["TechnicalReplicate"] = (
        param_master.groupby(["Strain", "Media", "BiologicalReplicate"]).cumcount() + 1
    )

    if param_master["BiologicalReplicate"].isna().any():
        bad = param_master[param_master["BiologicalReplicate"].isna()][["rep"]].drop_duplicates()
        raise ValueError(
            "Found unexpected rep labels (cannot map to n6/n7):\n" + bad.to_string(index=False)
        )

    # Build curves sheet1 aligned with final well order
    curves_pct = curves_pct.loc[param_master.index].reset_index(drop=True)
    time_axis = [int(c[1:]) for c in bin_cols]
    curves_sheet1: dict[str, Any] = {"Time": time_axis}
    for idx, well in enumerate(param_master["Well"]):
        curves_sheet1[well] = curves_pct.iloc[idx].to_list()
    curves_sheet1_df = pd.DataFrame(curves_sheet1)
    curves_sheet2_df = pd.DataFrame(
        [
            {
                "X_Max": 255,
                "Interval_X": 5,
                "Y_Max": 100,
                "Interval_Y": 20,
                "X_Title": "Intensity Levels",
                "Y_Title": "Percentage of Pixels per Cell",
            }
        ]
    )

    # Build platemap
    id_cols = [
        "Well",
        "Strain",
        "Media",
        "Orden",
        "Replicate",
        "BiologicalReplicate",
        "TechnicalReplicate",
    ]
    metadata_cols = ["analysis", "archivo", "condition", "rep", "stain", "cell_id"]
    param_cols = [c for c in param_master.columns if c not in id_cols + metadata_cols + [
        "join_key",
        "key_base",
        "_rank",
        "_rep_rank",
        "_row_id",
    ]]
    # Keep parameter columns numeric when possible.
    for c in param_cols:
        param_master[c] = pd.to_numeric(param_master[c], errors="coerce")

    datos_df = param_master[id_cols + param_cols].copy()
    y_max = []
    for p in param_cols:
        col = pd.to_numeric(datos_df[p], errors="coerce")
        mx = col.max(skipna=True)
        y_max.append(round_up_sig(float(mx), digits=2) if pd.notna(mx) else float("nan"))
    interval = [v / 5 if pd.notna(v) and v > 0 else float("nan") for v in y_max]
    plot_settings_df = pd.DataFrame(
        {"Parameter": param_cols, "Y_Max": y_max, "Interval": interval, "Y_Title": param_cols}
    )

    # Final validations
    curve_wells = [c for c in curves_sheet1_df.columns if c != "Time"]
    plate_wells = datos_df["Well"].astype(str).tolist()
    if curve_wells != plate_wells:
        raise ValueError("Curve wells and platemap wells are not aligned in the same order.")
    if set(datos_df["Media"].dropna().astype(str).unique()) != {"-", "siNEG", "siSOR"}:
        raise ValueError("Media labels are not exactly {'-', 'siNEG', 'siSOR'}.")
    if set(datos_df["BiologicalReplicate"].astype(int).unique()) != {6, 7}:
        raise ValueError("BiologicalReplicate values are not exactly {6, 7}.")

    output_dir.mkdir(parents=True, exist_ok=True)
    curves_backup = backup_if_exists(curves_out)
    platemap_backup = backup_if_exists(platemap_out)

    with pd.ExcelWriter(curves_out, engine="openpyxl") as writer:
        curves_sheet1_df.to_excel(writer, sheet_name="Sheet1", index=False)
        curves_sheet2_df.to_excel(writer, sheet_name="Sheet2", index=False)

    with pd.ExcelWriter(platemap_out, engine="openpyxl") as writer:
        datos_df.to_excel(writer, sheet_name="Datos", index=False)
        plot_settings_df.to_excel(writer, sheet_name="PlotSettings", index=False)

    summary_counts = (
        datos_df.groupby(["Strain", "Media", "BiologicalReplicate"], dropna=False)
        .size()
        .reset_index(name="n_cells")
        .sort_values(["Strain", "Media", "BiologicalReplicate"])
    )
    print("Build completed successfully.")
    print(f"Advanced source:   {advanced_path}")
    print(f"Comparative source:{comparative_path}")
    print(f"Curves output:     {curves_out}")
    print(f"Platemap output:   {platemap_out}")
    if curves_backup:
        print(f"Curves backup:     {curves_backup}")
    if platemap_backup:
        print(f"Platemap backup:   {platemap_backup}")
    print(f"Total wells: {len(datos_df)}")
    print("Counts by strain/media/biorep:")
    print(summary_counts.to_string(index=False))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
