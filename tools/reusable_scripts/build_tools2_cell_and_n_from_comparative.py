#!/usr/bin/env python3
"""Build BIOSZEN shared curves + cell/n platemaps from comparative and advanced cell-level sheets."""

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

ADV_INTENSITY_COLS = [
    "area_pixels",
    "centroid_y",
    "centroid_x",
    "background_intensity",
    "intensity_mean_raw",
    "intensity_integrated_raw",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]

ADV_BG_COLS = [
    "background_intensity",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]

ADV_SPATIAL_COLS = [
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

ADV_MITO_COLS = [
    "mito_threshold",
    "mito_pixels",
    "mito_area_fraction",
    "n_mito_components",
    "fragmentation_index",
    "skeleton_length",
    "branch_points",
]

COLOC_METRICS = ["pearson", "manders_m1", "manders_m2", "overlap_coeff"]

COMP_INTENSITY_RENAME = {
    "area_pixels": "comp_area_pixels",
    "intensity_mean": "comp_intensity_mean",
    "intensity_integrated": "comp_intensity_integrated",
    "mean_sq": "comp_mean_sq",
    "integrated_sq": "comp_integrated_sq",
}

COMP_HIST_RENAME = {
    "mean_hist_intensity": "comp_mean_hist_intensity",
    "sd_hist_intensity": "comp_sd_hist_intensity",
    "total_pixels": "comp_total_pixels",
}

REQUIRED_ADV_PARAMS = [
    "intensity_mean_raw",
    "intensity_integrated_raw",
    "intensity_mean_corrected",
    "intensity_integrated_corrected",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Create one shared Curvas file plus two Platemap files: "
            "cell-level biological replicates and n-level biological replicates."
        )
    )
    parser.add_argument("--advanced", required=True, help="organized_results_advanced.xlsx")
    parser.add_argument("--comparative", required=True, help="informe_comparativo_tools2_selector.xlsx")
    parser.add_argument("--output-dir", required=True, help="Output directory")
    parser.add_argument("--curves-name", default="Curvas_Int.xlsx")
    parser.add_argument("--platemap-cell-name", default="platemap_Int_cell.xlsx")
    parser.add_argument("--platemap-n-name", default="platemap_Int_n.xlsx")
    return parser.parse_args()


def backup_if_exists(path: Path) -> Path | None:
    if not path.exists():
        return None
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup = path.with_suffix(path.suffix + f".bak_{stamp}")
    shutil.copy2(path, backup)
    return backup


def round_up_sig(value: float, digits: int = 2) -> float:
    if pd.isna(value):
        return float("nan")
    if value <= 0:
        return 0.0
    power = math.floor(math.log10(abs(value)))
    scale = 10 ** (power - (digits - 1))
    return math.ceil(value / scale) * scale


def gen_wells(n: int) -> list[str]:
    base_letters = [letter for letter in "ABCDEFGH" for _ in range(12)]
    base_numbers = [num for _ in range(8) for num in range(1, 13)]
    base = [f"{letter}{num}" for letter, num in zip(base_letters, base_numbers)]
    if n <= 96:
        return base[:n]
    extra = [f"H{num}" for num in range(13, 13 + (n - 96))]
    return base + extra


def normalize_path_token(value: Any) -> str:
    out = str(value).upper().strip()
    out = out.replace(".TIF", "").replace(".ND2", "")
    out = re.sub(r"\s+", "", out)
    out = re.sub(r"[^A-Z0-9]", "", out)
    return out


def sanitize_token(value: Any) -> str:
    out = str(value).strip().lower()
    out = re.sub(r"[^a-z0-9]+", "_", out)
    out = re.sub(r"_+", "_", out).strip("_")
    return out or "na"


def extract_n_from_file(archivo: Any, rep: Any) -> tuple[str, int, str]:
    raw = str(archivo).replace("\\", "/")
    base = raw.split("/")[-1]
    no_ext = re.sub(r"\.[^.]+$", "", base)

    # Only explicit n-tokens count as n (for example: "n2", "n 2", "n-2", "n_2").
    match = re.search(r"(?i)(?:^|[^a-z0-9])n\s*[-_ ]?\s*(\d{1,3})(?=[^a-z0-9]|$)", no_ext)
    if match:
        n_val = int(match.group(1))
        return f"n{n_val}", n_val, "filename_n_token"

    rep_l = str(rep).strip().lower()
    if rep_l == "rep2":
        return "n7", 7, "fallback_rep2"
    return "n6", 6, "fallback_rep1_or_other"


def add_n_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    parsed = out.apply(lambda r: extract_n_from_file(r.get("archivo"), r.get("rep")), axis=1)
    out["n_label"] = parsed.map(lambda t: t[0])
    out["n_numeric"] = parsed.map(lambda t: t[1]).astype(int)
    out["n_source"] = parsed.map(lambda t: t[2])
    return out


def _mode_int(values: pd.Series) -> int | None:
    nums = pd.to_numeric(values, errors="coerce").dropna()
    if nums.empty:
        return None
    nums = nums.astype(int)
    modes = nums.mode()
    if len(modes):
        return int(modes.iloc[0])
    return int(nums.iloc[0])


def key_base(stain: Any, group: Any, rep: Any, archivo: Any, cell_id: Any) -> tuple[Any, ...]:
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


def add_occurrence(df: pd.DataFrame, key_col: str) -> pd.DataFrame:
    out = df.copy()
    out["_occ"] = out.groupby(key_col, dropna=False).cumcount() + 1
    out["join_key"] = list(zip(out[key_col], out["_occ"]))
    return out


def select_existing(df: pd.DataFrame, cols: list[str]) -> list[str]:
    return [c for c in cols if c in df.columns]


def prep_join_keys(df: pd.DataFrame, group_col: str, stain_override: str | None = None) -> pd.DataFrame:
    out = df.copy()
    if stain_override is not None:
        out["stain"] = stain_override
    out["key_base"] = out.apply(
        lambda r: key_base(
            r.get("stain"),
            r.get(group_col),
            r.get("rep"),
            r.get("archivo"),
            r.get("cell_id"),
        ),
        axis=1,
    )
    return add_occurrence(out, "key_base")


def build_advanced_n_maps(advanced_path: Path) -> tuple[dict[Any, int], dict[tuple[str, str, str, str], int]]:
    if not advanced_path.exists():
        return {}, {}
    df = pd.read_excel(advanced_path, sheet_name="cell_intensity")
    if "condition" not in df.columns:
        return {}, {}
    df = prep_join_keys(df, group_col="condition")
    n_num = pd.to_numeric(df.get("n_numeric"), errors="coerce")
    if "n_label" in df.columns:
        n_from_label = (
            df["n_label"]
            .astype(str)
            .str.extract(r"(?i)n\s*[-_ ]?\s*(\d{1,3})", expand=False)
        )
        n_from_label = pd.to_numeric(n_from_label, errors="coerce")
        n_num = n_num.fillna(n_from_label)
    df["n_numeric_adv"] = n_num

    exact_map: dict[Any, int] = {}
    exact = df.dropna(subset=["n_numeric_adv"]).groupby("join_key")["n_numeric_adv"].agg(_mode_int)
    for k, v in exact.items():
        if v is not None:
            exact_map[k] = int(v)

    file_map: dict[tuple[str, str, str, str], int] = {}
    df["stain_key"] = df["stain"].astype(str).str.strip().str.upper()
    df["group_key"] = df["condition"].astype(str).str.strip()
    df["rep_key"] = df["rep"].astype(str).str.strip().str.lower()
    df["archivo_key"] = df["archivo"].map(normalize_path_token)
    grouped = (
        df.dropna(subset=["n_numeric_adv"])
        .groupby(["stain_key", "group_key", "rep_key", "archivo_key"])["n_numeric_adv"]
        .agg(_mode_int)
        .reset_index()
    )
    for _, row in grouped.iterrows():
        if row["n_numeric_adv"] is None:
            continue
        key = (
            str(row["stain_key"]),
            str(row["group_key"]),
            str(row["rep_key"]),
            str(row["archivo_key"]),
        )
        file_map[key] = int(row["n_numeric_adv"])
    return exact_map, file_map


def assign_n_from_best_source(
    curves: pd.DataFrame,
    exact_map: dict[Any, int],
    file_map: dict[tuple[str, str, str, str], int],
) -> pd.DataFrame:
    out = curves.copy()
    labels: list[str] = []
    nums: list[int] = []
    sources: list[str] = []
    for _, row in out.iterrows():
        jk = row["join_key"]
        if jk in exact_map:
            n_val = int(exact_map[jk])
            labels.append(f"n{n_val}")
            nums.append(n_val)
            sources.append("advanced_exact")
            continue

        fkey = (
            str(row.get("stain")).strip().upper(),
            str(row.get("group")).strip(),
            str(row.get("rep")).strip().lower(),
            normalize_path_token(row.get("archivo")),
        )
        if fkey in file_map:
            n_val = int(file_map[fkey])
            labels.append(f"n{n_val}")
            nums.append(n_val)
            sources.append("advanced_file")
            continue

        n_label, n_numeric, n_source = extract_n_from_file(row.get("archivo"), row.get("rep"))
        labels.append(n_label)
        nums.append(int(n_numeric))
        sources.append(f"fallback_{n_source}")

    out["n_label"] = labels
    out["n_numeric"] = nums
    out["n_source"] = sources
    return out


def load_curves(comparative_path: Path, advanced_path: Path) -> tuple[pd.DataFrame, pd.DataFrame, list[str]]:
    curves = pd.read_excel(comparative_path, sheet_name="histograma_celula_cum_bins")
    if "photo_include" in curves.columns:
        include_flag = pd.to_numeric(curves["photo_include"], errors="coerce")
        if (include_flag == 1).any():
            curves = curves[include_flag == 1].copy()

    bin_cols = [c for c in curves.columns if re.fullmatch(r"I\d{3}", str(c))]
    if len(bin_cols) != 256:
        raise ValueError(f"Expected 256 bins I000-I255, found {len(bin_cols)}.")
    bin_cols = sorted(bin_cols, key=lambda c: int(str(c)[1:]))

    curves = prep_join_keys(curves, group_col="group")
    n_exact_map, n_file_map = build_advanced_n_maps(advanced_path)
    curves = assign_n_from_best_source(curves, n_exact_map, n_file_map)
    bin_vals = curves[bin_cols].apply(pd.to_numeric, errors="coerce").fillna(0)
    end_vals = pd.to_numeric(curves[bin_cols[-1]], errors="coerce").fillna(0)
    denom = end_vals.where(end_vals > 0, 1.0)
    curves_pct = bin_vals.div(denom, axis=0) * 100.0
    return curves, curves_pct, bin_cols


def load_comparative_params(comparative_path: Path) -> list[pd.DataFrame]:
    out_tables: list[pd.DataFrame] = []

    p_int = pd.read_excel(comparative_path, sheet_name="intensidad_celula")
    p_int = add_n_columns(p_int)
    p_int = prep_join_keys(p_int, group_col="group")
    existing = select_existing(p_int, list(COMP_INTENSITY_RENAME.keys()))
    rename_map = {c: COMP_INTENSITY_RENAME[c] for c in existing}
    out_tables.append(p_int[["join_key"] + existing].rename(columns=rename_map))

    p_hist = pd.read_excel(comparative_path, sheet_name="histograma_celula_metricas")
    p_hist = add_n_columns(p_hist)
    p_hist = prep_join_keys(p_hist, group_col="group")
    existing = select_existing(p_hist, list(COMP_HIST_RENAME.keys()))
    rename_map = {c: COMP_HIST_RENAME[c] for c in existing}
    out_tables.append(p_hist[["join_key"] + existing].rename(columns=rename_map))

    return out_tables


def load_advanced_params(advanced_path: Path) -> list[pd.DataFrame]:
    tables: list[pd.DataFrame] = []
    xl = pd.ExcelFile(advanced_path)
    cell_sheet_names = [
        s
        for s in xl.sheet_names
        if s.lower().startswith("cell_") or s.lower().endswith("_cell")
    ]
    if not cell_sheet_names:
        raise ValueError("No cell-level sheets found in advanced workbook.")

    if "cell_intensity" in xl.sheet_names:
        df = pd.read_excel(advanced_path, sheet_name="cell_intensity")
        df = prep_join_keys(df, group_col="condition")
        cols = select_existing(df, ADV_INTENSITY_COLS)
        tables.append(df[["join_key"] + cols].copy())

    if "cell_bg_corrected" in xl.sheet_names:
        df = pd.read_excel(advanced_path, sheet_name="cell_bg_corrected")
        df = prep_join_keys(df, group_col="condition")
        cols = select_existing(df, ADV_BG_COLS)
        rename = {c: f"bg_{c}" for c in cols}
        tables.append(df[["join_key"] + cols].rename(columns=rename))

    if "cell_spatial" in xl.sheet_names:
        df = pd.read_excel(advanced_path, sheet_name="cell_spatial")
        df = prep_join_keys(df, group_col="condition")
        cols = select_existing(df, ADV_SPATIAL_COLS)
        tables.append(df[["join_key"] + cols].copy())

    if "cell_mito" in xl.sheet_names:
        df = pd.read_excel(advanced_path, sheet_name="cell_mito")
        df = prep_join_keys(df, group_col="condition")
        cols = select_existing(df, ADV_MITO_COLS)
        tables.append(df[["join_key"] + cols].copy())

    if "cell_coloc" in xl.sheet_names:
        df = pd.read_excel(advanced_path, sheet_name="cell_coloc")
        df = prep_join_keys(df, group_col="condition", stain_override="MITO")
        metrics = select_existing(df, COLOC_METRICS)
        if metrics:
            df["pair_norm"] = df["pair"].map(sanitize_token) if "pair" in df.columns else "unknown_pair"
            pivot_frames = []
            for metric in metrics:
                piv = df.pivot_table(
                    index="join_key",
                    columns="pair_norm",
                    values=metric,
                    aggfunc="first",
                )
                piv.columns = [f"cell_coloc__{pair}__{metric}" for pair in piv.columns]
                pivot_frames.append(piv)
            if pivot_frames:
                coloc = pd.concat(pivot_frames, axis=1).reset_index()
                tables.append(coloc)

    return tables


def build_plot_settings(datos_df: pd.DataFrame, id_cols: list[str]) -> pd.DataFrame:
    param_cols = [c for c in datos_df.columns if c not in id_cols]
    for c in param_cols:
        datos_df[c] = pd.to_numeric(datos_df[c], errors="coerce")
    y_max = []
    for p in param_cols:
        mx = pd.to_numeric(datos_df[p], errors="coerce").max(skipna=True)
        y_max.append(round_up_sig(float(mx), digits=2) if pd.notna(mx) else float("nan"))
    interval = [v / 5 if pd.notna(v) and v > 0 else float("nan") for v in y_max]
    return pd.DataFrame(
        {"Parameter": param_cols, "Y_Max": y_max, "Interval": interval, "Y_Title": param_cols}
    )


def main() -> int:
    args = parse_args()
    advanced_path = Path(args.advanced)
    comparative_path = Path(args.comparative)
    out_dir = Path(args.output_dir)
    curves_out = out_dir / args.curves_name
    plate_cell_out = out_dir / args.platemap_cell_name
    plate_n_out = out_dir / args.platemap_n_name

    if not advanced_path.exists():
        raise FileNotFoundError(f"Advanced workbook not found: {advanced_path}")
    if not comparative_path.exists():
        raise FileNotFoundError(f"Comparative workbook not found: {comparative_path}")

    curves, curves_pct, bin_cols = load_curves(comparative_path, advanced_path)

    master = curves[
        [
            "join_key",
            "stain",
            "group",
            "rep",
            "archivo",
            "cell_id",
            "n_label",
            "n_numeric",
            "n_source",
        ]
    ].copy()
    master["_curve_row"] = range(len(master))

    for table in load_comparative_params(comparative_path) + load_advanced_params(advanced_path):
        master = master.merge(table, on="join_key", how="left")

    for req in REQUIRED_ADV_PARAMS:
        if req not in master.columns:
            raise ValueError(f"Missing required advanced parameter column: {req}")
        miss = int(master[req].isna().sum())
        if miss > 0:
            raise ValueError(f"Required advanced parameter '{req}' has {miss} missing values.")

    order_rank = {k: i for i, k in enumerate(CONDITION_ORDER, start=1)}
    rep_rank = {"rep1": 1, "rep2": 2}
    master["_rank"] = master.apply(
        lambda r: order_rank.get((str(r["stain"]).upper(), str(r["group"])), 999),
        axis=1,
    )
    master["_rep_rank"] = master["rep"].astype(str).str.lower().map(rep_rank).fillna(9)
    master["_row_id"] = range(len(master))
    master = master.sort_values(
        ["_rank", "n_numeric", "_rep_rank", "archivo", "cell_id", "_row_id"]
    ).reset_index(drop=True)

    master["Well"] = gen_wells(len(master))
    master["Strain"] = master["stain"].astype(str).str.upper().map(STAIN_LABEL_MAP)
    master["Media"] = master["group"].astype(str)
    master["Orden"] = master.apply(
        lambda r: order_rank[(str(r["stain"]).upper(), str(r["group"]))], axis=1
    )

    if master["Strain"].isna().any():
        bad = master[master["Strain"].isna()][["stain"]].drop_duplicates()
        raise ValueError(f"Unknown stain labels found:\n{bad.to_string(index=False)}")

    curves_sorted = curves_pct.iloc[master["_curve_row"].astype(int).to_numpy()].reset_index(drop=True)
    curves_sheet1: dict[str, Any] = {"Time": [int(c[1:]) for c in bin_cols]}
    for idx, well in enumerate(master["Well"].astype(str)):
        curves_sheet1[well] = curves_sorted.iloc[idx].to_list()
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

    id_cols = [
        "Well",
        "Strain",
        "Media",
        "Orden",
        "Replicate",
        "BiologicalReplicate",
        "TechnicalReplicate",
    ]
    non_param_cols = {
        "join_key",
        "stain",
        "group",
        "rep",
        "archivo",
        "cell_id",
        "n_label",
        "n_numeric",
        "n_source",
        "_curve_row",
        "_rank",
        "_rep_rank",
        "_row_id",
        "Well",
        "Strain",
        "Media",
        "Orden",
    }
    param_cols = [c for c in master.columns if c not in non_param_cols]
    for c in param_cols:
        master[c] = pd.to_numeric(master[c], errors="coerce")
    param_cols = [c for c in param_cols if master[c].notna().any()]

    cell_df = master.copy()
    cell_df["BiologicalReplicate"] = (
        cell_df.groupby(["Strain", "Media"]).cumcount() + 1
    )
    cell_df["Replicate"] = cell_df["BiologicalReplicate"]
    cell_df["TechnicalReplicate"] = 1
    cell_datos = cell_df[id_cols + param_cols].copy()

    n_df = master.copy()
    n_df["BiologicalReplicate"] = n_df["n_numeric"].astype(int)
    n_df["Replicate"] = n_df["BiologicalReplicate"]
    n_df["TechnicalReplicate"] = (
        n_df.groupby(["Strain", "Media", "BiologicalReplicate"]).cumcount() + 1
    )
    n_datos = n_df[id_cols + param_cols].copy()

    cell_plot = build_plot_settings(cell_datos.copy(), id_cols)
    n_plot = build_plot_settings(n_datos.copy(), id_cols)

    curve_wells = [c for c in curves_sheet1_df.columns if c != "Time"]
    if curve_wells != cell_datos["Well"].astype(str).tolist():
        raise ValueError("Curve wells do not align with cell-level platemap.")
    if curve_wells != n_datos["Well"].astype(str).tolist():
        raise ValueError("Curve wells do not align with n-level platemap.")
    if float(curves_sheet1_df.iloc[-1, 1:].min()) < 99.0:
        raise ValueError("Some curves do not end near 100%.")
    expected_media = {"-", "siNEG", "siSOR"}
    if set(cell_datos["Media"].dropna().astype(str).unique()) != expected_media:
        raise ValueError("Unexpected media labels in cell-level platemap.")
    if set(n_datos["Media"].dropna().astype(str).unique()) != expected_media:
        raise ValueError("Unexpected media labels in n-level platemap.")

    out_dir.mkdir(parents=True, exist_ok=True)
    curves_backup = backup_if_exists(curves_out)
    cell_backup = backup_if_exists(plate_cell_out)
    n_backup = backup_if_exists(plate_n_out)

    with pd.ExcelWriter(curves_out, engine="openpyxl") as writer:
        curves_sheet1_df.to_excel(writer, sheet_name="Sheet1", index=False)
        curves_sheet2_df.to_excel(writer, sheet_name="Sheet2", index=False)

    with pd.ExcelWriter(plate_cell_out, engine="openpyxl") as writer:
        cell_datos.to_excel(writer, sheet_name="Datos", index=False)
        cell_plot.to_excel(writer, sheet_name="PlotSettings", index=False)

    with pd.ExcelWriter(plate_n_out, engine="openpyxl") as writer:
        n_datos.to_excel(writer, sheet_name="Datos", index=False)
        n_plot.to_excel(writer, sheet_name="PlotSettings", index=False)

    # App-ready bundles with fixed file names.
    cell_bundle = out_dir / "cell_level_bundle"
    n_bundle = out_dir / "n_level_bundle"
    cell_bundle.mkdir(parents=True, exist_ok=True)
    n_bundle.mkdir(parents=True, exist_ok=True)
    shutil.copy2(curves_out, cell_bundle / "Curvas_Int.xlsx")
    shutil.copy2(plate_cell_out, cell_bundle / "platemap_Int.xlsx")
    shutil.copy2(curves_out, n_bundle / "Curvas_Int.xlsx")
    shutil.copy2(plate_n_out, n_bundle / "platemap_Int.xlsx")

    n_summary = (
        n_datos[["Strain", "Media", "BiologicalReplicate"]]
        .drop_duplicates()
        .sort_values(["Strain", "Media", "BiologicalReplicate"])
    )
    n_detected = (
        master[["n_label", "n_numeric", "n_source"]]
        .drop_duplicates()
        .sort_values(["n_numeric", "n_label"])
    )

    print("Build completed successfully.")
    print(f"Advanced source:    {advanced_path}")
    print(f"Comparative source: {comparative_path}")
    print(f"Curves output:      {curves_out}")
    print(f"Cell platemap:      {plate_cell_out}")
    print(f"N platemap:         {plate_n_out}")
    print(f"Cell app bundle:    {cell_bundle}")
    print(f"N app bundle:       {n_bundle}")
    if curves_backup:
        print(f"Curves backup:      {curves_backup}")
    if cell_backup:
        print(f"Cell backup:        {cell_backup}")
    if n_backup:
        print(f"N backup:           {n_backup}")
    print(f"Total shared wells: {len(master)}")
    print("Detected n labels and source:")
    print(n_detected.to_string(index=False))
    print("N-level strain/media/biorep combinations:")
    print(n_summary.to_string(index=False))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
