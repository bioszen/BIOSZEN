#!/usr/bin/env python3
"""Build a BIOSZEN platemap from CountTMM and a reference platemap template.

This script:
1. Reads gene expression from CountTMM.csv (one row per gene, one column per sample).
2. Reuses the 48-well structure and μMax values from a reference platemap.
3. Expands reference media blocks into explicit conditions per strain:
   Mock, Rapa, U18, Rapa-U18.
4. Writes all genes as parameter columns in `Datos`, keeping `μMax` last.
5. Generates `PlotSettings` for all gene parameters plus `μMax`.

Expected sample naming in CountTMM:
- <STRAIN>.C, <STRAIN>.C.1, <STRAIN>.C.2       -> Mock (rep 1..3)
- <STRAIN>.R, <STRAIN>.R.1, <STRAIN>.R.2       -> Rapa (rep 1..3)
- <STRAIN>.U, <STRAIN>.U.1, <STRAIN>.U.2       -> U18 (rep 1..3)
- <STRAIN>.RU, <STRAIN>.RU.1, <STRAIN>.RU.2    -> Rapa-U18 (rep 1..3)
"""

from __future__ import annotations

import argparse
import math
import shutil
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd


DEFAULT_COUNTTMM = Path(r"D:/Graficos_RNAseq/Correlaciones.SI/CountTMM.csv")
DEFAULT_REFERENCE = Path(r"C:/Users/user/OneDrive/Escritorio/platemap_exp CTP1.xlsx")
DEFAULT_OUTPUT = Path(
    r"C:/Users/user/OneDrive/Escritorio/platemap_exp_CountTMM_all_genes.xlsx"
)

MEDIA_ORDER = ["Mock", "Rapa", "U18", "Rapa-U18"]

# Reference rows are grouped as:
# - Control reps 1..3 -> Mock (C)
# - Control reps 4..6 -> Rapa (R)
# - U18 reps 1..3     -> U18 (U)
# - U18 reps 4..6     -> Rapa-U18 (RU)
REF_MEDIA_MAPPING = {
    ("Control", True): ("Mock", "C"),
    ("Control", False): ("Rapa", "R"),
    ("U18", True): ("U18", "U"),
    ("U18", False): ("Rapa-U18", "RU"),
}

DATA_META_COLS = [
    "Well",
    "Strain",
    "Media",
    "Orden",
    "Replicate",
    "BiologicalReplicate",
    "TechnicalReplicate",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Build a BIOSZEN platemap with all genes from CountTMM using a "
            "reference platemap layout and μMax values."
        )
    )
    parser.add_argument(
        "--counttmm",
        type=str,
        default=str(DEFAULT_COUNTTMM),
        help="Path to CountTMM.csv",
    )
    parser.add_argument(
        "--reference",
        type=str,
        default=str(DEFAULT_REFERENCE),
        help="Path to reference platemap workbook (.xlsx)",
    )
    parser.add_argument(
        "--output",
        type=str,
        default=str(DEFAULT_OUTPUT),
        help="Output path for generated platemap workbook (.xlsx)",
    )
    return parser.parse_args()


def to_float(value: Any) -> float:
    if value is None:
        return float("nan")
    if isinstance(value, (int, float, np.number)):
        return float(value)
    text = str(value).strip()
    if text == "":
        return float("nan")
    try:
        return float(text)
    except ValueError:
        return float("nan")


def build_sample_column(strain: str, condition_code: str, bio_replicate: int) -> str:
    suffix = "" if bio_replicate == 1 else f".{bio_replicate - 1}"
    return f"{strain}.{condition_code}{suffix}"


def nice_ceiling(value: float) -> float:
    if not np.isfinite(value) or value <= 0:
        return 1.0
    exponent = math.floor(math.log10(value))
    base = 10.0 ** exponent
    normalized = value / base
    if normalized <= 1:
        nice = 1
    elif normalized <= 2:
        nice = 2
    elif normalized <= 5:
        nice = 5
    else:
        nice = 10
    return float(nice * base)


def compute_axis(series: pd.Series) -> tuple[float, float]:
    vals = pd.to_numeric(series, errors="coerce").to_numpy(dtype=float)
    vals = vals[np.isfinite(vals)]
    if vals.size == 0:
        return 1.0, 0.2
    vmax = float(np.max(vals))
    if vmax <= 0:
        return 1.0, 0.2
    y_max = nice_ceiling(vmax * 1.10)
    interval = nice_ceiling(y_max / 5.0)
    if interval <= 0:
        interval = y_max
    return y_max, interval


def ensure_exists(path: Path, label: str) -> None:
    if not path.exists():
        raise FileNotFoundError(f"{label} not found: {path}")


def backup_if_exists(path: Path) -> Path | None:
    if not path.exists():
        return None
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup = path.with_name(f"{path.stem}.backup_{ts}{path.suffix}")
    shutil.copy2(path, backup)
    return backup


def load_counttmm(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    if df.shape[1] < 2:
        raise ValueError("CountTMM must contain one gene column and sample columns.")

    gene_col = df.columns[0]
    df[gene_col] = df[gene_col].astype(str).str.strip()
    if df[gene_col].duplicated().any():
        dup_count = int(df[gene_col].duplicated().sum())
        raise ValueError(
            f"Gene identifiers are not unique in CountTMM ({dup_count} duplicates)."
        )
    return df


def load_reference_datos(path: Path) -> pd.DataFrame:
    df = pd.read_excel(path, sheet_name="Datos", dtype=object, keep_default_na=False)
    required = {
        "Well",
        "Strain",
        "Media",
        "Replicate",
        "BiologicalReplicate",
        "TechnicalReplicate",
    }
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Reference Datos sheet is missing required columns: {missing}")
    if "μMax" not in df.columns:
        raise ValueError("Reference Datos sheet must include a 'μMax' column.")
    return df


def main() -> None:
    args = parse_args()
    counttmm_path = Path(args.counttmm).expanduser().resolve()
    reference_path = Path(args.reference).expanduser().resolve()
    output_path = Path(args.output).expanduser().resolve()

    ensure_exists(counttmm_path, "CountTMM file")
    ensure_exists(reference_path, "Reference platemap")

    count_df = load_counttmm(counttmm_path)
    reference_df = load_reference_datos(reference_path)
    gene_col = count_df.columns[0]
    gene_names = count_df[gene_col].tolist()
    count_matrix = count_df.set_index(gene_col)
    sample_columns = set(count_matrix.columns.tolist())

    source_wells: list[str] = []
    source_samples: list[str] = []
    source_codes: list[str] = []
    source_bio: list[int] = []

    meta_rows: list[dict[str, Any]] = []
    selected_sample_columns: list[str] = []
    umax_values: list[float] = []

    strain_order: list[str] = []
    seen_strains: set[str] = set()

    for idx, row in reference_df.iterrows():
        well = str(row.get("Well", "")).strip()
        strain = str(row.get("Strain", "")).strip()
        media_ref = str(row.get("Media", "")).strip()
        replicate_raw = int(to_float(row.get("Replicate")))
        if replicate_raw < 1 or replicate_raw > 6:
            raise ValueError(
                f"Reference replicate must be between 1 and 6. Row {idx + 2} has {replicate_raw}."
            )
        if not well:
            raise ValueError(f"Missing Well value at row {idx + 2} in reference Datos.")
        if not strain:
            raise ValueError(f"Missing Strain value at row {idx + 2} in reference Datos.")

        if strain not in seen_strains:
            strain_order.append(strain)
            seen_strains.add(strain)

        first_half = replicate_raw <= 3
        key = (media_ref, first_half)
        if key not in REF_MEDIA_MAPPING:
            raise ValueError(
                f"Unsupported reference Media/Replicate combination at row {idx + 2}: "
                f"Media='{media_ref}', Replicate={replicate_raw}."
            )
        media_out, condition_code = REF_MEDIA_MAPPING[key]
        bio_replicate = replicate_raw if first_half else replicate_raw - 3

        sample_col = build_sample_column(strain, condition_code, bio_replicate)
        if sample_col not in sample_columns:
            raise ValueError(
                f"Sample column '{sample_col}' (from row {idx + 2}) not found in CountTMM."
            )

        meta_rows.append(
            {
                "Well": well,
                "Strain": strain,
                "Media": media_out,
                "Orden": 0,  # assigned after strain/media ordering is known
                "Replicate": bio_replicate,
                "BiologicalReplicate": bio_replicate,
                "TechnicalReplicate": "A",
            }
        )
        selected_sample_columns.append(sample_col)
        umax_values.append(to_float(row.get("μMax")))

        source_wells.append(well)
        source_samples.append(sample_col)
        source_codes.append(condition_code)
        source_bio.append(bio_replicate)

    meta_df = pd.DataFrame(meta_rows)

    order_map: dict[tuple[str, str], int] = {}
    order_idx = 1
    for strain in strain_order:
        for media in MEDIA_ORDER:
            order_map[(strain, media)] = order_idx
            order_idx += 1
    meta_df["Orden"] = [
        order_map[(strain, media)]
        for strain, media in zip(meta_df["Strain"], meta_df["Media"], strict=True)
    ]

    expr_df = count_matrix.loc[:, selected_sample_columns].T.reset_index(drop=True)
    expr_df.columns = gene_names

    datos_out = pd.concat([meta_df, expr_df], axis=1)
    datos_out["μMax"] = umax_values

    for col in ("Orden", "Replicate", "BiologicalReplicate"):
        datos_out[col] = pd.to_numeric(datos_out[col], errors="coerce").astype("Int64")
    datos_out["TechnicalReplicate"] = datos_out["TechnicalReplicate"].astype(str)

    plot_rows: list[dict[str, Any]] = []
    for gene in gene_names:
        y_max, interval = compute_axis(datos_out[gene])
        plot_rows.append(
            {
                "Parameter": gene,
                "Y_Max": y_max,
                "Interval": interval,
                "Y_Title": f"Expression {gene}",
            }
        )

    umax_y_max, umax_interval = compute_axis(datos_out["μMax"])
    umax_title = "μMax"
    try:
        cfg_ref = pd.read_excel(reference_path, sheet_name="PlotSettings")
        cfg_ref["Parameter"] = cfg_ref["Parameter"].astype(str).str.strip()
        ref_umax = cfg_ref[cfg_ref["Parameter"] == "μMax"].head(1)
        if not ref_umax.empty:
            y_max_ref = to_float(ref_umax.iloc[0].get("Y_Max"))
            interval_ref = to_float(ref_umax.iloc[0].get("Interval"))
            y_title_ref = str(ref_umax.iloc[0].get("Y_Title", "")).strip()
            if np.isfinite(y_max_ref) and y_max_ref > 0:
                umax_y_max = y_max_ref
            if np.isfinite(interval_ref) and interval_ref > 0:
                umax_interval = interval_ref
            if y_title_ref:
                umax_title = y_title_ref
    except Exception:
        pass

    plot_rows.append(
        {
            "Parameter": "μMax",
            "Y_Max": umax_y_max,
            "Interval": umax_interval,
            "Y_Title": umax_title,
        }
    )
    plot_settings_out = pd.DataFrame(plot_rows)

    source_mapping = pd.DataFrame(
        {
            "Well": source_wells,
            "SourceSample": source_samples,
            "ConditionCode": source_codes,
            "BiologicalReplicate": source_bio,
        }
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    backup_path = backup_if_exists(output_path)

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        datos_out.to_excel(writer, sheet_name="Datos", index=False)
        plot_settings_out.to_excel(writer, sheet_name="PlotSettings", index=False)
        source_mapping.to_excel(writer, sheet_name="SourceMapping", index=False)

    print(f"Generated platemap: {output_path}")
    if backup_path is not None:
        print(f"Backup created: {backup_path}")
    print(f"Datos shape: {datos_out.shape[0]} rows x {datos_out.shape[1]} columns")
    print(
        f"Parameters in PlotSettings: {plot_settings_out.shape[0]} "
        f"({len(gene_names)} genes + μMax)"
    )
    print("Condition order per strain: Mock, Rapa, U18, Rapa-U18")


if __name__ == "__main__":
    main()
