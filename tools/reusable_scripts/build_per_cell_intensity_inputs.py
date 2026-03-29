#!/usr/bin/env python3
"""Build per-cell Curvas/Platemap workbooks for BIOSZEN intensity analysis.

This script expands averaged inputs into per-cell inputs:
- Curves workbook (Sheet1/Sheet2): one curve per cell/well.
- Platemap workbook (Datos/PlotSettings): one mean intensity per same well.

Expected source workbooks:
- resumen_hist_cum_sin_rep_usado_por_celula_seleccionada_por_tincion.xlsx
- informe_comparativo_por_celula_rep1_rep2.xlsx
"""

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

SUMMARY_FILENAME = (
    "resumen_hist_cum_sin_rep_usado_por_celula_seleccionada_por_tincion.xlsx"
)
REPORT_FILENAME = "informe_comparativo_por_celula_rep1_rep2.xlsx"

DEFAULT_PLATEMAP_PATH = Path(r"C:/Users/user/OneDrive/Escritorio/platemap_Int.xlsx")
DEFAULT_CURVES_PATH = Path(r"C:/Users/user/Downloads/Curvas_Int.xlsx")

SHEET_TO_STAIN = {
    "fill": "FILL",
    "lisor": "LYSOR",
    "lisog": "LYSOG",
    "mito": "MITO",
}

STAIN_LABELS = {
    "FILL": "Filipin",
    "MITO": "Mitotracker",
    "LYSOR": "Lysotracker Red",
    "LYSOG": "Lysotracker Green",
}

GROUP_TO_MEDIA = {
    "-": "-",
    "siNEG": "siNEG",
    "siSOR": "siSORSC2",
}

CONDITION_ORDER = [
    ("FILL", "-"),
    ("FILL", "siNEG"),
    ("FILL", "siSOR"),
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


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build per-cell Curvas_Int and platemap_Int workbooks."
    )
    parser.add_argument(
        "--summary",
        type=str,
        default=None,
        help=f"Path to {SUMMARY_FILENAME} (auto-discovered if omitted).",
    )
    parser.add_argument(
        "--report",
        type=str,
        default=None,
        help=f"Path to {REPORT_FILENAME} (auto-discovered if omitted).",
    )
    parser.add_argument(
        "--curves",
        type=str,
        default=str(DEFAULT_CURVES_PATH),
        help="Path to input curves workbook (used as template for Sheet2).",
    )
    parser.add_argument(
        "--platemap",
        type=str,
        default=str(DEFAULT_PLATEMAP_PATH),
        help="Path to input platemap workbook (output target by default).",
    )
    parser.add_argument(
        "--curves-out",
        type=str,
        default=None,
        help="Output path for curves workbook (defaults to --curves).",
    )
    parser.add_argument(
        "--platemap-out",
        type=str,
        default=None,
        help="Output path for platemap workbook (defaults to --platemap).",
    )
    parser.add_argument(
        "--search-root",
        type=str,
        default=r"C:/Users/user",
        help="Root folder used for auto-discovery of source workbooks.",
    )
    return parser.parse_args()


def normalize_photo_name(value: Any) -> str:
    out = str(value).strip().upper()
    out = out.replace(".TIF", "").replace(".ND2", "")
    out = re.sub(r"\s+", "", out)
    out = out.replace("_", "").replace("-", "")
    return out


def find_workbook(explicit: str | None, search_root: Path, filename: str) -> Path:
    if explicit:
        path = Path(explicit).expanduser()
        if not path.exists():
            raise FileNotFoundError(f"Workbook not found: {path}")
        return path

    matches = sorted(search_root.rglob(filename))
    if not matches:
        raise FileNotFoundError(f"Could not find '{filename}' under {search_root}")

    # Prefer most recently modified file if multiple matches exist.
    matches = sorted(matches, key=lambda p: p.stat().st_mtime, reverse=True)
    return matches[0]


def first_matching_column(columns: list[str], expected: str) -> str:
    expected_norm = expected.strip().lower()
    for col in columns:
        if str(col).strip().lower() == expected_norm:
            return col
    return columns[0]


def read_summary_curves(summary_path: Path) -> tuple[pd.Series, list[dict[str, Any]]]:
    records: list[dict[str, Any]] = []
    intensity_ref: pd.Series | None = None

    for sheet, stain in SHEET_TO_STAIN.items():
        df = pd.read_excel(summary_path, sheet_name=sheet)
        if df.empty:
            continue

        columns = [str(c) for c in df.columns]
        intensity_col = first_matching_column(columns, "intensity")
        intensity = pd.to_numeric(df[intensity_col], errors="coerce")

        if intensity_ref is None:
            intensity_ref = intensity
        else:
            if len(intensity_ref) != len(intensity):
                raise ValueError(
                    f"Intensity axis length mismatch in sheet '{sheet}': "
                    f"{len(intensity)} vs {len(intensity_ref)}"
                )

        for col in columns:
            if col == intensity_col:
                continue

            parts = col.split("|")
            if len(parts) < 4:
                continue

            group = parts[0].strip()
            rep = parts[1].strip().lower()
            archivo = parts[2].strip()
            cell_match = re.search(r"cell_(\d+)", col, flags=re.IGNORECASE)
            if not cell_match:
                continue
            cell_id = int(cell_match.group(1))

            curve_values = pd.to_numeric(df[col], errors="coerce")
            if curve_values.isna().all():
                continue

            key = (
                stain,
                group,
                rep,
                normalize_photo_name(archivo),
                cell_id,
            )

            records.append(
                {
                    "stain": stain,
                    "group": group,
                    "rep": rep,
                    "archivo": archivo,
                    "archivo_norm": key[3],
                    "cell_id": cell_id,
                    "key": key,
                    "curve": curve_values,
                }
            )

    if intensity_ref is None:
        raise ValueError("No intensity axis found in summary workbook.")

    if not records:
        raise ValueError("No per-cell curves were parsed from summary workbook.")

    return intensity_ref, records


def read_report_means(
    report_path: Path, required_keys: set[tuple[Any, ...]] | None = None
) -> dict[tuple[Any, ...], float]:
    df = pd.read_excel(report_path, sheet_name="intensidad_celula")
    required = {"stain", "group", "rep", "archivo", "cell_id", "intensity_mean"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(
            f"Missing required columns in 'intensidad_celula': {sorted(missing)}"
        )

    if "photo_include" in df.columns:
        include_flag = pd.to_numeric(df["photo_include"], errors="coerce")
        if (include_flag == 1).any():
            df = df[include_flag == 1].copy()

    df = df.copy()
    df["stain"] = df["stain"].astype(str)
    df["group"] = df["group"].astype(str)
    df["rep"] = df["rep"].astype(str).str.lower()
    df["archivo_norm"] = df["archivo"].map(normalize_photo_name)
    df["cell_id"] = pd.to_numeric(df["cell_id"], errors="coerce").astype("Int64")
    df["intensity_mean"] = pd.to_numeric(df["intensity_mean"], errors="coerce")
    df = df.dropna(subset=["cell_id", "intensity_mean"])
    df["cell_id"] = df["cell_id"].astype(int)

    df["key"] = list(
        zip(
            df["stain"],
            df["group"],
            df["rep"],
            df["archivo_norm"],
            df["cell_id"],
        )
    )

    if required_keys is not None:
        df = df[df["key"].isin(required_keys)].copy()

    if df["key"].duplicated().any():
        dup = df[df["key"].duplicated(keep=False)]
        conflict = (
            dup.groupby("key")["intensity_mean"].nunique().reset_index(name="nvals")
        )
        conflict = conflict[conflict["nvals"] > 1]
        if not conflict.empty:
            sample_keys = set(conflict["key"].head(10))
            sample = dup[dup["key"].isin(sample_keys)][
                ["stain", "group", "rep", "archivo", "cell_id", "intensity_mean"]
            ].head(20)
            raise ValueError(
                "Duplicate per-cell keys with conflicting intensities found in report workbook. "
                "Sample:\n"
                + sample.to_string(index=False)
            )
        df = df.drop_duplicates(subset=["key"], keep="first")

    return dict(zip(df["key"], df["intensity_mean"]))


def sort_records(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    rank_map = {pair: idx for idx, pair in enumerate(CONDITION_ORDER)}
    with_index: list[tuple[int, dict[str, Any]]] = list(enumerate(records))

    def rank_key(item: tuple[int, dict[str, Any]]) -> tuple[Any, ...]:
        idx, rec = item
        stain = rec["stain"]
        group = rec["group"]
        rank = rank_map.get((stain, group), 999)
        # Keep original source order within each condition block.
        return rank, idx

    return [rec for _, rec in sorted(with_index, key=rank_key)]


def gen_wells(n: int) -> list[str]:
    base_letters = [letter for letter in "ABCDEFGH" for _ in range(12)]
    base_numbers = [number for _ in range(8) for number in range(1, 13)]
    base = [f"{letter}{number}" for letter, number in zip(base_letters, base_numbers)]

    if n <= 96:
        return base[:n]
    extra = [f"H{number}" for number in range(13, 13 + (n - 96))]
    return base + extra


def round_up_sig(value: float, digits: int = 2) -> float:
    if pd.isna(value):
        return float("nan")
    if value <= 0:
        return 0.0
    power = math.floor(math.log10(abs(value)))
    scale = 10 ** (power - (digits - 1))
    return math.ceil(value / scale) * scale


def build_curve_settings_sheet(curves_template_path: Path, x_max: int) -> pd.DataFrame:
    default = {
        "X_Max": x_max,
        "Interval_X": 5,
        "Y_Max": 100,
        "Interval_Y": 20,
        "X_Title": "Intensity Levels",
        "Y_Title": "Percentage of Pixels per Cell",
    }

    try:
        template = pd.read_excel(curves_template_path, sheet_name="Sheet2")
        if template.empty:
            return pd.DataFrame([default])
        row = template.iloc[0].to_dict()
    except Exception:
        return pd.DataFrame([default])

    out = default.copy()
    out["Interval_X"] = row.get("Interval_X", default["Interval_X"])
    out["Interval_Y"] = row.get("Interval_Y", default["Interval_Y"])
    out["X_Title"] = row.get("X_Title", default["X_Title"])
    out["Y_Title"] = row.get("Y_Title", default["Y_Title"])
    out["X_Max"] = x_max
    out["Y_Max"] = 100
    return pd.DataFrame([out])


def backup_if_exists(path: Path) -> Path | None:
    if not path.exists():
        return None
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup = path.with_suffix(path.suffix + f".bak_{stamp}")
    shutil.copy2(path, backup)
    return backup


def validate_outputs(
    sheet1: pd.DataFrame,
    platemap: pd.DataFrame,
    records: list[dict[str, Any]],
) -> None:
    curve_wells = [c for c in sheet1.columns if c != "Time"]
    plate_wells = platemap["Well"].tolist()

    if len(curve_wells) != len(plate_wells):
        raise ValueError(
            f"Well count mismatch: curves={len(curve_wells)}, platemap={len(plate_wells)}"
        )
    if curve_wells != plate_wells:
        raise ValueError("Well order mismatch between curves and platemap.")

    if platemap["Intensity_Mean"].isna().any():
        missing = int(platemap["Intensity_Mean"].isna().sum())
        raise ValueError(f"Platemap has {missing} missing Intensity_Mean values.")

    tail = sheet1.iloc[-1, 1:]
    if (tail < 99.0).any():
        bad_count = int((tail < 99.0).sum())
        raise ValueError(
            f"{bad_count} curves do not look cumulative (last point below 99%)."
        )

    expected_media = {"-", "siNEG", "siSORSC2"}
    media_found = set(platemap["Media"].dropna().astype(str).unique())
    if not media_found.issubset(expected_media):
        raise ValueError(
            f"Unexpected media labels found: {sorted(media_found - expected_media)}"
        )

    expected_strains = set(STAIN_LABELS.values())
    strain_found = set(platemap["Strain"].dropna().astype(str).unique())
    if not strain_found.issubset(expected_strains):
        raise ValueError(
            f"Unexpected strain labels found: {sorted(strain_found - expected_strains)}"
        )

    # Exact one-to-one alignment check with source records.
    for rec in records:
        well = rec["Well"]
        plate_value = float(
            platemap.loc[platemap["Well"] == well, "Intensity_Mean"].iloc[0]
        )
        if abs(plate_value - float(rec["intensity_mean"])) > 1e-9:
            raise ValueError(f"Intensity mismatch for well {well}.")


def main() -> int:
    args = parse_args()
    search_root = Path(args.search_root)

    summary_path = find_workbook(args.summary, search_root, SUMMARY_FILENAME)
    report_path = find_workbook(args.report, search_root, REPORT_FILENAME)
    curves_in = Path(args.curves)
    platemap_in = Path(args.platemap)
    curves_out = Path(args.curves_out) if args.curves_out else curves_in
    platemap_out = Path(args.platemap_out) if args.platemap_out else platemap_in

    intensity_axis, records = read_summary_curves(summary_path)
    required_keys = {rec["key"] for rec in records}
    mean_map = read_report_means(report_path, required_keys=required_keys)

    missing_means = []
    for rec in records:
        mean_value = mean_map.get(rec["key"])
        if mean_value is None:
            missing_means.append(rec)
        else:
            rec["intensity_mean"] = float(mean_value)

    if missing_means:
        sample = missing_means[:10]
        details = "\n".join(
            [
                f"{r['stain']}|{r['group']}|{r['rep']}|{r['archivo']}|cell_{r['cell_id']}"
                for r in sample
            ]
        )
        raise ValueError(
            f"Missing mean intensity for {len(missing_means)} per-cell curves. Sample:\n"
            f"{details}"
        )

    records = sort_records(records)
    wells = gen_wells(len(records))
    for idx, rec in enumerate(records):
        rec["Well"] = wells[idx]

    condition_to_order = {
        condition: idx for idx, condition in enumerate(CONDITION_ORDER, start=1)
    }
    bio_counter: dict[tuple[str, str], int] = {}

    time_values = pd.to_numeric(intensity_axis, errors="coerce").fillna(0)
    curves_data: dict[str, Any] = {"Time": time_values.astype(int)}
    for rec in records:
        curves_data[rec["Well"]] = pd.to_numeric(rec["curve"], errors="coerce").fillna(0)
    sheet1 = pd.DataFrame(curves_data)
    sheet2 = build_curve_settings_sheet(curves_in, int(time_values.max()))

    rows = []
    for rec in records:
        condition_key = (rec["stain"], rec["group"])
        orden_value = condition_to_order.get(condition_key)
        if orden_value is None:
            raise ValueError(f"Condition not mapped to Orden: {condition_key}")
        bio_counter[condition_key] = bio_counter.get(condition_key, 0) + 1
        bio_rep = bio_counter[condition_key]

        rows.append(
            {
                "Well": rec["Well"],
                "Strain": STAIN_LABELS.get(rec["stain"], rec["stain"]),
                "Media": GROUP_TO_MEDIA.get(rec["group"], rec["group"]),
                "Orden": orden_value,
                "Replicate": bio_rep,
                "BiologicalReplicate": bio_rep,
                "TechnicalReplicate": "A",
                "Intensity_Mean": rec["intensity_mean"],
            }
        )
    datos = pd.DataFrame(rows)

    y_max = round_up_sig(float(datos["Intensity_Mean"].max()), digits=2)
    interval = y_max / 5 if pd.notna(y_max) and y_max > 0 else 1.0
    plot_settings = pd.DataFrame(
        [
            {
                "Parameter": "Intensity_Mean",
                "Y_Max": y_max,
                "Interval": interval,
                "Y_Title": "Mean intensity per cell",
            }
        ]
    )

    validate_outputs(sheet1, datos, records)

    curves_backup = backup_if_exists(curves_out)
    platemap_backup = backup_if_exists(platemap_out)

    with pd.ExcelWriter(curves_out, engine="openpyxl") as writer:
        sheet1.to_excel(writer, sheet_name="Sheet1", index=False)
        sheet2.to_excel(writer, sheet_name="Sheet2", index=False)

    with pd.ExcelWriter(platemap_out, engine="openpyxl") as writer:
        datos.to_excel(writer, sheet_name="Datos", index=False)
        plot_settings.to_excel(writer, sheet_name="PlotSettings", index=False)

    summary_counts = (
        datos.groupby(["Strain", "Media"], dropna=False)
        .size()
        .reset_index(name="n_cells")
        .sort_values(["Strain", "Media"])
    )

    print("Build completed successfully.")
    print(f"Summary source:   {summary_path}")
    print(f"Report source:    {report_path}")
    print(f"Curves output:    {curves_out}")
    print(f"Platemap output:  {platemap_out}")
    if curves_backup:
        print(f"Curves backup:    {curves_backup}")
    if platemap_backup:
        print(f"Platemap backup:  {platemap_backup}")
    print(f"Total per-cell wells: {len(records)}")
    print("Counts by strain/media:")
    print(summary_counts.to_string(index=False))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
