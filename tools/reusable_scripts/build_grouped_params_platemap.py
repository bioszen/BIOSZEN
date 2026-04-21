#!/usr/bin/env python3
"""Build a BIOSZEN-compatible platemap from a grouped-parameters workbook.

Input workbook format:
- One sheet per parameter (sheet name is used as parameter name).
- Each parameter sheet must contain:
  - `Strain`
  - `RepBiol`
  - One or more condition/media columns (for example `Control`, `Rapa`, `U18`).

Output workbook:
- `Datos` sheet:
  - BIOSZEN metadata first:
    `Well`, `Strain`, `Media`, `Orden`, `Replicate`,
    `BiologicalReplicate`, `TechnicalReplicate`
  - Then one numeric column per parameter sheet.
- `PlotSettings` sheet:
  - `Parameter`, `Y_Max`, `Interval`, `Y_Title`
- `SourceMapping` sheet:
  - Documents source key used per output row.

By default the script keeps only rows present in *all* parameter sheets
(`--merge-mode intersection`) to avoid missing parameter values in BIOSZEN.
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


DEFAULT_INPUT = Path(r"C:/Users/user/Downloads/Parametros_umax_fen_exp.xlsx")
DEFAULT_OUTPUT = Path(
    r"D:/BIOSZEN_2.0/BIOSZEN/tools/outputs/platemap_Parametros_umax_fen_exp.xlsx"
)

META_COLS = [
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
            "Convert grouped parameter sheets into a BIOSZEN platemap "
            "(Datos + PlotSettings)."
        )
    )
    parser.add_argument(
        "--input",
        type=str,
        default=str(DEFAULT_INPUT),
        help="Path to grouped-parameters workbook (.xlsx).",
    )
    parser.add_argument(
        "--output",
        type=str,
        default=str(DEFAULT_OUTPUT),
        help="Output path for platemap workbook (.xlsx).",
    )
    parser.add_argument(
        "--merge-mode",
        type=str,
        choices=["intersection", "union"],
        default="intersection",
        help=(
            "intersection: keep only keys present in all sheets (default, safer). "
            "union: keep all keys and allow missing parameter values."
        ),
    )
    parser.add_argument(
        "--control-label",
        type=str,
        default="Control",
        help=(
            "Optional label replacement for Media='Control'. "
            "Use 'Mock' if preferred."
        ),
    )
    return parser.parse_args()


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


def nice_ceiling(value: float) -> float:
    if not np.isfinite(value) or value <= 0:
        return 1.0
    exponent = math.floor(math.log10(value))
    base = 10.0**exponent
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


def gen_wells(n: int) -> list[str]:
    """Generate 96-well style positions: A1..A12, B1..B12, ..."""
    wells: list[str] = []
    for i in range(n):
        row_idx = i // 12
        col_idx = (i % 12) + 1
        row_label = ""
        n_tmp = row_idx + 1
        while n_tmp > 0:
            n_tmp, rem = divmod(n_tmp - 1, 26)
            row_label = chr(ord("A") + rem) + row_label
        wells.append(f"{row_label}{col_idx}")
    return wells


def normalize_sheet(sheet_df: pd.DataFrame, parameter_name: str) -> tuple[pd.DataFrame, list[str]]:
    df = sheet_df.copy()
    if "Strain" not in df.columns or "RepBiol" not in df.columns:
        raise ValueError(
            f"Sheet '{parameter_name}' must contain 'Strain' and 'RepBiol' columns."
        )

    df = df[df["Strain"].notna() & df["RepBiol"].notna()].copy()
    if df.empty:
        raise ValueError(f"Sheet '{parameter_name}' has no non-empty Strain/RepBiol rows.")

    df["Strain"] = df["Strain"].astype(str).str.strip()
    df["RepBiol"] = pd.to_numeric(df["RepBiol"], errors="coerce")
    df = df[df["Strain"].ne("") & df["RepBiol"].notna()].copy()
    if df.empty:
        raise ValueError(
            f"Sheet '{parameter_name}' has no valid Strain/RepBiol values after cleaning."
        )

    df["RepBiol"] = df["RepBiol"].astype(int)

    media_cols = [c for c in df.columns if c not in ("Strain", "RepBiol")]
    media_cols = [c for c in media_cols if str(c).strip() != ""]
    if not media_cols:
        raise ValueError(f"Sheet '{parameter_name}' has no media/condition columns.")

    long_df = df.melt(
        id_vars=["Strain", "RepBiol"],
        value_vars=media_cols,
        var_name="Media",
        value_name=parameter_name,
    )
    long_df["Media"] = long_df["Media"].astype(str).str.strip()
    long_df = long_df[long_df["Media"].ne("")].copy()
    long_df[parameter_name] = pd.to_numeric(long_df[parameter_name], errors="coerce")

    return long_df, media_cols


def main() -> None:
    args = parse_args()

    input_path = Path(args.input).expanduser().resolve()
    output_path = Path(args.output).expanduser().resolve()
    ensure_exists(input_path, "Input workbook")

    xl = pd.ExcelFile(input_path)
    sheet_names = [s for s in xl.sheet_names if str(s).strip() != ""]
    if not sheet_names:
        raise ValueError("Input workbook has no valid sheets.")

    parameter_frames: list[pd.DataFrame] = []
    media_order: list[str] = []
    first_strain_order: list[str] = []
    first_sheet_seen = False

    for sheet_name in sheet_names:
        raw = pd.read_excel(input_path, sheet_name=sheet_name)
        long_df, sheet_media_order = normalize_sheet(raw, sheet_name)

        if not first_sheet_seen:
            first_sheet_seen = True
            media_order = [str(m).strip() for m in sheet_media_order]
            first_strain_order = (
                raw.loc[raw["Strain"].notna(), "Strain"]
                .astype(str)
                .str.strip()
                .loc[lambda s: s.ne("")]
                .drop_duplicates()
                .tolist()
            )

        long_df = long_df.rename(columns={"RepBiol": "BiologicalReplicate"})
        parameter_frames.append(long_df)

    if not parameter_frames:
        raise ValueError("No usable parameter sheets found.")

    merge_keys = ["Strain", "Media", "BiologicalReplicate"]
    merged = parameter_frames[0].copy()
    join_how = "inner" if args.merge_mode == "intersection" else "outer"
    for frame in parameter_frames[1:]:
        merged = merged.merge(frame, on=merge_keys, how=join_how)

    if merged.empty:
        raise ValueError(
            "Merged table is empty. Check sheet consistency or use --merge-mode union."
        )

    merged["Media"] = merged["Media"].replace({"Control": args.control_label})

    if not first_strain_order:
        first_strain_order = (
            merged["Strain"].astype(str).str.strip().drop_duplicates().tolist()
        )
    strain_rank = {s: i for i, s in enumerate(first_strain_order)}

    if not media_order:
        media_order = merged["Media"].astype(str).str.strip().drop_duplicates().tolist()
    media_order = [args.control_label if m == "Control" else m for m in media_order]
    media_rank = {m: i for i, m in enumerate(media_order)}

    merged[".strain_rank"] = merged["Strain"].map(strain_rank).fillna(10**6).astype(int)
    merged[".media_rank"] = merged["Media"].map(media_rank).fillna(10**6).astype(int)

    merged = merged.sort_values(
        by=[".media_rank", ".strain_rank", "BiologicalReplicate"],
        kind="stable",
    ).reset_index(drop=True)

    merged["TechnicalReplicate"] = "A"
    merged["Replicate"] = merged["BiologicalReplicate"].astype(int)

    order_map: dict[tuple[str, str], int] = {}
    order_counter = 1
    for strain in first_strain_order:
        for media in media_order:
            order_map[(strain, media)] = order_counter
            order_counter += 1
    merged["Orden"] = [
        order_map.get((strain, media), order_counter)
        for strain, media in zip(merged["Strain"], merged["Media"], strict=True)
    ]

    merged["Well"] = gen_wells(len(merged))

    parameter_cols = [s for s in sheet_names if s in merged.columns]
    datos = merged[META_COLS + parameter_cols].copy()
    for col in ["Orden", "Replicate", "BiologicalReplicate"]:
        datos[col] = pd.to_numeric(datos[col], errors="coerce").astype("Int64")

    plot_rows: list[dict[str, Any]] = []
    for param in parameter_cols:
        y_max, interval = compute_axis(datos[param])
        plot_rows.append(
            {
                "Parameter": param,
                "Y_Max": y_max,
                "Interval": interval,
                "Y_Title": param,
            }
        )
    plot_settings = pd.DataFrame(plot_rows)

    source_mapping = datos[["Well", "Strain", "Media", "BiologicalReplicate"]].copy()
    source_mapping["SourceKey"] = (
        source_mapping["Strain"].astype(str)
        + "||"
        + source_mapping["Media"].astype(str)
        + "||"
        + source_mapping["BiologicalReplicate"].astype(str)
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    backup_path = backup_if_exists(output_path)

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        datos.to_excel(writer, sheet_name="Datos", index=False)
        plot_settings.to_excel(writer, sheet_name="PlotSettings", index=False)
        source_mapping.to_excel(writer, sheet_name="SourceMapping", index=False)

    missing_total = int(datos[parameter_cols].isna().sum().sum()) if parameter_cols else 0
    print(f"Generated platemap: {output_path}")
    if backup_path is not None:
        print(f"Backup created: {backup_path}")
    print(f"Input sheets merged as parameters: {len(parameter_cols)}")
    print(f"Merge mode: {args.merge_mode}")
    print(f"Rows in Datos: {len(datos)}")
    print(f"Missing parameter cells in Datos: {missing_total}")
    print(f"Strains: {sorted(datos['Strain'].astype(str).unique().tolist())}")
    print(f"Media order: {media_order}")
    print(f"Biological replicate range: {datos['BiologicalReplicate'].min()}..{datos['BiologicalReplicate'].max()}")


if __name__ == "__main__":
    main()
