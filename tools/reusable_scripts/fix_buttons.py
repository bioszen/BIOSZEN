#!/usr/bin/env python3
"""Normalize selected Shiny actionButton controls in an R UI file.

This script ensures target actionButton calls include:
1) class `w-100` (full width)
2) style rule `white-space: normal;`

Usage:
  python tools/reusable_scripts/fix_buttons.py
  python tools/reusable_scripts/fix_buttons.py --file inst/app/ui/ui_main.R --dry-run
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path


DEFAULT_TARGETS = {
    "runNorm": "btn btn-primary w-100",
    "runSig": "btn btn-primary w-100",
    "runAdvancedStats": "btn btn-primary w-100",
    "sig_auto_apply": "btn btn-primary w-100",
    "sig_update_label": "btn btn-success w-100",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--file",
        default="inst/app/ui/ui_main.R",
        help="Path to R UI file (default: inst/app/ui/ui_main.R)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Report changes without writing the file",
    )
    return parser.parse_args()


def find_call_end(text: str, open_paren_idx: int) -> int:
    depth = 0
    in_quote = ""
    escaped = False

    for idx in range(open_paren_idx, len(text)):
        ch = text[idx]

        if in_quote:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == in_quote:
                in_quote = ""
            continue

        if ch in ("'", '"'):
            in_quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return idx + 1

    raise ValueError("Unbalanced parentheses in actionButton call")


def ensure_class(value: str) -> str:
    classes = [token for token in value.split() if token]
    if "w-100" not in classes:
        classes.append("w-100")
    return " ".join(classes)


def ensure_style(value: str) -> str:
    cleaned = value.strip()
    if not cleaned.endswith(";"):
        cleaned += ";"
    if "white-space: normal;" not in cleaned:
        cleaned += " white-space: normal;"
    return cleaned.strip()


def patch_action_button_call(call_text: str, fallback_class: str) -> tuple[str, bool]:
    changed = False

    class_re = re.compile(r'class\s*=\s*["\']([^"\']*)["\']')
    style_re = re.compile(r'style\s*=\s*["\']([^"\']*)["\']')

    class_match = class_re.search(call_text)
    if class_match:
        old = class_match.group(1)
        new = ensure_class(old)
        if new != old:
            call_text = call_text[: class_match.start(1)] + new + call_text[class_match.end(1) :]
            changed = True
    else:
        insertion = f', class = "{fallback_class}"'
        call_text = call_text[:-1] + insertion + call_text[-1:]
        changed = True

    style_match = style_re.search(call_text)
    if style_match:
        old = style_match.group(1)
        new = ensure_style(old)
        if new != old:
            call_text = call_text[: style_match.start(1)] + new + call_text[style_match.end(1) :]
            changed = True
    else:
        insertion = ', style = "white-space: normal;"'
        call_text = call_text[:-1] + insertion + call_text[-1:]
        changed = True

    return call_text, changed


def patch_file(text: str, targets: dict[str, str]) -> tuple[str, int]:
    escaped_ids = "|".join(re.escape(key) for key in targets)
    pattern = re.compile(rf'actionButton\s*\(\s*["\'](?P<id>{escaped_ids})["\']')

    idx = 0
    patches = 0

    while True:
        match = pattern.search(text, idx)
        if not match:
            break

        button_id = match.group("id")
        open_paren_idx = text.find("(", match.start())
        call_end = find_call_end(text, open_paren_idx)

        original_call = text[match.start() : call_end]
        updated_call, changed = patch_action_button_call(
            original_call, fallback_class=targets[button_id]
        )

        if changed:
            text = text[: match.start()] + updated_call + text[call_end:]
            idx = match.start() + len(updated_call)
            patches += 1
        else:
            idx = call_end

    return text, patches


def main() -> int:
    args = parse_args()
    target_file = Path(args.file).resolve()

    if not target_file.exists():
        raise SystemExit(f"File not found: {target_file}")

    original = target_file.read_text(encoding="utf-8")
    updated, patched_count = patch_file(original, DEFAULT_TARGETS)

    if patched_count == 0:
        print("No target actionButton entries required changes.")
        return 0

    if args.dry_run:
        print(f"Dry run: {patched_count} actionButton call(s) would be updated in {target_file}")
        return 0

    target_file.write_text(updated, encoding="utf-8")
    print(f"Updated {patched_count} actionButton call(s) in {target_file}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
