#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_R="$SCRIPT_DIR/App.R"

if [ ! -f "$APP_R" ]; then
  echo "No se encontro el script de BIOSZEN: $APP_R"
  exit 1
fi

R_BIN="$(command -v Rscript || true)"

# Si R no esta en PATH, prueba la ubicacion tipica del framework de macOS
if [ -z "$R_BIN" ] && [ -x "/Library/Frameworks/R.framework/Resources/bin/Rscript" ]; then
  R_BIN="/Library/Frameworks/R.framework/Resources/bin/Rscript"
fi

if [ -z "$R_BIN" ]; then
  echo "No se detecto Rscript. Instala R desde https://cran.r-project.org/ y vuelve a abrir este launcher para el arranque silencioso."
  exit 1
fi

echo "Iniciando BIOSZEN con $R_BIN..."
"$R_BIN" "$APP_R"
