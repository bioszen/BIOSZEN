# BIOSZEN Package

[![Build binary](https://github.com/user/repo/actions/workflows/build-binary.yaml/badge.svg)](https://github.com/user/repo/actions/workflows/build-binary.yaml)
[![R tests](https://github.com/user/repo/actions/workflows/r-tests.yml/badge.svg)](https://github.com/user/repo/actions/workflows/r-tests.yml)


BIOSZEN is a modular Shiny application for biochemical data analysis. The main
script lives in `inst/app/app.R`. At the repository root there is a tiny
`app.R` loader that calls `shinyAppDir('inst/app')` so the application runs
correctly whether launched from the project directory or from the installed
package. Install the package and launch
the app with:

```R
BIOSZEN::run_app()
```

If the package is not installed, follow these steps to install it from a
local tarball and then launch the application:

1. Descarga o genera el archivo `BIOSZEN_*.tar.gz`.
2. Abre R o RStudio y ejecuta:

   ```R
   install.packages("ruta/al/BIOSZEN_1.2.0.tar.gz",
                    repos = NULL, type = "source")
   library(BIOSZEN)
   BIOSZEN::run_app()
   ```

User guides in English and Spanish are located in `inst/app/www`. They explain
how to use each button and workflow step.

## Structure
- All modules live inside `inst/app` and are sourced at runtime.
- `R/run_app.R` exposes the `run_app()` function.
- Tests under `tests/` ensure all R scripts parse correctly. Este flujo de
  trabajo también construye el paquete y publica el artefacto `.tar.gz`.

You can add more modules by placing R scripts in `inst/app/{params,stats,graficos}`.
They will be sourced automatically when the app launches.


## Downloading the package
The `build-binary.yaml` workflow now creates ready-to-download builds for both
platforms:

- **BIOSZEN-macos-tarball** (macOS): contiene `BIOSZEN_*.tar.gz` y
  `inst/launchers/App.R`.
- **BIOSZEN-windows-binary** (Windows): incluye `BIOSZEN_*.tar.gz` y
  `inst/launchers/App.R` para ejecutarlo con `Rscript`. El lanzador instala
  directamente desde el tar.gz (ya no se genera zip binario).

- **BIOSZEN-mac-App.R / BIOSZEN-win-App.R**: lanzadores de un solo archivo.
  Para macOS se incluye el paquete embebido; para Windows se distribuye sin
  payload para evitar un archivo gigante de miles de líneas. Ejecuta
  `Rscript BIOSZEN-*-App.R` y se instala en `R_libs` local, abriendo la app en
  el navegador predeterminado (ventana tipo app si es posible).
- **BIOSZEN-mac-portable.tar.gz / BIOSZEN-win-portable.zip**: bundles
  portables sin autoextracción. Incluyen el paquete (`tar.gz` en ambos casos) y
  `App.R` en una carpeta comprimida para minimizar alertas de antivirus; solo
  descomprime y ejecuta el script con `Rscript`.

To download either artifact, go to the *Actions* tab, open the **Build binary**
run and click the artifact you need. If you prefer to generate the package
locally, run:

```bash
R CMD build .
```

Esto producirá el archivo `BIOSZEN_*.tar.gz` en tu directorio de trabajo.

  ### Lanzadores sencillos (sin instalaciones ocultas)
  Los artefactos incluyen el backend común `inst/launchers/App.R`, que fija
  host/puerto (127.0.0.1:4321), evita abrir el navegador desde R y usa una
  librería local `R_libs` para dejar el entorno autocontenido. Si no encuentra
  BIOSZEN en `R_libs` o el `BIOSZEN_*.tar.gz` trae una versión superior, instala
  o actualiza dentro de `R_libs` y luego lanza la app.

  - **Windows:** abre una terminal en la carpeta extraída y ejecuta
    `Rscript App.R`. Esto instala las dependencias en `R_libs` dentro de la
    carpeta y arranca la app en el navegador.
  - **macOS:** ejecuta `Rscript App.R` desde la carpeta extraída (Terminal). El
    script usa `R_libs` local y no requiere instaladores adicionales.

  Estos lanzadores asumen que tienes R instalado en el sistema. Si R no está en
  tu `PATH`, usa la ruta estándar de tu instalación o ajústala antes de ejecutar
  `Rscript App.R`.

### Bundles portables para minimizar alertas
  - **macOS:** descarga `BIOSZEN-mac-portable.tar.gz`, descomprímelo en una
    carpeta de tu elección y ejecuta `Rscript App.R`. No hay binarios
    autoextraíbles ni instaladores adicionales.
  - **Windows:** descarga `BIOSZEN-win-portable.zip`, descomprímelo y ejecuta
    `Rscript App.R` desde esa carpeta. El bundle solo usa scripts y el paquete
    BIOSZEN; no incluye ejecutables SFX ni instaladores silenciosos para reducir
    falsos positivos de antivirus.

### ¿Cómo funciona cada caso? (explicación completa)

  **Caso 1: artefacto estándar (tar.gz) + App.R**
  - Descarga el artefacto correspondiente desde *Actions* (`BIOSZEN-macos-tarball`
    o `BIOSZEN-windows-binary`).
  - El artefacto trae el paquete (`BIOSZEN_*.tar.gz` en macOS y Windows) y
    `inst/launchers/App.R`.
  - Pasos en el equipo:
    1. Asegúrate de tener R instalado.
    2. (Opcional) instala el paquete manualmente desde el archivo incluido.
    - macOS/Linux/Windows: `install.packages('BIOSZEN_*.tar.gz', repos = NULL, type = 'source')`
    3. Ejecuta `Rscript App.R` dentro de la carpeta extraída. El script usa
       `R_libs` local, instala o actualiza el paquete desde el tar.gz incluido
       si no lo detecta o si el tar.gz tiene una versión superior, y abre la app
       en 127.0.0.1:4321.
  - Lo que hace internamente: el script fija host/puerto, evita abrir navegador desde R
    y usa la librería local `R_libs`. Si BIOSZEN no está en `R_libs` o el tarball
    tiene versión superior, instala/actualiza dentro de `R_libs` antes de correr
    `BIOSZEN::run_app()`.

**Caso 2: bundle portable para macOS (`BIOSZEN-mac-portable.tar.gz`)**
- Requisitos: tener R instalado. No necesita instalar BIOSZEN previamente.
- Flujo tras descomprimir:
  1. Descomprime el tar.gz en cualquier carpeta (no hay autoextracción ni
     instalador).

  2. Ejecuta `Rscript App.R` desde esa carpeta.

  3. `App.R` crea `R_libs` dentro de la carpeta del bundle, instala o actualiza
     BIOSZEN desde el tarball incluido si no lo detecta o si el tarball tiene
     versión superior, y lanza la app en 127.0.0.1:4321 sin abrir navegador desde R.

**Caso 3: bundle portable para Windows (`BIOSZEN-win-portable.zip`)**
- Requisitos: tener R instalado. No necesita instalación manual del paquete.
- Flujo tras descomprimir:
  1. Extrae el zip en una carpeta (no hay ejecutable SFX ni instalación
     silenciosa, lo que reduce falsos positivos de antivirus).
  2. Ejecuta `Rscript App.R` desde esa carpeta.
  3. `App.R` crea la librería local `R_libs`, instala o actualiza BIOSZEN desde
     el `BIOSZEN_*.tar.gz` que viene en la carpeta si no lo detecta o si ese
     tar.gz tiene versión superior, y llama a `BIOSZEN::run_app()` en el puerto
     4321. La app se abre en el navegador predeterminado, mientras el proceso
     sigue en segundo plano.

**Notas generales**
- En todos los casos el paquete se instala solo dentro de la carpeta extraída
  (`R_libs`), por lo que no modifica las bibliotecas globales de R ni toca
  otras rutas del sistema.

- El lanzador usa siempre la librería local `R_libs` para BIOSZEN y solo
  actualiza si el `BIOSZEN_*.tar.gz` incluido tiene una versión mayor. La
  instalación global no se usa para evitar interferencias.

## Instalación local desde un tarball

Una vez tengas el archivo `BIOSZEN_*.tar.gz`, sigue estos pasos:

1. Abre R o RStudio en la carpeta donde guardaste el tarball.
2. Ejecuta:

   ```R
   install.packages("BIOSZEN_1.2.0.tar.gz", repos = NULL, type = "source")
   library(BIOSZEN)
   BIOSZEN::run_app()
   ```

Esto instalará el paquete y abrirá la aplicación.
