# Manual de Usuario BIOSZEN (Español)

Guía práctica para usar BIOSZEN desde archivos crudos hasta salidas reproducibles.

![Vista general de BIOSZEN](manual_images/01_app_home_overview.png)

> **IMPORTANT:**
> Si es posible, usa el modo **Platemap + Curvas**. Es el flujo con mejor soporte para estadística, control de calidad de réplicas y exportaciones completas.

> **TIP:**
> Mantén este manual abierto mientras trabajas. Cada sección incluye acciones rápidas y referencia técnica.

## Mapa del Manual

- [1. Antes de Empezar](#1-antes-de-empezar)
- [2. Inicio Rápido por Escenario](#2-inicio-rápido-por-escenario)
- [3. Elegir un Modo de Entrada](#3-elegir-un-modo-de-entrada)
- [4. Especificaciones de Entrada](#4-especificaciones-de-entrada)
- [5. Flujo Estándar](#5-flujo-estándar)
- [6. Tipos de Gráfico y Controles](#6-tipos-de-gráfico-y-controles)
- [7. Normalización](#7-normalización)
- [8. Estadística](#8-estadística)
- [9. Anotaciones de Significancia](#9-anotaciones-de-significancia)
- [10. Control de Calidad y Réplicas](#10-control-de-calidad-y-réplicas)
- [11. Metadatos y Reproducibilidad](#11-metadatos-y-reproducibilidad)
- [12. Descargas](#12-descargas)
- [13. Módulo de Crecimiento](#13-módulo-de-crecimiento)
- [14. Guía de Solución de Problemas](#14-guía-de-solución-de-problemas)
- [15. Soporte](#15-soporte)

## 1. Antes de Empezar

Requisitos:

- R >= 4.1.
- BIOSZEN ejecutado desde `app.R`, `BIOSZEN::BIOSZEN()` o `BIOSZEN::run_app()`.
- Archivo de datos para **Cargar datos** en `Excel` (`.xlsx`, `.xls`) o `CSV` (`.csv`).
- Archivo de curvas para **Cargar curvas** en `Excel` (`.xlsx`, `.xls`) o `CSV` (`.csv`) cuando las curvas no vienen embebidas en el workbook principal.

Instala el paquete estable desde R-universe con:

```r
install.packages(
  "BIOSZEN",
  repos = c(
    "https://bioszen.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
BIOSZEN::BIOSZEN()
```

El lanzador del paquete abre el navegador configurado como predeterminado en el
sistema operativo. Usa `BIOSZEN::BIOSZEN(app_window = TRUE)` para solicitar una
ventana de aplicación independiente de un navegador Chromium. El Addin de
RStudio se instala y registra automáticamente con BIOSZEN; no es necesario
copiar archivos del Addin manualmente. Reinicia RStudio después de instalar o
actualizar BIOSZEN y usa **Addins > Launch BIOSZEN in Browser** para iniciar la
app sin escribir un comando. Opcionalmente, puedes asignar un atajo en **Addins
> Browse Addins > Keyboard Shortcuts**. Usa `launch.browser = FALSE` solo si no
deseas que el navegador se abra automáticamente.

Los métodos independientes mediante `App.R` y el bundle siguen disponibles
para quienes prefieran no instalar BIOSZEN desde un repositorio de paquetes.

Plantillas de referencia disponibles en la app (**Archivos de entrada de referencia (descargar)**) y en:

- `inst/app/www/reference_files/`

Archivos de plantilla:

- [Ejemplo_platemap_parametros.xlsx](reference_files/Ejemplo_platemap_parametros.xlsx)
- [Ejemplo_curvas.xlsx](reference_files/Ejemplo_curvas.xlsx)
- [Ejemplo_parametros_agrupados.xlsx](reference_files/Ejemplo_parametros_agrupados.xlsx)
- [Ejemplo_input_summary_mean_sd.xlsx](reference_files/Ejemplo_input_summary_mean_sd.xlsx)

> **NOTE:**
> En la primera ejecución se pueden instalar dependencias en `R_libs`. Conserva esa carpeta para evitar reinstalaciones.

## 2. Inicio Rápido por Escenario

### Escenario A: Tengo datos crudos de placa y curvas (recomendado)

1. Carga el platemap en **Cargar datos**.
2. Carga el archivo de curvas en **Cargar curvas**.
3. Selecciona alcance y tipo de gráfico.
4. Aplica filtros y QC de réplicas.
5. Ejecuta estadística y anotaciones.
6. Exporta gráfico, tablas, metadatos y bundle ZIP.

### Escenario B: Solo tengo datos agrupados o resumen

1. Carga el archivo agrupado/resumen en **Cargar datos**.
2. Configura gráficos y filtros.
3. Ejecuta la estadística disponible para ese modo.
4. Exporta gráficos y metadatos.

### Escenario C: Necesito mejor rendimiento con alto volumen

1. Comienza con `.csv` en **Cargar datos**.
2. Mantén pocos parámetros seleccionados durante iteración.
3. Activa capas avanzadas solo al final.

### Escenario D: Necesito un script reproducible en R

- Inicia la misma app con `BIOSZEN::BIOSZEN()`; `BIOSZEN::run_app()` sigue disponible.
- En RStudio, el comando instalado automáticamente **Addins > Launch BIOSZEN in Browser** ofrece el mismo inicio con un clic y abre el navegador predeterminado del sistema operativo.
- Usa `BIOSZEN::growth_parameters()` para obtener los mismos parámetros de crecimiento que la pestaña de crecimiento sin abrir la interfaz visual.
- `growth_parameters()` acepta data frames anchos/ordenados, uno o más archivos `.xlsx`/`.xls`/`.csv`, o una carpeta. No escribe archivos salvo que se indique `output_dir`.
- Usa `BIOSZEN::bioszen_update_available()` para revisar actualizaciones y `BIOSZEN::bioszen_update()` para instalar una después de confirmarla y cerrar la app.
- Cuando BIOSZEN se inicia como paquete de R instalado, el botón azul **Actualizar** mantiene el mismo flujo de actualización: informa si no existe una versión estable más reciente y, si existe, muestra ambas versiones antes de solicitar confirmación.
- Cuando BIOSZEN se inicia desde el bundle standalone local, el botón cambia a **Instalar paquete**. Después de confirmar, BIOSZEN se cierra e instala el paquete estable de R-universe en la biblioteca personal de R. Reinicia R y luego usa `BIOSZEN::BIOSZEN()`. Si ya existe una instalación normal del paquete, BIOSZEN lo informa en lugar de instalarlo nuevamente.
- **Seguridad de instalación y actualización:** ninguna acción instala silenciosamente. Guarda o descarga el trabajo no guardado antes de confirmar. BIOSZEN cierra Shiny antes de modificar archivos del paquete, y las comprobaciones no envían datos experimentales cargados, información personal ni credenciales. Si se interrumpe la descarga o la biblioteca de R está bloqueada, el launcher standalone continúa disponible; reinicia R y vuelve a intentarlo o ejecuta `BIOSZEN::bioszen_update()` para una instalación existente.
- Usa `BIOSZEN::bioszen_citation()` o `citation("BIOSZEN")` para la cita oficial; usa `BIOSZEN::bioszen_citation("rrid")` para el identificador de recurso de investigación y `BIOSZEN::bioszen_citation("methods")` para una frase lista para la sección Métodos.

![Configuración de gráficos y capas](manual_images/02_plot_setup_layers.png)

## 3. Elegir un Modo de Entrada

- **Platemap + Curvas**  
  Ideal cuando: Necesitas el flujo más completo.  
  Limitaciones principales: Requiere mapeo estricto de wells y estructura de hojas.

- **Parámetros agrupados**  
  Ideal cuando: Solo necesitas parámetros y estadística.  
  Limitaciones principales: Curvas requiere hojas embebidas tipo `Curves_Summary` (o cargar un archivo aparte en **Cargar curvas**).

- **Resumen (Media/SD/N)**  
  Ideal cuando: No dispones de réplicas crudas por fila.  
  Limitaciones principales: Algunas rutas de normalidad/no paramétrica pueden limitarse.

- **Modo CSV**  
  Ideal cuando: Tienes datasets grandes y buscas IO más liviano.  
  Limitaciones principales: Metadatos siguen en `.xlsx`.

## 4. Especificaciones de Entrada

### 4.1 Workbook de platemap

Hojas requeridas:

- `Datos`: metadata + parámetros.
- `PlotSettings`: configuración de ejes por parámetro.

Columnas esperadas en `Datos`:

- `Well`: ID de pocillo (`A1`, `B3`, etc.), clave para vincular curvas.
- `Strain`: cepa o grupo biológico.
- `Media`: condición/tratamiento (`Control`, `Drug A`, etc.).
- `BiologicalReplicate`: ID de réplica biológica (`1`, `2`, `3`, ...).
- `TechnicalReplicate`: réplica técnica dentro de cada réplica biológica (`A`, `B`, `C` o `1`, `2`, `3`).
- `Replicate` (compatibilidad): campo alternativo legado para réplica biológica.
- `Orden`: entero para orden de visualización/exportación.
- Columnas de parámetros: una o más variables numéricas.

Regla práctica de consistencia:

- `Strain` + `Media` + `BiologicalReplicate` + `TechnicalReplicate` debería identificar cada fila experimental de forma estable.

Columnas esperadas en `PlotSettings`:

- `Parameter`
- `Y_Max`
- `Interval`
- `Y_Title`

### 4.2 Archivo de curvas

Excel (`.xlsx`, `.xls`):

- `Sheet1`: primera columna `Time`, columnas restantes por well (`A1`, `A2`, ...).
- `Sheet2`: `X_Max`, `Interval_X`, `Y_Max`, `Interval_Y`, `X_Title`, `Y_Title`.

CSV (`.csv`):

- Primera columna `Time`, columnas restantes por well (`A1`, `A2`, ...).
- Configuración de ejes auto-generada:
  - `X_Max` y `Y_Max`: máximos observados.
  - `Interval_X` y `Interval_Y`: `max/4`.
  - `X_Title` y `Y_Title`: vacíos por defecto.

> **WARNING:**
> Los errores de merge de curvas suelen deberse a inconsistencias entre `Well` (platemap) y los encabezados de curvas.

### 4.3 Modo parámetros agrupados

- Carga el archivo agrupado en **Cargar datos**.
- Diseñado para gráficos/estadística de parámetros desde hojas agrupadas (por ejemplo `Parametro_1`, `Parametro_2`, ...).
- Soporta curvas embebidas opcionales mediante hojas de resumen de curvas en el mismo workbook.
- Mantén el flujo en **Cargar datos** para archivos agrupados (no subirlos en **Cargar curvas**).

### 4.4 Modo resumen

- Carga el archivo resumen en **Cargar datos**.
- BIOSZEN detecta resumen de parámetros con cualquiera de estos nombres de hoja:
  - `Parameters_Summary`
  - `Parametros_Summary`
  - `Summary_Parameters`
  - `Resumen_Parametros`
- BIOSZEN detecta resumen de curvas embebidas con cualquiera de estos nombres:
  - `Curves_Summary`
  - `Curvas_Summary`
  - `Summary_Curves`
  - `Resumen_Curvas`
- Útil cuando no existen réplicas crudas por fila.
- El gráfico de curvas requiere un archivo válido en **Cargar curvas** o una hoja de resumen de curvas embebida.

### 4.5 Modo CSV

- **Cargar datos** acepta `.csv` y detecta delimitador automáticamente (`,`, `;`, tab, `|`).
- BIOSZEN intenta convertir perfiles no platemap a un formato compatible.
- **Cargar curvas** también acepta `.csv` (`Time` + wells).

### 4.6 Skill de IA opcional para preparar entradas

El repositorio fuente/GitHub incluye una skill opcional para agentes de IA en la
[carpeta de GitHub `skills/bioszen-platemap-curves/`](https://github.com/bioszen/BIOSZEN/tree/main/skills/bioszen-platemap-curves).
Para usarla, entrega ese URL de la carpeta de GitHub a la IA o agente
correspondiente para que lea o adquiera la skill. Si el agente necesita archivos
locales, descarga el ZIP del repositorio desde
<https://github.com/bioszen/BIOSZEN/archive/refs/heads/main.zip> y copia la
carpeta `skills/bioszen-platemap-curves/` al sistema de skills de tu agente.

Usa esta skill desde Codex, Claude, Antigravity u otras herramientas agénticas
similares cuando necesites generar un platemap `Datos` + `PlotSettings` desde
cualquier archivo legible con datos, corregir un platemap existente, reparar
errores de tipeo en nombres de parámetros entre columnas de `Datos` y
`PlotSettings$Parameter`, preparar un workbook de curvas separado, o validar
que `Datos$Well` coincida exactamente con los encabezados de curvas antes de
subir los archivos a BIOSZEN.

La skill es un extra de documentación/herramientas. No modifica la app BIOSZEN
y no asume nombres fijos de parámetros, etiquetas fijas de experimento ni un
tipo específico de medición.

## 5. Flujo Estándar

1. Carga el archivo principal de datos.
2. Opcionalmente carga/mergea curvas.
3. Opcionalmente carga metadatos.
4. Elige alcance (`Por cepa` o `Combinado`).
5. Elige tipo de gráfico.
6. Aplica filtros y selección de réplicas.
7. Opcionalmente normaliza por control.
8. Ejecuta estadística.
9. Agrega anotaciones de significancia.
10. Exporta salidas.

![Filtrado por media/condiciones](manual_images/03_filter_media_conditions.png)

## 6. Tipos de Gráfico y Controles

### Caja

- Ideal para distribución de réplicas crudas.
- Controles: jitter, ancho de caja, tamaño de punto.
- Soporta anotaciones manuales/automáticas.
- `Voltear orientación (horizontal)` mejora legibilidad con etiquetas largas.

### Barras

- Ideal para comparación resumida por grupo.
- Soporta barras de error y puntos crudos opcionales.
- Orientación horizontal disponible.

### Violín

- Ideal para forma de distribución + réplica superpuesta.
- Comparte flujo de anotaciones con Caja/Barras.
- Orientación horizontal disponible.

### Apilado

- Selector y orden de parámetros.
- Configuración de barras de desviación y colores.
- La estadística y la significancia automática están disponibles por cada parámetro incluido. Las comparaciones se hacen dentro de cada parámetro, por ejemplo `Parámetro A - Grupo 1` contra `Parámetro A - Grupo 2`, no contra otro segmento apilado.
- Las etiquetas de significancia se pueden agregar sobre el grupo objetivo seleccionado para el parámetro seleccionado; la tabla de resultados incluye una columna `Parameter`.
- Para gráficos apilados se recomiendan las anotaciones como etiquetas.
- Orientación horizontal disponible; al voltear el gráfico se conservan leyendas, estilos de texto, barras de error y etiquetas de significancia.

### Correlación

- Selección de parámetros X/Y.
- Métodos: Pearson, Spearman, Kendall.
- Capas opcionales: recta, `r`, `p`, `R2`, ecuación.
- Panel avanzado con cribado uno-contra-todos y exportación Excel.

### Concentración-respuesta / dosis-respuesta

- Selecciona el parámetro de respuesta, la serie del compuesto, las cepas, las réplicas biológicas y las condiciones que se incluirán. Permanecen activos los mismos filtros de grupos y réplicas de los demás gráficos.
- BIOSZEN reconoce concentraciones desde los nombres de las condiciones y permite corregir manualmente cada concentración y unidad. Todas las filas incluidas deben usar una misma unidad mostrada antes del ajuste.
- Cada cepa se ajusta independientemente con un modelo inhibitorio log-logístico de cuatro parámetros (`LL.4`). Los puntos por réplica biológica siguen siendo la opción predeterminada. **Visualización de réplicas** permite mostrar en su lugar la media con DE/SEM u ocultar todos los puntos y dejar solo las curvas ajustadas.
- **Mostrar bandas de confianza del 95%** controla si se dibuja el margen sombreado de incertidumbre y permanece activo de forma predeterminada. Puede combinarse con cualquier visualización de réplicas, incluidas curvas ajustadas con bandas pero sin puntos. Estas opciones visuales no modifican los modelos ajustados ni los resultados estadísticos. La leyenda usa siempre el color opaco de la línea de la curva ajustada; las bandas de confianza no crean ni reemplazan sus claves.
- La respuesta puede ser el parámetro bruto o su valor normalizado al control. Las respuestas normalizadas se ajustan como porcentaje del control.
- Un IC50 menor indica mayor susceptibilidad solo si el ajuste es inhibitorio, el IC50 queda dentro del rango de concentraciones evaluado y la incertidumbre es aceptable. Los valores informados como `> máximo evaluado`, `< mínimo evaluado` o no estimables se excluyen del ranking de susceptibilidad.
- Los límites, intervalos y títulos de ejes, el grosor de la curva ajustada, el tamaño y contorno negro de los puntos y la opacidad de la banda de confianza solo cambian la presentación. No reajustan la curva ni modifican los parámetros. El intervalo X se usa en el eje lineal; el eje X logarítmico utiliza espaciado logarítmico automático.

#### Interpretación de valores de réplica y parámetros de curva

| Salida | Interpretación |
|---|---|
| `Strain`, `Parameter`, `Compound`, `ConcentrationUnit` | Identifican la cepa ajustada, la respuesta seleccionada, la serie del tratamiento y la unidad de concentración. No deben compararse parámetros en unidades distintas como si estuvieran en la misma escala. |
| `Condition`, `Concentration` | Etiqueta original de la condición/grupo y concentración numérica corregida usada para esa fila. Debe revisarse siempre el mapeo de concentraciones antes del ajuste. |
| `BiologicalReplicate`, `TechnicalReplicate` | Identificadores conservados en la hoja de valores por réplica. Las réplicas técnicas seleccionadas se promedian dentro de cada réplica biológica antes del ajuste no lineal; las réplicas biológicas son las observaciones independientes del modelo. |
| `RawValue` | Valor observado del parámetro conservado después de aplicar los filtros de grupos y réplicas. |
| `NormalizedValue` | Valor observado expresado como porcentaje del control seleccionado cuando la normalización está activa. |
| `ModelValue` | Valor realmente entregado al modelo: bruto o normalizado según el modo seleccionado. |
| `ResultBasis` | Indica qué parámetro de respuesta se usó para calcular el IC50 informado. |
| `IC50` / `Resultado IC50` | Concentración que produce el 50% del efecto inhibitorio ajustado respecto de la respuesta superior del modelo. Un valor menor suele indicar mayor susceptibilidad. Debe interpretarse solo si es estimable dentro del rango evaluado. |
| `ED50` | Dosis efectiva relativa del 50%. En la implementación inhibitoria `LL.4` actual corresponde numéricamente a la misma concentración ajustada que el IC50. |
| `EC50` | Campo convencional de concentración efectiva media. Se informa como `NA` porque la ruta actual de BIOSZEN ajusta un modelo inhibitorio IC50/ED50 y no un modelo estimulador EC50 independiente. |
| `IC50_SE` | Error estándar del IC50 calculado por método delta. Valores mayores indican menor precisión. |
| `CI_Lower`, `CI_Upper` (límites inferior/superior del IC 95% en la app) | Intervalo de confianza del IC50. Intervalos anchos o límites muy fuera del rango evaluado indican baja precisión. |
| `HillSlope` | Parámetro de forma que controla la pendiente de la transición en escala logarítmica de dosis. En las curvas inhibitorias decrecientes aceptadas por BIOSZEN es positivo; un valor mayor genera una transición más brusca pero no mide por sí solo susceptibilidad. |
| `LowerAsymptote` | Respuesta ajustada a la que se aproxima la curva a concentraciones altas. Es un límite extrapolado y puede diferir del menor valor observado. |
| `UpperAsymptote` | Respuesta ajustada a la que se aproxima la curva a concentración cero o baja. Es un límite extrapolado y puede diferir del mayor valor observado. |
| `ResponseRange` | `UpperAsymptote - LowerAsymptote`; amplitud de respuesta ajustada. |
| `InflectionPoint` | Concentración central de la transición ajustada. En este modelo normalmente coincide con ED50/IC50 relativo. |
| `MaximumSlope` | Cambio ajustado más pronunciado en el eje de concentración bruto: `-(ResponseRange * HillSlope) / (4 * InflectionPoint)`. Un valor más negativo indica una caída local más rápida pero depende de las unidades de respuesta y concentración. |
| `MaximumSlopeMagnitude` | Valor absoluto de `MaximumSlope`, útil para comparar la intensidad de la pendiente sin el signo inhibitorio negativo. Las comparaciones requieren las mismas unidades. |
| `MinTested`, `MaxTested` | Concentraciones positivas mínima y máxima incluidas en la serie. Definen si el IC50 está dentro del rango experimental. |
| `DoseLevels` | Número de concentraciones distintas incluidas, considerando cero cuando está presente. Más niveles bien distribuidos suelen mejorar la identificabilidad. |
| `BiologicalReplicates` | Número de réplicas biológicas distintas que contribuyen al ajuste de la cepa. |
| `Comparable` | Es `TRUE` solo para un ajuste inhibitorio decreciente con IC50 finito y positivo dentro del rango positivo evaluado. Solo estas filas participan en ranking y comparaciones pareadas de IC50. |
| `SusceptibilityRank` | Ranking de cepas comparables ordenado por IC50 ascendente. Rango 1 corresponde al IC50 menor; es descriptivo si la comparación pareada ajustada no lo respalda. |
| `RelativeToLowestIC50` | IC50 de la cepa dividido por el menor IC50 comparable. La cepa menor vale 1; un valor 2 significa que requirió el doble de concentración para el mismo efecto ajustado del 50%. |
| `Status` / `Estado del ajuste` | Indica si el ajuste es utilizable o por qué no: dosis insuficientes, respuesta plana, falta de convergencia, respuesta no inhibitoria, IC50 no estimable, sobre el rango o bajo el rango. |

#### Interpretación del diagnóstico y comparaciones entre cepas

| Salida | Interpretación |
|---|---|
| `Model` | Modelo usado para la cepa; actualmente el log-logístico de cuatro parámetros (`LL.4`). |
| `Observations` | Número de valores de respuesta de réplicas biológicas usados después del filtrado y del promedio técnico dentro de cada dosis. |
| `ResidualDF` | Grados de libertad residuales: observaciones menos los cuatro parámetros ajustados. |
| `RSS` | Suma de cuadrados residual. Un valor menor es mejor solo al comparar ajustes de la misma respuesta y observaciones. |
| `RMSE` | Error residual típico en unidades de respuesta. Valores menores indican predicciones más cercanas a las observaciones. |
| `R_Squared`, `Adjusted_R_Squared` | Proporción descriptiva de variación representada por la curva; el R² ajustado considera los cuatro parámetros. En modelos no lineales no deben ser el único criterio de aceptación. |
| `AIC`, `BIC` | Criterios de información para comparar modelos sobre los mismos datos y respuesta. Se prefieren valores menores; el valor absoluto no tiene interpretación biológica aislada. |
| `LogLikelihood` | Log-verosimilitud del modelo. Valores mayores indican mejor verosimilitud solo entre modelos comparables ajustados a las mismas observaciones. |
| `LinearSlope`, `LinearSlopeSE` | Pendiente opcional de `Respuesta ~ Concentración` y su error estándar. Resume de forma aproximada todo el rango y no reemplaza al IC50 ni a la pendiente máxima no lineal. |
| `LinearSlopeCI_Lower`, `LinearSlopeCI_Upper` | Intervalo de confianza del 95% de la pendiente lineal. Si incluye cero no respalda una tendencia lineal distinta de cero. |
| `LinearSlopeP_Value` | Prueba opcional de que la pendiente lineal en todo el rango difiere de cero. No prueba igualdad entre valores IC50. |
| `Linear_R_Squared` | R² descriptivo de la tendencia lineal opcional. |
| `Converged` | Indica si se obtuvo correctamente el objeto del ajuste no lineal. La convergencia es necesaria pero no garantiza plausibilidad biológica ni precisión. |
| `StrainA`, `StrainB` | Identifican el par ordenado de cepas usado para la razón y la comparación de Wald. |
| `IC50_Ratio_A_over_B` (razón IC50 A/B en la app) | `IC50_A / IC50_B`. Una razón mayor que 1 significa que la cepa A requirió mayor concentración y es descriptivamente menos susceptible que B. |
| `Ratio_CI_Lower`, `Ratio_CI_Upper` | Intervalo de confianza del 95% por método delta para la razón IC50. Si excluye 1 respalda una diferencia antes de considerar el ajuste por comparaciones múltiples. |
| `P_Value`, `P_Adjusted` | Prueba bilateral de Wald sobre la razón logarítmica de IC50 y su corrección de Holm entre pares. Debe usarse el valor ajustado para la conclusión pareada. |
| `LowerIC50Strain` | Identifica qué integrante del par tiene el IC50 estimable menor; es la cepa descriptivamente más susceptible cuando los ajustes son comparables. |
| `ConclusionCode` / `Interpretación` | Conclusión legible por máquina (`different` o `not_significant`) y su traducción en lenguaje directo en la app, basadas en el resultado ajustado por Holm. |

### Mapa de calor

- Selección de subconjunto de parámetros.
- Escalado: ninguno, por fila o columna.
- Clustering/dendrogramas opcionales.
- Etiquetas de valor en celdas opcionales.

### Matriz de correlación

- Selección múltiple de parámetros.
- Método de correlación + corrección de p-values.
- Opción de mostrar solo etiquetas significativas.

### Curvas

- Configura ejes, etiquetas, grosor de línea y tamaño de puntos de curvas.
- Elige geometría de línea e intervalo de confianza.
- Opción de mostrar trayectorias crudas de réplicas.
- **Tamaño de puntos de curvas** controla los marcadores visibles cuando se selecciona la geometría de línea y puntos. Solo cambia el tamaño de los marcadores; no modifica los valores, la geometría de las líneas ni los resultados estadísticos.

### Controles compartidos de apariencia

- El selector **Estadístico de barras de error** controla las barras de desviación cuando están disponibles:
  - `SD`/`DE`: media +/- desviación estándar.
  - `SEM`: media +/- error estándar.
  - `Min-Max`: mínimo observado a máximo observado; disponible solo en Caja.
- La sección desplegable **Estilo de texto** está disponible para gráficos individuales.
- **Familia tipográfica** se aplica a todo el texto del gráfico actual. Las opciones incluyen fuentes comunes de publicación y sistema como Helvetica, Arial, Calibri, Cambria, Segoe UI, Times New Roman, Georgia, Verdana y variantes relacionadas.
- Negrita, cursiva y subrayado se aplican de forma independiente por tipo de texto: título del gráfico, títulos de ejes, etiquetas de ticks de ejes, leyenda, etiquetas de datos y texto de significancia.
- El estilo de títulos de eje se aplica tanto al título del eje X como al del eje Y cuando esos títulos están visibles. El estilo de etiquetas de ticks se aplica a las etiquetas mostradas en los ejes, sean números o categorías.
- Los controles de leyenda incluyen si se muestra a la derecha cuando corresponde, además del tamaño y estilo del texto de la leyenda (normal, negrita, cursiva y/o subrayado).
- Cada tipo de texto puede tener su propia combinación de estilos; subrayar significancia, por ejemplo, no obliga a subrayar el título ni la leyenda.
- `Voltear orientación (horizontal)`, cuando está disponible, solo cambia la orientación visual. Conserva los mismos valores graficados, leyendas, familia tipográfica, ajustes de negrita/cursiva/subrayado, barras de error y anotaciones de significancia.
- Estos ajustes se aplican a la previsualización y se incluyen al exportar `PNG` y `PDF`.

![Controles de estilo de texto](manual_images/11_text_styling_controls.png)

### Panel de Composición

Flujo recomendado:

1. Crear y editar cada gráfico de origen y luego usar **Añadir al panel**.
2. Abrir **Panel de Composición** y usar el selector para incluir, excluir y ordenar gráficos. Deseleccionar un gráfico lo quita de la composición activa sin borrar el gráfico de origen.
3. Definir filas y columnas. Para distribuciones no rectangulares o celdas repetidas, ingresar la malla; los anchos de columna y altos de fila controlan el tamaño y la posición relativos.
4. Definir ancho y alto de la composición en píxeles. Estos valores determinan el lienzo de previsualización y su proporción; son independientes de los DPI de exportación y del tamaño de diapositiva de PowerPoint.
5. Configurar estilo, leyendas compartidas, tipografía, paleta, texto enriquecido y ajustes opcionales por gráfico. **Aplicar la tipografía de la composición a todos los gráficos** modifica tamaños de etiquetas de ticks, ángulos y alineación X/Y, familia tipográfica y estilos de texto en todos los gráficos incluidos sin cambiar la geometría de los datos.
6. Configurar el formato de diapositiva (`4:3`, `16:9` o personalizado), ancho, alto, orientación y margen de borde.
7. Revisar la previsualización y exportar `PNG`, `PPTX`, `PDF` o el metadata de composición.

#### DPI y dimensiones físicas

- La resolución de exportación predeterminada es **300 DPI**. El rango compatible es **72 a 600 DPI** y el valor sigue siendo editable.
- Los DPI se aplican a salidas raster, incluyendo `PNG`, imagen raster copiada al portapapeles y el respaldo raster que se usa solo cuando no está disponible la representación vectorial editable de PowerPoint.
- `PDF` y la ruta normal editable de `PPTX` son vectoriales, por lo que los DPI no modifican sus elementos vectoriales. El DPI seleccionado igualmente se guarda en el metadata para reproducibilidad y para cualquier respaldo raster.
- La previsualización del navegador utiliza densidad de píxeles de pantalla/CSS. Cambiar los DPI de exportación no redimensiona ni reposiciona la previsualización.
- Aumentar los DPI incrementa la cantidad de píxeles, el tiempo de renderizado, el uso de memoria y el tamaño del archivo. No cambia el ancho/alto de la composición, las proporciones, las posiciones del layout ni las dimensiones de la diapositiva.
- El ancho/alto de composición controlan el lienzo lógico. El ancho/alto de PowerPoint controlan la diapositiva física. Los DPI controlan la calidad de muestreo raster. Son ajustes independientes.
- El metadata guarda el DPI efectivo y lo restaura al cargarlo. El metadata antiguo sin campo DPI usa 300 DPI. Valores ausentes, no numéricos, cero, negativos o fuera de rango vuelven de forma segura a 300 DPI; los demás campos válidos igualmente se restauran.

Los controles de estilo de la composición se aplican en paralelo a todos los gráficos seleccionados. La sección **Estilo de texto** de composición replica los controles de gráficos individuales: la familia tipográfica se aplica a todo el texto de todos los gráficos, mientras que negrita/cursiva/subrayado se seleccionan por separado para títulos, ejes, leyendas, etiquetas de datos y texto de significancia. El metadata de composición también conserva layout, dimensiones, ajustes de diapositiva, DPI, tipografía, configuración de leyendas, paleta, texto enriquecido y ajustes por gráfico.

La exportación a PowerPoint siempre crea una diapositiva y ajusta proporcionalmente el layout de la previsualización dentro de los márgenes seleccionados. Si la diapositiva es demasiado pequeña, BIOSZEN reduce proporcionalmente la composición y avisa al usuario en lugar de superponer o cortar gráficos. Los layouts muy densos, etiquetas largas, fuentes grandes y diapositivas verticales pequeñas pueden requerir una diapositiva mayor, menos gráficos, etiquetas más breves o márgenes más amplios. La tipografía puede diferir levemente entre la previsualización y PowerPoint porque el navegador y PowerPoint usan métricas distintas; BIOSZEN aplica un margen proporcional de seguridad al reducir el layout.

![Configuración de significancia y anotaciones](manual_images/10_significance_annotations.png)

## 7. Normalización

Activa **Normalizar por control** y selecciona un medio control.

- BIOSZEN crea columnas con sufijo `_Norm`.
- Correlación permite normalizar por eje (`ambos`, `solo X`, `solo Y`).
- Si no hay emparejamiento estricto, se aplica lógica de respaldo.

## 8. Estadística

### Herramientas estadísticas principales

- Shapiro-Wilk: `stats::shapiro.test`
- Kolmogorov-Smirnov: `stats::ks.test`
- Anderson-Darling: `nortest::ad.test`
- ANOVA: `stats::aov`
- Kruskal-Wallis: `stats::kruskal.test`
- Rutas t-test: `rstatix::t_test`, `rstatix::pairwise_t_test`
- Rutas Wilcoxon: `rstatix::wilcox_test`
- Corrección múltiple: `stats::p.adjust`

Rutas post hoc por selección:

- Tukey / Games-Howell: `rstatix`
- Dunn: `rstatix::dunn_test`
- Dunnett: `DescTools::DunnettTest`
- Scheffe, Conover, Nemenyi, DSCF: `PMCMRplus`

Estadística de curvas (`S1`-`S4`):

El acordeón **Estadística de curvas** aparece para gráficos de Curvas. Selecciona uno o más métodos y luego usa **Ejecutar estadística de curvas** para generar la tabla de resultados.

- `S1`: `stats::lm` + `splines::ns` + `stats::anova`
- `S2`: `stats::pnorm` + `stats::pchisq`
- `S3`: `stats::pnorm`
- `S4`: `gcplyr::auc` + comparaciones guiadas por normalidad (`stats::t.test`, `stats::wilcox.test`, `stats::aov`, `stats::kruskal.test`)

Modos de comparación:

- Todos contra todos
- Control contra todos
- Par

Opciones de corrección p-value:

- Holm
- FDR
- Bonferroni
- Ninguna

Para gráficos **Apilados**, la normalidad y la significancia se calculan por separado para cada parámetro incluido. La tabla de salida incluye `Parameter`, y cada comparación por parámetro debe coincidir con la misma comparación ejecutada desde el gráfico de ese parámetro individual.

> **CAUTION:**
> En modo Resumen, la normalidad puede ser `NA` y algunas rutas no paramétricas que requieren datos crudos se desactivan.

## 9. Anotaciones de Significancia

Flujo manual:

1. Selecciona Grupo 1 y Grupo 2.
2. Ingresa etiqueta (`*`, `**`, `***`, `ns`, texto libre).
3. Agrega/reordena/edita/elimina anotaciones.

Flujo automático:

1. Ejecuta pruebas de significancia.
2. Abre opciones de auto-anotación.
3. Define inclusión (`solo significativos` o `todos`).
4. Elige formato (`estrellas` o `p-value`).
5. Reemplaza o agrega anotaciones.

Para gráficos **Apilados**, elige el parámetro antes de agregar una etiqueta. Las etiquetas automáticas conservan la identidad del parámetro y se ubican sobre el grupo objetivo seleccionado para ese parámetro.

## 10. Control de Calidad y Réplicas

Paneles QC para revisar:

- Valores faltantes.
- Outliers por grupo.
- Tamaño muestral y cobertura de réplicas.

### Réplicas biológicas

- Inclusión/exclusión manual.
- Filtrado automático por IQR.
- Selección Keep-N por reproducibilidad.

Comportamiento Keep-N:

- Ordena réplicas por distancia a la mediana del grupo entre parámetros.
- Conserva las de menor puntaje (más reproducibles).

### Réplicas técnicas

Disponible cuando hay estructura técnica válida:

- Pestaña dedicada de QC técnico.
- Selectores por grupo y réplica biológica.
- Botones globales seleccionar/deseleccionar.
- Detección automática de outliers técnicos por IQR.
- Keep-N técnico por subgrupo.

![Filtrado de réplicas biológicas](manual_images/04_filter_biological_replicates.png)

## 11. Metadatos y Reproducibilidad

Flujo de metadatos:

- **Descargar metadatos** para guardar estado actual.
- Reimportar metadatos en sesiones futuras.
- El estado de orientación horizontal se conserva en roundtrip.
- Las opciones tipográficas se conservan en el roundtrip de metadatos, incluyendo familia de letra, tamaños y estado normal/negrita/cursiva/subrayado para título del gráfico, títulos de eje X/Y, etiquetas de ticks de ejes, texto de leyenda, etiquetas de datos y texto de significancia.
- La visibilidad/posición de la leyenda, incluyendo la selección de leyenda a la derecha cuando corresponde, se guarda en metadatos y se aplica nuevamente al cargarlos.
- El tamaño de puntos de curvas se guarda en los metadatos de diseño de curvas y se restaura al cargarlos. Los metadatos de diseño no restauran el orden de grupos/muestras, el ámbito ni la selección de cepa.
- El estadístico de barras de error y la selección de métodos de estadística de curvas se conservan en el roundtrip de metadatos.
- La serie y cepas de dosis-respuesta, la asignación corregida de concentraciones, el modo de visualización de réplicas, la visibilidad de las bandas de confianza, los límites e intervalos de ejes, los títulos de ejes, los tamaños de línea y puntos, el contorno de puntos y la opacidad de la banda se conservan en los metadatos de dosis-respuesta y en las versiones guardadas del gráfico.

Bundle reproducible:

- Guardar versiones de gráficos en sesión.
- Exportar ZIP con gráficos + metadatos.
- Reabrir análisis con configuración consistente.

Cobertura de regresión incluye:

- Orientación horizontal solo en Caja/Barras/Violín/Apilado.
- Persistencia de metadatos roundtrip.
- Verificación de orientación en constructores finales.

## 12. Descargas

Salidas principales:

- Imagen de gráfico (`PNG`, `PDF`, según gráfico).
- Exportación de datos.
- Exportación de metadatos.
- Exportación de estadística.
- Bundle ZIP.
- Tabla de correlación avanzada.
- Exportación de merge platemap/curvas (si se usó merge).

Las exportaciones de gráficos conservan la configuración visual activa, incluyendo familia tipográfica, estilos por tipo de texto (negrita/cursiva/subrayado), estadístico de barras de error seleccionado, etiquetas de significancia y ajustes de ejes/leyenda. Las exportaciones de composición conservan los mismos controles tipográficos en todos los gráficos del layout.

Las exportaciones raster usan **300 DPI de forma predeterminada** y aceptan valores compatibles elegidos por el usuario entre 72 y 600 DPI. El DPI efectivo seleccionado se incluye en el metadata y en las versiones del bundle. Los elementos vectoriales PDF/PPTX no usan DPI; el ancho, alto y tamaño de diapositiva se configuran por separado.

## 13. Módulo de Crecimiento

Soporte de archivos en pestaña crecimiento:

- Tipo aceptado: `Excel` (`.xlsx`).
- Estructuras auto-detectadas:
  - Layout crudo tipo lector/Tecan (normalmente datos desde filas posteriores en `Sheet1`).
  - Tabla procesada desde `A1` (primera columna tiempo, siguientes columnas curvas/wells).

Parámetros extraídos:

- `uMax`: pendiente máxima en fase exponencial.
- `max_percap_time`: ventana temporal de máximo crecimiento per-cápita.
- `doub_time`: tiempo de duplicación (`ln(2) / uMax`).
- `lag_time`: transición previa al crecimiento exponencial.
- `ODmax`: señal/OD máxima medida.
- `max_time`: tiempo en que se alcanza `ODmax`.
- `AUC`: área bajo la curva.
- `OD0`: señal/OD inicial en el primer punto medido de cada curva.

Flujo típico:

1. Carga uno o más archivos de crecimiento.
2. Define tiempo máximo e intervalo.
3. Ejecuta extracción.
4. Descarga ZIP de resultados.
5. Reusa resultados en flujos de gráficos.

Autoguardado y manejo de interrupciones:

- La **Carpeta de autoguardado** opcional se puede escribir manualmente o seleccionar con **Examinar...**.
- Si no quieres autoguardado, deja esta carpeta en blanco y descarga el ZIP con **Descargar resultados** al final.
- Si escribes una carpeta, debe existir previamente. Si la ruta no existe, BIOSZEN muestra un mensaje para corregirla y no inicia esa corrida hasta que la ruta se corrija o se borre.
- Cuando se define una carpeta de autoguardado, los archivos finales `Curvas_*.xlsx` / `Parametros_*.xlsx` se copian allí automáticamente, y la opción normal **Descargar resultados** en ZIP sigue disponible.
- Durante procesos largos, BIOSZEN guarda puntos de control por well en una carpeta temporal `BIOSZEN_growth_checkpoints` dentro de la carpeta de autoguardado seleccionada. Estos puntos de control permiten reanudar una corrida interrumpida desde los wells ya completados, en lugar de empezar desde cero.
- Los puntos de control se eliminan automáticamente después de completar correctamente el proceso o después de reanudarlo con éxito. Solo se conservan cuando el procesamiento se interrumpe antes de terminar.
- **Detener proceso** solicita una cancelación segura. La app puede terminar el well/punto de control actual antes de liberar la corrida para que los archivos parciales sigan siendo utilizables y no se modifique el cálculo de parámetros de crecimiento.

Comando equivalente en R:

```r
parametros <- BIOSZEN::growth_parameters("Curvas.xlsx")
parametros <- BIOSZEN::growth_parameters(
  "Curvas.xlsx",
  output_dir = "resultados_crecimiento",
  overwrite = FALSE
)

parametros_irregulares <- BIOSZEN::growth_parameters_irregular(
  "curva_irregular.xlsx",
  time_column = "Tiempo"
)
```

El comando ejecuta primero el mismo detector robusto y usa el mismo fallback
permisivo solo para los valores que el método robusto no pudo calcular. Las
columnas y resultados numéricos coinciden con la pestaña de crecimiento. Sin
`output_dir`, devuelve el resultado en R y no crea archivos.

Para tiempos registrados desiguales o discontinuos, usa
`BIOSZEN::growth_parameters_irregular()`. Esta función lee directamente los
valores numéricos de tiempo del archivo y puede detectar automáticamente nombres
comunes como `Time`, `Tiempo`, `Hour` u `Hora`; usa `time_column` para seleccionar
una columna específica.

![Flujo de parámetros de crecimiento](manual_images/13_growth_parameters_workflow.png)

## 14. Guía de Solución de Problemas

- **Error al cargar archivo**  
  Causa probable: Hojas/columnas obligatorias faltantes.  
  Qué hacer: Validar estructura y encabezados exactos.

- **No se genera gráfico**  
  Causa probable: Parámetro/grupo ausente tras filtros.  
  Qué hacer: Resetear filtros y validar disponibilidad.

- **Solo aparece Curvas en el selector de tipo de gráfico**  
  Causa probable: No se detectaron columnas de parámetros válidas en el archivo cargado.  
  Qué hacer: Revisar estructura de hojas agrupadas/resumen y encabezados de parámetros, luego recargar.

- **Normalización no disponible**  
  Causa probable: Falta medio control en alcance activo.  
  Qué hacer: Confirmar grupo control en el subconjunto.

- **Estadística deshabilitada**  
  Causa probable: Mismatch entre modo y prueba.  
  Qué hacer: Cambiar prueba o usar modo con datos compatibles.

- **Falla merge de curvas**  
  Causa probable: IDs de well inconsistentes.  
  Qué hacer: Alinear `Well` con columnas de curvas.

- **El workbook agrupado/resumen carga, pero Curvas queda sin datos**  
  Causa probable: Falta la hoja de resumen de curvas embebida.  
  Qué hacer: Agregar `Curves_Summary` (o alias) al workbook, o cargar curvas por separado en **Cargar curvas**.

- **CSV no reconocido**  
  Causa probable: Delimitador erróneo o headers faltantes.  
  Qué hacer: Revisar delimitador y columnas requeridas.

- **Rendimiento lento**  
  Causa probable: Demasiados parámetros/capas activas.  
  Qué hacer: Reducir parámetros y capas pesadas.

## 15. Soporte

Soporte y reporte de errores: `bioszenf@gmail.com`
