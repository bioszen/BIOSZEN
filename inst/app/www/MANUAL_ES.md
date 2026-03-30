# Manual de Usuario BIOSZEN (Español)

Este manual explica cómo ejecutar los flujos de BIOSZEN desde la importación de datos hasta la exportación reproducible.

## 1. Requisitos Previos

- R >= 4.1 instalado.
- BIOSZEN ejecutado desde `App.R` o `BIOSZEN::run_app()`.
- Archivos de entrada para **Cargar datos** en `Excel` (`.xlsx`, `.xls`) o `CSV` (`.csv`).
- Archivos de curvas para **Cargar curvas** en `Excel` (`.xlsx`, `.xls`) o `CSV` (`.csv`).

Las plantillas de referencia están disponibles en la app en **Archivos de entrada de referencia (descargar)**:

- `Ejemplo_platemap_parametros.xlsx`
- `Ejemplo_curvas.xlsx`
- `Ejemplo_parametros_agrupados.xlsx`
- `Ejemplo_input_summary_mean_sd.xlsx`

## 2. Modos de Entrada

### 2.1 Modo recomendado: Platemap + Curvas

Usa este modo para tener toda la funcionalidad y mejor soporte estadístico.

1. Carga un archivo de platemap en **Cargar datos**.
2. Carga un archivo de curvas en **Cargar curvas** (`.xlsx`, `.xls` o `.csv`).

Requisitos del platemap:

- Hoja `Datos` con columnas de metadata y parámetros (detalle abajo).
- Hoja `PlotSettings` con configuración de ejes por parámetro (detalle abajo).

Detalle de columnas en hoja `Datos` (platemap):

- `Well`: identificador de pocillo (por ejemplo `A1`, `B3`). Es clave para enlazar correctamente el platemap con el archivo de curvas.
- `Strain`: cepa o grupo biológico.
- `Media`: condición o tratamiento (por ejemplo `Control`, `Drug A`).
- `BiologicalReplicate`: identificador de réplica biológica (recomendado: `1`, `2`, `3`, ...). Representa cultivos/muestras biológicas independientes.
- `TechnicalReplicate`: identificador de réplica técnica dentro de cada réplica biológica (por ejemplo `A`, `B`, `C` o `1`, `2`, `3`).
- `Replicate` (compatibilidad): columna legada/alternativa usada en algunos archivos. Cuando aplica, BIOSZEN la interpreta como referencia de réplica biológica.
- `Orden`: entero para definir el orden de visualización/exportación de los grupos en gráficos y tablas.
- Columnas de parámetros: una o más columnas numéricas con las variables a analizar (por ejemplo viabilidad, fluorescencia, `uMax`, etc.).

Notas prácticas de estructura:

- Para un flujo completo de réplicas y QC, usa explícitamente `BiologicalReplicate` y, si existe, `TechnicalReplicate`.
- La combinación `Strain` + `Media` + `BiologicalReplicate` + `TechnicalReplicate` debe identificar de forma consistente cada fila experimental.
- BIOSZEN reconoce alias comunes para encabezados de `Strain`/`Media` (por ejemplo `Cepa`/`Muestra`, `Condicion`/`Tratamiento`).

Detalle de columnas en hoja `PlotSettings`:

- `Parameter`: nombre exacto de la columna de parámetro en hoja `Datos`.
- `Y_Max`: límite superior inicial del eje Y para ese parámetro.
- `Interval`: separación de marcas del eje Y.
- `Y_Title`: etiqueta de eje Y mostrada en el gráfico.

Requisitos del archivo de curvas:

- En `Excel` (`.xlsx`, `.xls`):
  `Sheet1`: primera columna `Time`, columnas adicionales por well (`A1`, `A2`, etc.).
  `Sheet2`: `X_Max`, `Interval_X`, `Y_Max`, `Interval_Y`, `X_Title`, `Y_Title`.
- En `CSV` (`.csv`):
  primera columna `Time`, columnas adicionales por well (`A1`, `A2`, etc.).
  Es un formato de tabla única (sin hojas adicionales).
  La configuración de ejes se genera automáticamente:
  `X_Max` y `Y_Max` toman el máximo observado, `Interval_X` y `Interval_Y` usan `máximo/4`.
  `X_Title` y `Y_Title` quedan vacíos por defecto.

### 2.2 Modo de parámetros agrupados (solo parámetros)

- Carga el archivo de parámetros agrupados en **Cargar datos**.
- Úsalo cuando solo necesitas gráficos y estadísticas de parámetros.
- No es válido como archivo de curvas.

### 2.3 Modo resumen en un solo archivo (Media/SD/N)

- Carga el archivo resumen en **Cargar datos**.
- Parámetros y curvas pueden detectarse desde hojas de resumen dedicadas.
- Útil cuando no hay filas crudas por réplica.

### 2.4 Modo CSV (alto volumen de datos)

- **Cargar datos** acepta archivos `.csv`.
- Recomendado cuando trabajas con volúmenes grandes por ser un formato más liviano.
- BIOSZEN detecta automáticamente delimitador común (`,`, `;`, tab o `|`).
- Si el CSV no viene en formato platemap directo, BIOSZEN intenta convertirlo al perfil de trabajo compatible.
- **Cargar curvas** también acepta `.csv` para trayectorias por well (`Time` + columnas de well).
- Archivos de metadatos siguen usando `.xlsx`.

## 3. Flujo Estándar

1. Carga el archivo principal de datos.
2. Opcionalmente carga/concatena curvas.
3. Opcionalmente carga archivo de metadatos.
4. Elige alcance (`Por cepa` o `Combinado`).
5. Elige tipo de gráfico.
6. Aplica filtros y selección de réplicas.
7. Opcionalmente normaliza por control.
8. Ejecuta análisis estadísticos.
9. Agrega anotaciones de significancia.
10. Exporta gráficos, tablas, metadatos y bundle.

## 4. Tipos de Gráfico y Uso

### Caja

- Ideal para distribución de réplicas crudas.
- Controla dispersión con jitter, ancho de caja y tamaño de punto.
- Agrega barras o etiquetas de significancia después de estadística.
- Opción **Voltear orientación (horizontal)** para invertir ejes (`X`/`Y`) y mejorar legibilidad cuando hay etiquetas de grupos largas.

### Barras

- Ideal para comparación resumida por grupo.
- Soporta media con barras de error y puntos crudos opcionales (modo crudo).
- Soporta anotaciones manuales y automáticas de significancia.
- Opción **Voltear orientación (horizontal)** para presentar barras horizontales y facilitar comparaciones por categoría.

### Violín

- Ideal para forma de distribución con superposición de réplicas.
- Usa el mismo flujo de anotaciones que caja/barras.
- Opción **Voltear orientación (horizontal)** para intercambiar ejes y priorizar lectura de nombres de grupo.

### Apilado

- Usa selector de parámetros y control de orden de parámetros.
- Configura barras de desviación y comportamiento de color por parámetro.
- Soporta modo de significancia con barras o etiquetas.
- Opción **Voltear orientación (horizontal)** para mostrar apilados en horizontal cuando conviene priorizar etiquetas/categorías.

### Correlación

- Selecciona parámetros X e Y.
- Elige Pearson, Spearman o Kendall.
- Capas opcionales: recta de regresión, r, p, R2, ecuación.
- El panel avanzado permite cribado uno-contra-todos y exportación a Excel.

### Mapa de calor

- Selecciona subconjunto de parámetros.
- Modo de escala: ninguno, por fila o por columna.
- Clustering opcional por filas/columnas con dendrogramas.
- Etiquetas de valores en celdas opcionales.
- Exportación de clusters disponible cuando se activa clustering por filas.

### Matriz de correlación

- Selección múltiple de parámetros.
- Elige método de correlación.
- Corrección por múltiples pruebas (Holm, FDR, Bonferroni, ninguna).
- Opción para mostrar solo etiquetas significativas.

### Curvas

- Configura límites de ejes, etiquetas y grosor de línea.
- Elige geometría de línea y estilo de intervalo de confianza.
- Opción de trayectorias de réplica en modo crudo.

### Panel de Composición (múltiples gráficos)

Flujo recomendado:

1. En cada gráfico individual usa **Añadir al panel**.
2. Abre la pestaña **Panel de Composición**.
3. En **1) Selección de gráficos**, elige qué gráficos combinar y ajusta el orden (subir, bajar, mover al inicio/final o quitar).
4. En **2) Diseño y lienzo**, define filas/columnas, malla personalizada (opcional), anchos/altos de columna/fila y tamaño final en px.
5. En **3) Estilo visual**, ajusta leyenda (modo y lado), tipografía, tamaños globales y paleta.
6. Opcional: agrega cajas de texto enriquecido y overrides por gráfico.
7. Previsualiza y exporta como `PNG`, `PPTX` o `PDF`.

Metadatos de composición:

- **Descargar metadata** guarda diseño y estilo del panel.
- **Cargar metadata composición (.xlsx)** restaura una composición en otra sesión.

## 5. Normalización

Activa **Normalizar por control** y elige un medio control.

- BIOSZEN crea columnas normalizadas con sufijo `_Norm`.
- Correlación permite normalización por eje (`ambos`, `solo X`, `solo Y`).
- Si no existe emparejamiento estricto de control, BIOSZEN aplica lógica de respaldo.

## 6. Estadística

### Pruebas estadísticas y paquetes de R (panel principal)

- Shapiro-Wilk (normalidad): `stats::shapiro.test`
- Kolmogorov-Smirnov (normalidad): `stats::ks.test`
- Anderson-Darling (normalidad): `nortest::ad.test`
- ANOVA (significancia): `stats::aov`
- Kruskal-Wallis (significancia): `stats::kruskal.test`
- t-test (significancia): `rstatix::t_test` y `rstatix::pairwise_t_test`
- Wilcoxon (significancia): `rstatix::wilcox_test`
- Corrección por múltiples pruebas (Holm/FDR/Bonferroni): `stats::p.adjust`

Post hoc disponibles según selección:

- Tukey y Games-Howell: `rstatix`
- Dunn: `rstatix::dunn_test`
- Dunnett: `DescTools::DunnettTest`
- Scheffe, Conover, Nemenyi y DSCF: `PMCMRplus`

Estadística de curvas (S1-S4):

- S1 (diferencia global de forma): `stats::lm` + `splines::ns` + `stats::anova`
- S2 (comparación punto a punto Fisher): `stats::pnorm` + `stats::pchisq`
- S3 (diferencia en endpoint): `stats::pnorm`
- S4 (AUC): `gcplyr::auc`, con selección de prueba por normalidad (`stats::shapiro.test`) y comparación por `stats::t.test`, `stats::wilcox.test`, `stats::aov` o `stats::kruskal.test` según corresponda

Modos de comparación:

- Todos contra todos
- Control contra todos
- Par

Corrección por múltiples pruebas:

- Holm
- FDR
- Bonferroni
- Ninguna

Notas para modo resumen:

- La normalidad puede no estar disponible (`NA`).
- Algunas rutas no paramétricas que requieren observaciones crudas se desactivan.

## 7. Flujo de Anotaciones de Significancia

### Manual

1. Selecciona Grupo 1 y Grupo 2.
2. Ingresa etiqueta (por ejemplo `*`, `**`, `***`, `ns`).
3. Agrega, reordena, edita o elimina anotaciones.

### Automático

1. Ejecuta pruebas de significancia.
2. Abre opciones de auto-anotación.
3. Elige inclusión (`solo significativos` o `todos`).
4. Elige formato de etiqueta (`estrellas` o `p-value`).
5. Reemplaza o agrega sobre anotaciones existentes.

## 8. Calidad de Datos y Gestión de Réplicas

Usa paneles de QC para revisar:

- Valores faltantes.
- Outliers por grupo.
- Tamaño muestral y cobertura de réplicas.

### Réplicas biológicas

- Controles manuales de incluir/excluir.
- Filtrado automático por IQR.
- Selección de N réplicas más reproducibles (Keep-N).

Selección automática Keep-N:

- Ordena réplicas por reproducibilidad (distancia a la mediana del grupo entre parámetros).
- Conserva las más cercanas a ese centro (menor score).
- Complementa este paso con el filtro por outliers IQR cuando quieras una selección más estricta.

### Réplicas técnicas (dentro de cada réplica biológica)

- Pestaña **Control de calidad de réplicas técnicas** (aparece cuando hay réplicas técnicas válidas).
- Selector por grupo y por réplica biológica para ver y deseleccionar réplicas técnicas manualmente.
- Botones globales para seleccionar o deseleccionar todas las réplicas técnicas.
- Detección automática de outliers técnicos por IQR.
- Keep-N técnico para conservar las N réplicas técnicas más reproducibles por subgrupo.

## 9. Metadatos, Versionado y Reproducibilidad

Flujo de metadatos:

- Exporta estado de UI con **Descargar metadatos**.
- Reimporta metadatos para restaurar configuración compatible.
- El estado de **Voltear orientación (horizontal)** se conserva en el flujo exportar/importar metadatos (roundtrip).

Cobertura de regresión:

- Pruebas automatizadas validan que la opción de orientación aplique solo a `Boxplot`, `Barras`, `Violin` y `Apiladas`.
- También validan persistencia de metadatos (roundtrip) y la aplicación de orientación en la construcción final del gráfico.

Flujo de versionado y bundle:

- Guarda versiones de gráficos en sesión.
- Construye ZIP reproducible con gráficos y metadatos.
- Reabre trabajo con configuración consistente.

## 10. Descargas

Salidas principales:

- Imagen de gráfico (`PNG`, `PDF` según tipo).
- Exportación de datos.
- Exportación de metadatos.
- Exportación de estadística.
- Bundle ZIP.
- Exportación de tabla de correlación avanzada.
- Exportación de platemap/curvas concatenados (cuando se usan herramientas de merge).

## 11. Módulo de Crecimiento

La pestaña de crecimiento extrae parámetros desde archivos cargados:

- Formato de archivo aceptado en la pestaña: `Excel` (`.xlsx`).
- Estructuras de entrada soportadas (auto-detección):
  - Formato crudo tipo lector/Tecan (datos en `Sheet1`, normalmente desde filas posteriores con columnas iniciales de índice).
  - Formato de tabla procesada desde la celda `A1`: primera columna de tiempo (por ejemplo `Time`) y cada columna siguiente como una curva/well.
- Esto permite calcular parámetros aunque el archivo no venga en el layout original del equipo, siempre que respete la estructura de tiempo + curvas.

Definición de parámetros obtenidos (GrowthRates):

- `uMax`: pendiente máxima estimada en fase exponencial (tasa específica de crecimiento).
- `max_percap_time`: tiempo promedio del tramo donde se detecta la fase exponencial máxima.
- `doub_time`: tiempo de duplicación estimado como `ln(2) / uMax`.
- `lag_time`: tiempo estimado de transición previo al crecimiento exponencial.
- `ODmax`: valor máximo de señal/OD medido en la curva.
- `max_time`: tiempo en que se alcanza `ODmax`.
- `AUC`: área bajo la curva en el intervalo temporal analizado.

Uso típico:

1. Carga uno o más archivos de crecimiento.
2. Define tiempo máximo e intervalo.
3. Ejecuta extracción.
4. Descarga salida ZIP.
5. Importa resultados al flujo de gráficos cuando corresponda.

## 12. Solución de Problemas

- **Error de carga**: verifica hojas y nombres de columnas obligatorias.
- **No aparece gráfico**: confirma que cepa/parámetro exista después de filtros.
- **Normalización no disponible**: verifica que el medio control exista en el alcance actual.
- **Estadística deshabilitada**: revisa si el modo de entrada soporta esa prueba.
- **Error en merge de curvas**: primero carga merge de platemap para remapeo de wells.
- **CSV no reconocido**: valida encabezados requeridos y delimitador consistente (`.csv` de datos: `Strain`/`Media`; `.csv` de curvas: `Time` + columnas de well).
- **Rendimiento lento**: reduce parámetros seleccionados, desactiva capas pesadas o filtra grupos.

## 13. Soporte

Para soporte o reporte de errores: `bioszenf@gmail.com`
