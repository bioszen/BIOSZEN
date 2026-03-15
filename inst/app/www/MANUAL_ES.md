# Manual BIOSZEN

BIOSZEN genera gráficos, vistas filtradas, flujos de normalización, análisis estadísticos y paquetes de exportación a partir de archivos Excel.

## 1. Qué Puedes Hacer en BIOSZEN

- Generar gráficos desde uno o más archivos Excel.
- Filtrar por cepa, medio/grupo y réplica biológica.
- Normalizar datos respecto de un medio control.
- Ejecutar pruebas de normalidad y significancia.
- Agregar barras y etiquetas de significancia de forma manual o automática desde resultados estadísticos.
- Analizar curvas, correlaciones, mapas de calor y matrices de correlación.
- Guardar versiones de gráficos y exportar paquetes de reproducibilidad.

## 2. Preparar Archivos de Entrada

Usa **Archivos de entrada de referencia (descargar)** para obtener plantillas:

- `Ejemplo_platemap_parametros.xlsx`
- `Ejemplo_curvas.xlsx`
- `Ejemplo_parametros_agrupados.xlsx`
- `Ejemplo_input_summary_mean_sd.xlsx`

### 2.1 Pack Recomendado: Platemap + Curvas (Mayor Control)

Este es el formato recomendado para tener mayor control y mayor capacidad de análisis dentro de BIOSZEN.

Se deben cargar juntos estos dos archivos:

- Archivo principal en **Cargar datos**
- Archivo de curvas en **Cargar curvas**

#### Archivo principal (platemap)

Estructura del libro: hojas `Datos` y `PlotSettings`.

Columnas obligatorias en `Datos`:

- `Well`: identificador único de pozo (debe coincidir con los pozos usados en curvas cuando se cargan curvas).
- `Strain`: identidad de cepa/grupo biológico (acepta alias: `Cepa`, `Sample`, `Muestra`).
- `Media`: identidad de condición/tratamiento (acepta alias: `Condition`, `Treatment`, `Condicion`, `Tratamiento`).
- `BiologicalReplicate`: identificador de réplica biológica para filtros y estadística.
- `TechnicalReplicate`: identificador de réplica técnica dentro de cada réplica biológica.
- `Orden`: índice de orden para mostrar grupos en ejes categóricos.
- Una columna por parámetro (por ejemplo `Parameter 1`, `Viability`, `ROS`): valor crudo usado en gráficos y estadística.

No agregar `SD_<NombreParametro>` ni `N_<NombreParametro>` en modo platemap; BIOSZEN calcula la variabilidad desde los datos por réplica.

Columnas obligatorias en `PlotSettings`:

- `Parameter`: nombre del parámetro exactamente como aparece en `Datos`.
- `Y_Max`: límite superior del eje Y para ese parámetro.
- `Interval`: separación entre marcas del eje Y para ese parámetro.
- `Y_Title`: título del eje Y para ese parámetro.

Estos valores definen límites, separación y título del eje Y; luego se pueden ajustar dentro de la app.

#### Archivo de curvas (formato de 2 hojas)

Estructura del libro: `Sheet1` y `Sheet2`.

Columnas de `Sheet1`:

- Primera columna `Time`: coordenada temporal del eje X para cada fila de medición.
- Las columnas restantes son series de curva por `Well` (A1, A2, ...): valores Y por pozo a lo largo del tiempo.

Columnas de `Sheet2`:

- `X_Max`: límite superior del eje X en gráficos de curvas.
- `Interval_X`: separación entre marcas del eje X.
- `Y_Max`: límite superior del eje Y en gráficos de curvas.
- `Interval_Y`: separación entre marcas del eje Y.
- `X_Title`: título del eje X.
- `Y_Title`: título del eje Y.

Estos campos definen límites, separación y títulos de ejes para curvas; luego se pueden modificar en la app.

### 2.2 Archivo de Parámetros Agrupados (Solo Parámetros)

Este formato es solo para datos de parámetros y no sirve para curvas.

- Se carga en **Cargar datos**.
- No se debe usar como archivo de curvas.
- Si se necesitan gráficos de curvas, se debe cargar un archivo de curvas válido (`Sheet1` + `Sheet2`) o usar el modo de resumen descrito abajo.

Estructura:

- Una hoja por parámetro.
- Cada hoja contiene valores agrupados por cepa/grupo y réplicas.

BIOSZEN convierte este formato internamente para habilitar gráficos y estadísticas de parámetros.

### 2.3 Modo Resumen en un Solo Archivo (Media/Desviación/N en Hojas Distintas)

En este modo, los resúmenes de parámetros y de curvas se cargan en el mismo archivo, cada uno en hojas diferentes.

- Se carga el archivo en **Cargar datos**.
- Si el mismo archivo también incluye la hoja de resumen de curvas, BIOSZEN puede detectar curvas desde ese mismo archivo.
- Opcionalmente, el mismo archivo también se puede subir en **Cargar curvas** para forzar la ruta de importación de curvas.

#### Hoja de resumen de parámetros

Nombres aceptados: `Parameters_Summary`, `Parametros_Summary`, `Summary_Parameters` o `Resumen_Parametros`.

Columnas obligatorias:

- `Strain`: identidad del grupo.
- `Media`: identidad de la condición.
- `Parameter`: nombre del parámetro a graficar/analizar.
- `Mean`: valor central resumido para dibujar barra o punto de línea.

Columnas opcionales:

- `SD`: variabilidad resumida para errores y pruebas en modo resumen.
- `N`: tamaño muestral usado en incertidumbre/estadística del modo resumen.
- `Orden`: índice de orden de grupos.

#### Hoja de resumen de curvas

Nombres aceptados: `Curves_Summary`, `Curvas_Summary`, `Summary_Curves` o `Resumen_Curvas`.

Columnas obligatorias:

- `Time`: coordenada temporal del eje X.
- `Strain`: identidad del grupo.
- `Media`: identidad de la condición.
- `Mean`: valor resumido de la curva en cada tiempo.

Columnas opcionales:

- `SD`: variabilidad resumida en cada tiempo.
- `N`: tamaño muestral en cada tiempo.
- `Orden`: índice de orden para vistas agrupadas.

## 3. Flujo de la Aplicación

1. Cargar el archivo principal de datos.
2. Cargar opcionalmente el archivo de curvas (o usar modo resumen en un solo archivo).
3. Cargar opcionalmente archivos de metadatos.
4. Elegir alcance (**Por cepa** o **Combinado**).
5. Elegir tipo de gráfico y ajustes.
6. Aplicar filtros y normalización.
7. Ejecutar estadísticas.
8. Agregar anotaciones de significancia.
9. Exportar resultados.

## 4. Tipos de Gráfico y Disponibilidad

| Tipo de gráfico | Platemap / Agrupado | Resumen Media/Desviación/N |
|---|---|---|
| Caja | Sí | No |
| Barras | Sí | Sí |
| Violín | Sí | No |
| Curvas | Sí | Sí |
| Apilado | Sí | Sí |
| Correlación | Sí | Sí |
| Mapa de calor | Sí | Sí |
| Matriz de correlación | Sí | Sí |

Notas:

- En modo resumen, Caja y Violín se deshabilitan.

## 5. Controles Globales

- Tamaño del gráfico: ancho y alto en píxeles.
- Tipografía: tamaño base, título, ejes y leyenda.
- Estilo de ejes: grosor de líneas.
- Legibilidad en eje X: ángulo de etiquetas, ajuste en varias líneas y máximo de líneas.
- Paletas de color y controles avanzados de paleta.
- Colores personalizados por grupo.

## 6. Ajustes por Tipo de Gráfico

### 6.1 Caja

- Ancho de caja.
- Dispersión y tamaño de puntos.
- Barras o etiquetas de significancia.
- Opción de leyenda a la derecha (modo color).

### 6.2 Barras

- Barra de media con barras de error.
- Superposición de puntos crudos (modo crudo).
- En modo resumen, se dibujan barras y errores sin puntos de réplica cruda.
- Barras o etiquetas de significancia.

### 6.3 Violín

- Cuerpo de violín con superposición de puntos.
- Controles compartidos de ejes y puntos.
- Barras de significancia.

### 6.4 Apilado

- Selector de parámetros incluidos.
- Orden de parámetros (lista separada por comas, de abajo hacia arriba).
- Mostrar u ocultar barras de desviación.
- Color de barras de error por parámetro.
- Contorno negro solo para la barra total.
- Modo de significancia: barras de corchete o etiquetas.
- El modo de etiquetas permite seleccionar parámetro objetivo y color de etiqueta por parámetro.

### 6.5 Correlación

- Selección de parámetro X e Y.
- Métodos: Pearson, Spearman, Kendall.
- Capas opcionales: recta de regresión, r, p, R^2, ecuación y etiquetas.
- Estilo de intervalo de confianza:
  - Banda sombreada.
  - Límites discontinuos.
- Nivel de confianza ajustable.
- Controles de mínimo/máximo/intervalo en ejes.
- Etiquetas personalizadas de ejes.
- Objetivo de normalización en correlación:
  - Ambos ejes.
  - Solo eje X.
  - Solo eje Y.

### 6.6 Mapa de Calor

- Selección de subconjunto de parámetros.
- Modo de escala:
  - Sin escala.
  - Por fila (parámetro).
  - Por columna (grupo).
- Método de agrupamiento: `ward.D2`, `ward.D`, `complete`, `average`, `single`, `mcquitty`, `median`, `centroid`.
- Mostrar u ocultar dendrogramas por fila y columna.
- Mostrar u ocultar etiquetas de valor por celda.

### 6.7 Matriz de Correlación

- Selección múltiple de parámetros.
- Métodos: Pearson, Spearman, Kendall.
- Corrección por múltiples pruebas: Holm, FDR, Bonferroni o ninguna.
- Opción para mostrar solo correlaciones significativas en las etiquetas.

### 6.8 Curvas

- Límites e intervalos de ejes (`X_Max`, `Y_Max`, cortes).
- Etiquetas personalizadas de ejes.
- Grosor de línea.
- Geometría:
  - Línea con puntos.
  - Solo línea.
- Modo de color:
  - Por paleta de grupo.
  - Color único.
- Estilo de intervalo de confianza:
  - Banda.
  - Barras de error.
- Opción de mostrar trayectorias de réplica (solo modo crudo) con control de transparencia.

## 7. Filtros y Selección de Réplicas

### Alcance Por Cepa

- Selector de cepa.
- Filtro de medios (activar/desactivar todo).
- Filtros de réplicas por medio.
- Exclusión global de réplicas.

### Alcance Combinado

- Filtro de grupos visibles (`Strain-Media`).
- Filtros de réplicas por grupo.
- Opción de etiquetas solo por cepa.
- Exclusión global de réplicas.

## 8. Normalización por Control

Activa normalización y elige un medio control.

- Se usa normalización con conciencia de réplica cuando es posible.
- Si no hay emparejamiento estricto de control, BIOSZEN aplica una lógica alternativa y lo reporta.
- Las columnas normalizadas se almacenan internamente con sufijo `_Norm`.
- En correlación se puede normalizar ambos ejes o solo uno.

## 9. Análisis Estadístico

### 9.1 Normalidad

- Shapiro-Wilk (`stats::shapiro.test`).
- Kolmogorov-Smirnov (`stats::ks.test`).
- Anderson-Darling (`nortest::ad.test`).

### 9.2 Significancia

Pruebas principales:

- ANOVA (`stats::aov`; los flujos post hoc incluyen `rstatix` y `DescTools`).
- Kruskal-Wallis (`stats::kruskal.test`).
- Prueba t independiente (`rstatix::t_test`).
- Prueba de Wilcoxon independiente (`rstatix::wilcox_test`).

Modos de comparación:

- Todos contra todos.
- Control contra todos.
- Par a par.

Corrección por múltiples pruebas:

- Holm (`stats::p.adjust`, método `"holm"`).
- FDR (Benjamini-Hochberg; `stats::p.adjust`, método `"fdr"`).
- Bonferroni (`stats::p.adjust`, método `"bonferroni"`).
- Ninguna.

Las opciones post hoc dependen de la prueba principal seleccionada.

### 9.3 Comportamiento en Modo Resumen (Media/Desviación/N)

- La normalidad no se puede calcular desde entradas solo resumidas; los valores de normalidad se devuelven como `NA`.
- Kruskal-Wallis y Wilcoxon se deshabilitan (requieren observaciones crudas).
- La significancia usa comparaciones pareadas tipo Welch derivadas de Media/Desviación/N.

## 10. Anotaciones de Significancia

Modos de anotación para Caja/Barras/Violín/Apilado:

- Barras de corchete.
- Etiquetas directas.

### Flujo manual

1. Elegir Grupo 1 y Grupo 2.
2. Ingresar texto de etiqueta (`*`, `**`, `***`, `ns` o personalizado).
3. Agregar elemento.
4. Reordenar elementos.
5. Editar elemento seleccionado.
6. Quitar elementos seleccionados o limpiar todo.

### Generación automática desde pruebas de significancia

1. Abrir la generación automática desde el panel de significancia.
2. Elegir modo de inclusión:
   - solo significativos.
   - todas las comparaciones.
3. Elegir modo de etiqueta:
   - estrellas.
   - valor p (3 decimales).
4. Elegir reemplazar o anexar.
5. Aplicar generación automática.

Después de la generación automática, se pueden editar, reordenar y borrar elementos individuales.

Si ya existen barras/etiquetas:

- Si `Reemplazar barras actuales` está activado, los elementos existentes se reemplazan por el nuevo conjunto automático.
- Si `Reemplazar barras actuales` está desactivado, se conservan los elementos existentes y se agregan nuevos.
- En modo de agregado, las comparaciones duplicadas se omiten automáticamente (mismo par de grupos; y mismo contexto de parámetro en modo etiquetas apiladas).

### Controles de diseño para anotaciones

- Grosor de línea.
- Desplazamiento respecto de los datos.
- Separación vertical entre barras.
- Relleno interno del texto.
- Tamaño de texto.
- Ocultar remates.

## 11. Panel de Estadística de Curvas

Las comparaciones de curvas están disponibles mediante estos procedimientos estadísticos:
Paquetes principales de R en este panel: `stats`, `splines` y `gcplyr`.

- **Comparación global de forma de curva:** modelos lineales ponderados con splines naturales (`stats::lm`, `splines::ns`) comparados con `stats::anova`.
- **Diferencias punto a punto:** comparaciones tipo z por tiempo construidas desde incertidumbre SD/N, combinadas con el método de Fisher (`stats::pnorm`, `stats::pchisq`).
- **Prueba de diferencia en punto final:** comparación tipo z en el último tiempo compartido con error estándar propagado (`stats::pnorm`).
- **Área bajo la curva (AUC):**
  - El AUC se calcula con `gcplyr::auc`.
  - El cribado de normalidad por grupo usa Shapiro-Wilk (`stats::shapiro.test`).
  - Las pruebas de significancia se seleccionan según estructura:
    - 2 grupos: prueba t de Welch (`stats::t.test`) o prueba de Wilcoxon rango-suma (`stats::wilcox.test`).
    - 3 o más grupos: ANOVA (`stats::aov`) o Kruskal-Wallis (`stats::kruskal.test`).
  - También se reportan comparaciones pareadas de AUC.
- **Corrección por múltiples pruebas en la tabla de curvas:** Holm (`stats::p.adjust`).

Campos de salida: `Estimate`, `P_value`, `P_adjusted` y `Stars`.

## 12. Panel de Calidad de Datos

Tablas de control de calidad para los filtros actuales:

- Valores faltantes por parámetro.
- Valores atípicos por parámetro y grupo (regla IQR).
- Tamaño muestral y cobertura de réplicas biológicas por grupo.

Herramientas de deselección automática de réplicas biológicas:

- **Deselección por outliers IQR:** detecta réplicas outlier por grupo usando el multiplicador IQR seleccionado y deselecciona las réplicas biológicas marcadas.
- **Límite de N réplicas a conservar:** conserva solo las N réplicas biológicas más reproducibles por grupo (ordenadas por distancia a la mediana del grupo entre parámetros).
- **Control manual de conservar/deseleccionar:** los selectores de réplicas siguen siendo editables, por lo que puedes dejar exactamente las réplicas biológicas que quieras después de los pasos automáticos.

## 13. Panel de Composición

- Agregar gráficos al panel.
- Reordenar gráficos incluidos.
- Configurar filas/columnas y tamaño de lienzo.
- Aplicar estilos globales o por tipo de gráfico.
- Exportar como PNG, PPTX o PDF.
- Descargar o subir metadatos de composición.

## 14. Descargas y Reproducibilidad

Exportaciones disponibles desde la pestaña principal de gráficos:

- Gráfico en PNG/PDF.
- Libro de datos.
- Libro de metadatos de gráfico.
- Libro de resultados estadísticos.
- Paquete ZIP completo con versiones guardadas y activos de reproducibilidad.

## 15. Extracción de Parámetros de Crecimiento

En el módulo de crecimiento, los resultados están pensados para la obtención de parámetros de curvas de crecimiento microbiano. Se puede:

- Cargar uno o más libros de crecimiento.
- Definir tiempo máximo e intervalo.
- Calcular:
  - `uMax`: tasa específica máxima de crecimiento (por unidad de tiempo).
  - `max_percap_time`: tiempo en que la tasa per cápita de crecimiento alcanza su máximo.
  - `doub_time`: tiempo de duplicación estimado (usualmente `ln(2) / uMax`).
  - `lag_time`: duración estimada de la fase lag antes del crecimiento exponencial sostenido.
  - `ODmax`: señal/densidad óptica máxima alcanzada en la ventana analizada.
  - `max_time`: tiempo en que se alcanza `ODmax`.
  - `AUC`: área bajo la curva de crecimiento en el rango temporal analizado.
- Descargar resultados en ZIP.
- Importar resultados a Gráficos y Estadística cuando se carga un solo archivo.

## Contacto

Para comentarios o soporte: `bioszenf@gmail.com`

© BioSzen. Todos los derechos reservados.
