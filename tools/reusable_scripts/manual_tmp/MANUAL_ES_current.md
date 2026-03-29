# MANUAL BIOSZEN

Con un único Excel tendrás visualizaciones y análisis flexibles; filtra,
normaliza y elimina datos sin crear archivos adicionales. Además, podrás
graficar varios parámetros a la vez y relacionarlos: la app lo gestiona
por ti.

## 1. Preparar tus archivos de entrada

### Descargar plantilla

Haz clic en «Archivos de entrada de referencia (descargar)» para obtener
un ZIP con ejemplos de platemap, parámetros agrupados y curvas ya
formateados.

### 1.1 Estructura del archivo Platemap-parametros (.xlsx)

#### Hoja «Datos»

- Columnas obligatorias:

- Well: pocillo (ej. A1, B3). Imprescindible para el gráfico de Curvas:
  permite enlazar cada curva con su muestra original.

- Strain: nombre de la cepa o grupo la columna tambien puede titularse
  Cepa o Muestra

- Media: condición o tratamiento (ej. Control, Tratamiento A), y la
  columna tambien puede titularse Condicion o Tratamiento.

- BiologicalReplicate: réplica biológica (1, 2, 3…).

- TechnicalReplicate: réplica técnica (A, B…); si no existe, deja en
  blanco o en “A”.

- Una columna por cada parámetro que quieras graficar (ej. Viabilidad,
  Fluorescencia).

- Orden: número que define el orden de los medios a mostrar y exportar.
  Se recomienda definirlo para que los gráficos por cepa y combinados
  mantengan cohesión.

#### Hoja «PlotSettings»

- Cada fila corresponde a un parámetro presente en Datos.

- Columnas obligatorias:

  - Parameter: nombre exacto de la columna en «Datos».

  - Y_Max: límite superior inicial del eje Y (número).

  - Interval: separación entre marcas del eje Y (número).

  - Y_Title: etiqueta del eje Y (texto).

### 1.2 Estructura de datos agrupados (.xlsx)

Este formato alternativo al platemap te permite graficar y analizar
parámetros agrupados sin necesidad de especificar wells o réplicas
técnicas.

- El Excel debe contener una pestaña (hoja) por cada parámetro que
  quieras representar. La hoja debe llamarse exactamente como el
  parámetro correspondiente.

- En cada pestaña, la tabla debe incluir al menos las columnas:

  - **Strain**: nombre de la cepa o grupo. La columna tambien puede
    titularse Cepa o Muestra.

  - **BiologicalReplicate**: réplica biológica (1, 2, 3…).

  - **Media**: condición o tratamiento asociado a la cepa (si aplica).
    Para ordenar los grupos en el gráfico se utilizará el orden en que
    aparezcan las filas en cada hoja.

  - **Valores**: valores del parámetro para cada cepa, condición y
    replica.

- El orden de entrada en los gráficos generados seguirá el orden en que
  se presenten las combinaciones **Cepa – Medio** dentro de cada hoja.

Con este formato simplificado es posible cargar directamente datos
resumen por cepa y réplica, sin necesidad de un platemap completo.

| **Strain** | **RepBiol** | **Ampicillin 1µM** | **Ampicillin 2µM** | **Ampicillin 5µM** | **Control** |
|----|---:|---:|---:|---:|---:|
| Cepa1 | 1 | 0.55 | 0.46 | 0.52666667 | 0.63666667 |
| Cepa1 | 2 | 0.80333333 | 0.49333333 | 0.65333333 | 0.6 |
| Cepa2 | 1 | 0.64333333 | 0.36 | 0.45 | 0.53666667 |
| Cepa2 | 2 | 0.55666667 | 0.47 | 0.64 | 0.91 |

### 

### 

### 1.3 Estructura del archivo de curvas (.xlsx)

Si vas a usar el tipo de gráfico Curvas, necesitas un archivo con dos
hojas:

- **Sheet1 – Datos crudos de curva**

  - Primera columna: Time (o parámetro del eje X).

  - Columnas siguientes: valores de cada curva, nombradas según el Well
    de origen (ej. A1, B3). El Well debe coincidir con la columna «Well»
    del archivo principal.

- **Sheet2 – Configuración de ejes**

  - Columnas necesarias:

    - X_Max: valor máximo inicial del eje X.

    - Interval_X: separación entre marcas en X.

    - Y_Max: valor máximo inicial del eje Y.

    - Interval_Y: separación entre marcas en Y.

    - X_Title: etiqueta del eje X.

    - Y_Title: etiqueta del eje Y.

## 2. Interfaz de la app

Sigue estos pasos en orden:

- **Cargar Datos (.xlsx)**: selecciona tu Excel platemap o bien
  parametros agrupados, con la estructura de cada uno descrita
  anteriormente.

- **Cargar Curvas (.xlsx)**: selecciona tu archivo con Sheet1 y Sheet2
  (opcional).

- **Cargar metadata diseño (.xlsx)**: carga uno o más archivos con todas
  las especificaciones de diseño (dimensiones, tamaños de letra,
  títulos, ángulos, etc.) para aplicar un estilo predefinido. Los
  archivos se guardan por tipo de gráfico y se aplican automáticamente
  al cambiar de gráfico. Los archivos de diseño se obtienen en la opción
  de descargar Metadata Diseño.

- **Instrucciones (descargar)**: descarga este manual.

- **Archivos de entrada de referencia (descargar):** descarga un ZIP con
  ejemplos de platemap, parámetros agrupados y curvas.

- **Ámbito**: elige Por Cepa o Combinado.

- **Cepa**: menú desplegable con las cepas disponibles (solo en Por
  Cepa).

- Gráfico: tipo de visualización (Boxplot, Barras, Violin, Curvas,
  Apiladas, Correlación).

- **Ajustes específicos**: se mostrarán según el tipo de gráfico
  seleccionado:

  - Boxplot: ancho de caja y dispersión de puntos.

  - Violin: grosor del contorno y ancho del violín.

  - Apiladas: parámetros, orden, barras de error, contorno solo barra
    total y color de barras de error según parámetro.

  - Correlación: ejes X e Y, metodo, recta, ecuación, R^2 y etiquetas.

  - Curvas: límites, intervalos, etiquetas de ejes y grosor de curvas.

- **Normalización**: activa «Normalizar por un control» y selecciona el
  Medio normalizador.

- **Filtros**: según ámbito, ajusta medios, grupos y réplicas.

- **Ajustes y estilo**: escala, títulos, tamaño base, tamaños de fuente,
  grosor de líneas de eje, grosor de barras de error, ángulo y ajustar
  etiquetas X (número de líneas), además del tamaño y dispersión de
  puntos.

- **Parámetro a graficar y título**: elige variable, orden, etiqueta del
  eje Y y título manual.

- **Tabla de datos**: debajo del gráfico se muestra la tabla con valores
  y promedios según los filtros activos; no disponible para Curvas.

- **Barra de acciones bajo el gráfico**: etiqueta opcional, Copiar
  gráfico al portapapeles, Guardar versión y Añadir al panel.

- **Descargas**: PNG, Datos, Metadata de diseño y Resultados
  Estadísticos. La Metadata de diseño descargada contiene todas las
  especificaciones del gráfico o panel actual (dimensiones, tamaños de
  letra, títulos, ángulos, etc.), y puede subirse posteriormente
  mediante «Cargar metadata diseño» para aplicar ese estilo a futuros
  proyectos.

## 3. Procesamiento de datos

Cada réplica biológica se calcula como el promedio de sus réplicas
técnicas correspondientes. La app agrupa los datos por Strain, Media y
BiologicalReplicate,

## 4. Ámbito y selección de grupos

### Ámbito

- **Por Cepa**: un gráfico por cepa a la vez (también puede ser por
  muestra según corresponda).

- **Combinado**: todos los grupos «Cepa – Medio» en un mismo gráfico.

### Por Cepa

- Cepa: menú con las cepas detectadas.

- Filtrar Medios:

  - Seleccionar/Deseleccionar todos.

  - Casillas para incluir o excluir cada medio.

- Réplicas: elige qué réplicas biológicas mostrar (por medio si lo
  necesitas).

- Orden (csv): orden de los medios en el eje X (separa con comas).

### Combinado

- Filtrar Grupos: casillas «Cepa – Medio».

- Mostrar solo la cepa en las etiquetas.

- Réplicas - : filtra réplicas por grupo (por combinación).

- Orden (csv): orden manual de grupos.

Excluir réplica(s) en todos los grupos: elimina réplicas biológicas de
forma global en gráficos y tabla.

## 5. Elegir tipo de gráfico y colores

- **Gráfico**: Boxplot \| Barras \| Violin \| Curvas (requiere archivo
  de curvas) \| Apiladas \| Correlación.

- **Paleta de color**: elige entre paletas por defecto, monocromáticas
  uniformes para datos continuos, y paletas cualitativas para grupos.

- **Repetir colores por cepa (Combinado)**: reutiliza el conjunto de
  colores para cada cepa.

- Control avanzado de paletas: permite elegir paletas
  secuenciales/divergentes/cualitativas, filtrar por
  dalt?nicos/impresi?n/fotocopia, invertir el orden y usar esquemas
  extendidos adem?s de la lista por defecto. Personalizar colores por
  grupo: selecciona uno o varios grupos y aplica un color espec?fico.

## 6. Normalizar datos respecto a un control

- Activar normalización: marca «Normalizar por un control».

> Donde la app divide el valor de cada réplica biológica por el valor de
> la misma en el medio control seleccionado (réplica 1 con réplica 1,
> réplica 2 con réplica 2, etc.).

- Medio normalizador: selecciona el grupo (p. ej. «Control») que valdrá
  1 para cada réplica biológica.

- Por cepa independiente: cada cepa se normaliza por su propio control,
  incluso en ámbito Combinado.

<!-- -->

- Gráficos afectados:

  - Boxplot, Barras y Correlación: mostrarán la versión normalizada.

- Tests estadísticos: puedes ejecutarlos sobre datos normalizados.
  Antes, deselecciona el medio normalizador para evitar valores
  constantes.

## 7. Gráficos apilados

En un stacked bar chart agrupas varios parámetros en una misma columna
por «cepa–condición».

- Parámetros incluidos: marca cuáles deseas apilar.

- Orden de parámetros: define de abajo a arriba con una lista separada
  por comas.

- Ámbito: idéntico a Boxplot/Barras (Por Cepa o Combinado).

- Normalización: disponible igual que en otros gráficos.

- Barras de desviación: activa o desactiva la desviación estándar de
  cada segmento; opcionalmente con color según parámetro.

- Contorno negro solo barra total: dibuja el contorno solo en el total
  de la barra.

- Interactividad: cada parámetro sigue disponible por separado en
  Boxplot o Barras; los tests estadísticos comparan parámetros.

## 8. Ajustes de escala y títulos

- Y max: límite superior del eje Y (0 = valor de PlotSettings o de
  normalización).

- Int Y: separación de marcas en Y.

- Título del gráfico: si se deja vacío, la app generará uno por defecto.

- Ángulo de etiquetas del eje X: ajusta la rotación (por ejemplo, 0°,
  45°, 90°) y opcionalmente envuelve en varias líneas (elige el número
  de líneas) para mejorar su legibilidad cuando los grupos tienen
  nombres largos o numerosos.

## 9. Tamaño y estilo de la imagen

- Ancho px / Alto px (se usa para descargas y portapapeles).

- Tamaño base más tamaños de título, ejes y leyenda.

- Grosor de líneas de eje y grosor de barras de error.

- Tamaño de puntos (Boxplot/Barras/Violin/Apiladas); jitter y ancho de
  caja en Boxplot; grosor y ancho del violín.

- Curvas: grosor de líneas.

## 10. Análisis estadísticos

En **Seleccionar gráficas → Análisis Estadísticos** encontrarás dos
pestañas.

### 10.1 Normalidad

- Shapiro–Wilk (stats::shapiro.test).

- Kolmogorov–Smirnov (stats::ks.test).

- Anderson–Darling (nortest::ad.test).

Pulsa **Ejecutar Normalidad** para obtener la tabla con valores p y
«Sí/No» (p \> 0,05). Nota: con datos normalizados, deselecciona el medio
normalizador antes de ejecutar la normalidad, al quedar un grupo sin
variación el test generará un error.

### 10.2 Significancia

- **Test global**:

  - ANOVA (stats::aov).

  - Kruskal–Wallis (stats::kruskal.test).

  - t‑test independiente (rstatix::t_test).

  - Wilcoxon independiente (rstatix::wilcox_test).

- **Posthoc** (según test):

  - Tukey (stats::TukeyHSD).

  - Bonferroni, Sidak (rstatix::pairwise_t_test).

  - Dunnett (DescTools::DunnettTest).

  - Scheffé, Conover, Nemenyi, DSCF (PMCMRplus).

  - Games–Howell (rstatix::games_howell_test).

- **Modos**:

  - Todos vs Todos.

  - Control vs Todos.

  - Pareo.

Estas opciones se encuentran en la pestaña **Análisis estadísticos** de
la interfaz.

## 10.3 Análisis estadístico dinámico

- Las pruebas siempre se realizan sobre el parámetro seleccionado en
  Boxplot/Barras.

- Por Cepa: compara los Medios activos.

- Combinado: compara combinaciones «Cepa – Medio».

- Si cambias filtros o réplicas, pulsa **Ejecutar Normalidad** o
  **Significancia** para repetir el análisis.

## 11. Barras o etiquetas de significancia

- Modo: Barras (comparación) o Etiquetas sobre grupo.

Elige Grupo 1 y Grupo 2 (en modo etiquetas, Grupo 2 indica el grupo que
recibirá la etiqueta).

En Apiladas + etiquetas, selecciona el parámetro y opcionalmente colorea
la etiqueta según el parámetro.

- Escribe la Etiqueta (ej., \*, \*\*, n.s.) y pulsa Añadir
  barra/etiqueta. Las barras se apilan sin borrar las anteriores.

- Usa la lista de barras agregadas para seleccionar, eliminar o cambiar
  el orden (Subir/Bajar); Borrar todas.

- Ajusta grosor de línea, distancia a la primera barra, separación entre
  barras, separación de etiqueta y tamaño de etiqueta; opcionalmente
  quita las líneas verticales.

## 12. Gráfico de correlación

Esta sección explora la relación entre dos parámetros manteniendo las
facilidades de filtrado y personalización.

- Selección de ejes: elige libremente parámetros X e Y, crudos o
  normalizados.

> -Etiquetas de ejes: personaliza X e Y.

- Correlaciones normalizadas: si activas la normalizacion, puedes
  aplicarla al eje X, al eje Y o a ambos, y usar los valores
  normalizados para calcular correlaciones entre parametros.

- Metodo de correlacion: Pearson o Spearman. Puedes mostrar el
  coeficiente (r), el p-valor y R^2 en el grafico.

- Línea de regresión: opcional recta lineal (dashed).

> -Ecuación de la recta: opcional.

- Etiquetas de puntos: configurables; ggrepel evita solapamientos.

> -Tamaño de etiquetas: ajustable.

- Ámbito Por Cepa o Combinado: con los mismos filtros que otros
  gráficos.

- Límites e intervalos de ejes: define mínimos, máximos e intervalos.

- Título personalizable: por defecto «Correlación Y vs X», editable.

Con este módulo, el análisis de relaciones cruzadas entre parámetros
cobra nueva vida, permitiendo identificar patrones y asociaciones en un
único paso.

## 13. Panel de Composición

Los gráficos pueden añadirse al panel con el botón Añadir al panel
situado bajo el área del gráfico; la pestaña Panel de Composición
aparece tras el primer añadido. Desde este panel se puede:

- Seleccionar los gráficos a unir y reordenarlos (Subir/Bajar).

> -Mostrar u ocultar la leyenda.

- Definir el número de filas y columnas del mosaico, además del ancho y
  alto en píxeles.

- Ajustar el tamaño base y tamaños de texto (título, títulos de ejes,
  ticks y leyenda) y elegir la paleta (Original o sobrescrita).

- Usar Overrides (aplicar a todos o a un tipo específico) para cambiar
  títulos, tamaños de fuente, grosor de líneas de eje, grosor de
  curvas/tamaño de puntos en curvas, ancho de caja, grosor de barras de
  error, tamaño/jitter de puntos y grosor/tamaño de texto de
  significancia.

- Ajustar de forma conjunta las dimensiones y proporciones de todos los
  gráficos añadidos. Puedes modificar la relación de aspecto de cada
  panel individual o globalmente, de modo que todas las figuras se
  aprecien bien en el mismo plano.

- Previsualizar el resultado y descargarlo en PNG, PPTX o PDF;
  descargar/subir metadata de composición para reutilizar ajustes.

## 14. Descargar resultados

- Descargar PNG o PDF (300 dpi).

- Descargar Datos: datos detallados y resumen (no disponible al usar
  parámetros agrupados).

- Descargar Metadata de diseño: obtén un Excel con todas las
  especificaciones de diseño del gráfico o panel actual (dimensiones,
  tamaños de letra, títulos, ángulos, etc.) para reutilizarlas en
  futuros proyectos.

- Resultados estadísticos: tests realizados para todos los parámetros.

Descargar paquete (ZIP): habilitado tras guardar versiones; incluye
datasets (CSVs combinados/agrupados y libro de parámetros), gráficos
guardados (PNG/PDF), metadata, estadísticas y archivos INFO.

## 15. Growth Rates

En la pestaña **Growth Rates** puedes calcular y descargar parámetros de
crecimiento a partir de archivos Excel con curvas de interés. La
interfaz se compone de un campo para cargar el archivo de curvas, las
casillas para indicar el tiempo máximo e intervalo de muestreo, y los
botones para calcular y descargar resultados. La figura siguiente
muestra esta sección de la app:

### Archivos de entrada

- **Cargar curvas de crecimiento (.xlsx)**: selecciona uno o varios
  archivos Excel. Cada archivo debe contener una hoja con los datos
  crudos de las curvas (columna Time y valores por pocillo nombrados
  según el Well).

- **Ejemplo de estructura del archivo** (diseñado para archivos
  generados por el equipo Tecan directamente):

<table style="width:100%;">
<colgroup>
<col style="width: 16%" />
<col style="width: 17%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr>
<th><ul>
<li>Well positions</li>
</ul></th>
<th><ul>
<li>Raw data</li>
</ul></th>
<th></th>
<th></th>
<th></th>
<th></th>
</tr>
</thead>
<tbody>
<tr>
<td></td>
<td></td>
<td><ul>
<li>A1</li>
</ul></td>
<td><ul>
<li>A2</li>
</ul></td>
<td><ul>
<li>A3</li>
</ul></td>
<td><ul>
<li>A4</li>
</ul></td>
</tr>
<tr>
<td><ul>
<li>0</li>
</ul></td>
<td><ul>
<li>37.0°C</li>
</ul></td>
<td><ul>
<li>0.149</li>
</ul></td>
<td><ul>
<li>0.148</li>
</ul></td>
<td><ul>
<li>0.152</li>
</ul></td>
<td><ul>
<li>0.143</li>
</ul></td>
</tr>
<tr>
<td><ul>
<li>1800</li>
</ul></td>
<td><ul>
<li>37.0°C</li>
</ul></td>
<td><ul>
<li>0.147</li>
</ul></td>
<td><ul>
<li>0.145</li>
</ul></td>
<td><ul>
<li>0.147</li>
</ul></td>
<td><ul>
<li>0.141</li>
</ul></td>
</tr>
<tr>
<td><ul>
<li>3600</li>
</ul></td>
<td><ul>
<li>37.0°C</li>
</ul></td>
<td><ul>
<li>0.144</li>
</ul></td>
<td><ul>
<li>0.143</li>
</ul></td>
<td><ul>
<li>0.144</li>
</ul></td>
<td><ul>
<li>0.139</li>
</ul></td>
</tr>
<tr>
<td><ul>
<li>5400</li>
</ul></td>
<td><ul>
<li>37.0°C</li>
</ul></td>
<td><ul>
<li>0.142</li>
</ul></td>
<td><ul>
<li>0.141</li>
</ul></td>
<td><ul>
<li>0.143</li>
</ul></td>
<td><ul>
<li>0.138</li>
</ul></td>
</tr>
<tr>
<td><ul>
<li>…</li>
</ul></td>
<td><ul>
<li>…</li>
</ul></td>
<td><ul>
<li>…</li>
</ul></td>
<td><ul>
<li>…</li>
</ul></td>
<td><ul>
<li>…</li>
</ul></td>
<td><ul>
<li>…</li>
</ul></td>
</tr>
</tbody>
</table>

Si no es un archivo de curvas obtenido desde Tecan, se debe ajustar a
ese formato; no importa el contenido de las dos primeras columnas, solo
se considera desde la tercera columna (primer well). Los tiempos son
agregados por la aplicación al indicar los intervalos y tiempo máximo de
medición para considerar:

- **Nota**: asegúrate de que las curvas fueron medidas al menos hasta el
  **Tiempo máximo** que especifiques (p. ej., 48). Si las mediciones no
  alcanzan ese tiempo, el cálculo de parámetros puede fallar o devolver
  valores incompletos.

### Parámetros necesarios

- Tiempo máximo: valor tope de tiempo a considerar (por ejemplo, 48).

- Intervalo: separación entre puntos de muestreo (por ejemplo, 0.5).
  Debe usar la misma unidad que el Tiempo máximo.

### Botones

- **Calcular parámetros**: inicia el procesamiento de cada curva. Una
  vez finalizado, se obtienen:

  - µMax: tasa de crecimiento máxima.

  - max_percap_time: tiempo promedio en la fase exponencial.

  - doub_time: tiempo de duplicación (log(2)/µMax).

  - lag_time: tiempo de latencia.

  - ODmax: valor máximo de OD alcanzado.

  - max_time: instante en el que se alcanza ODmax.

  - AUC: área bajo la curva.

- **Descargar resultados**: descarga un ZIP que incluye, para cada
  archivo de entrada:

  - `Curvas_<nombre>.xlsx`: datos de curvas y configuración de ejes.

  - `Parametros``_<nombre>.xlsx`: tabla con todos los parámetros
    calculados.

- **Importar a Gráficos & Stats**: (solo si se cargó un único archivo)
  al pulsar este botón se incorporan los parámetros generados al módulo
  Gráficos & Stats, permitiendo graficarlos junto con el platemap
  cargado. Importante: asegúrate de que el platemap cargado en la
  pestaña Gráficos & Stats incluya en su hoja PlotSettings los nombres
  de los parámetros que se requieran graficar desde Growth Rates; de lo
  contrario, no se mostrarán en los gráficos.

### Visualización previa

- La tabla **growthTable** muestra todos los parámetros obtenidos para
  su revisión antes de la descarga.

### Consejos de uso

- Para acelerar el cálculo, elimina previamente las columnas de wells
  vacíos en el archivo de curvas. Si lo haces, asegúrate de sincronizar
  tu platemap en Gráficos & Stats para evitar incongruencias.

- Mantén siempre cargado el archivo de platemap en la pestaña Gráficos &
  Stats, ya que la función de importación requiere la correspondencia
  entre wells y datos de crecimiento.

## Flujo de trabajo recomendado

- Sube tu archivo principal y/o de curvas.

- Explora el gráfico interactivo.

- Ajusta escala, colores y títulos.

- Filtra grupos y réplicas.

- (Opcional) Normaliza datos.

- Corre Normalidad y Significancia si es necesario.

- Descarga la imagen o el ZIP con todo.

## Formato unificado de archivos para múltiples placas

### Archivo Platemap-parametros

- Añade nuevas placas en filas (debajo), sin repetir los títulos de
  columna.

- Mantén la numeración consecutiva (p. ej. continuando desde H13) para
  asegurar la correspondencia entre archivos.

### Archivo de Curvas

- Concatena nuevas curvas a la derecha de las existentes; esto es
  aplicable tanto a las curvas listas para graficar como al archivo de
  curvas para la obtención de parámetros de crecimiento.

- No repitas títulos de columna; si la primera placa termina en H12,
  puedes crear encabezados H13, H14, etc.

- Añade tantas placas como necesites, respetando el diseño experimental.

- Ejemplo: placa 1 con réplicas 1 y 2; placa 2 con réplicas 3 y 4. Esta
  asignación debe reflejarse en ambos archivos para combinar todas las
  mediciones correctamente.

## Información de contacto

Para comentarios, sugerencias o soporte, escribe a <bioszenf@gmail.com>.

© 2025 BioSzen – Todos los derechos reservados
