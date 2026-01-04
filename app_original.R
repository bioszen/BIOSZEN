# app.R - Shiny interactivo con carga dinámica de archivos, gráfico por cepa o combinado,  
# descarga de PNG y Excel (.xlsx)  

library(shiny)  
library(plotly)  
library(ggplot2)  
library(readxl)  
library(dplyr)  
library(tidyr)  
library(scales)  
library(openxlsx)  
library(zip)  
library(rstatix)  
library(DT)  
library(shinyBS)  
library(nortest)  
library(DescTools)    # para DunnettTest()  
library(multcomp)     # alternativa Dunnett y glht()  
library(PMCMRplus)    # Scheffé, Conover, Nemenyi, DSCF  
library(dunn.test)    # otra implementación de Dunn  
library(broom)  
library(stringr)  
library(forcats)  
library(tibble)  
library(RColorBrewer)  
library(viridis)
library(htmlwidgets)   # para saveWidget()
library(webshot2)      # PNG desde html/plotly   (trae Chrome headless)
library(webshot)
library(ggrepel)        # etiquetas que no se sobre-ponen
library(shinyjs)
library(bslib)
library(gcplyr)
library(rlang)
library(patchwork)
library(officer)
library(rvg)


#──────── helpers genéricos para objetos sin método broom::tidy() ────────  
matrix_to_tibble <- function(mat, colname = "p.adj") {  
  tibble::as_tibble(mat, rownames = "grupo1") |>  
    tidyr::pivot_longer(-grupo1,  
                        names_to  = "grupo2",  
                        values_to = colname) |>  
    dplyr::filter(!is.na(.data[[colname]]))  
}  

# ── helper: convierte matriz de p-values de PMCMRplus a tibble ──────────────  
pmcmr_to_tibble <- function(obj) {  
  mat <- obj$p.value                   # extrae la matriz interna de p-values  
  tibble::as_tibble(mat, rownames = "grupo1") |>  
    tidyr::pivot_longer(  
      -grupo1,  
      names_to  = "grupo2",  
      values_to = "p.adj"  
    ) |>  
    dplyr::filter(!is.na(p.adj))  
}  


#──────────────────────────────────────────────────────────────────────────  
# Helpers estadísticos ─ NUEVOS  
split_comparison <- function(x) {  
  stringr::str_split_fixed(x, "-", 2)  
}  

dunnett_to_tibble <- function(obj) {  
  # obj es la lista que devuelve DescTools::DunnettTest()  
  mat <- obj[[1]][ , 4, drop = FALSE]        # 4ª columna = p ajustado  
  cmp <- split_comparison(rownames(mat))  
  tibble(  
    grupo1 = cmp[, 1],  
    grupo2 = cmp[, 2],  
    p.adj  = mat[, 1]  
  )  
}  

set_control <- function(df, control_lbl) {  
  if (!is.null(control_lbl) && control_lbl %in% df$Label)  
    df$Label <- forcats::fct_relevel(df$Label, control_lbl)  
  df  
}  

# Si ya existían matrix_to_tibble() y pmcmr_to_tibble() NO los dupliques.  
#──────────────────────────────────────────────────────────────────────────  
safe_pairwise_t <- function(df, method = "sidak") {  
  res <- rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = method)  
  if (nrow(res) == 0) tibble() else res  
}  


# ── Nombre seguro para archivos (mantiene la extensión) ────────────  
safe_file <- function(x) {  
  # separa nombre y extensión  
  ext  <- tools::file_ext(x)           # "png"  
  name <- tools::file_path_sans_ext(x) # "NAD+_Boxplot"  
  
  # sanea solo el nombre (letras, números, _ y -)  
  name <- gsub("[^A-Za-z0-9_\\-]", "_", name)  
  
  paste0(name, ".", ext)               # vuelve a unir con .  
}  

# ── Nombre seguro para hojas de Excel ──────────────────────────────  
safe_sheet <- function(x) {  
  ## solo letras, números o "_" (lo demás → "_")  
  gsub("[^A-Za-z0-9_]", "_", x)  
}  
# ── Nombre seguro para etiquetas simples ────────────────────  
sanitize <- function(x) {  
  gsub("[/\\\\:*?\"<>|]", "_", x)  
}  

theme_light <- bs_theme(version = 5)
theme_dark  <- bs_theme(version = 5, bootswatch = "cyborg")

tab_compos <- tabPanel(
  "Panel de Composición",
  sidebarLayout(
    sidebarPanel(
      uiOutput("plotPicker"),
      checkboxInput("show_legend_combo", "Mostrar leyenda", value = FALSE),
      hr(),
      numericInput("nrow_combo", "Filas", 1, min = 1, max = 4),
      numericInput("ncol_combo", "Columnas", 1, min = 1, max = 4),
      numericInput("combo_width",  "Ancho px", 1920, min = 400),
      numericInput("combo_height", "Alto  px", 1080, min = 400),
      numericInput("base_size_combo", "Tamaño base (pts)", 18, min = 8),
      numericInput("fs_title_all",      "Tamaño título (pts)",     20, min = 6),
      numericInput("fs_axis_title_all", "Títulos de ejes",         16, min = 6),
      numericInput("fs_axis_text_all",  "Ticks de ejes",           14, min = 6),
      numericInput("fs_legend_all",     "Texto de leyenda",        16, min = 6),
      conditionalPanel(
        condition = "input.plot_edit != null && input.plot_edit != ''", # se muestra sólo cuando hay algo para editar
        h4("Ajustes Boxplot / Barras"),
        numericInput("ov_box_w",  "Ancho de caja:",     NA, min = 0.1, max = 1.5, step = 0.05),
        numericInput("ov_pt_size","Tamaño de puntos:",  NA, min = 0.5, max = 6,  step = 0.5),
        numericInput("ov_pt_jit", "Dispersión puntos:", NA, min = 0,   max = 0.5, step = 0.01)
      ),
      actionButton("makeCombo", "Actualiza / Previsualiza",
                   class = "btn btn-primary"),
      br(), br(),
      downloadButton("dl_combo_png",  "Descargar PNG"),
      downloadButton("dl_combo_pptx", "Descargar PPTX")
    ),
    mainPanel(
      plotOutput("comboPreview", height = "auto")
    )
  )
)


# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = theme_light,          # por defecto; el server lo cambia a oscuro/claro
  useShinyjs(),
  
  # ------ Modo Light/Dark guardado en localStorage ---------------------------
  tags$script(HTML("
    $(function () {
      var m = localStorage.getItem('appMode') || 'light';
      Shiny.setInputValue('mode', m, {priority: 'event'});
    });
    Shiny.addCustomMessageHandler('saveMode', function (m) {
      localStorage.setItem('appMode', m);
    });
  ")),
  # ------ Botones de modo + idioma (arriba-derecha) ---------------------------
  absolutePanel(
    top  = 10, right = 10, draggable = FALSE,
    style = 'z-index:1000; display:flex; gap:8px;',
    
    ## ─── Botón “Idioma” con menú desplegable (Bootstrap 5) ───────────────────
    tags$div(
      class = "dropdown",
      actionButton(
        "lang_btn", "Idioma",
        class = "btn btn-secondary dropdown-toggle",
        `data-bs-toggle` = "dropdown", `aria-expanded` = "false"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(actionLink("lang_es", "Español", class = "dropdown-item")),
        tags$li(actionLink("lang_en", "English", class = "dropdown-item"))
      )
    ),
    
    ## Botones modo claro/oscuro (sin cambios)
    actionButton('btn_light', label = 'Tema Claro',
                 class = 'btn btn-light', style = 'color:#000;'),
    actionButton('btn_dark',  label = 'Tema Oscuro',
                 class = 'btn btn-dark',  style = 'color:#fff;')
  ),
  
  # ------ Google Analytics ---------------------------------------------------
  tags$head(HTML("
    <!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=G-Q5FYW8FV3Z'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-Q5FYW8FV3Z');
    </script>
  ")),
  tags$head(HTML('
  <!-- Traductor de Google -->
  <style>#google_translate_element {display:none;}</style>
  <div id="google_translate_element"></div>

  <script type="text/javascript">
    function googleTranslateElementInit(){
      new google.translate.TranslateElement(
        {pageLanguage:"es", includedLanguages:"es,en", autoDisplay:false},
        "google_translate_element");
    }

    /* Cambia la selección del <select> interno */
    function setLang(lang){
      var sel=document.querySelector("#google_translate_element select");
      if(!sel) return;
      for(var i=0;i<sel.options.length;i++){
        if(sel.options[i].value.indexOf(lang)>-1){
          sel.selectedIndex=i;
          sel.dispatchEvent(new Event("change"));
          break;
        }
      }
      /* guarda preferencia */
      localStorage.setItem("appLang",lang);
          var link = document.getElementById("manual_link");
    if(link){
      link.setAttribute("href", lang==="en" ? "MANUAL_EN.docx" : "MANUAL_ES.docx");
    }
  }

    /* restaura idioma guardado */
    document.addEventListener("DOMContentLoaded",function(){
      var lg=localStorage.getItem("appLang")||"es";
      setTimeout(function(){ setLang(lg); },500);
    });
  </script>

  <script src="//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"></script>
')),
  tags$head(
    tags$style(HTML("
    #statsTabs .checkbox { margin-top: 6px; }
  "))
  ),
  tags$head(
    tags$style(HTML("
    /* Hace más grande el encabezado del acordeón de estadística */
    #statsPanel .accordion-header .accordion-button{
      font-size: 18px;     /* cámbialo a gusto */
      font-weight: 700;
    }
  "))
  ),
  tags$head(
    tags$style(HTML("
    /* Tamaño/estilo uniforme para TODOS los encabezados de accordion() que
       nos interesan (estadística y réplicas) */
    #statsPanel .accordion-header .accordion-button,
    #repsPanel  .accordion-header .accordion-button,
    #repsGrpPanel .accordion-header .accordion-button {
      font-size: 18px;     /* mismo valor que pusiste a “Análisis estadísticos” */
      font-weight: 700;
    }
  "))
  ),
  # ------ Handler de descarga Plotly ----------------------------------------
  tags$script(HTML("
    Shiny.addCustomMessageHandler('downloadPlotlyImage', function(msg){
      var gd = document.getElementById('plotInteractivo');
      Plotly.downloadImage(gd, {
        format:  'png',
        filename: msg.filename,
        width:    msg.width,
        height:   msg.height,
        scale:    1
      });
    });
  ")),
  
  # ------ Cabecera -----------------------------------------------------------
  titlePanel(tags$div(
    style = 'display:flex; align-items:center; gap:10px;',
    uiOutput('logo_img'),
    tags$span('Centro de Gráficos y Análisis',
              style = 'font-size:30px; font-weight:bold;')
  )),
  # ——— Inicio de pestañas principales ———
  tabsetPanel(id = "mainTabs", type = "tabs",
              
              # ─ Pestaña 1: Gráficos & Stats ────────────────────────────────────────
              tabPanel("Gráficos & Stats",
                       # --------------------------------------------------------------------------
                       #  Layout principal: barra lateral + área de trazado/tablas
                       # --------------------------------------------------------------------------
                       sidebarLayout(
                         
                         ###########################################################################
                         #  SIDEBAR ----------------------------------------------------------------
                         ###########################################################################
                         sidebarPanel(
                           
                           fileInput('dataFile',  'Cargar metadata', accept = '.xlsx'),
                           fileInput('curveFile', 'Cargar Curvas (.xlsx)',              accept = '.xlsx'),
                           
                           # ---- Botones de descarga de ejemplos ----------------------------------
                           tags$div(
                             style = 'margin-top:-10px; margin-bottom:15px;',
                             tags$a(id = 'manual_link',
                                    href = 'MANUAL_ES.docx',      # ← ES el idioma por defecto
                                    download = NA,
                                    'Instrucciones (descargar)'), tags$br(), tags$br(),
                             tags$a(href = 'Ejemplo_platemap_parametros.xlsx', download = NA,
                                    'Archivo de referencia platemap-parametros (descargar)'),
                             tags$br(), tags$br(),
                             tags$a(href = 'Ejemplo_curvas.xlsx',           download = NA,
                                    'Archivo de referencia de curvas (descargar)')
                           ),
                           hr(),
                           
                           # ---- Controles principales -------------------------------------------
                           radioButtons('scope', 'Ámbito:',
                                        choices = c('Por Cepa', 'Combinado'),
                                        selected = 'Por Cepa'),
                           
                           conditionalPanel(
                             condition = "input.scope=='Por Cepa'",
                             selectInput('strain', 'Cepa:', choices = NULL)
                           ),
                           uiOutput('paramSel'),
                           
                           radioButtons('tipo', 'Gráfico:',
                                        choices  = c('Boxplot', 'Barras', 'Curvas', 'Apiladas',
                                                     'Correlación'),
                                        selected = 'Boxplot'),
                           
                           # ---------- SECCIÓN “Apiladas” ----------------------------------------
                           conditionalPanel(
                             condition = "input.tipo == 'Apiladas'",
                             h4('Ajustes Apiladas'),
                             checkboxGroupInput('stackParams', 'Parámetros incluidos:',
                                                choices = NULL, selected = NULL),
                             textInput('orderStack',
                                       'Orden de parámetros (CSV, de abajo a arriba):', ''),
                             checkboxInput('showErrBars', 'Mostrar barras de desviación', TRUE),
                             numericInput('errbar_size', 'Grosor de barras de error:',
                                          value = 0.6, min = 0.1, step = 0.1)
                           ),
                           
                           # ---------- SECCIÓN “Correlación” ---------------------------------
                           conditionalPanel(
                             condition = "input.tipo == 'Correlación'",
                             h4("Ajustes Correlación"),
                             
                             fluidRow(
                               column(6,
                                      selectInput("corr_param_x", "Parámetro eje X:", choices = NULL)
                               ),
                               column(6,
                                      selectInput("corr_param_y", "Parámetro eje Y:", choices = NULL)
                               )
                             ),
                             
                             radioButtons(
                               "corr_method", "Método de correlación:",
                               choices  = c(Pearson = "pearson", Spearman = "spearman"),
                               selected = "pearson", inline = TRUE
                             ),
                             
                             checkboxInput("corr_show_line",   "Mostrar recta de regresión", TRUE),
                             checkboxInput("corr_show_labels", "Mostrar etiquetas",          TRUE),
                             checkboxInput("corr_show_eq", "Mostrar ecuación de la recta", FALSE),
                             
                             textInput ("corr_xlab",         "Etiqueta eje X:", value = ""),
                             textInput ("corr_ylab",         "Etiqueta eje Y:", value = ""),
                             
                             fluidRow(
                               column(6, numericInput("xmin_corr", "X min:", value = 0,  min = 0)),
                               column(6, numericInput("xmax_corr", "X max:", value = 0,  min = 0))
                             ),
                             fluidRow(
                               column(6, numericInput("xbreak_corr", "Int X:", value = 1,  min = 0.001)),
                               column(6, numericInput("ybreak_corr", "Int Y:", value = 1,  min = 0.001))
                             ),
                             fluidRow(
                               column(6, numericInput("ymin_corr", "Y min:", value = 0,  min = 0)),
                               column(6, numericInput("ymax_corr", "Y max:", value = 0,  min = 0)),
                               column(6, numericInput("corr_label_size",
                                                      "Tamaño etiquetas (pts):", value = 4, min = 1))
                             )
                           ),
                           
                           
                           # ---------- Controles comunes -----------------------------------------
                           checkboxInput('doNorm', 'Normalizar por un control', FALSE),
                           uiOutput('ctrlSelUI'),
                           
                           
                           # ---------- Eje Y para Boxplot/Barras ---------------------------------
                           conditionalPanel(
                             condition = "['Boxplot','Barras','Apiladas'].indexOf(input.tipo) >= 0",
                             h4('Ajustes Eje Y'),
                             fluidRow(
                               column(6, numericInput('ymax',   'Y max:',  value = 0, min = 0)),
                               column(6, numericInput('ybreak', 'Int Y:',  value = 1, min = 0.001))
                             )
                           ),
                           
                           # ---------- Ajustes Curvas --------------------------------------------
                           conditionalPanel(
                             condition = "input.tipo == 'Curvas'",
                             h4('Ajustes Curvas'),
                             fluidRow(
                               column(6, numericInput('xmax_cur',   'X max:', value = 3000, min = 0)),
                               column(6, numericInput('xbreak_cur', 'Int X:', value = 1000, min = 1))
                             ),
                             fluidRow(
                               column(6, numericInput('ymax_cur',   'Y max:', value = 1.5, min = 0)),
                               column(6, numericInput('ybreak_cur', 'Int Y:', value = 0.5, min = 0.01))
                             ),
                             fluidRow(
                               column(6, textInput('cur_xlab', 'Etiqueta eje X:', '')),
                               column(6, textInput('cur_ylab', 'Etiqueta eje Y:', ''))
                             )
                           ),
                           
                           # Réplicas a mostrar para Curvas
                           conditionalPanel(
                             "input.tipo == 'Curvas'",
                             uiOutput("repSelCurvas")
                           ),
                           # ---------- Paleta de color -------------------------------------------
                           selectInput('colorMode', 'Paleta de color:',
                                       choices = c('Default', 'Blanco y Negro', 'Viridis', 'Plasma',
                                                   'Magma', 'Cividis', 'Set1', 'Set2', 'Set3', 'Dark2',
                                                   'Accent', 'Paired', 'Pastel1', 'Pastel2'),
                                       selected = 'Default'),
                           
                           ## ─── Boxplot ──────────────────────────────────────────────
                           conditionalPanel(
                             condition = "input.tipo == 'Boxplot'",
                             numericInput("box_w",   "Ancho de caja:",     value = 0.8,
                                          min = 0.1, max = 1.5, step = 0.05),
                             numericInput("pt_jit",  "Dispersión puntos:", value = 0.1,
                                          min = 0,   max = 0.5, step = 0.01)
                           ),
                           
                           ## ─── Boxplot *y* Barras (tamaño de puntos) ────────────────
                           conditionalPanel(
                             condition = "['Boxplot','Barras'].indexOf(input.tipo) >= 0",
                             numericInput("pt_size", "Tamaño de puntos:", value = 3,
                                          min = 0.5, max = 6,  step = 0.5)
                           ),
                           
                           
                           hr(), h4("Tamaño y Fuente"),
                           numericInput("base_size", "Tamaño base (pts)", 18, min = 8),
                           numericInput("fs_title",  "Tamaño título:",   20, min = 6),
                           numericInput("fs_axis",   "Tamaño ejes:",     15, min = 6),
                           numericInput("fs_legend", "Tamaño leyenda:",  17, min = 6),
                           numericInput("axis_line_size", "Grosor líneas de eje:",
                                        value = 1.2, min = .1, step = .1),
                           
                           
                           # ---------- Parámetro a graficar --------------------------------------
                           textInput('yLab', 'Etiqueta eje Y:', ''),
                           
                           conditionalPanel(
                             condition = "input.scope=='Por Cepa'",
                             textInput('orderMedios', 'Orden (csv):', '')
                           ),
                           
                           textInput('plotTitle', 'Título del gráfico:', ''),
                           
                           
                           # ---------- Filtro de medios (Por Cepa) -------------------------------
                           conditionalPanel(
                             condition = "input.scope=='Por Cepa'",
                             h4('Filtrar Medios'),
                             checkboxInput('toggleMedios', 'Seleccionar / Deseleccionar todos', TRUE),
                             uiOutput('showMediosUI'),
                             
                             accordion(                     # <-- en lugar de bsCollapse
                               id       = 'repsPanel',
                               open     = FALSE,
                               multiple = TRUE,
                               accordion_panel(
                                 'Réplicas por medio',
                                 uiOutput('repsStrainUI'),
                                 style = 'default'
                               )
                             )
                           ),
                           # ---------- Filtro de grupos (Combinado) ------------------------------
                           conditionalPanel(
                             condition = "input.scope=='Combinado'",
                             h4("Filtrar Grupos"),
                             checkboxInput("toggleGroups", "Seleccionar / Deseleccionar todos", TRUE),
                             
                             uiOutput("groupSel"),       # ← sigue igual
                             
                             uiOutput("repsGrpUI"),      # ← NUEVO hueco para el acordeón dinámico
                             
                             checkboxInput("labelMode", "Mostrar sólo la cepa en las etiquetas", FALSE)
                           ),
                           
                           
                           # ---------- Estadística para Boxplot/Barras ---------------------------
                           conditionalPanel(
                             condition = "['Boxplot','Barras'].indexOf(input.tipo) >= 0",
                             accordion(
                               id       = "statsPanel",
                               open     = FALSE,
                               multiple = TRUE,
                               accordion_panel(
                                 "Análisis estadísticos",
                                 tabsetPanel(
                                   id = 'statsTabs',
                                   tabPanel(
                                     title = 'Normalidad',
                                     checkboxGroupInput('normTests', NULL,
                                                        choices  = c('Shapiro–Wilk'       = 'shapiro',
                                                                     'Kolmogorov–Smirnov' = 'ks',
                                                                     'Anderson–Darling'   = 'ad'),
                                                        selected = c('shapiro','ks','ad')),
                                     actionButton('runNorm',
                                                  label = tagList(icon('vial'), 'Ejecutar Normalidad'),
                                                  class = 'btn btn-primary'),
                                     br(), br(),
                                     DTOutput('normTable')
                                   ),
                                   tabPanel(
                                     title = 'Significancia',
                                     radioButtons('sigTest', NULL,
                                                  choices = c('ANOVA'                  = 'ANOVA',
                                                              'Kruskal–Wallis'         = 'Kruskal–Wallis',
                                                              't-test independiente'   = 'ttest',
                                                              'Wilcoxon independiente' = 'wilcox'),
                                                  selected = 'ANOVA'),
                                     uiOutput('postHocUI'),
                                     radioButtons('compMode', NULL,
                                                  choices = c('Todos vs Todos'   = 'all',
                                                              'Control vs Todos' = 'control',
                                                              'Pareo'            = 'pair'),
                                                  selected = 'all'),
                                     conditionalPanel(
                                       condition = "input.compMode=='control'",
                                       selectInput('controlGroup', 'Control:', choices = NULL)
                                     ),
                                     conditionalPanel(
                                       condition = "input.compMode=='pair'",
                                       selectInput('group1', 'Grupo 1:', choices = NULL),
                                       selectInput('group2', 'Grupo 2:', choices = NULL)
                                     ),
                                     actionButton('runSig',
                                                  label = tagList(icon('chart-bar'),
                                                                  'Ejecutar Significancia'),
                                                  class = 'btn btn-primary'),
                                     br(), br(),
                                     DTOutput('sigTable')
                                   )
                                 ),
                                 style = 'primary'
                               )
                             ),
                             hr(), h4('Barras de significancia'),
                             fluidRow(
                               column(6, selectInput('sig_group1', 'Grupo 1:', choices = NULL)),
                               column(6, selectInput('sig_group2', 'Grupo 2:', choices = NULL))
                             ),
                             textInput('sig_label', 'Etiqueta (*, **, n.s.):', '*'),
                             fluidRow(
                               column(6, actionButton('add_sig',
                                                      label = tagList(icon('plus'), 'Añadir barra'),
                                                      class = 'btn btn-primary')),
                               column(6, actionButton('clear_sig',
                                                      label = tagList(icon('eraser'), 'Borrar todas'),
                                                      class = 'btn btn-primary'))
                             ),
                             fluidRow(
                               column(6,
                                      numericInput('sig_linewidth', 'Grosor línea:',
                                                   .8,  min = .2, step = .2)),
                               column(6,
                                      numericInput('sig_sep',       'Separación entre barras (% rango Y):',
                                                   .05, min = .01, step = .01))
                             ),
                             numericInput('sig_textpad', 'Separación etiqueta-barra (% rango Y):',
                                          value = .01, min = .005, step = .005),   # ← NUEVO
                             numericInput('sig_textsize', 'Tamaño etiqueta (pts):',
                                          value = 5,          #  ←  tamaño por defecto visible
                                          min   = 1,
                                          step  = .5
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(6, numericInput('plot_w', 'Ancho px:', 1000, min = 100)),
                             column(6, numericInput('plot_h', 'Alto  px:',  700, min = 100))
                           ),
                           br(), br(),
                           
                           # ── Botones de descarga ──────────────────────────────────────────────
                           conditionalPanel(
                             "input.tipo == 'Apiladas'",
                             actionButton("downloadPlotly_png", "Descargar PNG",
                                          class = "btn btn-primary")
                           ),
                           conditionalPanel(
                             "input.tipo != 'Apiladas'",
                             downloadButton("downloadPlot", "Descargar PNG",
                                            class = "btn btn-primary")
                           ),
                           
                           br(), br(),
                           
                           downloadButton("downloadExcel",    "Descargar Datos",
                                          class = "btn btn-primary"),
                           br(), br(),
                           
                           downloadButton("downloadMetadata", "Descargar Metadata",
                                          class = "btn btn-primary"),
                           br(), br(),
                           
                           downloadButton("downloadStats",    "Descargar Resultados Estadísticos",
                                          class = "btn btn-primary"),
                           hr(),
                           
                         ),  # ---- FIN sidebarPanel -----------------------------------------------
                         
                         ###########################################################################
                         #  MAIN PANEL -------------------------------------------------------------
                         ###########################################################################
                         mainPanel(
                           plotlyOutput('plotInteractivo', width = '100%', height = 'auto'),
                           br(),
                           tags$div(
                             style = "text-align: right; margin-top: 8px;",
                             actionButton(
                               "add2panel",
                               label = tagList(icon("plus-circle"), "Añadir al panel"),
                               class = "btn btn-success"
                             )
                           ),
                           DTOutput('statsTable'),
                           tags$div(
                             style = "font-size:16px; text-align:center; color:#555;",
                             "Comentarios y sugerencias a ",
                             tags$a("bioszenf@gmail.com",
                                    href = "mailto:bioszenf@gmail.com",
                                    style = "font-weight:bold;")
                             
                           )   # ← comentario dice “FIN sidebarLayout”, pero en realidad cierra sólo <div>
                         ) # ← este cierra <mainPanel>
                       )
              ),
              # ─ Pestaña 2: Growth Rates ─────────────────────────────────────────────
              tabPanel("Obtención de Parámetros de Crecimiento",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(
                             "growthFiles",
                             "Cargar curvas de crecimiento (.xlsx)",
                             accept = ".xlsx",
                             multiple = TRUE
                           ),
                           numericInput(
                             "maxTime",
                             "Tiempo máximo",
                             value = 48,
                             min = 0
                           ),
                           numericInput(
                             "timeInterval",
                             "Intervalo (by)",
                             value = 0.5,
                             min = 0.01
                           ),
                           actionButton(
                             "runGrowth",
                             "Calcular parámetros",
                             class = "btn btn-primary"
                           ),
                           br(), br(),
                           downloadButton(
                             "downloadGrowthZip",
                             "Descargar resultados",
                             class = "btn btn-success"
                           ),
                           # solo mostramos cuando el usuario haya subido exactamente 1 archivo
                           uiOutput("showImportBtn")
                         ),
                         mainPanel(
                           DTOutput("growthTable")
                         )
                       )
              )  # ← cierre de tabPanel Growth Rates (sin coma si es la última)
  )  # fin de tabsetPanel(id = "mainTabs", …)
)  # fin de fluidPage(


# ──────────────────────────────────────────────────────────────────────────────  
# Helper robusto: lee una hoja Excel aunque el nombre temporal NO tenga extensión  
read_excel_tmp <- function(path, sheet = NULL) {  
  sig <- readBin(path, "raw", n = 8)  
  
  # 0xD0CF11E0 = formato OLE (xls 97-2003)  
  is_xls  <- identical(sig[1:4], as.raw(c(0xD0,0xCF,0x11,0xE0)))  
  # 0x504B0304 = inicio de archivo ZIP (xlsx, xlsm, ods…)  
  is_zip  <- identical(sig[1:4], as.raw(c(0x50,0x4B,0x03,0x04)))  
  
  if (is_xls)       return(readxl::read_xls (path, sheet = sheet))  
  if (is_zip)       return(readxl::read_xlsx(path, sheet = sheet))  
  
  stop("El archivo subido no es un Excel válido (.xls o .xlsx)")  
}  
# ──────────────────────────────────────────────────────────────────────────────  

# ── Función para generar el Excel de resumen por parámetro ────────────  
generate_summary_wb <- function(datos, params) {  
  wb <- createWorkbook()
  for (param in params) {
    sheet <- safe_sheet(param)
    addWorksheet(wb, sheet)

    # 1) Detalle técnico
    det <- datos %>%
      dplyr::filter(!is.na(Strain), !is.na(Media)) %>%
      dplyr::select(
        Strain, Media, Orden,
        BiologicalReplicate, TechnicalReplicate,
        Valor = !!rlang::sym(param)
      ) %>%
      dplyr::group_by(Strain, Media, Orden,
                       BiologicalReplicate, TechnicalReplicate) %>%
      dplyr::summarise(Valor = mean(Valor, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(Strain, BiologicalReplicate, TechnicalReplicate)
    
    # 2) Orden de medios según 'Orden'  
    medias_order <- det %>%  
      dplyr::distinct(Media, Orden) %>%  
      dplyr::arrange(Orden) %>%  
      dplyr::pull(Media)  
    
    # 3) Escribir por cepa  
    fila <- 1  
    for (s in unique(det$Strain)) {  
      writeData(wb, sheet, paste("Strain:", s),  
                startRow = fila, startCol = 1)  
      fila <- fila + 1  
      
      tab_cepa <- det %>%
        dplyr::filter(Strain == s) %>%
        dplyr::select(BiologicalReplicate, TechnicalReplicate, Media, Valor) %>%
        tidyr::pivot_wider(
          id_cols    = c(BiologicalReplicate, TechnicalReplicate),
          names_from = Media,
          values_from = Valor,
          values_fill = NA,
          values_fn  = list(Valor = ~ mean(.x, na.rm = TRUE))
        ) %>%
        dplyr::select(BiologicalReplicate,
                      dplyr::any_of(medias_order)) %>%
        dplyr::rename(RepBiol = BiologicalReplicate)
      
      writeData(wb, sheet, tab_cepa,  
                startRow    = fila,  
                startCol    = 1,  
                headerStyle = createStyle(textDecoration = "bold"))  
      fila <- fila + nrow(tab_cepa) + 2  
    }  
    
    # 4) Resumen por réplica biológica  
    writeData(wb, sheet, "Resumen por réplica biológica",  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(fontSize = 12, textDecoration = "bold"))  
    fila <- fila + 1  
    
    resumen <- det %>%
      dplyr::group_by(Strain, BiologicalReplicate, Media) %>%
      dplyr::summarise(Promedio = mean(Valor, na.rm = TRUE),
                       .groups = "drop") %>%
      tidyr::pivot_wider(
        id_cols    = c(Strain, BiologicalReplicate),
        names_from = Media,
        values_from = Promedio,
        values_fill = NA,
        values_fn  = list(Promedio = ~ mean(.x, na.rm = TRUE))
      ) %>%
      dplyr::arrange(Strain, BiologicalReplicate) %>%
      dplyr::rename(RepBiol = BiologicalReplicate)
    
    writeData(wb, sheet, resumen,  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(textDecoration = "bold"))  
  }  
  wb  
}  

# ────────────────────────────────────────────────────────────────
# Funciones Growth Rates – robustas y permisivas
# (colócalas en el ámbito global, antes de server)
# ────────────────────────────────────────────────────────────────

# 1. Fase exponencial - ROBUSTA
identify_exponential_phase_robust <- function(df, time_col, measure_col,
                                              umax_lower_bound = 0.05,
                                              umax_upper_bound = 0.25,
                                              max_iterations = 10,
                                              initial_r_squared_threshold = 0.95) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  # limpia NAs y filtrado del 5-95 %
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts            <- 10
  r2_threshold       <- initial_r_squared_threshold
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2   <- summary(model)$r.squared
        umax <- coef(model)[2]
        
        if (!is.na(r2) &&
            umax > umax_lower_bound &&
            umax < umax_upper_bound &&
            r2   > r2_threshold &&
            r2   > best_r2) {
          
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    
    # heurística de ajuste de límites
    if (!is.null(best_model)) {
      umax <- coef(best_model)[2]
      if (umax < umax_lower_bound) {
        min_pts          <- max(min_pts - 1, 5)
        umax_lower_bound <- umax_lower_bound - 0.01
        r2_threshold     <- max(r2_threshold - 0.01, 0.90)
      } else if (umax > umax_upper_bound) {
        min_pts          <- min(min_pts + 1, nrow(df) - 5)
        umax_upper_bound <- umax_upper_bound + 0.01
        r2_threshold     <- min(r2_threshold + 0.01, 0.99)
      } else {
        break
      }
    }
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 2. Fase exponencial - PERMISIVA
identify_exponential_phase_permissive <- function(df, time_col, measure_col,
                                                  umax_lower_bound = 0.01,
                                                  umax_upper_bound = 0.50,
                                                  max_iterations   = 10) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts <- 10
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2 <- summary(model)$r.squared
        if (!is.na(r2) && r2 > best_r2) {
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    break        # en permisiva basta la 1.ª iteración
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 3. Calcular parámetros – ROBUSTO
calculate_growth_rates_robust <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_robust(.,
                                                 time_col = "Time",
                                                 measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 4. Calcular parámetros – PERMISIVO
calculate_growth_rates_permissive <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_permissive(.,
                                                     time_col = "Time",
                                                     measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 5. Detectar valores vacíos
is_empty_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}
# ────────────────────────────────────────────────────────────────

#───────────────────────────────────────────────────────────────────────────────
# Helper: asegura que existan PlotSettings y columnas de parámetros
#───────────────────────────────────────────────────────────────────────────────
prepare_platemap <- function(df_datos, cfg) {
  
  # ─ 1. PlotSettings inexistente o vacío ────────────────────────────────────
  if (is.null(cfg) || nrow(cfg) == 0 || !"Parameter" %in% names(cfg)) {
    message("⚠️  PlotSettings no encontrado: se genera configuración mínima")
    # Identificamos columnas numéricas >> posibles parámetros
    param_cols <- setdiff(names(df_datos),
                          c("Strain","Media","Orden",
                            "BiologicalReplicate","TechnicalReplicate","Well"))
    if (length(param_cols) == 0) {
      param_cols <- "Parametro_dummy"
      df_datos[[param_cols]] <- NA_real_
    }
    cfg <- tibble::tibble(
      Parameter = param_cols,
      Y_Max     = 1,
      Interval  = 1,
      Y_Title   = param_cols
    )
  }
  
  # ─ 2. Asegurar que todas las columnas de cfg$Parameter existan en df_datos ─
  faltantes <- setdiff(cfg$Parameter, names(df_datos))
  if (length(faltantes)) {
    df_datos[ faltantes ] <- NA_real_
  }
  
  list(datos = df_datos, cfg = cfg)
}



# --- Server ---  
server <- function(input, output, session) {
  
  # almacenamiento reactivo de los plots que el usuario añade
  plot_bank  <- reactiveValues(all = list())
  
  # bandera para saber si ya inserté la pestaña “Combinado”
  panel_inserto <- reactiveVal(FALSE)
  
  # trigger para forzar repintado tras overrides globales
  ov_trigger <- reactiveVal(0)
  
  observeEvent(input$add2panel, {
    id <- paste0("p", as.integer(Sys.time()))
    snap <- plot_base()                     # tu copia del plot
    plot_bank$all[[id]] <- list(id = id,
                                plot = snap,
                                overrides = list())
    
    
    ## si aún no existe la pestaña, la insertamos
    if (!panel_inserto()) {
      insertTab(inputId  = "mainTabs",
                tab      = tab_compos,
                target   = "Obtención de Parámetros de Crecimiento", # la coloca al final
                position = "after",
                select   = FALSE)
      panel_inserto(TRUE)
    }
  })
  
  ov_trigger <- reactiveVal(0)  
  output$plotOverrideUI <- renderUI({
    tagList(
      textInput   ("ov_title",     "Título (override)",      ""),
      numericInput("ov_fs_title",  "Tamaño título (pts)",    NA, min = 6),
      numericInput("ov_fs_axis",   "Tamaño ejes (pts)",      NA, min = 6),
      numericInput("ov_fs_legend", "Tamaño leyenda (pts)",   NA, min = 6),
      
      hr(), h4("Ajustes Boxplot / Barras"),
      numericInput("ov_box_w",  "Ancho de caja",     NA, min = 0.1, max = 1.5, step = 0.05),
      numericInput("ov_pt_size","Tamaño puntos",     NA, min = 0.5, max = 6,   step = 0.5),
      numericInput("ov_pt_jit", "Dispersión puntos", NA, min = 0,   max = 0.5, step = 0.01),
      
      actionButton("apply_ov",  "Aplicar cambios a TODOS",
                   class = "btn btn-success")
    )
  })
  
  # ⬇️  BLOQUE que actualiza el selector “Editar gráfico”
  observe({
    ids <- names(plot_bank$all)
    if (is.null(ids)) ids <- character(0)   # ← línea nueva
    
    choices <- if (length(ids))
      setNames(ids, paste("Plot", seq_along(ids)))
    else
      character(0)
    
    updateSelectInput(session, "plot_edit", choices = choices)
  })
  
  observeEvent(input$apply_ov, {
    ov_new <- list(
      title     = if (nzchar(input$ov_title))    input$ov_title    else NULL,
      fs_title  = if (!is.na(input$ov_fs_title)) input$ov_fs_title else NULL,
      fs_axis   = if (!is.na(input$ov_fs_axis))  input$ov_fs_axis  else NULL,
      fs_legend = if (!is.na(input$ov_fs_legend))input$ov_fs_legend else NULL,
      box_w     = if (!is.na(input$ov_box_w))    input$ov_box_w    else NULL,
      pt_size   = if (!is.na(input$ov_pt_size))  input$ov_pt_size  else NULL,
      pt_jit    = if (!is.na(input$ov_pt_jit))   input$ov_pt_jit   else NULL
    )
    
    # Aplica el mismo override a todos los plots guardados
    for (id in names(plot_bank$all)) {
      plot_bank$all[[id]]$overrides <- ov_new
    }
    
    # dispara repintado del combo
    ov_trigger(ov_trigger() + 1)
  })

  # ── Growth Rates: process + download ZIP ─────────────────────────────────
  
  # Directorio global para resultados de crecimiento
  growth_out_dir <- file.path(tempdir(), "growth_results")
  
  observeEvent(input$runGrowth, {
    req(input$growthFiles)
    files <- input$growthFiles$datapath
    names <- input$growthFiles$name
    
    # (Re)crea el directorio de salida
    if (dir.exists(growth_out_dir)) unlink(growth_out_dir, recursive = TRUE)
    dir.create(growth_out_dir)
    
    # BARRA DE PROGRESO GLOBAL SOBRE ARCHIVOS
    withProgress(message = "Procesando archivos…", value = 0, {
      n_files <- length(files)
      
      for (i in seq_along(files)) {
        f  <- files[i]
        nm <- tools::file_path_sans_ext(names[i])
        
        ## 1) Generar .xlsx de curvas (idéntico a tu código)
        raw <- readxl::read_excel(f, skip = 2)
        Time <- seq(0, input$maxTime, by = input$timeInterval)
        raw <- raw[seq_len(min(length(Time), nrow(raw))), ]
        new_data <- data.frame(
          Time = Time[seq_len(nrow(raw))],
          raw[, -c(1,2)]
        )
        fixed_params <- data.frame(
          X_Max      = 50,
          Interval_X = 10,
          Y_Max      = 1.5,
          Interval_Y = 0.5,
          X_Title    = "Tiempo (h)",
          Y_Title    = "OD620",
          stringsAsFactors = FALSE
        )
        curvas_file <- file.path(growth_out_dir, paste0("Curvas_", nm, ".xlsx"))
        writexl::write_xlsx(
          list(Sheet1 = new_data, Sheet2 = fixed_params),
          path = curvas_file
        )
        
        ## 2) Leer cada hoja excepto Sheet2 y calcular parámetros
        raw_wide <- gcplyr::read_wides(curvas_file, sheet = "Sheet1", startrow = 1, startcol = 1)
        tidy_df  <- gcplyr::trans_wide_to_tidy(raw_wide[, -1], id_cols = "Time")
        wells    <- unique(tidy_df$Well)
        n_wells  <- length(wells)
        total_steps <- n_wells * 2

        
        
        # BARRA DE PROGRESO INTERNA POR CADA CURVA
        withProgress(
          message = paste("Procesando curvas de", nm),
          value   = 0,
          {
            step <- 0
            all_results <- vector("list", n_wells)
            
            for (k in seq_along(wells)) {
              w  <- wells[k]
              df_w <- tidy_df %>%
                filter(Well == w) %>%
                mutate(Well = factor(Well, levels = wells),
                       Time = as.numeric(Time))
              
              # 1) Cálculo robusto
              rob <- calculate_growth_rates_robust(df_w)
              step <- step + 1
              incProgress(1/total_steps,
                          detail = sprintf("R %d/%d: %s", step, total_steps, w))
              
              # 2) Cálculo permisivo
              perm <- calculate_growth_rates_permissive(df_w)
              step <- step + 1
              incProgress(1/total_steps,
                          detail = sprintf("P %d/%d: %s", step, total_steps, w))
              
              # 3) Fallback
              idx <- which(is_empty_value(rob$µMax))
              if (length(idx)) rob[idx, ] <- perm[idx, ]
              rob$Well <- w
              all_results[[k]] <- rob
            }
          }
        )
        
        
        # 3) Combina resultados y guarda parámetros
        # wells es el vector que definiste antes: unique(tidy_df$Well)
        final_df <- bind_rows(all_results) %>%
          # 1) asegúrate de que Well es factor en el orden original
          mutate(Well = factor(Well, levels = wells)) %>%
          # 2) ordena por ese factor
          arrange(Well) %>%
          # 3) incluye Well en el primer lugar
          dplyr::select(
            Well,
            µMax,
            ODmax,
            AUC,
            lag_time,
            max_percap_time,
            doub_time,
            max_time
          )
        
        param_file <- file.path(growth_out_dir, paste0("Parametros_", nm, ".xlsx"))
        openxlsx::write.xlsx(final_df, param_file, sheetName = "Resultados Combinados",
                             colNames = TRUE, rowNames = FALSE)
        
        # AVANZA LA BARRA GLOBAL
        incProgress(1 / n_files,
                    detail = paste("Archivo", nm, "completado"))
      }
    })
    
    # Al terminar, muestras la tabla y habilitas la descarga igual que antes…
    files <- list.files(growth_out_dir, pattern = "^Parametros_.*\\.xlsx$", full.names = TRUE)
    dfs <- lapply(files, readxl::read_excel)
    names(dfs) <- basename(files)
    combined <- bind_rows(dfs, .id = "Archivo")
    output$growthTable <- DT::renderDT(combined, options = list(pageLength = 10))
  })
  
  
  # Download ZIP siempre registrado
  output$downloadGrowthZip <- downloadHandler(
    filename = function() "growth_results.zip",
    content = function(file) {
      # 1) Guardamos el wd actual
      old_wd <- getwd()
      # 2) Nos movemos al directorio de salida para evitar rutas absolutas
      setwd(growth_out_dir)
      # 3) Al salir, volvemos al wd original
      on.exit(setwd(old_wd), add = TRUE)
      
      # 4) Listamos solo los nombres de fichero .xlsx en growth_out_dir
      files_to_zip <- list.files(pattern = "\\.xlsx$")
      
      # 5) Creamos el ZIP usando nombres relativos
      zip::zip(zipfile = file, files = files_to_zip)
    },
    contentType = "application/zip"
  )
  
  
  # espacio para mensajes de progreso (opcional)
  output$growthProgress <- renderUI({
    # podrías volcar aquí notifications o simplemente dejar el withProgress
    NULL
  })
  
  observeEvent(input$btn_light, {
    # 1· cambia el tema visual
    session$setCurrentTheme(theme_light)
    # 2· guarda la preferencia en localStorage
    session$sendCustomMessage("saveMode", "light")
    # 3· actualiza input$mode (por si lo usas en otros lugares)
    shinyjs::runjs(
      "Shiny.setInputValue('mode', 'light', {priority: 'event'});"
    )
  })
  
  observeEvent(input$btn_dark, {
    session$setCurrentTheme(theme_dark)
    session$sendCustomMessage("saveMode", "dark")
    shinyjs::runjs(
      "Shiny.setInputValue('mode', 'dark', {priority: 'event'});"
    )
  })
  # ---- Cambio de idioma -------------------------------------------
  # ---- Cambio de idioma desde el nuevo menú desplegable -------------------
  observeEvent(input$lang_es, {
    shinyjs::runjs("setLang('es');")          # Google Translate → Español
  })
  
  observeEvent(input$lang_en, {
    shinyjs::runjs("setLang('en');")          # Google Translate → English
  })
  

  # ---------- helper: genera SIEMPRE un ggplot (o grob) -----------------
  make_snapshot <- function(){
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL
    
    if (input$tipo == "Apiladas"){
      # build_plot() devuelve la versión ggplot de Apiladas
      build_plot(scope_sel, strain_sel, "Apiladas")
    } else {
      # para los demás tipos, plot_base() ya es ggplot
      plot_base()
    }
  }
  
  # ---------- NUEVO bloque ‘Añadir al panel’ -----------------------------
  observeEvent(input$add2panel, {
    id   <- paste0("p", as.integer(Sys.time()))
    snap <- make_snapshot()               # ← ahora siempre ggplot/grob ✅
    
    plot_bank$all[[id]] <- list(
      id        = id,
      plot      = snap,
      overrides = list()
    )
    
    showNotification("Gráfico añadido al panel",
                     type = "message", duration = 2)
    
    # si aún no existe la pestaña de composición, la insertamos
    if (!panel_inserto()) {
      insertTab(
        inputId  = "mainTabs",
        tab      = tab_compos,
        target   = "Obtención de Parámetros de Crecimiento",
        position = "after",
        select   = FALSE
      )
      panel_inserto(TRUE)
    }
  })
  
  
  # ─── Logo siempre visible ─────────────────────────────────────────
  output$logo_img <- renderUI({
    # si todavía no hay input$mode asumimos “light”
    modo <- input$mode %||% "light"      # operador %||%  → usa la derecha si es NULL
    
    archivo <- if (modo == "dark") "logo_dark.png" else "logo_light.png"
    tags$img(src = archivo, style = "height:220px;")
  })
  
  
  # ───── contenedores reactivos (vacíos al arrancar) ─────  
  datos_box     <- reactiveVal(NULL)   # hoja «Datos»  
  plot_cfg_box  <- reactiveVal(NULL)   # hoja «PlotSettings»  
  cur_data_box  <- reactiveVal(NULL)   # curvas Sheet1  
  cur_cfg_box   <- reactiveVal(NULL)   # curvas Sheet2  
  sig_list <- reactiveVal(list())   # guardará comparaciones  
  
  observeEvent(input$doNorm, {  
    if (isTRUE(input$doNorm) && is.null(input$ctrlMedium)) {  
      showNotification("Selecciona primero el medio control",  
                       type = "warning", duration = 4)  
    }  
  })  
  
  observeEvent(input$add_sig, {  
    req(input$sig_group1, input$sig_group2, nzchar(input$sig_label))  
    # no duplicar  
    new_cmp <- list(g1 = input$sig_group1,  
                    g2 = input$sig_group2,  
                    lab = input$sig_label)  
    sl <- sig_list()  
    if (!any(vapply(sl, function(x) identical(x, new_cmp), logical(1)))) {  
      sig_list( append(sl, list(new_cmp)) )  
    }  
  })  
  
  observeEvent(input$clear_sig, {  
    sig_list(list())  
  })  
  
  
  observeEvent(input$curveFile, {
    ok <- tryCatch({
      # 1) Leer curvas (Sheet1)
      d <- read_excel_tmp(input$curveFile$datapath, sheet = "Sheet1")
      
      # 2) Leer configuración (Sheet2)
      s <- tryCatch(
        read_excel_tmp(input$curveFile$datapath, sheet = "Sheet2"),
        error = function(e) NULL
      )
      
      # 3) Si falta config o le faltan columnas, armamos defaults basados en d
      required_cols <- c("X_Max","Interval_X","Y_Max","Interval_Y","X_Title","Y_Title")
      if (is.null(s) || !all(required_cols %in% names(s))) {
        s <- tibble::tibble(
          X_Max      = max(d$Time, na.rm = TRUE),
          Interval_X = max(d$Time, na.rm = TRUE) / 3,
          Y_Max      = max(d[ , setdiff(names(d), "Time")], na.rm = TRUE),
          Interval_Y = max(d[ , setdiff(names(d), "Time")], na.rm = TRUE) / 3,
          X_Title    = "Tiempo (min)",
          Y_Title    = "OD (620 nm)",
          Well       = NA,                    # si tu lógica luego asume estas columnas
          BiologicalReplicate = NA
        )
      }
      
      # 4) Guardar en los reactivos
      cur_data_box(d)
      cur_cfg_box(s)
      
      # 5) Inicializar límites de curvas
      ylims$Curvas <- list(
        xmax   = s$X_Max[1],
        xbreak = s$Interval_X[1],
        ymax   = s$Y_Max[1],
        ybreak = s$Interval_Y[1]
      )
      
      TRUE
    }, error = function(e) {
      showNotification(
        paste("❌ Archivo de curvas inválido:", e$message),
        type = "error", duration = 6
      )
      FALSE
    })
    if (!ok) return()
  }, ignoreInit = TRUE)
  
  observeEvent(curve_settings(), {
    req(curve_settings())
    # tomo la primera fila de tu Sheet2
    cfg <- curve_settings()[1, ]
    # actualizo los controles de Curvas
    updateNumericInput(session, "xmax_cur",   value = cfg$X_Max)
    updateNumericInput(session, "xbreak_cur", value = cfg$Interval_X)
    updateNumericInput(session, "ymax_cur",   value = cfg$Y_Max)
    updateNumericInput(session, "ybreak_cur", value = cfg$Interval_Y)
    
    fluidRow(
      column(6, numericInput("ymin_corr",      "Y min:",            value = 0, min = 0)),
      column(6, numericInput("ymax_corr",      "Y max:",            value = 1, min = 0))
    )
    fluidRow(
      column(6, numericInput("corr_label_size","Tamaño etiquetas:", value = 5, min = 1 ))
    )
    
    
    updateTextInput(session, "cur_xlab", value = cfg$X_Title)
    updateTextInput(session, "cur_ylab", value = cfg$Y_Title)
  })
  
  
  
  curve_data     <- reactive( cur_data_box() )  
  curve_settings <- reactive( cur_cfg_box() )  
  
  
  # ── Inputs dinámicos para Curvas: selección de réplicas por pozo ──  
  output$repSelCurvas <- renderUI({  
    # metadatos de wells  
    cfg <- curve_settings()  
    req(cfg$Well)  # asumimos que sheet2 tiene una columna Well  
    
    # cada well es un valor único de cfg$Well  
    wells <- unique(cfg$Well)  
    lapply(wells, function(w) {  
      reps <- sort(unique(cfg$BiologicalReplicate[cfg$Well == w]))  
      checkboxGroupInput(  
        paste0("reps_cur_", make.names(w)),  
        paste("Réplicas –", w),  
        choices  = reps,  
        selected = reps  
      )  
    })  
  })  
  
  # --- Reactivos de lectura seguros -------------------------------------------  
  ## ────────── NUEVO manejo robusto del archivo principal ──────────  
  datos_raw      <- reactiveVal(NULL)   #   guarda «Datos»  
  plot_settings  <- reactiveVal(NULL)   #   guarda «PlotSettings»  
  
  # ── Lectura robusta del Excel de metadata+parámetros ─────────────────────────
  observeEvent(input$dataFile, {
    
    ok <- tryCatch({
      
      df_raw <- read_excel_tmp(input$dataFile$datapath, sheet = "Datos")
      cfg_raw <- tryCatch(
        read_excel_tmp(input$dataFile$datapath, sheet = "PlotSettings"),
        error = function(e) NULL
      )
      
      # Prepara platemap (crea cfg/columnas vacías si faltan)
      prep <- prepare_platemap(df_raw, cfg_raw)
      df   <- prep$datos
      cfg  <- prep$cfg
      
      # Limpia estado anterior y guarda
      datos_box(NULL);          plot_cfg_box(NULL)
      datos_box(df);            plot_cfg_box(cfg)
      
      # ← reinicia límites, etc. (dejamos lo que ya tenías)
      ylims <<- reactiveValues()
      
      TRUE
      
    }, error = function(e){
      showNotification(paste("❌ Archivo inválido:", e$message),
                       type = "error", duration = 6)
      FALSE
    })
    
    if (!ok) return()
    
    ## ──  REFRESCAR UI   (según haya o no parámetros «reales») ────────────────
    params <- plot_cfg_box()$Parameter
    
    # 2.a) actualiza selector de tipo de gráfico ------------------------------
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      updateRadioButtons(session, "tipo",
                         choices  = c("Curvas"), selected = "Curvas")
    } else {
      updateRadioButtons(session, "tipo",
                         choices  = c("Boxplot","Barras","Curvas","Apiladas",
                                      "Correlación"),
                         selected = "Boxplot")
    }
    
    # 2.b) selector de parámetro (lo deje vacío si no hay)
    updateSelectInput(session, "param",
                      choices  = params,
                      selected = if (length(params)) params[1] else character(0))
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(plot_settings(), {  
    params <- plot_settings()$Parameter  
    updateCheckboxGroupInput(  
      session, "stackParams",  
      choices  = params,  
      selected = params           # default = todos  
    )  
    # ---------- Correlación: poblar selectInputs --------------------------
    updateSelectInput(
      session, "corr_param_x",
      choices  = params,
      selected = params[1]
    )
    updateSelectInput(
      session, "corr_param_y",
      choices  = params,
      selected = params[min(2, length(params))]
    )
    
    # inicializar también el orden de parámetros apilados  
    updateTextInput(  
      session, "orderStack",  
      value = paste(params, collapse = ",")  
    )  
  }, ignoreInit = FALSE)  
  
  
  ## accesos rápidos (usan las reactiveVal que acabamos de crear)  
  datos_combinados <- reactive( datos_box() )  
  plot_settings    <- reactive( plot_cfg_box() )  
  
  # --- Parám. seguro: siempre existe en el Excel cargado ----  
  safe_param <- reactive({  
    req(input$param)  
    if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
      paste0(input$param, "_Norm")  
    else  
      input$param  
  })  
  
  
 
  
  # justo después, dentro de server(), inicializa también la entrada “Curvas”  
  observeEvent(curve_settings(), {  
    req(curve_settings())          # ← evita que se ejecute con NULL  
    cfg <- curve_settings()[1, ]  
    ylims$Curvas <- list(  
      ymax   = cfg$Y_Max,  
      ybreak = cfg$Interval_Y,  
      xmax   = cfg$X_Max,  
      xbreak = cfg$Interval_X  
    )  
  }, ignoreNULL = TRUE)             # ← cualquiera de las dos opciones  
  
  
  # ——————————————————————————————————————————————  
  # Paleta segura: devuelve un vector vacío si n = 0  
  safe_hue <- function(n) {  
    if (n > 0) scales::hue_pal()(n) else character(0)  
  }  
  # ——————————————————————————————————————————————  
  
  
  # 1) Inicialización: crear una entrada en ylims para cada parámetro  
  observeEvent(plot_settings(), {  
    params <- plot_settings()$Parameter  
    for (p in params) {  
      # ya estáis haciendo esto  
      if (is.null(ylims[[p]])) {  
        cfg <- plot_settings() %>% filter(Parameter == p)  
        ylims[[p]] <- list(  
          ymax   = cfg$Y_Max,  
          ybreak = cfg$Interval  
        )  
      }  
      # ─── PEGA ABAJO este bloque ─────────────────────────────────────────  
      # límites por defecto para valores normalizados  
      ylims[[paste0(p, "_Norm")]] <- list(  
        ymax   = 1,     # rango típico normalizado: 0–1  
        ybreak = 0.2    # intervalo de 0.2  
      )  
      # ────────────────────────────────────────────────────────────────────  
    }  
  }, ignoreNULL = FALSE)  
  
  
  # ── Reset general al cargar un nuevo archivo ───────────────────────  
  observeEvent(input$dataFile, {  
    req(plot_settings())                     # esperamos a tener la hoja  
    
    ## 1· reiniciar parámetro seleccionado  
    updateSelectInput(session, "param",  
                      choices  = plot_settings()$Parameter,  
                      selected = plot_settings()$Parameter[1])  
    
    ## 2· reiniciar strain / scope  
    updateRadioButtons(session, "scope", selected = "Por Cepa")  
    updateSelectInput(session, "strain", choices = NULL)  
    
    ## 3· reiniciar selección de gráficos  
    isolate({  
      strains <- sort(unique(datos_combinados()$Strain))  
      tipos <- c("Boxplot","Barras","Curvas","Apiladas")  
      cepa    <- as.vector(t(outer(strains, tipos,  
                                   FUN = function(s, t) paste0(s, "_", t))))  
      combo   <- paste0("Combinado_", tipos)  
      
    })  
  })  
  
  # Para Boxplot/Barras  
  ## ── guardar cambios de Y‑axis hechos por el usuario ──────────────────  
  observeEvent(  
    list(input$ymax, input$ybreak, input$doNorm, input$ctrlMedium, input$param),  
    {  
      # clave correcta:  “PAR”    o  “PAR_Norm”  
      tgt <- if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
        paste0(input$param, "_Norm") else input$param  
      
      ylims[[tgt]] <- list(  
        ymax   = input$ymax,  
        ybreak = input$ybreak  
      )  
    },  
    ignoreInit = TRUE  
  )  
  
  # Para Curvas  
  observeEvent(list(input$xmax_cur, input$xbreak_cur, input$ymax_cur, input$ybreak_cur), {  
    req(curve_settings())  
    ylims$Curvas <- list(  
      xmax   = input$xmax_cur,  
      xbreak = input$xbreak_cur,  
      ymax   = input$ymax_cur,  
      ybreak = input$ybreak_cur  
    )  
  }, ignoreInit = TRUE)  
  
  
  
  # 3) Y en build_plot(), para obtener los límites, ya no usas Curvas/AUC fijos,  
  #    sino:  
  get_ylim <- function(param) {  
    req(ylims[[param]])  
    ylims[[param]]  
  }  
  
  
  # dentro de server(), tras definir plot_settings()  
  output$paramSel <- renderUI({
    req(plot_cfg_box())                         # cfg ya cargada
    params <- plot_cfg_box()$Parameter
    if (length(params) == 0 || identical(params, "Parametro_dummy")) {
      helpText("⚠️ Sin parámetros: sólo se pueden graficar Curvas")
    } else {
      selectInput("param", "Parámetro:",
                  choices = params, selected = params[1])
    }
  })
  
  
  # --- Procesamiento de datos dinámico ---  
  datos_agrupados <- reactive({
    req(datos_combinados(), plot_settings())
    
    # 1) Parametros de configuración vs columnas reales
    params <- plot_settings()$Parameter
    df     <- datos_combinados()
    present <- intersect(params, names(df))
    missing <- setdiff(params, present)
    
    # 2) Notificar si faltan
    if (length(missing) > 0) {
      showNotification(
        paste0("Estos parámetros no existen en los datos y fueron omitidos: ",
               paste(missing, collapse = ", ")),
        type = "warning", duration = 5
      )
    }
    
    # 3) Agrupar y resumir sólo con los parámetros presentes
    df %>%
      filter(!is.na(Strain), !is.na(Media), Strain != "C-") %>%
      group_by(Strain, Media, BiologicalReplicate) %>%
      summarise(
        across(all_of(present), ~ mean(.x, na.rm = TRUE)),
        Orden = first(Orden),
        .groups = "drop"
      )
  })
  
  
  
  # --- Inputs dinámicos: Por Cepa ---  
  
  observeEvent(datos_agrupados(), {  
    
    
    # 1) poblar selector de cepas  
    updateSelectInput(session, "strain",  
                      choices = sort(unique(datos_agrupados()$Strain)))  
    # 2) poblar filtro de medios  
    medias <- sort(unique(datos_agrupados()$Media))  
    output$showMediosUI <- renderUI({  
      checkboxGroupInput("showMedios", "Medios:",  
                         choices = medias, selected = medias)  
    })  
    # 3) inicializar orden de medios  
    # Inicializa orderMedios usando el orden de la columna ‘Orden’
    medias_order <- datos_agrupados() %>%
      filter(Strain == input$strain) %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)
    
    updateTextInput(session, "orderMedios",
                    value = paste(medias_order, collapse = ","))
    
    updateCheckboxInput(session, "toggleMedios",  value = TRUE)  
    updateCheckboxInput(session, "toggleGroups", value = TRUE)  
    
    # ------------------------------------------------------------------  
    
    
    
  })  
  
  
  ## ------------------------------------------------------------------  
  ## Reactive: datos_agrupados_norm() – copia de datos_agrupados() con  
  ##           columnas normalizadas (sufijo "_Norm")  
  ## ------------------------------------------------------------------  
  datos_agrupados_norm <- reactive({  
    df <- datos_agrupados()  
    if (!isTRUE(input$doNorm)) return(df)          # sin normalizar  
    
    params <- plot_settings()$Parameter  
    
    # 2a) si el control NO está definido, solo ‘clonamos’ las columnas  
    if (is.null(input$ctrlMedium)) {  
      return(  
        df %>% mutate(across(all_of(params),  
                             ~ .x,                # copia tal cual  
                             .names = "{.col}_Norm"))  
      )  
    }  
    
    # 2b) control definido → normalización real  
    ctrl <- input$ctrlMedium  
    df %>%  
      group_by(Strain, BiologicalReplicate) %>%  
      mutate(across(  
        all_of(params),  
        ~ {  
          base <- .x[Media == ctrl][1]          # valor del control  
          if (is.na(base) || base == 0) NA_real_ else .x / base  
        },  
        .names = "{.col}_Norm"  
      )) %>%  
      ungroup()  
  })  
  
  
  # ── Toggle “Por Cepa” ───────────────────────────────────  
  observeEvent(input$toggleMedios, {  
    medias <- sort(unique(datos_agrupados()$Media))  
    sel    <- if (isTRUE(input$toggleMedios)) medias else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showMedios",  
                             choices  = medias,  
                             selected = sel  
    )  
  })  
  
  # ── Toggle “Combinado” ─────────────────────────────────  
  observeEvent(input$toggleGroups, {  
    grps <- unique(paste(datos_agrupados()$Strain, datos_agrupados()$Media, sep = "-"))  
    sel  <- if (isTRUE(input$toggleGroups)) grps else character(0)  
    updateCheckboxGroupInput(session,  
                             inputId  = "showGroups",  
                             choices  = grps,  
                             selected = sel  
    )  
    
    
  })  
  
  
  
  # ---------- NUEVO: checkboxes de réplicas por medio (modo Por Cepa) ----------  
  output$repsStrainUI <- renderUI({  
    df <- datos_agrupados() %>%              # datos promediados  
      filter(Strain == input$strain)  
    
    # construye un “sub-checkbox” por cada medio de esa cepa  
    tagList(lapply(unique(df$Media), function(m){  
      reps <- sort(unique(df$BiologicalReplicate[df$Media == m]))  
      checkboxGroupInput(  
        paste0("reps_", make.names(m)),       # id = reps_<medio>  
        paste("Réplicas -", m),  
        choices  = reps,  
        selected = reps  
      )  
    }))  
  })  
  
  output$ctrlSelUI <- renderUI({  
    req(input$doNorm)                          # sólo cuando se active el check  
    
    # ‑‑ si la cepa aún no está elegida, muestra TODOS los medios  
    if (input$scope == "Por Cepa" && !is.null(input$strain)) {  
      opts <- sort(unique(  
        datos_agrupados()$Media[  
          datos_agrupados()$Strain == input$strain]))  
    } else {  
      opts <- sort(unique(datos_agrupados()$Media))  
    }  
    
    selectInput("ctrlMedium", "Medio normalizador:",    # ← etiqueta genérica  
                choices = opts,  
                selected = if (length(opts)) opts[1] else character(0))  
  })  
  
  # --- Inputs dinámicos: Combinado ---  
  observeEvent(datos_agrupados(), {
    grps <- unique(paste(datos_agrupados()$Strain, datos_agrupados()$Media, sep = "-"))
    # ── SERVER  ─────────────────────────────────────────────
    output$groupSel <- renderUI({
      grps <- unique(paste(datos_agrupados()$Strain,
                           datos_agrupados()$Media, sep = "-"))
      
      labels_order <- datos_agrupados() %>%
        distinct(Strain, Media, Orden) %>%
        mutate(Label = paste(Strain, Media, sep = "-")) %>%
        arrange(Orden) %>% pull(Label)
      
      tagList(
        checkboxGroupInput(
          "showGroups", "Grupos:",
          choices  = grps,
          selected = grps
        ),
        textInput(
          "orderGroups", "Orden (csv):",
          value = paste(labels_order, collapse = ",")
        )
      )
    })
    updateCheckboxInput(session, "toggleGroups", value = TRUE)
  })
  
  output$repsGrpUI <- renderUI({
    grps <- unique(
      paste(datos_agrupados()$Strain,
            datos_agrupados()$Media, sep = "-")
    )
    
    accordion(
      id       = "repsGrpPanel",
      open     = FALSE,   # empieza cerrada
      multiple = TRUE,
      accordion_panel(
        "Réplicas por grupo",
        tagList(                       # envolver lapply
          lapply(grps, function(g){
            reps <- unique(
              datos_agrupados()$BiologicalReplicate[
                paste(datos_agrupados()$Strain,
                      datos_agrupados()$Media, sep = "-") == g
              ]
            )
            checkboxGroupInput(
              paste0("reps_grp_", make.names(g)),
              paste("Réplicas -", g),
              choices  = reps,
              selected = reps
            )
          })
        ),
        style = "default"
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # ► ACTUALIZA las listas de grupos disponibles para las barras de
  #   significancia.  Toma SIEMPRE los grupos que realmente se muestran en
  #   el gráfico (tras pasar por los filtros order_filter_*).
  # -------------------------------------------------------------------------
  observe({
    req(datos_agrupados())                       # hay datos cargados
    
    if (input$scope == "Por Cepa") {             # ── modo ‘Por Cepa’ ──
      req(input$strain)                          # ya hay una cepa elegida
      
      # Sólo los medios visibles con los filtros actuales
      medios_visibles <- datos_agrupados()       |>
        filter(Strain == input$strain)           |>
        order_filter_strain()                    |>      # respeta showMedios / orden
        pull(Media)                              |> 
        unique()                                 |> 
        sort()
      
      updateSelectInput(session, "sig_group1",
                        choices  = medios_visibles,
                        selected = medios_visibles[1])
      updateSelectInput(session, "sig_group2",
                        choices  = medios_visibles,
                        selected = if (length(medios_visibles) > 1)
                          medios_visibles[2] else medios_visibles[1])
      
    } else {                                     # ── modo ‘Combinado’ ──
      # Los grupos que siguen visibles después de los filtros “showGroups”
      grupos_visibles <- datos_agrupados()       |>
        order_filter_group()                     |>
        pull(Label)                              |>
        unique()                                 |>
        sort()
      
      updateSelectInput(session, "sig_group1",
                        choices  = grupos_visibles,
                        selected = grupos_visibles[1])
      updateSelectInput(session, "sig_group2",
                        choices  = grupos_visibles,
                        selected = if (length(grupos_visibles) > 1)
                          grupos_visibles[2] else grupos_visibles[1])
    }
  })
  # -------------------------------------------------------------------------
  
  
  # -- actualizar listas de Control / Pareo cuando cambian los grupos visibles --  
  observeEvent(input$showGroups, {  
    grps <- input$showGroups  
    updateSelectInput(session, "controlGroup", choices = grps,  
                      selected = if (length(grps)) grps[1] else NULL)  
    updateSelectInput(session, "group1", choices = grps,  
                      selected = if (length(grps)) grps[1] else NULL)  
    updateSelectInput(session, "group2", choices = grps,  
                      selected = if (length(grps) > 1) grps[2] else NULL)  
  }, ignoreNULL = FALSE)  
  
  # --- Defaults de escala y labels -----------------------------------
  observeEvent(input$param, {
    req(plot_settings(), input$param)
    
    cfg <- plot_settings() %>%
      dplyr::filter(Parameter == input$param)
    
    # 1· refrescar las cajas
    updateNumericInput(session, "ymax",
                       label  = paste0("Y max (", input$param, "):"),
                       value  = cfg$Y_Max)
    updateNumericInput(session, "ybreak",
                       label  = paste0("Int Y (", input$param, "):"),
                       value  = cfg$Interval)
    
    # 2· sincronizar ylims con el Excel
    ylims[[ input$param ]] <- list(
      ymax   = cfg$Y_Max,
      ybreak = cfg$Interval
    )
  }, ignoreNULL = TRUE)
  
  # ---------- Correlación: actualizar límites por defecto -------------
  observeEvent(
    list(input$corr_param_x, input$corr_param_y,
         input$doNorm, input$ctrlMedium),
    {
      req(plot_settings(), datos_box())
      # nombres reales (sin "_Norm")
      raw_x <- input$corr_param_x
      raw_y <- input$corr_param_y
      updateTextInput(session, "corr_xlab", value = raw_x)
      updateTextInput(session, "corr_ylab", value = raw_y)
      # hoja PlotSettings para cada parámetro
      cfg_x <- plot_settings() %>% filter(Parameter == raw_x)
      cfg_y <- plot_settings() %>% filter(Parameter == raw_y)
      df    <- datos_box()

      xmax <- if (nrow(cfg_x)) cfg_x$Y_Max[1] else NA_real_
      if (!is.finite(xmax)) {
        xmax <- suppressWarnings(max(df[[raw_x]], na.rm = TRUE))
      }
      if (!is.finite(xmax) || xmax <= 0) xmax <- 1

      xbreak <- if (nrow(cfg_x)) cfg_x$Interval[1] else NA_real_
      if (!is.finite(xbreak) || xbreak <= 0) xbreak <- xmax/5

      ymax <- if (nrow(cfg_y)) cfg_y$Y_Max[1] else NA_real_
      if (!is.finite(ymax)) {
        ymax <- suppressWarnings(max(df[[raw_y]], na.rm = TRUE))
      }
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1

      ybreak <- if (nrow(cfg_y)) cfg_y$Interval[1] else NA_real_
      if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax/5

      updateNumericInput(session, "xmax_corr", value = xmax, max = Inf)
      updateNumericInput(session, "xbreak_corr", value = xbreak, min = 0.0001)
      updateNumericInput(session, "xmin_corr", value = 0)
      updateNumericInput(session, "ymin_corr", value = 0)
      updateNumericInput(session, "ymax_corr", value = ymax, max = Inf)
      updateNumericInput(session, "ybreak_corr", value = ybreak, min = 0.0001)
    },
    ignoreInit = FALSE
  )
  
  
  
  ## ── reajustar Y‑axis cuando se (des)activa la normalización ─────────  
  observeEvent(  
    list(input$doNorm, input$ctrlMedium, input$param),  
    {  
      req(plot_settings(), input$param)  
      
      tgt <- if (isTRUE(input$doNorm) && !is.null(input$ctrlMedium))  
        paste0(input$param, "_Norm") else input$param  
      
      lims <- get_ylim(tgt)  
      
      updateNumericInput(session, "ymax",  
                         label  = paste0("Y max (", tgt, "):"),  
                         value  = lims$ymax)  
      updateNumericInput(session, "ybreak",  
                         label  = paste0("Int Y (", tgt, "):"),  
                         value  = lims$ybreak)  
    },  
    ignoreInit = TRUE  
  )  
  
  
  # ── Sincronizar título editable con el título por defecto ─────────────────  
  observeEvent(
    list(input$scope, input$tipo, input$param, input$strain,
         input$corr_param_x, input$corr_param_y),
    {
      req(input$tipo)
      defaultTitle <- switch(
        input$tipo,
        "Correlación" = paste("Correlación",
                              input$corr_param_y, "vs",
                              input$corr_param_x),
        if (input$scope == "Combinado")
          paste(input$tipo, "combinado de", input$param)
        else
          paste(input$tipo, "de", input$param, "para", input$strain)
      )
      updateTextInput(session, "plotTitle", value = defaultTitle)
    }, ignoreInit = FALSE)
  
  
  
  # ---- Helpers para filtrar réplicas -------------------------------  
  filter_reps_strain <- function(df){  
    for (m in unique(df$Media)){  
      sel <- input[[paste0("reps_", make.names(m))]]  
      if (!is.null(sel))  
        df <- df[ !(df$Media == m & !df$BiologicalReplicate %in% sel), ]  
    }  
    df  
  }  
  
  filter_reps_group <- function(df){  
    grps <- input$showGroups  
    if (is.null(grps)) return(df[0, ])  
    df <- df[ paste(df$Strain, df$Media, sep = "-") %in% grps, ]  
    for (g in grps){  
      sel <- input[[paste0("reps_grp_", make.names(g))]]  
      if (!is.null(sel))  
        df <- df[ !(paste(df$Strain, df$Media, sep = "-") == g &  
                      !df$BiologicalReplicate %in% sel), ]  
    }  
    df  
  }  
  
  # --- helper: orden seguro de grupos (vacío si el input está vacío) ----  
  safe_orderGroups <- function() {  
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {  
      trimws(unlist(strsplit(input$orderGroups, ",")))  
    } else {  
      NULL  
    }  
  }  
  
  
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Helpers de filtrado + orden basados en la columna ‘Orden’ del platemap
  # ──────────────────────────────────────────────────────────────────────────────
  
  order_filter_strain <- function(df) {
    # 1) Aplica el filtro showMedios
    if (!is.null(input$showMedios)) {
      df <- df %>% filter(Media %in% input$showMedios)
    }
    # 2) Niveles originales según Orden
    final_levels <- df %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)
    # 3) Si el usuario escribió un CSV en orderMedios, lo prioriza
    if (!is.null(input$orderMedios) && nzchar(input$orderMedios)) {
      user_order   <- trimws(strsplit(input$orderMedios, ",")[[1]])
      final_levels <- intersect(user_order, final_levels)
    }
    # 4) Devuelve Media como factor con niveles en el orden correcto
    df %>% mutate(Media = factor(Media, levels = final_levels))
  }
  
  order_filter_group <- function(df) {
    # 1) Aplica el filtro de réplicas por grupo
    df2 <- filter_reps_group(df)
    # 2) Etiquetas originales según Orden
    final_levels <- datos_agrupados() %>%
      distinct(Strain, Media, Orden) %>%
      mutate(Label = paste(Strain, Media, sep = "-")) %>%
      arrange(Orden) %>%
      pull(Label)
    # 3) Si el usuario escribió un CSV en orderGroups, lo prioriza
    if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
      user_order   <- trimws(strsplit(input$orderGroups, ",")[[1]])
      final_levels <- intersect(user_order, final_levels)
    }
    # 4) Devuelve Label como factor con niveles en el orden correcto
    df2 %>% mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_levels))
  }
  
  
  
  
  # ── Post-hoc dinámico ────────────────────────────────────────────────────  
  output$postHocUI <- renderUI({  
    req(input$sigTest)  
    if (input$sigTest == "ANOVA") {  
      selectInput("postHoc", "Post-hoc:",  
                  choices = c(  
                    "Tukey"         = "Tukey",  
                    "Bonferroni"    = "Bonferroni",  
                    "Sidak"         = "Sidak",  
                    "Dunnett"       = "Dunnett",  
                    "Scheffé"       = "Scheffe",  
                    "Games–Howell"  = "GamesHowell"  
                  ),  
                  selected = "Tukey"  
      )  
    } else if (input$sigTest == "Kruskal–Wallis") {  
      selectInput("postHoc", "Post-hoc:",  
                  choices = c(  
                    "Dunn (Bonf.)" = "Dunn",  
                    "Conover"      = "Conover",  
                    "Nemenyi"      = "Nemenyi",  
                    "DSCF"         = "DSCF"  
                  ),  
                  selected = "Dunn"  
      )  
    } else {  
      NULL  
    }  
  })  
  
  
  
  # ── helpers de filtrado + orden ─────────────────────────────────────────  
  # (deja los tuyos: order_filter_strain() / order_filter_group())  
  
  # ── Data frame unificado para Significancia ───────────────  
  make_test_df <- function() {  
    p   <- safe_param()                                  # puede traer *_Norm  
    src <- if (isTRUE(input$doNorm))                     # ← NUEVO  
      datos_agrupados_norm() else datos_agrupados()  
    
    if (input$scope == "Por Cepa") {  
      src %>%  
        filter(Strain == input$strain) %>%  
        order_filter_strain() %>%  
        filter_reps_strain() %>%  
        transmute(Label = Media,  
                  Valor = .data[[p]])  
    } else {  
      src %>%  
        order_filter_group() %>%  
        transmute(Label,  
                  Valor = .data[[p]])  
    }  
  }  
  
  
  
  # ── Reusar el mismo para Normalidad ────────────────────────  
  make_norm_df <- make_test_df  
  
  observe({  
    # aseguramos que ya hay configuración y datos  
    req(plot_settings(), nrow(datos_agrupados()) > 0)  
    
    df_test <- make_test_df()  
    req(nrow(df_test) > 0)  
    
    grupos <- unique(df_test$Label)  
    updateSelectInput(session, "controlGroup",  
                      choices  = grupos,  
                      selected = if ("Control" %in% grupos) "Control" else grupos[1])  
    updateSelectInput(session, "group1",  
                      choices  = grupos,  
                      selected = grupos[1])  
    updateSelectInput(session, "group2",  
                      choices  = grupos,  
                      selected = if (length(grupos) >= 2) grupos[2] else grupos[1])  
  })  
  
  
  # ── Normalidad ───────────────────────────────────────────────────────────  
  norm_res <- eventReactive(input$runNorm, {  
    df <- make_norm_df()  
    # Deben existir al menos 2 grupos distintos  
    if (nrow(df)==0 || dplyr::n_distinct(df$Label)<2) {  
      showNotification("Se necesitan ≥2 grupos con datos para normalidad", type="error", duration=4)  
      return(tibble::tibble(Label=character(), shapiro.stat=numeric(), shapiro.p=numeric(),  
                            ks.stat=numeric(), ks.p=numeric(),  
                            ad.stat=numeric(), ad.p=numeric()))  
    }  
    # Shapiro–Wilk  
    sw <- df %>% group_by(Label) %>%  
      summarise(  
        shapiro.stat = stats::shapiro.test(Valor)$statistic,  
        shapiro.p    = stats::shapiro.test(Valor)$p.value,  
        .groups="drop"  
      )  
    res <- sw  
    # Kolmogorov–Smirnov (opcional)  
    if ("ks" %in% input$normTests) {  
      ksdf <- df %>% group_by(Label) %>%  
        summarise(  
          ks.stat = stats::ks.test(Valor, "pnorm",  
                                   mean(Valor), sd(Valor))$statistic,  
          ks.p    = stats::ks.test(Valor, "pnorm",  
                                   mean(Valor), sd(Valor))$p.value,  
          .groups="drop"  
        )  
      res <- left_join(res, ksdf, by="Label")  
    } else {  
      res <- mutate(res, ks.stat=NA_real_, ks.p=NA_real_)  
    }  
    # Anderson–Darling (opcional, con nortest) — sólo si n ≥ 8  
    if ("ad" %in% input$normTests) {  
      addf <- df %>%  
        dplyr::group_by(Label) %>%  
        dplyr::summarise(  
          ad.stat = if (dplyr::n() >= 8) nortest::ad.test(Valor)$statistic else NA_real_,  
          ad.p    = if (dplyr::n() >= 8) nortest::ad.test(Valor)$p.value     else NA_real_,  
          .groups = "drop"  
        )  
      res <- dplyr::left_join(res, addf, by = "Label")  
    } else {  
      res <- dplyr::mutate(res, ad.stat = NA_real_, ad.p = NA_real_)  
    }  
    res  
  })  
  
  
  # ── Significancia ─────────────────────────────────────────────────────────  
  sig_res <- eventReactive(input$runSig, {  
    df <- make_test_df()  
    
    
    if (n_distinct(df$Label) < 2) return(tibble())  
    
    tryCatch({  
      
      do_anova <- function() {  
        aovm <- aov(Valor ~ Label, data = df)  
        switch(input$postHoc,  
               "Tukey"      = broom::tidy(TukeyHSD(aovm)) |> rename(p.adj = adj.p.value),  
               "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label,  
                                                       p.adjust.method = "bonferroni"),  
               "Sidak"      = safe_pairwise_t(df, "sidak"),  
               "Dunnett"    = dunnett_to_tibble(  
                 DescTools::DunnettTest(Valor ~ Label,  
                                        data = set_control(df, input$controlGroup))),  
               "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),  
               "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)  
        )  
      }  
      
      do_kw <- function() {  
        switch(input$postHoc,  
               "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),  
               "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),  
               "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),  
               "DSCF"    = {  
                 f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))  
                   PMCMRplus::kwAllPairsDSCFTest  
                 else  
                   PMCMRplus::kwAllPairsDscfTest  
                 pmcmr_to_tibble(f(df$Valor, df$Label))  
               }  
        )  
      }  
      
      switch(input$sigTest,  
             "ANOVA"          = do_anova(),  
             "Kruskal–Wallis" = do_kw(),  
             "ttest" = {  
               if (input$compMode == "all") {  
                 rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = "holm")  
               } else if (input$compMode == "control") {  
                 rstatix::t_test(df, Valor ~ Label, ref.group = input$controlGroup)  
               } else {  
                 sub <- df %>% filter(Label %in% c(input$group1, input$group2))  
                 rstatix::t_test(sub, Valor ~ Label, paired = TRUE)  
               }  
             },  
             "wilcox" = {  
               if (input$compMode == "all") {  
                 rstatix::pairwise_wilcox_test(df, Valor ~ Label, p.adjust.method = "holm")  
               } else if (input$compMode == "control") {  
                 rstatix::wilcox_test(df, Valor ~ Label, ref.group = input$controlGroup)  
               } else {  
                 sub <- df %>% filter(Label %in% c(input$group1, input$group2))  
                 rstatix::wilcox_test(sub, Valor ~ Label, paired = TRUE)  
               }  
             }  
      )  
      
    }, error = function(e) {  
      showNotification(paste("Error en test:", e$message),  
                       type = "error", duration = 5)  
      tibble()  
    })  
  })  
  
  
  # ─── Hacer que al pulsar abra el panel ────────────────────────────────  
  observeEvent(input$runNorm, {  
    updateCollapse(session, "statsPanel", open = "Analisis Estadísticos")  
  })  
  observeEvent(input$runSig, {  
    updateCollapse(session, "statsPanel", open = "Analisis Estadísticos")  
  })  
  
  # ─── Renderizar tabla de normalidad ─────────────────────────────────  
  output$normTable <- renderDT({  
    req(input$runNorm)            # solo después de pulsar  
    df <- norm_res()              # eventReactive definido arriba  
    
    # ── Indicadores “Normal / No” para cada test ──────────────────────  
    df2 <- df %>%  
      mutate(  
        Shapiro = if_else(!is.na(shapiro.p) & shapiro.p  > 0.05, "Sí", "No"),  
        KS      = if_else(!is.na(ks.p)      & ks.p       > 0.05, "Sí", "No"),  
        AD      = if_else(!is.na(ad.p)      & ad.p       > 0.05, "Sí", "No")  
      )  
    
    # Opcional: ordena columnas a gusto  
    df2 <- df2 %>%   
      dplyr::select(Label,  
                    shapiro.stat, shapiro.p, Shapiro,  
                    ks.stat,      ks.p,      KS,  
                    ad.stat,      ad.p,      AD)  
    validate(need(nrow(df2) > 0, "No hay datos para normalidad."))  
    datatable(df2, options = list(pageLength = 10, scrollX = TRUE))  
  }, server = FALSE)  
  
  
  # ─── Renderizar tabla de significancia ──────────────────────────────  
  output$sigTable <- renderDT({  
    req(input$runSig)  
    df <- sig_res()  
    # ── Unificar nombres de columna de comparación ────────────  
    # 1) broom::tidy(TukeyHSD) sale con 'comparison'  
    if ("comparison" %in% names(df)) {  
      cmp <- split_comparison(df$comparison)  
      df$group1 <- cmp[, 1]  
      df$group2 <- cmp[, 2]  
    }  
    # 2) PMCMRplus (pmcmr_to_tibble) devuelve 'grupo1','grupo2'  
    if (all(c("grupo1","grupo2") %in% names(df))) {  
      df$group1 <- df$grupo1  
      df$group2 <- df$grupo2  
    }  
    
    
    # --- detectar la columna de p-value de forma robusta -----------------  
    p_candidates <- intersect(  
      c("p", "p.value", "p.adj", "adj.p.value", "p_val", "p.value.adj"),  
      names(df)  
    )  
    if (length(p_candidates) == 0) {  
      warning("Columnas en df: ", paste(names(df), collapse = ", "))  
      stop("No se encontró ninguna columna de p-value válida")  
    }  
    pcol <- p_candidates[1]   # primera coincidencia  
    
    
    df2 <- df %>%  
      mutate(  
        P_valor       = .data[[pcol]],  
        Significativo = if_else(P_valor < 0.05, "Sí", "No"),  
        Estrellas     = case_when(  
          P_valor < 0.001 ~ "***",  
          P_valor < 0.01  ~ "**",  
          P_valor < 0.05  ~ "*",  
          TRUE            ~ ""  
        )  
      )  
    
    validate(need(nrow(df2)>0, "No hay comparaciones válidas."))  
    datatable(df2, options = list(pageLength=10, scrollX=TRUE))  
  }, server = FALSE)  
  
  # ── Tabla combinada de normalidad + significancia ─────────────────────────  
  output$statsTable <- renderDT({  
    nm <- norm_res()  
    sg <- sig_res()  
    # Asegura que sg siempre tenga una columna 'Label'
    if (!"Label" %in% names(sg)) {
      sg <- tibble::add_column(sg, Label = NA_character_, .before = 1)
    }
    
    if (nrow(sg)>0 && "p" %in% names(sg)) {  
      sg <- rename(sg, sig.p = p)  
    }  
    # Junta por Label  
    tab <- left_join(nm, sg, by="Label")  
    datatable(tab, options=list(pageLength=10, scrollX=TRUE))  
  })  
  # ──────────────────────────────────────────────────────────────────────────────  
  
  # ── Helper para elegir paleta según input$colorMode ────────────────  
  get_palette <- function(n) {  
    switch(input$colorMode,  
           "Default"          = safe_hue(n),  
           "Blanco y Negro"   = rep("black", n),  
           "Viridis"          = viridis::viridis(n),  
           "Plasma"           = viridis::plasma(n),  
           "Magma"            = viridis::magma(n),  
           "Cividis"          = viridis::cividis(n),  
           "Set1"             = RColorBrewer::brewer.pal(n = n, name = "Set1"),  
           "Set2"             = RColorBrewer::brewer.pal(n = n, name = "Set2"),  
           "Set3"             = RColorBrewer::brewer.pal(n = n, name = "Set3"),  
           "Dark2"            = RColorBrewer::brewer.pal(n = n, name = "Dark2"),  
           "Accent"           = RColorBrewer::brewer.pal(n = n, name = "Accent"),  
           "Paired"           = RColorBrewer::brewer.pal(n = n, name = "Paired"),  
           "Pastel1"          = RColorBrewer::brewer.pal(n = n, name = "Pastel1"),  
           "Pastel2"          = RColorBrewer::brewer.pal(n = n, name = "Pastel2"),  
           safe_hue(n)        # fallback  
    )  
  }  
  # ───────────────────────────────────────────────────────────────────  
  
  ###############################################################################  
  # PEGAR AQUÍ  ➜  antes de build_plot() y después de las demás helpers  
  ###############################################################################  
  # Barra de significancia estilo “T” (base‑ggplot2)  
  ###############################################################################
  # 1-A  Dibuja UNA barra de significancia tipo “T”
  add_sigline <- function(p, group1, group2, label = "*",
                          height   = .05,  # separación barra–datos  (proporción del rango Y)
                          vsize    = .02,  # largo de los “postes”
                          tpad     = .01,  # distancia texto-barra
                          linewidth = .8,
                          textsize  = 5){
    build  <- ggplot_build(p)
    
    ## coordenadas X ------------------------------------------------------------
    xbreaks <- build$layout$panel_params[[1]]$x$breaks
    get_x   <- function(g) if (is.numeric(g)) g else match(g, xbreaks)
    x1 <- get_x(group1);  x2 <- get_x(group2)
    
    ## coordenadas Y ------------------------------------------------------------
    dat   <- build$data[[1]]
    ytop  <- if ("ymax" %in% names(dat)) max(dat$ymax, na.rm = TRUE)
    else                         max(dat$y   , na.rm = TRUE)
    yrng  <- diff(range(build$layout$panel_params[[1]]$y.range))
    ybar  <- ytop + height * yrng
    ycap  <- ybar - vsize * yrng
    ytxt  <- ybar + tpad  * yrng          # texto un poco más arriba
    
    ## dibujo -------------------------------------------------------------------
    p +
      annotate("segment", x=x1,xend=x2, y=ybar,yend=ybar, linewidth=linewidth) +
      annotate("segment", x=x1,xend=x1, y=ybar,yend=ycap, linewidth=linewidth) +
      annotate("segment", x=x2,xend=x2, y=ybar,yend=ycap, linewidth=linewidth) +
      annotate("text",    x=mean(c(x1,x2)), y=ytxt,
               label=label, size=textsize, vjust=0) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
  }
  
  ###############################################################################
  # 1-B  Coloca MUCHAS barras sin que se choquen entre sí
  stack_siglines <- function(p, sigs,
                             sep       = .05,  # distancia entre niveles
                             linewidth = .8,
                             vsize     = .02,
                             tpad      = .01,
                             tsize     = 5){
    
    if (length(sigs) == 0 || length(ggplot_build(p)$data) == 0)
      return(p)
    
    build  <- ggplot_build(p)
    yrng   <- build$layout$panel_params[[1]]$y.range
    xranks <- build$layout$panel_params[[1]]$x$breaks   # posiciones 1,2,3,…
    
    get_span <- function(cmp){
      x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, xranks)
      x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, xranks)
      c(min(x1,x2), max(x1,x2))
    }
    
    levels <- list()                # ocupación por nivel
    bar_level <- integer(length(sigs))
    
    for (i in seq_along(sigs)){
      span <- get_span(sigs[[i]])
      placed <- FALSE
      for (lvl in seq_along(levels)){
        overlap <- vapply(levels[[lvl]], function(iv)
          !(span[2] < iv[1] || span[1] > iv[2]), logical(1))
        if (!any(overlap)){                      # cabe en este nivel
          levels[[lvl]] <- append(levels[[lvl]], list(span))
          bar_level[i]  <- lvl
          placed <- TRUE; break
        }
      }
      if (!placed){                             # crea nivel nuevo
        levels[[length(levels)+1]] <- list(span)
        bar_level[i] <- length(levels)
      }
    }
    
    extra <- (length(levels)+1) * sep * diff(yrng)
    p <- p + expand_limits(y = max(yrng)+extra)
    
    # Dibuja cada barra en su nivel correspondiente -----------------------------
    for (i in seq_along(sigs)){
      h <- bar_level[i] * sep
      cmp <- sigs[[i]]
      p <- add_sigline(p,
                       group1   = cmp$g1,
                       group2   = cmp$g2,
                       label    = cmp$lab,
                       height   = h,
                       vsize    = vsize,
                       tpad     = tpad,
                       linewidth= linewidth,
                       textsize = tsize)
    }
    p
  }
  
  
  # helpers_plotly.R  (o donde la tengas)
  export_plotly_png <- function(p, file,
                                width, height,
                                delay = 0.5,   # deja que Plotly acabe de renderizar
                                zoom  = 3) {    # 3 × ⇒ 300 dpi aprox. si usas 100 px = 1 in
    # Fondo transparente
    p <- p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
    
    tmp_html <- tempfile(fileext = ".html")
    on.exit(unlink(tmp_html), add = TRUE)
    
    htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
    
    webshot2::webshot(
      url      = tmp_html,
      file     = file,
      vwidth   = width,
      vheight  = height,
      delay    = delay,
      zoom     = zoom          # ↑ resolución final  = zoom × vwidth
    )
  }
  
  
  
  ###############################################################################  
  # build_plotly_stack() – 100 % reactiva a todos los controles del panel  
  ###############################################################################  
  build_plotly_stack <- function(scope, strain = NULL) {  
    
    ## ─── Helper seguro: convierte “4,6” → 4.6  ──────────────────────────────  
    num <- function(x) as.numeric(gsub(",", ".", x))  
    
    ## 1 · Validaciones y datos -------------------------------------------------  
    params_apilar <- input$stackParams  
    validate(need(length(params_apilar) > 0,  
                  "Selecciona ≥1 parámetro en “Parámetros incluidos”"))  
    
    base_df <- if (isTRUE(input$doNorm))  
      datos_agrupados_norm() else datos_agrupados()  
    
    df_f <- if (scope == "Por Cepa") {
      base_df |>
        filter(Strain == strain) |>
        order_filter_strain()    |>
        filter_reps_strain()
    } else {
      base_df |> order_filter_group()
    }
    
    # 1) Definir primero cuál es la variable de eje X
    eje_x <- if (scope == "Por Cepa") {
      "Media"
    } else if (isTRUE(input$labelMode)) {
      "Strain"
    } else {
      "Label"
    }
    
    # 2) Sólo droplevels() si es factor, luego forzamos character
    if (is.factor(df_f[[eje_x]])) {
      df_f[[eje_x]] <- droplevels(df_f[[eje_x]])
    }
    df_f[[eje_x]] <- as.character(df_f[[eje_x]])
    
    
    
    ## 2 · Media ± SD por tramo -------------------------------------------------  
    # -----------------------------
    # Tomo el orden pedido por el usuario,
    # pero me quedo sólo con los parámetros que están seleccionados
    order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
    order_levels <- intersect(order_stack_input, params_apilar)
    # -----------------------------
    
    # 1) Filtrado y transformación
    base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
    df_f <- if (scope == "Por Cepa") {
      base_df %>% filter(Strain == strain) %>% order_filter_strain() %>% filter_reps_strain()
    } else {
      base_df %>% order_filter_group()
    }
    
    # elegimos eje_x según scope y labelMode
    eje_x <- if (scope == "Por Cepa") {
      "Media"
    } else if (isTRUE(input$labelMode)) {
      "Strain"
    } else {
      "Label"
    }
    
    df_long <- df_f |>  
      pivot_longer(all_of(params_apilar),  
                   names_to  = "Parametro",  
                   values_to = "Valor") |>  
      group_by(.data[[eje_x]], Parametro) |>  
      summarise(  
        Mean = mean(Valor, na.rm = TRUE),  
        SD   = sd  (Valor, na.rm = TRUE),  
        .groups = "drop"  
      ) |>  
      mutate(Parametro = factor(Parametro, levels = order_levels)) |>  
      arrange(.data[[eje_x]], Parametro)  
    
    ## 3 · Paleta --------------------------------------------------------------  
    pal <- get_palette(length(params_apilar))  
    names(pal) <- params_apilar  
    
    ## 4 · Trazas de barras -----------------------------------------------------  
    plt <- plot_ly(
      width  = input$plot_w,
      height = input$plot_h
    )                      # ← sin width / height aquí
    for (p in order_levels) {  
      sub <- df_long[df_long$Parametro == p, ]  
      plt <- add_trace(  
        plt,  
        x        = sub[[eje_x]],  
        y        = sub$Mean,  
        type     = "bar",  
        name     = p,  
        marker   = list(color = pal[[p]],  
                        line  = list(color = "black", width = 1))  
      )  
    }  
    
    ## 5 · Trazas “fantasma” con barras de error --------------------------------  
    if (isTRUE(input$showErrBars)) {
      
      err_df <- df_long %>%                       # tope de cada tramo
        group_by(.data[[eje_x]]) %>%
        arrange(Parametro, .by_group = TRUE) %>%
        mutate(y_top = cumsum(Mean)) %>%
        ungroup()
      
      thick <- num(input$errbar_size) * 1.6       # grosor cabeza
      
      for (p in order_levels) {                   # una traza fantasma por tramo
        sub <- err_df[err_df$Parametro == p, ]
        plt <- add_trace(
          plt,
          x          = sub[[eje_x]],
          y          = sub$y_top,
          type       = "scatter",
          mode       = "markers",
          marker     = list(size = 1, opacity = 0),
          showlegend = FALSE,
          hoverinfo  = "skip",
          error_y = list(
            type       = "data",
            symmetric  = FALSE,
            array      = sub$SD,                  # +SD hacia arriba
            arrayminus = rep(0, nrow(sub)),       # nada hacia abajo
            color      = "black",
            thickness  = thick,
            width      = 20                       # longitud de la “cabeza”
          )
        )
      }
    }
    
    ## 6 · Layout (ejes y cuadrícula) ------------------------------------------  
    plt <- plt %>%
      layout(
        barmode = "stack",
        margin = list(
          t = input$fs_title * 2 + 20    # reserva espacio arriba en píxeles
        ),
        title = list(
          text = input$plotTitle,
          font = list(size = input$fs_title),
          y    = 0.95                     # opcional: también puedes mover el título un poco hacia abajo
        ),
        yaxis = list(
          title     = if (nzchar(input$yLab)) input$yLab else "",
          titlefont = list(size = input$fs_axis),
          tickfont  = list(size = input$fs_axis),
          range     = c(0, input$ymax),
          dtick     = input$ybreak,
          showline  = TRUE,
          linecolor = "black",
          linewidth = input$axis_line_size,
          ticks     = "outside",
          ticklen   = 5,
          tickcolor = "black",
          showgrid  = FALSE
        ),
        xaxis = list(
          title         = "",
          type          = "category",
          categoryorder = "array",
          categoryarray = unique(df_long[[eje_x]]),
          titlefont     = list(size = input$fs_axis),
          tickfont      = list(size = input$fs_axis),
          showline      = TRUE,
          linecolor     = "black",
          linewidth     = input$axis_line_size,
          ticks         = "outside",
          ticklen       = 5,
          tickcolor     = "black",
          showgrid      = FALSE
        ),
        legend = list(
          title      = list(text = ""),
          traceorder = "normal",
          font       = list(size = input$fs_legend)
        )
      )
    plt  
  }  
  ###############################################################################  
  
  
  
  
  # --------------------------------------------------------------------  
  # Función auxiliar para exportar PNG sin tocar <input> (para ZIP)  
  #         ► MISMO LOOK que plot_base()  
  # --------------------------------------------------------------------  
  # ── build_plot() ──  
  build_plot <- function(scope, strain = NULL, tipo) {  
    req(plot_settings(), input$param)  
    
    # ───── Nuevo bloque para normalización ─────  
    rawParam <- input$param  
    is_norm  <- isTRUE(input$doNorm)  
    param_sel <- if (is_norm) paste0(rawParam, "_Norm") else rawParam  
    
    # toma siempre la config del parámetro sin “_Norm”  
    # → calcula el título por defecto  
    ps       <- plot_settings() %>% filter(Parameter == rawParam)  
    defaultY <- if (is_norm)  
      paste0(ps$Y_Title, " (normalizado)")  
    else  
      ps$Y_Title  
    
    # → si el usuario puso algo en input$yLab, úsalo; sino el default  
    ylab     <- if (nzchar(input$yLab)) input$yLab else defaultY  
    
    
    # límites según param_sel  
    lims    <- get_ylim(param_sel)  
    ymax    <- lims$ymax  
    ybreak  <- lims$ybreak  
    # ────────────────────────────────────────────  
    
    ## ---- parche: si ymax o ybreak no son finitos, asigna valores seguros  
    if (!is.finite(ymax)  || ymax  <= 0) ymax  <- 1  
    if (!is.finite(ybreak) || ybreak <= 0) ybreak <- ymax / 5  
    
    # ——— 2) Estilos comunes ———  
    colourMode <- input$colorMode  
    fs_title   <- input$fs_title  
    fs_axis    <- input$fs_axis  
    fs_legend  <- input$fs_legend      # ← nuevo  
    axis_size  <- input$axis_line_size  
    
    
    # 0) Si es Curvas, lo procesamos aquí y devolvemos  
    
    # ─── 3.x) NUEVO: Barras apiladas ───────────────────────────────  
    if (tipo == "Apiladas") {  
      params_apilar <- input$stackParams  
      if (length(params_apilar) == 0) {  
        return(  
          ggplot() + theme_void() +  
            annotate("text", 0, 0, label = "Selecciona ≥1 parámetro")  
        )  
      }  
      
      # 1) Filtrado y transformación  
      df_src <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()  
      df_f   <- if (scope == "Por Cepa") {  
        df_src %>% filter(Strain == strain) %>% order_filter_strain() %>% filter_reps_strain()  
      } else {  
        df_src %>% order_filter_group()  
      }  
      
      # 2) Revisa que existan las columnas  
      missing <- setdiff(params_apilar, names(df_f))  
      if (length(missing)) {  
        return(  
          ggplot() + theme_void() +  
            annotate("text", 0, 0, label =  
                       paste0("No se encontró el/los parámetro(s):\n",  
                              paste(missing, collapse = ", "))  
            )  
        )  
      }  
      
      # 3) Prepara df_long con medias y SD  
      # -----------------------------
      # Tomo el orden pedido por el usuario,
      # pero me quedo sólo con los parámetros que están seleccionados
      order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
      order_levels <- intersect(order_stack_input, params_apilar)
      # -----------------------------
      
      # 1) Elige la variable de eje X según el scope  
      eje_x <- if (scope == "Por Cepa") {
        "Media"
      } else if (isTRUE(input$labelMode)) {
        # si piden sólo cepa, usamos directamente la columna Strain
        "Strain"
      } else {
        "Label"
      }
      
      
      # 2) Ahora construyes df_long  
      df_long <- df_f %>%  
        pivot_longer(all_of(params_apilar),  
                     names_to  = "Parametro",  
                     values_to = "Valor") %>%  
        group_by(.data[[eje_x]], Parametro) %>%  
        summarise(  
          Mean = mean(Valor, na.rm = TRUE),  
          SD   = sd(Valor, na.rm = TRUE),  
          .groups = "drop"  
        ) %>%  
        mutate(Parametro = factor(Parametro, levels = order_levels)) %>%  
        arrange(.data[[eje_x]], Parametro)  
      
      # -----------------------------  
      
      # 4) Gráfico base  
      pal <- get_palette(length(params_apilar))  
      names(pal) <- params_apilar  
      p <- ggplot(df_long,
                  aes(x = .data[[eje_x]], y = Mean, fill = Parametro)) +
        geom_col(position = "stack", width = 0.7, colour = "black", linewidth = 0.3) +
        scale_fill_manual(
          values  = alpha(pal[order_levels], 0.6),   # respeta el orden del usuario
          breaks  = order_levels,                    # mismo orden en la leyenda
          guide   = guide_legend(title = NULL, reverse = FALSE)
        )
      
      
      # 5 ·  BARRAS DE DESVIACIÓN – versión auto-contenida -------------------
      if (isTRUE(input$showErrBars)) {
        
        ## ancho de la barra → 0.7  → mitad = 0.35
        cap_half_width <- 0.7 / 2   # ← NUEVO
        
        err_df <- df_long %>%                        # … (código idéntico)
          mutate(
            xnum   = as.numeric(factor(.data[[eje_x]], levels = unique(.data[[eje_x]]))),
            Parametro = factor(Parametro, levels = order_levels)
          ) %>%
          arrange(xnum, Parametro) %>%
          group_by(xnum) %>%
          mutate(
            ybottom = cumsum(Mean) - Mean,
            ytop    = ybottom + Mean,
            ystart  = ytop,
            yend    = ytop + SD
          ) %>%
          ungroup()
        
        p <- p +
          geom_segment(
            data        = err_df,
            inherit.aes = FALSE,
            aes(x = xnum, xend = xnum, y = ystart, yend = yend),
            size        = input$errbar_size,
            colour      = "black",
            show.legend = FALSE
          ) +
          geom_segment(
            data        = err_df,
            inherit.aes = FALSE,
            aes(
              x    = xnum - cap_half_width,
              xend = xnum + cap_half_width,
              y    = yend,
              yend = yend
            ),
            size        = input$errbar_size,
            colour      = "black",
            show.legend = FALSE
          )
      }
      
      
      # 6) Etiquetas, límites y tema final  
      p <- p +  
        labs(title = input$plotTitle, x = NULL, y = if (nzchar(input$yLab)) input$yLab else ps$Y_Title) +  
        scale_y_continuous(limits = c(0, input$ymax), breaks = seq(0, input$ymax, by = input$ybreak), expand = c(0,0)) +
        theme_minimal(base_size = input$base_size) +
        theme(  
          plot.title      = element_text(size = fs_title, face = "bold"),  
          axis.title.y    = element_text(size = fs_axis, face = "bold"),  
          axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),  
          axis.text.y     = element_text(size = fs_axis),  
          axis.line       = element_line(linewidth = axis_size),  
          axis.ticks      = element_line(linewidth = axis_size),  
          panel.grid      = element_blank(),  
          legend.text     = element_text(size = fs_legend),  
          legend.key.size = unit(1.4, "lines")  
        )  
      
      return(p)  
    }  
    
    # --- 3.x) Correlación -------------------------------------------------
    if (tipo == "Correlación") {
      
      ## 1 · nombres reales de las columnas (con o sin _Norm)
      raw_x   <- input$corr_param_x
      raw_y   <- input$corr_param_y
      param_x <- if (isTRUE(input$doNorm)) paste0(raw_x, "_Norm") else raw_x
      param_y <- if (isTRUE(input$doNorm)) paste0(raw_y, "_Norm") else raw_y
      
      ## 2 · tabla fuente
      base_df <- if (isTRUE(input$doNorm))
        datos_agrupados_norm() else datos_agrupados()
      
      ## 3 · filtrado + promedio por grupo ---------------------------------
      df_raw <- if (scope == "Por Cepa") {
        base_df %>%
          filter(Strain == strain) %>%
          order_filter_strain() %>%
          filter_reps_strain()
      } else {
        base_df %>% order_filter_group() %>% filter_reps_group()
      }
      
      df <- if (scope == "Por Cepa") {          # ― etiquetas = medio
        df_raw %>%
          group_by(Media) %>%
          summarise(
            X = mean(.data[[param_x]], na.rm = TRUE),
            Y = mean(.data[[param_y]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          rename(Label = Media)
        
      } else {                                  # ― etiquetas combinadas
        df_raw %>%
          group_by(Strain, Media) %>%
          summarise(
            X = mean(.data[[param_x]], na.rm = TRUE),
            Y = mean(.data[[param_y]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            Label = if (isTRUE(input$labelMode))
              Strain
            else
              paste(Strain, Media, sep = "-")
          )
      }
      
      ## 4 · chequeo de puntos
      validate(need(nrow(df) >= 3,
                    "Se necesitan ≥3 puntos para calcular la correlación"))
      
      cor_res <- cor.test(df$X, df$Y, method = input$corr_method)
      # --- Ecuación de la recta -------------------------------------------------
      eq_lab <- NULL
      if (isTRUE(input$corr_show_eq)) {
        fit <- lm(Y ~ X, data = df)
        slope <- coef(fit)[2]
        intercept <- coef(fit)[1]
        r2 <- summary(fit)$r.squared
        # y = ax + b  (3 decimales) y R²
        eq_lab <- sprintf("y = %.3f·x %+.3f\nR² = %.3f", slope, intercept, r2)
      }
      
      
      ## 5 · saneo de ejes y posición del texto r / p
      xmin   <- ifelse(is.finite(input$xmin_corr),  input$xmin_corr, 0)
      xmax   <- ifelse(is.finite(input$xmax_corr),  input$xmax_corr, xmin + 1)
      if (!is.finite(xmax) || xmax <= xmin) xmax <- xmin + 1
      xbreak <- ifelse(is.finite(input$xbreak_corr) && input$xbreak_corr > 0,
                       input$xbreak_corr, (xmax - xmin)/5)
      ymin   <- ifelse(is.finite(input$ymin_corr),  input$ymin_corr, 0)
      ymax   <- ifelse(is.finite(input$ymax_corr),  input$ymax_corr, ymin + 1)
      if (!is.finite(ymax) || ymax <= ymin) ymax <- ymin + 1
      ybreak <- ifelse(is.finite(input$ybreak_corr) && input$ybreak_corr > 0,
                       input$ybreak_corr, (ymax - ymin)/5)
      dx  <- 0.05 * (xmax - xmin)
      dy  <- 0.04 * (ymax - ymin)
      x_t <- xmax - dx
      y_t <- ymax - dy

      ## 6 · gráfico --------------------------------------------------------
      p <- ggplot(df, aes(X, Y)) +
        geom_point(size = 3, colour = "black") +
        { if (isTRUE(input$corr_show_line))
          geom_smooth(method = "lm", se = FALSE,
                      colour = "black", linetype = "dashed") } +
        { if (isTRUE(input$corr_show_labels))
          geom_text(aes(label = Label),
                    nudge_y = 0.05 * (ymax - ymin),
                    size    = input$corr_label_size) } +
        annotate("text",
                 x = x_t, y = y_t, hjust = 1, vjust = 1,
                 label = sprintf("r = %.3f\np = %.3g",
                                 cor_res$estimate, cor_res$p.value),
                 size = 5) +
        { if (!is.null(eq_lab))
          annotate("text",
                   x = x_t, y = y_t - dy*2.3,  # un poco más abajo
                   hjust = 1, vjust = 1,
                   label = eq_lab, size = 5) } +
        labs(
          title = input$plotTitle,
          x     = if (nzchar(input$corr_xlab)) input$corr_xlab else raw_x,
          y     = if (nzchar(input$corr_ylab)) input$corr_ylab else raw_y
        ) +
        scale_x_continuous(
          limits = c(xmin, xmax),
          breaks = seq(xmin, xmax, by = xbreak),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(ymin, ymax),
          breaks = seq(ymin, ymax, by = ybreak),
          expand = c(0, 0)
        ) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = input$base_size) +
        theme(
          plot.title = element_text(size = input$fs_title, face = "bold"),
          axis.title = element_text(size = input$fs_axis,  face = "bold"),
          axis.text  = element_text(size = input$fs_axis),
          axis.line  = element_line(linewidth = input$axis_line_size),
          axis.ticks = element_line(linewidth = input$axis_line_size),
          panel.grid = element_blank(),
          plot.margin = margin(20, 50, 10, 10)
        )
      
      return(p)
    }
    
    
    # --- 3.x) Curvas (Por Cepa y Combinado) ---  
    if (tipo == "Curvas") {
      req(curve_data(), curve_settings())
      
      # 1) Leer y unir curvas + metadatos
      df_cur <- curve_data() %>%
        # 1) Convertimos todo menos Time a numeric de golpe
        mutate(across(-Time, ~ suppressWarnings(as.numeric(.x)))) %>%
        # 2) Al pivotar, todos los valores ya son numeric
        pivot_longer(cols = -Time, names_to = "Well", values_to = "Value") %>%
        left_join(datos_combinados(), by = "Well")
      
      # 2) Filtrar y ordenar según ámbito
      if (scope == "Por Cepa") {
        df_cur <- df_cur %>%
          filter(Strain == strain) %>%
          order_filter_strain() %>%
          filter_reps_strain()
        
        media_order <- df_cur %>%
          distinct(Media, Orden) %>%
          arrange(Orden) %>%
          pull(Media)
        
        df_sum <- df_cur %>%
          group_by(Time, Media) %>%
          summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
          mutate(Label = factor(Media, levels = media_order))
        
      } else {
        df_cur <- df_cur %>%
          order_filter_group() %>%
          filter_reps_group()
        
        platemap_labels <- datos_agrupados() %>%
          distinct(Strain, Media, Orden) %>%
          mutate(Label = paste(Strain, Media, sep = "-")) %>%
          arrange(Orden) %>%
          pull(Label)
        
        user_order <- if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
          trimws(strsplit(input$orderGroups, ",")[[1]])
        } else {
          NULL
        }
        final_order <- if (!is.null(user_order)) intersect(user_order, platemap_labels) else platemap_labels
        
        if (isTRUE(input$labelMode)) {
          strain_order <- datos_agrupados() %>%
            group_by(Strain) %>%
            summarise(minO = min(Orden), .groups = "drop") %>%
            arrange(minO) %>%
            pull(Strain)
          
          df_sum <- df_cur %>%
            group_by(Time, Strain) %>%
            summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            mutate(Label = factor(Strain, levels = strain_order))
          
        } else {
          df_sum <- df_cur %>%
            group_by(Time, Strain, Media) %>%
            summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_order))
        }
      }
      
      # 3) Extraer etiquetas de ejes desde la configuración (ahora editables)
      cfg_cur <- curve_settings()[1, ]
      x_lab <- if (nzchar(input$cur_xlab)) input$cur_xlab else cfg_cur$X_Title
      y_lab <- if (nzchar(input$cur_ylab)) input$cur_ylab else cfg_cur$Y_Title
      
      # 4) Crear gráfico con ggplot2
      plt <- ggplot(df_sum, aes(x = Time, y = Avg, colour = Label, group = Label)) +
        geom_line(size = 1.5) +
        geom_point(size = 1.5) +
        scale_colour_manual(
          values = get_palette(nlevels(df_sum$Label)),
          breaks = levels(df_sum$Label)
        ) +
        labs(
          title  = input$plotTitle,
          x      = x_lab,
          y      = y_lab,
          colour = NULL
        ) +
        scale_x_continuous(
          limits = c(0, input$xmax_cur),
          breaks = seq(0, input$xmax_cur, by = input$xbreak_cur),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, input$ymax_cur),
          breaks = seq(0, input$ymax_cur, by = input$ybreak_cur),
          expand = c(0, 0)
        ) +
        theme_minimal(base_size = input$base_size) +
        theme(
          plot.title      = element_text(size = input$fs_title, face = "bold"),
          axis.title      = element_text(size = input$fs_axis, face = "bold"),
          axis.text       = element_text(size = input$fs_axis),
          axis.line       = element_line(linewidth = input$axis_line_size),
          axis.ticks      = element_line(linewidth = input$axis_line_size),
          panel.grid      = element_blank(),
          legend.text     = element_text(size = input$fs_legend),
          legend.key.size = unit(1.5, "lines")
        )
      
      return(plt)
    }
    
    
    
    
    # =======================================================================================
    
    
    
    # ——— 3) Lógica principal ———  
    if (scope == "Combinado") {  
      
      # --- 3.1) Boxplot combinado ---  
      if (scope == "Combinado" && tipo == "Boxplot") {  
        df_plot <- datos_agrupados_norm() %>%   # ← ahora sí contiene las columnas *_Norm  
          order_filter_group() %>%  
          transmute(Label, Valor = .data[[param_sel]])  
        cols <- nlevels(df_plot$Label)  
        pal  <- get_palette(cols)  
        
        p <- ggplot(df_plot, aes(Label, Valor))  
        
        if (input$colorMode == "Blanco y Negro") {  
          p <- p +  
            stat_boxplot(geom = "errorbar", width = .2, linewidth = .6, colour = "black") +  
            geom_boxplot(fill = "white", colour = "black", width = input$box_w, linewidth = .6) +  
            geom_jitter(colour = "black", width = input$pt_jit, size = input$pt_size)  
        } else {  
          p <- p +  
            stat_boxplot(geom = "errorbar", width = .2, linewidth = .6) +  
            geom_boxplot(aes(fill = Label), width = input$box_w, colour = "black", linewidth = .6) +  
            geom_jitter(aes(colour = Label), width = input$pt_jit, size = input$pt_size) +  
            scale_fill_manual(values = alpha(pal, .5)) +  
            scale_colour_manual(values = pal)  
        }  
        
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = ybreak), expand = c(0,0)) +  
          theme_minimal(base_size = input$base_size) +
          theme(  
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis, face = "bold"),  
            axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),  
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        
        if (isTRUE(input$labelMode)) {  
          p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))  
        }  
        p <- stack_siglines(p, sig_list(),
                            sep       = input$sig_sep,        # los controles que ya tienes
                            linewidth = input$sig_linewidth,
                            vsize     = .02,                  # o el tamaño que prefieras
                            tpad      = input$sig_textpad,
                            tsize     = input$sig_textsize)
        
        return(p)  
      }  
      
      # --- 3.2) Barras combinado ---  
      if (scope == "Combinado" && tipo == "Barras") {  
        df_raw <- datos_agrupados_norm() %>%  
          filter_reps_group() %>%  
          order_filter_group()  
        if (nrow(df_raw) == 0) {  
          return(  
            ggplot() +  
              theme_void() +  
              annotate("text", 0, 0, label = "Sin datos con la selección actual")  
          )  
        }  
        
        resumen <- df_raw %>%  
          group_by(Label) %>%  
          summarise(  
            Mean = mean(.data[[param_sel]], na.rm = TRUE),  
            SD   = sd  (.data[[param_sel]], na.rm = TRUE),  
            .groups = "drop"  
          )  
        cols <- max(                         # asegura colores suficientes
          nlevels(resumen$Label),            # …para las barras
          nlevels(df_raw$Media)              # …y para los puntos
        )
        pal  <- get_palette(cols)
        
        p <- ggplot(resumen, aes(Label, Mean))  
        if (input$colorMode == "Blanco y Negro") {  
          p <- p +  
            geom_col(fill = "white", colour = "black", width = .6) +  
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),  
                          width = .2, linewidth = .6, colour = "black") +  
            geom_jitter(
              data   = df_raw,
              aes(x = Label,                     # <- reemplaza Media por Label
                  y = .data[[param_sel]]),
              colour = "black",
              width  = input$pt_jit,
              size   = input$pt_size,
              inherit.aes = FALSE
            )
          
        } else {  
          # ⬇︎ BLOQUE NUEVO listo para pegar -----------------------------
          p <- p +
            geom_col(aes(fill = Label), width = .6) +
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                          width = .2, linewidth = .6) +
            geom_jitter(
              data   = df_raw,
              aes(x = Label,                                   # ← ahora alinea con la barra
                  y = .data[[param_sel]],
                  colour = Media),
              width  = input$pt_jit,
              size   = input$pt_size
            ) +
            scale_fill_manual(values = alpha(pal, .5)) +
            scale_colour_manual(values = pal)
          # --------------------------------------------------------------
          
        }  
        
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = ybreak), expand = c(0,0)) +  
          theme_minimal(base_size = input$base_size) +
          theme(  
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis, face = "bold"),  
            axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),  
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        
        if (isTRUE(input$labelMode)) {  
          p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))  
        }  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep       = input$sig_sep,  
                            linewidth = input$sig_linewidth,  
                            vsize     = .02,  
                            tpad      = input$sig_textpad,  
                            tsize     = input$sig_textsize)   # ← NUEVO  
        
        
        return(p)  
      }  
      
      
    } else {  
      
      # --- 3.3) Boxplot por cepa ---  
      if (tipo == "Boxplot") {  
        df <- datos_agrupados_norm() %>%  
          filter(Strain == strain) %>%  
          order_filter_strain() %>%  
          filter_reps_strain()  
        cols <- nlevels(factor(df$Media))  
        p <- if (colourMode == "Blanco y Negro") {  
          ggplot(df, aes(Media, .data[[param_sel]])) +  
            stat_boxplot(geom = "errorbar", width = .2, linewidth = .6, colour = "black") +  
            geom_boxplot(fill = "white", colour = "black", width = input$box_w, linewidth = .6) +  
            geom_jitter(colour = "black", width = input$pt_jit, size = input$pt_size)  
        } else {  
          pal <- get_palette(cols)  
          ggplot(df, aes(Media, .data[[param_sel]], fill = Media)) +  
            stat_boxplot(geom = "errorbar", width = .2, linewidth = .6) +  
            geom_boxplot(width = input$box_w, colour = "black", linewidth = .6) +  
            scale_fill_manual(values = alpha(pal, .5)) +  
            geom_jitter(aes(colour = Media),  width = input$pt_jit, size = input$pt_size) +  
            scale_colour_manual(values = pal)  
        }  
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(limits = c(0, ymax),  
                             breaks = seq(0, ymax, by = ybreak),  
                             expand = c(0, 0)) +  
          theme_minimal(base_size = input$base_size) +
          theme(  
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis,  face = "bold"),  
            axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),  
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep       = input$sig_sep,  
                            linewidth = input$sig_linewidth,  
                            vsize     = .02,  
                            tpad      = input$sig_textpad,  
                            tsize     = input$sig_textsize)   # ← NUEVO  
        
        
        return(p)  
      }  
      
      
      # --- 3.4) Barras por cepa ---  
      if (tipo == "Barras") {  
        df_raw <- datos_agrupados_norm() %>%  
          filter(Strain == strain) %>%  
          order_filter_strain() %>%  
          filter_reps_strain()  
        if (nrow(df_raw) == 0) {  
          return(ggplot() + theme_void() +  
                   annotate("text", x = 0, y = 0, label = "Sin datos con la selección actual"))  
        }  
        resumen <- df_raw %>%  
          group_by(Media) %>%  
          summarise(  
            Mean = mean(.data[[param_sel]], na.rm = TRUE),  
            SD   = sd  (.data[[param_sel]], na.rm = TRUE),  
            .groups = "drop"  
          )  
        cols <- nlevels(resumen$Media)  
        pal  <- get_palette(cols)  
        p <- if (colourMode == "Blanco y Negro") {
          ggplot(resumen, aes(Media, Mean)) +
            geom_col(fill = "white", colour = "black", width = .6) +
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                          width = .2, linewidth = .6, colour = "black") +
            geom_jitter(data   = df_raw,
                        aes(x = Media,                 # ✔ ahora existe en df_raw
                            y = .data[[param_sel]]),
                        colour = "black",
                        width  = input$pt_jit,
                        size   = input$pt_size)
        
          
        } else {  
          ggplot(resumen, aes(Media, Mean, fill = Media)) +  
            geom_col(width = .6) +  
            scale_fill_manual(values = alpha(pal, .5)) +  
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),  
                          width = .2, linewidth = .6) +  
            geom_jitter(data = df_raw,  
                        aes(x = Media, y = .data[[param_sel]], colour = Media),  
                        width = .07, size = 2.5) +  
            scale_colour_manual(values = pal)  
        }  
        p <- p +  
          labs(title = input$plotTitle, y = ylab, x = NULL) +  
          scale_y_continuous(limits = c(0, ymax),  
                             breaks = seq(0, ymax, by = ybreak),  
                             expand = c(0, 0)) +  
          theme_minimal(base_size = input$base_size) +
          theme(  
            plot.title      = element_text(size = fs_title, face = "bold"),  
            axis.title.y    = element_text(size = fs_axis,  face = "bold"),  
            axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),  
            axis.text.y     = element_text(size = fs_axis),  
            axis.line       = element_line(linewidth = axis_size),  
            axis.ticks      = element_line(linewidth = axis_size),  
            panel.grid      = element_blank(),  
            legend.position = "none"  
          )  
        # ── Añadir barras de significancia seleccionadas ──────────────────────────  
        p <- stack_siglines(p, sig_list(),  
                            sep       = input$sig_sep,  
                            linewidth = input$sig_linewidth,  
                            vsize     = .02,  
                            tpad      = input$sig_textpad,  
                            tsize     = input$sig_textsize)   # ← NUEVO  
        
        return(p)  
      }  
      
      # ─────────────────────────────────────────────────────────────────────────────  
    }  
    
    # ——— fallback para nunca retornar NULL ———  
    return( ggplot() + theme_void() )  
  }  
  
  # ──────────────────────────────────────────────────────────────────────────
  # Helper: versión protegida de ggplotly que descarta capas vacías
  # ──────────────────────────────────────────────────────────────────────────
  safe_ggplotly <- function(p, ...) {
    if (!inherits(p, "ggplot")) return(p)          # ya es plotly
    build  <- ggplot_build(p)
    keep   <- vapply(build$data, nrow, integer(1)) > 0
    if (!all(keep)) p$layers <- p$layers[keep]     # quita sólo las vacías
    ggplotly(p, ...)
  }
  
  # ---- plot_base: la versión “reactive” que usa la interfaz actual ----  
  plot_base <- reactive({  
    scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"  
    strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL  
    
    if (input$tipo == "Apiladas") {  
      # Devuelve un objeto *plotly* listo  
      build_plotly_stack(scope_sel, strain_sel)  
    } else {  
      # Todo lo demás sigue con tu función ggplot2  
      build_plot(scope_sel, strain_sel, input$tipo)  
    }  
  })  
  
  # --- Salidas ---  
  output$plotInteractivo <- renderPlotly({
    req(input$dataFile)
    if (input$tipo == "Curvas") {
      req(cur_data_box(), cur_cfg_box())
    }
    
    p <- plot_base()
    if (inherits(p, "ggplot")) {
      validate(need(length(ggplot_build(p)$data)>0, "Sin datos para graficar"))
      safe_ggplotly(
        p,
        tooltip      = "all",
        width        = input$plot_w,    # ya se pasan aquí
        height       = input$plot_h,
        originalData = FALSE
      ) %>% config(responsive = FALSE)
    } else {
      # Ya p es un plotly puro generado por build_plotly_stack()
      p %>% config(responsive = FALSE)
    }
  })
  
  
  # --- Descarga individual PNG -----------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste0(
        if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
        "_", input$tipo, ".png"
      )
    },
    content = function(file){
      # identifica el ámbito y la cepa
      scope_sel  <- if (input$scope=="Combinado") "Combinado" else "Por Cepa"
      strain_sel <- if (scope_sel=="Por Cepa") input$strain else NULL
      
      if (input$tipo == "Apiladas") {
        plt <- build_plotly_stack(scope_sel, strain_sel)
        export_plotly_png(
          p      = plt,
          file   = file,
          width  = input$plot_w,
          height = input$plot_h
        )
      }
      else {
        # el resto sigue igual
        p <- plot_base()
        if (inherits(p, "ggplot")) {
          ggplot2::ggsave(
            filename = file,
            plot     = p,
            width    = input$plot_w  / 100,
            height   = input$plot_h  / 100,
            dpi      = 300,
            bg       = "transparent"
          )
        } else {
          # plotly normal
          p <- p %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
          export_plotly_png(
            p      = p,
            file   = file,
            width  = input$plot_w,
            height = input$plot_h
          )
        }
      }
    }
  )
  
  observeEvent(input$downloadPlotly_png, {
    req(input$tipo == "Apiladas")
    fname <- paste0(
      if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
      "_", input$tipo
    )
    session$sendCustomMessage("downloadPlotlyImage", list(
      filename = fname,
      width    = input$plot_w,
      height   = input$plot_h
    ))
  })
  
  
  output$downloadExcel <- downloadHandler(  
    filename    = function() "Parametros_por_grupo.xlsx",  
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",  
    content     = function(file) {  
      datos  <- datos_combinados()  
      params <- plot_settings()$Parameter  
      wb_sum <- generate_summary_wb(datos, params)  
      saveWorkbook(wb_sum, file, overwrite = TRUE)  
    }  
  )  
  
  ##############################################################################
  ## DESCARGAR RESULTADOS ESTADÍSTICOS (normalidad + significancia)
  ##############################################################################
  output$downloadStats <- downloadHandler(
    filename = function() "Tests_estadisticos.xlsx",
    content  = function(file){
      
      req(input$dataFile)                # <-- asegúrate de que hay datos cargados
      
      params <- plot_settings()$Parameter
      datos  <- datos_combinados()
      
      ## ===== capturamos la configuración ACTUAL del panel =====================
      scope_sel    <- isolate(input$scope)          %||% "Por Cepa"
      strain_sel   <- isolate(input$strain)
      sigTest_sel  <- isolate(input$sigTest)        %||% "ANOVA"
      postHoc_sel  <- isolate(input$postHoc)        %||% "Tukey"
      compMode_sel <- isolate(input$compMode)       %||% "all"
      controlGroup_sel <- isolate(input$controlGroup) %||% ""
      group1_sel   <- isolate(input$group1)         %||% ""
      group2_sel   <- isolate(input$group2)         %||% ""
      
      wb_tests <- createWorkbook()      # <- libro que vamos a rellenar
      
      ## helper interno (idéntico al usado arriba)
      split_comparison <- function(x) stringr::str_split_fixed(x, "-", 2)
      
      ## ===== RECORREMOS todos los parámetros que el usuario cargó =============
      for (param in params){
        
        sheet <- safe_sheet(param)      # nombre seguro de hoja
        addWorksheet(wb_tests, sheet)
        
        # ---------- reconstruimos el df con los mismos filtros -----------------
        if (scope_sel == "Por Cepa"){
          df_param <- datos_agrupados() |>
            dplyr::filter(Strain == strain_sel) |>
            order_filter_strain()              |>
            filter_reps_strain()               |>
            dplyr::transmute(Label = Media,
                             Valor = .data[[param]])
        } else { # Combinado
          df_param <- datos_agrupados() |>
            order_filter_group() |>
            dplyr::transmute(Label,
                             Valor = .data[[param]])
        }
        
        # ── si no hay datos suficientes se elimina la hoja y se pasa al sig.
        if (nrow(df_param) < 3 || dplyr::n_distinct(df_param$Label) < 2){
          removeWorksheet(wb_tests, sheet)
          next
        }
        
        # =================== NORMALIDAD (Shapiro por grupo) ====================
        norm_tbl <- df_param |>
          dplyr::group_by(Label) |>
          dplyr::summarise(
            Shapiro.stat = stats::shapiro.test(Valor)$statistic,
            Shapiro.p    = stats::shapiro.test(Valor)$p.value,
            Normal       = dplyr::if_else(Shapiro.p > 0.05, "Sí", "No"),
            .groups      = "drop"
          )
        
        writeData(wb_tests, sheet, "Normalidad",
                  startRow = 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, norm_tbl,
                  startRow = 2, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        # ======================= SIGNIFICANCIA GLOBAL ==========================
        ## ► helpers ───────────────────────────────────────────────────────────
        do_anova <- function(df){
          aovm <- aov(Valor ~ Label, data = df)
          switch(postHoc_sel,
                 "Tukey"      = broom::tidy(TukeyHSD(aovm)),
                 "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "bonferroni"),
                 "Sidak"      = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "sidak"),
                 "Dunnett"    = dunnett_to_tibble(
                   DescTools::DunnettTest(Valor ~ Label,
                                          data = set_control(df, controlGroup_sel))),
                 "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),
                 "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)
          )
        }
        do_kw <- function(df){
          switch(postHoc_sel,
                 "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
                 "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),
                 "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),
                 "DSCF"    = {
                   f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))
                     PMCMRplus::kwAllPairsDSCFTest
                   else
                     PMCMRplus::kwAllPairsDscfTest
                   pmcmr_to_tibble(f(df$Valor, df$Label))
                 }
          )
        }
        ## ► cálculo ────────────────────────────────────────────────────────────
        sig_raw <- switch(sigTest_sel,
                          "ANOVA"          = do_anova(df_param),
                          "Kruskal–Wallis" = do_kw(df_param),
                          "ttest" = {
                            if (compMode_sel == "all"){
                              rstatix::pairwise_t_test(df_param, Valor ~ Label, p.adjust.method = "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::t_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              rstatix::t_test(
                                df_param |> dplyr::filter(Label %in% c(group1_sel, group2_sel)),
                                Valor ~ Label, paired = TRUE)
                            }
                          },
                          "wilcox" = {
                            if (compMode_sel == "all"){
                              rstatix::pairwise_wilcox_test(df_param, Valor ~ Label, p.adjust.method = "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::wilcox_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              rstatix::wilcox_test(
                                df_param |> dplyr::filter(Label %in% c(group1_sel, group2_sel)),
                                Valor ~ Label, paired = TRUE)
                            }
                          }
        )
        
        # armoniza columnas de comparación
        if ("comparison" %in% names(sig_raw)){
          cmp <- split_comparison(sig_raw$comparison)
          sig_raw$group1 <- cmp[,1]; sig_raw$group2 <- cmp[,2]
        }
        if (all(c("grupo1","grupo2") %in% names(sig_raw))){
          sig_raw$group1 <- sig_raw$grupo1
          sig_raw$group2 <- sig_raw$grupo2
        }
        
        # identifica la columna de p-value
        p_candidates <- intersect(
          c("p","p.value","p.adj","adj.p.value","p_val","p.value.adj"),
          names(sig_raw)
        )
        pcol <- p_candidates[1]
        
        sig_tbl <- sig_raw |>
          dplyr::mutate(
            P_valor       = .data[[pcol]],
            Significativo = dplyr::if_else(P_valor < 0.05, "Sí", "No"),
            Estrellas     = dplyr::case_when(
              P_valor < 0.001 ~ "***",
              P_valor < 0.01  ~ "**",
              P_valor < 0.05  ~ "*",
              TRUE            ~ ""
            )
          )
        
        # ------------------- ESCRITURA EN LA HOJA ------------------------------
        start <- nrow(norm_tbl) + 4
        
        writeData(wb_tests, sheet,
                  paste("Test usado:", sigTest_sel),
                  startRow = start - 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        
        if (sigTest_sel %in% c("ANOVA","Kruskal–Wallis")){
          writeData(wb_tests, sheet,
                    paste("Post-hoc:", postHoc_sel),
                    startRow = start, startCol = 1,
                    headerStyle = createStyle(textDecoration = "bold"))
          sig_header_row <- start + 1
        } else {
          sig_header_row <- start
        }
        
        writeData(wb_tests, sheet, "Significancia",
                  startRow = sig_header_row, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, sig_tbl,
                  startRow = sig_header_row + 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
      } # fin for(param)
      
      ## ========================================================================
      saveWorkbook(wb_tests, file, overwrite = TRUE)
    }
  )
  
  output$showImportBtn <- renderUI({
    req(input$growthFiles)
    if (length(input$growthFiles$name) == 1)
      actionButton("importToPlots", "Importar a Gráficos & Stats", class = "btn btn-primary")
    else
      NULL
  })
  
  
  # --- Corregir descarga de Metadata ---  
  output$downloadMetadata <- downloadHandler(
    filename = function(){
      paste0("metadata_", input$tipo, ".xlsx")
    },
    content  = function(file){

      wb <- createWorkbook()
      addWorksheet(wb, "Metadata")

      base_vals <- list(
        scope          = input$scope,
        tipo           = input$tipo,
        colorMode      = input$colorMode,
        plot_w         = input$plot_w,
        plot_h         = input$plot_h,
        base_size      = input$base_size,
        fs_title       = input$fs_title,
        fs_axis        = input$fs_axis,
        fs_legend      = input$fs_legend,
        axis_line_size = input$axis_line_size
      )
      meta <- tibble::tibble(
        Campo = names(base_vals),
        Valor = vapply(base_vals, as.character, character(1))
      )

      if (input$tipo %in% c("Boxplot", "Barras", "Apiladas")) {
        meta <- add_row(
          meta,
          Campo = c("pt_size", "x_angle", "x_wrap", "x_wrap_lines"),
          Valor = c(
            as.character(input$pt_size),
            as.character(input$x_angle),
            as.character(input$x_wrap),
            as.character(input$x_wrap_lines)
          )
        )
      }
      if (input$tipo == "Boxplot") {
        meta <- add_row(meta,
                        Campo = c("pt_jit", "box_w"),
                        Valor = c(as.character(input$pt_jit),
                                  as.character(input$box_w)))
      }
      if (input$tipo == "Curvas") {
        meta <- add_row(meta,
                        Campo = "curve_lwd", Valor = as.character(input$curve_lwd))
      }
      if (input$tipo %in% c("Boxplot", "Barras", "Apiladas")) {
        meta <- add_row(
          meta,
          Campo = c("sig_linewidth", "sig_textsize", "sig_sep", "sig_textpad"),
          Valor = c(
            as.character(input$sig_linewidth),
            as.character(input$sig_textsize),
            as.character(input$sig_sep),
            as.character(input$sig_textpad)
          )
        )
      }

      if (input$tipo %in% c("Boxplot", "Barras")){
        meta <- add_row(
          meta,
          Campo = c("param", "doNorm", "ctrlMedium",
                    "errbar_size", "ymax", "ybreak"),
          Valor = c(
            safe_param(),
            as.character(input$doNorm),
            if (is.null(input$ctrlMedium)) "NULL" else input$ctrlMedium,
            as.character(input$errbar_size),
            as.character(get_ylim(safe_param())$ymax),
            as.character(get_ylim(safe_param())$ybreak)
          )
        )
      } else if (input$tipo == "Apiladas"){
        meta <- add_row(
          meta,
          Campo = c("stackParams",
                    "orderStack",
                    "showErrBars",
                    "errbar_size",
                    "ymax", "ybreak"),
          Valor = c(
            paste(input$stackParams, collapse = ","),
            input$orderStack %||% "",
            as.character(input$showErrBars),
            as.character(input$errbar_size),
            as.character(input$ymax),
            as.character(input$ybreak)
          )
        )
      } else if (input$tipo == "Curvas"){
        meta <- add_row(
          meta,
          Campo = c("xmax_cur","xbreak_cur",
                    "ymax_cur","ybreak_cur"),
          Valor = c(
            as.character(input$xmax_cur),
            as.character(input$xbreak_cur),
            as.character(input$ymax_cur),
            as.character(input$ybreak_cur)
          )
        )
        if (!is.null(curve_settings())){
          addWorksheet(wb, "CurvasSettings")
          writeData(wb, "CurvasSettings", curve_settings())
        }
      }

      writeData(wb, "Metadata", meta,
                headerStyle = createStyle(textDecoration = "bold"))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # 5.1 PNG
  output$dl_combo_png <- downloadHandler(
    filename = function() "combo.png",
    content  = function(file){
      ggsave(
        file, combo_plot(),
        width  = input$combo_width  / 100,
        height = input$combo_height / 100,
        dpi    = 300, bg = "white"
      )
    }
  )
  
  # 5.2 PPTX vectorial editable
  output$dl_combo_pptx <- downloadHandler(
    filename = function() "combo.pptx",
    content  = function(file){
      library(officer); library(rvg)
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title and Content",
                       master = "Office Theme") |>
        ph_with(dml(ggobj = combo_plot()),
                location = ph_location_fullsize())
      print(doc, target = file)
    }
  )
  
  observeEvent(input$importToPlots, {
    # 0) Asegurarnos de que ya hay datos cargados en Gráficos & Stats
    if (is.null(datos_box())) {
      showNotification(
        "Primero carga el archivo de metadata-parametros en la pestaña Gráficos & Stats.",
        type = "error", duration = 5
      )
      return()
    }
    # 1) Localiza los dos archivos que generó runGrowth
    curvas_f <- list.files(growth_out_dir, pattern = "^Curvas_.*\\.xlsx$", full.names = TRUE)
    params_f <- list.files(growth_out_dir, pattern = "^Parametros_.*\\.xlsx$", full.names = TRUE)
    
    # Validación
    if (length(curvas_f) != 1 || length(params_f) != 1) {
      showNotification("Necesitas generar primero un archivo de curvas y uno de parámetros.",
                       type = "error")
      return()
    }
    
    # 2) Leer Sheet1 y Sheet2 del Excel de curvas
    curvas     <- read_excel_tmp(curvas_f, sheet = "Sheet1")
    curvas_cfg <- read_excel_tmp(curvas_f, sheet = "Sheet2")
    params      <- readxl::read_excel(params_f, sheet = "Resultados Combinados")
    
    params <- params %>% dplyr::select(where(~ !all(is.na(.x))))
    
    # 3) Guardar ambos reactivos
    cur_data_box(curvas)
    cur_cfg_box(curvas_cfg)
    
    df0 <- datos_box() %>%
      dplyr::select(where(~ !all(is.na(.x))))
    
    # 4) Hacer el join de parámetros sobre los datos originales
    df1 <- df0 %>% left_join(params, by = "Well")
    datos_box(df1)
    # 5) Sincroniza PlotSettings con los parámetros que SÍ existen
    plot_cfg_box(
      plot_cfg_box() %>%
        filter(Parameter %in% names(df1))          # descarta los que ya no existen
    )
    
    new_params <- plot_cfg_box()$Parameter
    if (!is.null(input$param) && !input$param %in% new_params) {
      updateSelectInput(session, "param",
                        choices  = new_params,
                        selected = new_params[1])
    }
    
    # 5) Refrescar todos los inputs que dependen de plot_cfg_box()
    updateCheckboxGroupInput(session, "stackParams",
                             choices  = plot_cfg_box()$Parameter,
                             selected = plot_cfg_box()$Parameter)
    updateSelectInput(session, "param",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[1])
    updateSelectInput(session, "corr_param_x",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[1])
    updateSelectInput(session, "corr_param_y",
                      choices  = plot_cfg_box()$Parameter,
                      selected = plot_cfg_box()$Parameter[min(2, length(plot_cfg_box()$Parameter))])
    
    # 6) Volvemos a la pestaña y disparamos descarga del ZIP
    updateTabsetPanel(session, "mainTabs", selected = "Gráficos & Stats")
    shinyjs::runjs("document.getElementById('downloadGrowthZip').click();")
  })
  
  # 4.1  Muestra todos los gráficos guardados
  output$plotPicker <- renderUI({
    if (length(plot_bank$all) == 0)
      return(helpText("Aún no has añadido gráficos."))
    
    choices <- setNames(names(plot_bank$all),
                        paste("Plot", seq_along(plot_bank$all)))
    checkboxGroupInput("plots_chosen",
                       "Selecciona los gráficos que quieres unir:",
                       choices  = choices,
                       selected = choices)
  })
  
  # 4.2  Combinación reactiva
  combo_plot <- eventReactive(
    list(                     # 👈 NUEVO: vector de inputs que disparan el cálculo
      input$makeCombo,            # botón “Actualiza / Previsualiza”
      ov_trigger()),
    {
    req(input$plots_chosen)
    
    library(patchwork)
    
      theme_ppt <- function(bs = 18, ax = 1.2){
        theme_minimal(base_size = bs) +
          theme(
            axis.line  = element_line(linewidth = ax),
            axis.ticks = element_line(linewidth = ax),
            panel.grid = element_blank()
            # ⬅️  NO pongas axis.text / axis.title / legend.text aquí
          )
      }
    
    
    # 1) Generamos la lista de ggplots listos
      plots <- lapply(plot_bank$all[input$plots_chosen], function(info){
        
        p  <- info$plot
        ov <- info$overrides
        
        # ── 2a) estilo base común ────────────────────────────
        p <- p + theme_ppt(input$base_size_combo)
        
        # ── 2b) overrides del usuario ────────────────────────
        if (!is.null(ov$title))
          p <- p + ggtitle(ov$title)
        
        if (!is.null(ov$fs_title))
          p <- p + theme(plot.title = element_text(size = ov$fs_title, face = "bold"))
        
        if (!is.null(ov$fs_axis))
          p <- p + theme(
            axis.title = element_text(size = ov$fs_axis, face = "bold"),
            axis.text  = element_text(size = ov$fs_axis)
          )
        
        if (!is.null(ov$fs_legend))
          p <- p + theme(legend.text = element_text(size = ov$fs_legend))
        # ---------- NUEVO ------------
        if (isFALSE(input$show_legend_combo)){
          p <- p + theme(legend.position = "none")
        }
        # ------------------------------
        
        p
      })
      
      
      wrap_plots(
        plots,
        nrow = input$nrow_combo,
        ncol = input$ncol_combo
      ) &
        theme(                     # aplica los tamaños globales que pediste
          plot.title  = element_text(size = input$fs_title_all,      face = "bold"),
          axis.title  = element_text(size = input$fs_axis_title_all, face = "bold"),
          axis.text   = element_text(size = input$fs_axis_text_all),
          legend.text = element_text(size = input$fs_legend_all)
        )
  })
  
  
  # 4.3  Previsualización adaptada a los px que el usuario puso
  output$comboPreview <- renderPlot({
    req(combo_plot())
    combo_plot()
  }, width = function() input$combo_width,
  height = function() input$combo_height)
  
}  
# Lanzar app  
shinyApp(ui, server)
