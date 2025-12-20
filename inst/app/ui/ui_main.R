# --- Main UI ---
# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = theme_light,          # por defecto; el server lo cambia a oscuro/claro
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      /* Chrome / Edge / Opera — zoom nativo */
      @media (min-width: 1280px) {
        body { zoom: 0.8; }              /* 80 % */
      }

      /* Firefox no soporta ‘zoom’: usamos transform */
      @media (min-width: 1280px) {
        @-moz-document url-prefix() {
          html {
            transform: scale(0.8);
            transform-origin: top left;
            width: 125%;                 /* 1 / 0.8 ≈ 125 %  → evita barra lateral */
          }
        }
      }
    "))
  ),
  
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
    # Panel fijo de modo/idioma
  absolutePanel(
    top  = 10, right = 10, draggable = FALSE, fixed = TRUE,
    style = 'z-index:1000; display:flex; gap:8px;',
    
    ## ─── Botón “Idioma” con menú desplegable (Bootstrap 5) ───────────────────
    tags$div(
      class = "dropdown",
      actionButton(
        "lang_btn",
        "Idioma",
        class = "btn btn-secondary dropdown-toggle",
        `data-bs-toggle` = "dropdown",
        `aria-expanded`  = "false"
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
      // Actualiza enlace de manual a PDF con fallback a DOCX
      var link = document.getElementById("manual_link");
      if(link){
        var pdf  = (lang === "en" ? "MANUAL_EN.pdf"  : "MANUAL_ES.pdf");
        var docx = (lang === "en" ? "MANUAL_EN.docx" : "MANUAL_ES.docx");
        fetch(pdf, {method: "HEAD"}).then(function(r){
          link.setAttribute("href", (r && r.ok) ? pdf : docx);
        }).catch(function(){
          link.setAttribute("href", docx);
        });
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
  tags$head(
    tags$style(HTML("
      .bundle-action-bar {
        margin-top: 12px;
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        align-items: center;
        justify-content: flex-end;
      }
      .bundle-action-bar .bundle-label-input {
        min-width: 200px;
      }
      .bundle-action-bar .bundle-label-input input {
        height: 40px;
      }
    "))
  ),
  # ------ Handler de descarga Plotly ----------------------------------------
  tags$script(HTML("
    Shiny.addCustomMessageHandler('downloadPlotlyImage', function(msg){
      var gd = document.getElementById('plotInteractivo');
      Plotly.downloadImage(gd, {
        format:  msg.format || 'png',
        filename: msg.filename,
        width:    msg.width,
        height:   msg.height,
        scale:    1
      });
    });
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyPlotToClipboard', function(msg){
      var container = document.getElementById('plotInteractivo');
      var failId    = msg.fail    || 'plot_copy_error';
      var successId = msg.success || 'plot_copy_success';
      if(!container){
        Shiny.setInputValue(failId, {message:'Plot container not found', ts: Date.now()}, {priority:'event'});
        return;
      }
      var plotElem = container.querySelector('.js-plotly-plot');
      if (!plotElem && container.classList.contains('js-plotly-plot')) {
        plotElem = container;
      }
      if (!plotElem) {
        var alt = container.querySelector('.plotly');
        if (alt) plotElem = alt;
      }
      if (!plotElem && container.firstElementChild) {
        var child = container.firstElementChild;
        if (child.classList && (child.classList.contains('js-plotly-plot') || child.classList.contains('plotly'))) {
          plotElem = child;
        }
      }
      if(!plotElem){
        Shiny.setInputValue(failId, {message:'Plot element not found', ts: Date.now()}, {priority:'event'});
        return;
      }
      var width  = msg.width  || plotElem.clientWidth  || 800;
      var height = msg.height || plotElem.clientHeight || 600;
      Plotly.toImage(plotElem, {
        format: 'png',
        width:  width,
        height: height,
        scale:  msg.scale || 3
      }).then(function(dataUrl){
        if (navigator.clipboard && window.ClipboardItem) {
          fetch(dataUrl)
            .then(function(res){ return res.blob(); })
            .then(function(blob){
              navigator.clipboard.write([new ClipboardItem({[blob.type]: blob})])
                .then(function(){
                  Shiny.setInputValue(successId, {message:'copied', ts: Date.now()}, {priority:'event'});
                })
                .catch(function(err){
                  Shiny.setInputValue(failId, {message: err && err.message ? err.message : String(err), ts: Date.now()}, {priority:'event'});
                });
            })
            .catch(function(err){
              Shiny.setInputValue(failId, {message: err && err.message ? err.message : String(err), ts: Date.now()}, {priority:'event'});
            });
        } else {
          var win = window.open();
          if (win) {
            win.document.write('<img src=\"' + dataUrl + '\" />');
            Shiny.setInputValue(successId, {message:'opened', ts: Date.now()}, {priority:'event'});
          } else {
            Shiny.setInputValue(failId, {message:'Clipboard API not available', ts: Date.now()}, {priority:'event'});
          }
        }
      }).catch(function(err){
        Shiny.setInputValue(failId, {message: err && err.message ? err.message : String(err), ts: Date.now()}, {priority:'event'});
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
                        sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
                           
                          fileInput('dataFile',  'Cargar Datos (.xlsx):', accept = '.xlsx'),
                          fileInput('curveFile', 'Cargar Curvas (.xlsx)',              accept = '.xlsx'),
                          fileInput('metaFiles', 'Cargar metadata diseño (.xlsx)',    accept = '.xlsx', multiple = TRUE),
                          tags$p('Los archivos deben llamarse metadata_<Tipo>.xlsx, por ejemplo metadata_Boxplot.xlsx',
                                 style='margin-top: -6px; margin-bottom: 22px; color: #555;'),

                           # ---- Botones de descarga de ejemplos ----------------------------------
                           tags$div(
                             style = 'margin-top:24px; margin-bottom:15px;',
                             tags$a(id = 'manual_link',
                                    class = 'btn btn-secondary',
                                    href = 'MANUAL_ES.pdf',
                                    download = NA,
                                    'Instrucciones (descargar)'),
                             tags$br(), tags$br(),
                             downloadButton('download_refs', 'Archivos de entrada de referencia (descargar)', class = 'btn btn-secondary')
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
                                        choices  = c('Boxplot', 'Barras', 'Violin', 'Curvas', 'Apiladas',
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
                             checkboxInput('showErrBars', 'Mostrar barras de desviación', TRUE)
                          ),

                          ## Grosor barras de desviación (Barras/Boxplot/Apiladas)
                          conditionalPanel(
                            condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                            numericInput('errbar_size', 'Grosor de barras de error:',
                                         value = 0.6, min = 0.1, step = 0.1)
                          ),
                          
                          conditionalPanel(
                            condition = "input.tipo == 'Violin'",
                            numericInput('violin_linewidth', 'Grosor contorno violín:',
                                         value = 0.6, min = 0.1, max = 3, step = 0.05),
                            numericInput('violin_width', 'Ancho violín:',
                                         value = 0.45, min = 0.1, max = 2, step = 0.05)
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
                             condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
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
                            ),
                            numericInput('curve_lwd', 'Grosor de curvas:', value = 1.5, min = 0.5, step = 0.1)
                          ),
                           
                           # Réplicas a mostrar para Curvas
                           conditionalPanel(
                             "input.tipo == 'Curvas'",
                             uiOutput("repSelCurvas")
                           ),
                          # ---------- Paleta de color -------------------------------------------
                          selectInput('colorMode', 'Paleta de color:',
                                      choices = c('Default', 'Default Suave',
                                                  'Blanco y Negro', 'Blanco y Negro Suave',
                                                  'Viridis', 'Viridis Suave',
                                                  'Plasma', 'Plasma Suave',
                                                  'Magma', 'Magma Suave',
                                                  'Cividis', 'Cividis Suave',
                                                  'Set1', 'Set1 Suave',
                                                  'Set2', 'Set2 Suave',
                                                  'Set3', 'Set3 Suave',
                                                  'Dark2', 'Dark2 Suave',
                                                  'Accent', 'Accent Suave',
                                                  'Paired', 'Paired Suave',
                                                  'Pastel1', 'Pastel1 Suave',
                                                  'Pastel2', 'Pastel2 Suave',
                                                  'OkabeIto', 'OkabeIto Suave',
                                                  'Tableau', 'Tableau Suave'),
                                      selected = 'Default'),
                          conditionalPanel(
                            condition = "input.scope=='Combinado'",
                            checkboxInput('repeat_colors_combined',
                                          'Repetir colores por cepa (Combinado)',
                                          FALSE)
                          ),
                           
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
                             condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                             numericInput("pt_size", "Tamaño de puntos:", value = 3,
                                          min = 0.5, max = 20,  step = 0.5),
                             # ─── Ángulo de las etiquetas del eje X ──────────────────────────────────
                             numericInput(
                               "x_angle",                     # <-- NUEVO input
                               "Ángulo etiquetas eje X (°):",
                               value = NA,                    #  NA ⇒ “automático” (no fuerza nada)
                               min   = 0,
                               max   = 90,
                               step  = 5
                             ),
                             checkboxInput(
                               "x_wrap",
                               "Etiquetas X en varias líneas",
                               FALSE
                             ),
                             conditionalPanel(
                               condition = "input.x_wrap == true",
                               numericInput(
                                 "x_wrap_lines",
                                 "Número de filas:",
                                 value = 2,
                                 min = 1,
                                 step = 1
                               )
                             )
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
                          
                          uiOutput('rmRepsGlobalUI'),
                          
                          
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
                             condition = "['Boxplot','Barras','Violin'].indexOf(input.tipo) >= 0",
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
                              column(4, actionButton('add_sig',
                                                     label = tagList(icon('plus'), 'Añadir barra'),
                                                     class = 'btn btn-primary w-100')),
                              column(4, actionButton('remove_sig',
                                                     label = tagList(icon('minus'), 'Eliminar seleccionadas'),
                                                     class = 'btn btn-secondary w-100')),
                              column(4, actionButton('clear_sig',
                                                     label = tagList(icon('eraser'), 'Borrar todas'),
                                                     class = 'btn btn-secondary w-100'))
                            ),
                            selectizeInput('sig_current', 'Barras agregadas (selecciona para mover/eliminar):',
                                           choices = NULL, multiple = TRUE,
                                           options = list(plugins = list('remove_button'),
                                                          placeholder = 'Sin barras agregadas')),
                            fluidRow(
                              column(6, actionButton('sig_move_up',
                                                     label = tagList(icon('arrow-up'), 'Subir orden'),
                                                     class = 'btn btn-outline-primary w-100')),
                              column(6, actionButton('sig_move_down',
                                                     label = tagList(icon('arrow-down'), 'Bajar orden'),
                                                     class = 'btn btn-outline-primary w-100'))
                            ),
                            fluidRow(
                              column(6,
                                     numericInput('sig_linewidth', 'Grosor línea:',
                                                  .8,  min = .2, step = .2)),
                              column(6,
                                      numericInput('sig_offset',    'Distancia a primera barra (% rango Y):',
                                                   .05, min = .0,  step = .01))
                            ),
                            fluidRow(
                              column(6,
                                      numericInput('sig_sep',       'Separación entre barras (% rango Y):',
                                                   .05, min = .01, step = .01)),
                              column(6,
                                      numericInput('sig_textpad', 'Separación etiqueta-barra (% rango Y):',
                                                   value = .01, min = .005, step = .005))
                            ),
                            numericInput('sig_textsize', 'Tamaño etiqueta (pts):',
                                         value = 5,          #  ←  tamaño por defecto visible
                                         min   = 1,
                                         step  = .5
                             ),
                            checkboxInput('sig_hide_caps', 'Quitar lineas verticales', FALSE)
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
                             tags$div(
                               class = "dropdown",
                               actionButton(
                                 "downloadPlotly_btn",
                                 "Descargar gráfico",
                                 class = "btn btn-primary dropdown-toggle",
                                 `data-bs-toggle` = "dropdown",
                                 `aria-expanded`  = "false"
                               ),
                               tags$ul(
                                 class = "dropdown-menu",
                                 tags$li(actionLink("downloadPlotly_png", "PNG", class="dropdown-item")),
                                 tags$li(actionLink("downloadPlotly_pdf", "PDF", class="dropdown-item"))
                               )
                             )
                           ),
                           conditionalPanel(
                             "input.tipo != 'Apiladas'",
                             tags$div(
                               class = "dropdown",
                               actionButton(
                                 "downloadPlot_btn",
                                 "Descargar gráfico",
                                 class = "btn btn-primary dropdown-toggle",
                                 `data-bs-toggle` = "dropdown",
                                 `aria-expanded`  = "false"
                               ),
                               tags$ul(
                                 class = "dropdown-menu",
                                 tags$li(downloadLink("downloadPlot_png", "PNG", class="dropdown-item")),
                                 tags$li(downloadLink("downloadPlot_pdf", "PDF", class="dropdown-item"))
                               )
                             )
                           ),
                           
                           div(
                             id = "downloadExcel_section",
                             br(), br(),
                             downloadButton("downloadExcel",    "Descargar Datos",
                                            class = "btn btn-primary")
                           ),

                           br(),

                           downloadButton("downloadMetadata", "Descargar Metadata",
                                          class = "btn btn-primary"),
                           br(), br(),

                           downloadButton("downloadStats",    "Descargar Resultados Estadísticos",
                                          class = "btn btn-primary"),
                           br(), br(),

                           downloadButton("downloadBundleZip", "Descargar paquete (ZIP)",
                                          class = "btn btn-secondary"),
                           hr(),
                           
                         ),  # ---- FIN sidebarPanel -----------------------------------------------
                         
                         ###########################################################################
                         #  MAIN PANEL -------------------------------------------------------------
                         ###########################################################################
                         mainPanel(
                           plotlyOutput('plotInteractivo', width = '100%', height = 'auto'),
                           br(),
                           tags$div(
                             class = "bundle-action-bar",
                             tags$div(
                               class = "bundle-label-input",
                               textInput(
                                 "bundle_label",
                                 label = NULL,
                                 placeholder = "Etiqueta opcional"
                               )
                             ),
                             actionButton(
                               "copy_plot_clipboard",
                               label = tagList(icon("copy"), "Copiar gráfico"),
                               class = "btn btn-secondary"
                             ),
                             actionButton(
                               "save_bundle_version",
                               label = tagList(icon("save"), "Guardar versión"),
                               class = "btn btn-primary"
                             ),
                             actionButton(
                               "add2panel",
                               label = tagList(icon("plus-circle"), "Añadir al panel"),
                               class = "btn btn-success"
                             )
                           ),
                         DTOutput('statsTable'),
                         hr(),
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
                         sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
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
