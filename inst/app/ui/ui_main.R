# --- Main UI ---
# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = theme_light,          # por defecto; el server lo cambia a oscuro/claro
  useShinyjs(),
  usei18n(i18n),

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
    (function () {
      function onShinyConnected(fn, tries) {
        tries = tries || 0;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          if (Shiny.shinyapp && typeof Shiny.shinyapp.isConnected === 'function' && Shiny.shinyapp.isConnected()) {
            fn();
          } else {
            $(document).one('shiny:connected', fn);
          }
        } else if (tries < 200) {
          setTimeout(function () { onShinyConnected(fn, tries + 1); }, 50);
        }
      }

      $(function () {
        var m = localStorage.getItem('appMode') || 'light';
        var ann = localStorage.getItem('announcementSeen') || '';
        var remoteAnn = localStorage.getItem('bioszenRemoteAnnouncementsSeen') || '';
        onShinyConnected(function () {
          Shiny.setInputValue('mode', m, {priority: 'event'});
          Shiny.setInputValue('announcement_seen', ann, {priority: 'event'});
          Shiny.setInputValue('remote_announcement_seen', remoteAnn, {priority: 'event'});
          Shiny.addCustomMessageHandler('saveMode', function (m) {
            localStorage.setItem('appMode', m);
          });
        });
      });
    })();
  ")),
  tags$script(HTML("
    (function () {
      var annJsonDefault = 'https://raw.githubusercontent.com/bioszen/BIOSZEN-Announcements/main/announcements/latest.json';
      var storageKey = 'bioszenRemoteAnnouncementsSeen';
      var hasRun = false;
      var inFlight = false;
      var attempts = 0;
      var maxAttempts = 3;

      function normalizeUrl(url) {
        if (!url) return url;
        var m = url.match(/^https:\\/\\/github\\.com\\/([^\\/]+)\\/([^\\/]+)\\/raw\\/refs\\/heads\\/([^\\/]+)\\/(.+)$/i);
        if (m) return 'https://raw.githubusercontent.com/' + m[1] + '/' + m[2] + '/' + m[3] + '/' + m[4];
        var m2 = url.match(/^https:\\/\\/github\\.com\\/([^\\/]+)\\/([^\\/]+)\\/blob\\/([^\\/]+)\\/(.+)$/i);
        if (m2) return 'https://raw.githubusercontent.com/' + m2[1] + '/' + m2[2] + '/' + m2[3] + '/' + m2[4];
        return url;
      }

      function splitSeen(raw) {
        if (!raw) return [];
        return raw.split('|').map(function (x) { return x.trim(); }).filter(Boolean);
      }

      function fetchJson(url, timeoutMs) {
        if (!window.fetch) return Promise.reject(new Error('fetch not supported'));
        if (window.AbortController) {
          var controller = new AbortController();
          var timer = setTimeout(function () { controller.abort(); }, timeoutMs);
          return fetch(url, { signal: controller.signal }).then(function (res) {
            clearTimeout(timer);
            if (!res.ok) throw new Error('http ' + res.status);
            return res.json();
          });
        }
        return fetch(url).then(function (res) {
          if (!res.ok) throw new Error('http ' + res.status);
          return res.json();
        });
      }

      function run() {
        if (hasRun) return;
        if (inFlight) return;
        if (!window.Shiny || typeof Shiny.setInputValue !== 'function') {
          setTimeout(run, 200);
          return;
        }
        inFlight = true;
        attempts += 1;
        var annUrl = normalizeUrl(window.BIOSZEN_ANNOUNCEMENT_JSON || annJsonDefault);
        var seen = splitSeen(localStorage.getItem(storageKey) || '');
        fetchJson(annUrl, 4000).then(function (ann) {
          if (!ann || typeof ann !== 'object') {
            hasRun = true;
            return;
          }
          if (ann.enabled !== undefined && ann.enabled !== null) {
            var enabled = String(ann.enabled).toLowerCase().trim();
            if (enabled === 'false' || enabled === '0' || enabled === 'no' || enabled === 'n') {
              hasRun = true;
              return;
            }
          }
          var id = (ann.id || '').trim();
          if (!id) {
            hasRun = true;
            return;
          }
          if (seen.indexOf(id) !== -1) {
            hasRun = true;
            return;
          }
          ann.id = id;
          ann.ts = Date.now();
          Shiny.setInputValue('remote_announcement_payload', ann, {priority: 'event'});
          hasRun = true;
        }).catch(function () {
          if (attempts < maxAttempts) {
            setTimeout(run, 2000);
          }
        }).finally(function () {
          inFlight = false;
        });
      }

      function schedule() {
        if (hasRun) return;
        setTimeout(run, 500);
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', schedule);
      } else {
        schedule();
      }
      if (window.jQuery) {
        $(document).on('shiny:connected', schedule);
      }
    })();
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
        tr("lang_label"),
        class = "btn btn-secondary dropdown-toggle",
        `data-bs-toggle` = "dropdown",
        `aria-expanded`  = "false"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(actionLink("lang_es", tr("lang_es"), class = "dropdown-item")),
        tags$li(actionLink("lang_en", tr("lang_en"), class = "dropdown-item"))
      )
    ),
    
    ## Botones modo claro/oscuro (sin cambios)
    actionButton('btn_light', label = tr("theme_light"),
                 class = 'btn btn-light', style = 'color:#000;'),
    actionButton('btn_dark',  label = tr("theme_dark"),
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

  tags$script(HTML(sprintf("window.BIOSZEN_LANG = '%s';", i18n_lang))),
  tags$script(HTML("
    (function () {
      function onShinyConnected(fn, tries) {
        tries = tries || 0;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          if (Shiny.shinyapp && typeof Shiny.shinyapp.isConnected === 'function' && Shiny.shinyapp.isConnected()) {
            fn();
          } else {
            $(document).one('shiny:connected', fn);
          }
        } else if (tries < 200) {
          setTimeout(function () { onShinyConnected(fn, tries + 1); }, 50);
        }
      }

      function applyLang(lang) {
        if (!lang) return;
        localStorage.setItem('appLang', lang);
        var link = document.getElementById('manual_link');
        if (link) {
          var pdf  = (lang === 'en' ? 'MANUAL_EN.pdf' : 'MANUAL_ES.pdf');
          var docx = (lang === 'en' ? 'MANUAL_EN.docx' : 'MANUAL_ES.docx');
          fetch(pdf, {method: 'HEAD'}).then(function(r){
            link.setAttribute('href', (r && r.ok) ? pdf : docx);
          }).catch(function(){
            link.setAttribute('href', docx);
          });
        }
        onShinyConnected(function () {
          Shiny.setInputValue('app_lang', lang, {priority: 'event'});
          Shiny.setInputValue('manual_lang', lang, {priority: 'event'});
        });
      }

      window.BIOSZEN_applyLang = applyLang;

      document.addEventListener('DOMContentLoaded', function(){
        var lg = localStorage.getItem('appLang') || 'en';
        applyLang(lg);
      });
    })();
  ")),
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
    tags$span(tr("app_title"),
              style = 'font-size:30px; font-weight:bold;')
  )),
  uiOutput("app_announcement"),
  # --- Inicio de pesta?as principales ---
  tabsetPanel(id = "mainTabs", type = "tabs",
    tabPanel(title = HTML(as.character(tr("tab_plots"))), value = "tab_plots",
             # --------------------------------------------------------------------------
             #  Layout principal: barra lateral + área de trazado/tablas
             # --------------------------------------------------------------------------
             sidebarLayout(
               
               ###########################################################################
               #  SIDEBAR ----------------------------------------------------------------
               ###########################################################################
              sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
                 
                fileInput('dataFile',  tr("file_data"), accept = '.xlsx'),
                fileInput('curveFile', tr("file_curves"),              accept = '.xlsx'),
                fileInput('metaFiles', tr("file_meta"),    accept = '.xlsx', multiple = TRUE),

                 # ---- Botones de descarga de ejemplos ----------------------------------
                 tags$div(
                   style = 'margin-top:24px; margin-bottom:15px;',
                   tags$a(id = 'manual_link',
                          class = 'btn btn-secondary',
                          href = 'MANUAL_EN.pdf',
                          download = NA,
                          tr("manual_download")),
                   tags$br(), tags$br(),
                   downloadButton('download_refs', tr("download_refs"), class = 'btn btn-secondary')
                 ),
                 hr(),
                 
                 # ---- Controles principales -------------------------------------------
                 radioButtons(
                   "scope",
                   tr("scope_label"),
                   choices = named_choices(
                     c("Por Cepa", "Combinado"),
                     c(tr("scope_by_strain"), tr("scope_combined"))
                   ),
                   selected = "Por Cepa"
                 ),
                 conditionalPanel(
                   condition = "input.scope=='Por Cepa'",
                   selectInput("strain", tr("strain_label"), choices = NULL)
                 ),
                 uiOutput("paramSel"),
                 
                 radioButtons(
                   "tipo",
                   tr("plot_type_label"),
                   choices = named_choices(
                     c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion"),
                     c(
                       tr("plot_boxplot"),
                       tr("plot_bars"),
                       tr("plot_violin"),
                       tr("plot_curves"),
                       tr("plot_stacked"),
                       tr("plot_correlation")
                     )
                   ),
                   selected = "Boxplot"
                 ),
                 
                 # ---------- SECCION "Apiladas" ----------------------------------------
                 conditionalPanel(
                   condition = "input.tipo == 'Apiladas'",
                   h4(tr("stack_settings")),
                   checkboxGroupInput("stackParams", tr("stack_params"),
                                      choices = NULL, selected = NULL),
                   textInput("orderStack", tr("stack_order"), ""),
                   checkboxInput("showErrBars", tr("stack_show_errbars"), TRUE),
                   checkboxInput("stack_outline_only", tr("stack_outline_only"), FALSE),
                   numericInput("errbar_size", tr("errbar_size"),
                                value = 0.6, min = 0.1, step = 0.1)
                 ),
                 
                 # ---------- SECCION "Correlacion" -------------------------------------
                 conditionalPanel(
                   condition = "input.tipo == 'Correlacion'",
                   h4(tr("corr_settings")),
                   fluidRow(
                     column(6, selectInput("corr_param_x", tr("corr_param_x"), choices = NULL)),
                     column(6, selectInput("corr_param_y", tr("corr_param_y"), choices = NULL))
                   ),
                   radioButtons(
                     "corr_method", tr("corr_method_label"),
                     choices = named_choices(
                       c("pearson", "spearman"),
                       c(tr("corr_method_pearson"), tr("corr_method_spearman"))
                     ),
                     selected = "pearson", inline = TRUE
                   ),
                   checkboxInput("corr_show_line",   tr("corr_show_line"), TRUE),
                   checkboxInput("corr_show_labels", tr("corr_show_labels"), TRUE),
                   checkboxInput("corr_show_r",      tr("corr_show_r"), TRUE),
                   checkboxInput("corr_show_p",      tr("corr_show_p"), TRUE),
                   checkboxInput("corr_show_r2",     tr("corr_show_r2"), FALSE),
                   checkboxInput("corr_show_eq",     tr("corr_show_eq"), FALSE),
                   
                   textInput("corr_xlab", tr("corr_xlab"), value = ""),
                   textInput("corr_ylab", tr("corr_ylab"), value = ""),
                   
                   fluidRow(
                     column(6, numericInput("xmin_corr", tr("corr_xmin"), value = 0,  min = 0)),
                     column(6, numericInput("xmax_corr", tr("corr_xmax"), value = 0,  min = 0))
                   ),
                   fluidRow(
                     column(6, numericInput("xbreak_corr", tr("corr_xbreak"), value = 1,  min = 0.001)),
                     column(6, numericInput("ybreak_corr", tr("corr_ybreak"), value = 1,  min = 0.001))
                   ),
                   fluidRow(
                     column(6, numericInput("ymin_corr", tr("corr_ymin"), value = 0,  min = 0)),
                     column(6, numericInput("ymax_corr", tr("corr_ymax"), value = 0,  min = 0)),
                     column(6, numericInput("corr_label_size",
                                            tr("corr_label_size"), value = 4, min = 1))
                   )
                 ),
                 
                
                # ---------- Controles comunes -----------------------------------------
                checkboxInput('doNorm', tr("norm_by_control"), FALSE),
                uiOutput('ctrlSelUI'),
                conditionalPanel(
                  condition = "input.tipo == 'Correlacion' && input.doNorm",
                  radioButtons(
                    "corr_norm_target", tr("corr_norm_target"),
                    choices = setNames(
                      c("both", "x_only", "y_only"),
                      tr_text(c("corr_norm_both", "corr_norm_x", "corr_norm_y"), i18n_lang)
                    ),
                    selected = "both", inline = TRUE
                  )
                ),
                 
                 
                 # ---------- Eje Y para Boxplot/Barras ---------------------------------
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                   h4(tr("y_axis_settings")),
                   fluidRow(
                     column(6, numericInput('ymax',   tr("y_max"),  value = 0, min = 0)),
                     column(6, numericInput('ybreak', tr("y_interval"),  value = 1, min = 0.001))
                   )
                 ),
                 
                 # ---------- Ajustes Curvas --------------------------------------------
                 conditionalPanel(
                   condition = "input.tipo == 'Curvas'",
                   h4(tr("curves_settings")),
                   fluidRow(
                     column(6, numericInput('xmax_cur',   tr("curves_xmax"), value = 3000, min = 0)),
                     column(6, numericInput('xbreak_cur', tr("curves_xbreak"), value = 1000, min = 1))
                   ),
                   fluidRow(
                     column(6, numericInput('ymax_cur',   tr("curves_ymax"), value = 1.5, min = 0)),
                     column(6, numericInput('ybreak_cur', tr("curves_ybreak"), value = 0.5, min = 0.01))
                   ),
                  fluidRow(
                    column(6, textInput('cur_xlab', tr("curves_xlab"), '')),
                    column(6, textInput('cur_ylab', tr("curves_ylab"), ''))
                  ),
                  numericInput('curve_lwd', tr("curves_linewidth"), value = 1.5, min = 0.5, step = 0.1)
                ),
                 
                 # Réplicas a mostrar para Curvas
                 conditionalPanel(
                   "input.tipo == 'Curvas'",
                   uiOutput("repSelCurvas")
                 ),
                # ---------- Paleta de color -------------------------------------------
                selectInput(
                  'colorMode',
                  tr("palette_label"),
                  choices = named_choices(
                    c(
                      "Default", "Default Suave",
                      "Blanco y Negro", "Blanco y Negro Suave",
                      "Viridis", "Viridis Suave",
                      "Plasma", "Plasma Suave",
                      "Magma", "Magma Suave",
                      "Cividis", "Cividis Suave",
                      "Set1", "Set1 Suave",
                      "Set2", "Set2 Suave",
                      "Set3", "Set3 Suave",
                      "Dark2", "Dark2 Suave",
                      "Accent", "Accent Suave",
                      "Paired", "Paired Suave",
                      "Pastel1", "Pastel1 Suave",
                      "Pastel2", "Pastel2 Suave",
                      "OkabeIto", "OkabeIto Suave",
                      "Tableau", "Tableau Suave"
                    ),
                    c(
                      tr("palette_default"),
                      tr("palette_default_soft"),
                      tr("palette_bw"),
                      tr("palette_bw_soft"),
                      tr("palette_viridis"),
                      tr("palette_viridis_soft"),
                      tr("palette_plasma"),
                      tr("palette_plasma_soft"),
                      tr("palette_magma"),
                      tr("palette_magma_soft"),
                      tr("palette_cividis"),
                      tr("palette_cividis_soft"),
                      tr("palette_set1"),
                      tr("palette_set1_soft"),
                      tr("palette_set2"),
                      tr("palette_set2_soft"),
                      tr("palette_set3"),
                      tr("palette_set3_soft"),
                      tr("palette_dark2"),
                      tr("palette_dark2_soft"),
                      tr("palette_accent"),
                      tr("palette_accent_soft"),
                      tr("palette_paired"),
                      tr("palette_paired_soft"),
                      tr("palette_pastel1"),
                      tr("palette_pastel1_soft"),
                      tr("palette_pastel2"),
                      tr("palette_pastel2_soft"),
                      tr("palette_okabeito"),
                      tr("palette_okabeito_soft"),
                      tr("palette_tableau"),
                      tr("palette_tableau_soft")
                    )
                  ),
                  selected = "Default"
                ),
                conditionalPanel(
                  condition = "input.scope=='Combinado'",
                  checkboxInput('repeat_colors_combined',
                                tr("repeat_colors_combined"),
                                FALSE)
                ),
                accordion(
                  id       = "advPalettePanel",
                  open     = FALSE,
                  multiple = TRUE,
                  accordion_panel_safe(
                    tr("palette_advanced"),
                    checkboxInput("adv_pal_enable",
                                  tr("palette_advanced_enable"),
                                  FALSE),
                    conditionalPanel(
                      condition = "input.adv_pal_enable == true",
                      radioButtons(
                        "adv_pal_type",
                        tr("palette_type_label"),
                        choices = named_choices(
                          c("seq", "div", "qual"),
                          c(tr("palette_type_seq"), tr("palette_type_div"), tr("palette_type_qual"))
                        ),
                        selected = "seq",
                        inline = TRUE
                      ),
                      checkboxInput(
                        "adv_pal_reverse",
                        tr("palette_reverse"),
                        FALSE
                      ),
                      checkboxGroupInput(
                        "adv_pal_filters",
                        tr("palette_filters_label"),
                        choices = named_choices(
                          c("colorblind", "print", "photocopy"),
                          c(
                            tr("palette_filter_colorblind"),
                            tr("palette_filter_print"),
                            tr("palette_filter_photocopy")
                          )
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      selectInput(
                        "adv_pal_name",
                        tr("palette_scheme_label"),
                        choices = c("BuGn"),
                        selected = "BuGn"
                      )
                    ),
                    style = "default"
                  )
                ),
                 
                 ## ─── Boxplot ──────────────────────────────────────────────
                 conditionalPanel(
                   condition = "input.tipo == 'Boxplot'",
                   numericInput("box_w",   tr("box_width"),     value = 0.8,
                                min = 0.1, max = 1.5, step = 0.05),
                   numericInput("pt_jit",  tr("point_jitter"), value = 0.1,
                                min = 0,   max = 0.5, step = 0.01)
                 ),
                 
                 ## ─── Boxplot *y* Barras (tamaño de puntos) ────────────────
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                   numericInput("pt_size", tr("point_size"), value = 3,
                                min = 0.5, max = 20,  step = 0.5),
                   # ─── Ángulo de las etiquetas del eje X ──────────────────────────────────
                   numericInput(
                     "x_angle",                     # <-- NUEVO input
                     tr("x_angle"),
                     value = NA,                    #  NA ⇒ “automático” (no fuerza nada)
                     min   = 0,
                     max   = 90,
                     step  = 5
                   ),
                   checkboxInput(
                     "x_wrap",
                     tr("x_wrap"),
                     FALSE
                   ),
                   conditionalPanel(
                     condition = "input.x_wrap == true",
                     numericInput(
                       "x_wrap_lines",
                       tr("x_wrap_lines"),
                       value = 2,
                       min = 1,
                       step = 1
                     )
                   )
                ),
                 
                
                hr(), h4(tr("size_section")),
                numericInput("base_size", tr("base_size"), 18, min = 8),
                numericInput("fs_title",  tr("title_size"),   20, min = 6),
                numericInput("fs_axis",   tr("axis_size"),     15, min = 6),
                numericInput("fs_legend", tr("legend_size"),  17, min = 6),
                numericInput("axis_line_size", tr("axis_line_size"),
                             value = 1.2, min = .1, step = .1),
                
                 # ---------- Parámetro a graficar --------------------------------------
                 textInput('yLab', tr("y_label"), ''),
                 
                 conditionalPanel(
                   condition = "input.scope=='Por Cepa'",
                   textInput('orderMedios', tr("order_csv"), '')
                 ),
                
                textInput('plotTitle', tr("plot_title"), ''),
                
                uiOutput('rmRepsGlobalUI'),
                
                
                # ---------- Filtro de medios (Por Cepa) -------------------------------
                conditionalPanel(
                  condition = "input.scope=='Por Cepa'",
                  h4(tr("filter_media")),
                   checkboxInput('toggleMedios', tr("toggle_all"), TRUE),
                   uiOutput('showMediosUI'),
                   
                   accordion(                     # <-- en lugar de bsCollapse
                     id       = 'repsPanel',
                     open     = FALSE,
                     multiple = TRUE,
                    accordion_panel_safe(
                      tr("reps_by_media"),
                      uiOutput('repsStrainUI'),
                      style = 'default'
                    )
                    )
                  ),
                # ---------- Filtro de grupos (Combinado) ------------------------------
                conditionalPanel(
                  condition = "input.scope=='Combinado'",
                  h4(tr("filter_groups")),
                  checkboxInput("toggleGroups", tr("toggle_all"), TRUE),
                  
                  uiOutput("groupSel"),       # ← sigue igual
                  
                  uiOutput("repsGrpUI"),      # ← NUEVO hueco para el acordeón dinámico
                   
                   checkboxInput("labelMode", tr("label_mode"), FALSE)
                 ),
                 
                 
                 # ---------- Estadística para Boxplot/Barras ---------------------------
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin'].indexOf(input.tipo) >= 0",
                   accordion(
                     id       = "statsPanel",
                     open     = FALSE,
                     multiple = TRUE,
                     accordion_panel_safe(
                       tr("stats_title"),
                       tabsetPanel(
                         id = 'statsTabs',
                         tabPanel(
                           title = tr("stats_tab_normality"),
                           checkboxGroupInput(
                           'normTests', NULL,
                           choices = named_choices(
                             c('shapiro', 'ks', 'ad'),
                             c(tr("norm_shapiro"), tr("norm_ks"), tr("norm_ad"))
                           ),
                           selected = c('shapiro','ks','ad')),
                           actionButton('runNorm',
                                        label = tagList(icon('vial'), tr("run_normality")),
                                        class = 'btn btn-primary'),
                           br(), br(),
                           DTOutput('normTable')
                         ),
                         tabPanel(
                           title = tr("stats_tab_significance"),
                           radioButtons('sigTest', NULL,
                                        choices = named_choices(
                                          c('ANOVA', 'Kruskal-Wallis', 'ttest', 'wilcox'),
                                          c(
                                            tr("sigtest_anova"),
                                            tr("sigtest_kruskal"),
                                            tr("sigtest_ttest"),
                                            tr("sigtest_wilcox")
                                          )
                                        ),
                                        selected = 'ANOVA'),
                           uiOutput('postHocUI'),
                           radioButtons('compMode', NULL,
                                        choices = named_choices(
                                          c('all', 'control', 'pair'),
                                          c(tr("comp_all"), tr("comp_control"), tr("comp_pair"))
                                        ),
                                        selected = 'all'),
                           conditionalPanel(
                             condition = "input.compMode=='control'",
                             selectInput('controlGroup', tr("control_label"), choices = NULL)
                           ),
                           conditionalPanel(
                             condition = "input.compMode=='pair'",
                             selectInput('group1', tr("group1_label"), choices = NULL),
                             selectInput('group2', tr("group2_label"), choices = NULL)
                           ),
                           actionButton('runSig',
                                        label = tagList(icon('chart-bar'),
                                                        tr("run_significance")),
                                        class = 'btn btn-primary'),
                           br(), br(),
                           DTOutput('sigTable')
                         )
                       ),
                       style = 'primary'
                     )
                 )),
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                   hr(), h4(tr("sig_bars_title")),
                  radioButtons('sig_mode', NULL,
                               choices = named_choices(
                                 c('bars', 'labels'),
                                 c(tr("sig_mode_bars"), tr("sig_mode_labels"))
                               ),
                               selected = 'bars', inline = TRUE),
                  conditionalPanel(
                    condition = "input.sig_mode == 'labels'",
                    conditionalPanel(
                      condition = "input.tipo == 'Apiladas'",
                      selectInput('sig_param', tr("sig_param_label"), choices = NULL),
                      checkboxInput('sig_label_param_color',
                                    tr("sig_label_param_color"), FALSE)
                    )
                  ),
                  fluidRow(
                    column(6, selectInput('sig_group1', tr("group1_label"), choices = NULL)),
                    column(6, selectInput('sig_group2', tr("group2_label"), choices = NULL))
                  ),
                  textInput('sig_label', tr("sig_label"), '*'),
                  fluidRow(
                    column(4, actionButton('add_sig',
                                           label = tagList(icon('plus'), tr("sig_add_bar")),
                                           class = 'btn btn-primary w-100')),
                    column(4, actionButton('remove_sig',
                                           label = tagList(icon('minus'), tr("sig_remove")),
                                           class = 'btn btn-secondary w-100')),
                    column(4, actionButton('clear_sig',
                                           label = tagList(icon('eraser'), tr("sig_clear")),
                                           class = 'btn btn-secondary w-100'))
                  ),
                  selectizeInput('sig_current', tr("sig_current_label"),
                                 choices = NULL, multiple = TRUE,
                                 options = list(plugins = list('remove_button'),
                                                placeholder = tr_text("sig_current_placeholder"))),
                  fluidRow(
                    column(6, actionButton('sig_move_up',
                                           label = tagList(icon('arrow-up'), tr("sig_move_up")),
                                           class = 'btn btn-outline-primary w-100')),
                    column(6, actionButton('sig_move_down',
                                           label = tagList(icon('arrow-down'), tr("sig_move_down")),
                                           class = 'btn btn-outline-primary w-100'))
                  ),
                  fluidRow(
                    column(6,
                           numericInput('sig_linewidth', tr("sig_linewidth"),
                                        .8,  min = .2, step = .2)),
                    column(6,
                            numericInput('sig_offset',    tr("sig_offset"),
                                         .05, min = .0,  step = .01))
                  ),
                  fluidRow(
                    column(6,
                            numericInput('sig_sep',       tr("sig_sep"),
                                         .05, min = .01, step = .01)),
                    column(6,
                            numericInput('sig_textpad', tr("sig_textpad"),
                                         value = .01, min = .005, step = .005))
                  ),
                  numericInput('sig_textsize', tr("sig_textsize"),
                               value = 5,          #  ←  tamaño por defecto visible
                               min   = 1,
                               step  = .5
                   ),
                  checkboxInput('sig_hide_caps', tr("sig_hide_caps"), FALSE)
                 ),
                 hr(),
                 fluidRow(
                   column(6, numericInput('plot_w', tr("plot_width"), 1000, min = 100)),
                   column(6, numericInput('plot_h', tr("plot_height"),  700, min = 100))
                 ),
                 br(), br(),
                 
                 # ── Botones de descarga ──────────────────────────────────────────────
                 conditionalPanel(
                   "input.tipo == 'Apiladas'",
                   tags$div(
                     class = "dropdown",
                     actionButton(
                       "downloadPlotly_btn",
                       tr("download_plot"),
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
                       tr("download_plot"),
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
                   downloadButton("downloadExcel",    tr("download_data"),
                                  class = "btn btn-primary")
                 ),

                 br(),

                 downloadButton("downloadMetadata", tr("download_metadata"),
                                class = "btn btn-primary"),
                 br(), br(),

                 downloadButton("downloadStats",    tr("download_stats"),
                                class = "btn btn-primary"),
                 br(), br(),

                 downloadButton("downloadBundleZip", tr("download_bundle"),
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
                       placeholder = tr_text("bundle_label_placeholder")
                     )
                   ),
                   actionButton(
                     "copy_plot_clipboard",
                     label = tagList(icon("copy"), tr("copy_plot")),
                     class = "btn btn-secondary"
                   ),
                   actionButton(
                     "save_bundle_version",
                     label = tagList(icon("save"), tr("save_version")),
                     class = "btn btn-primary"
                   ),
                   actionButton(
                     "add2panel",
                     label = tagList(icon("plus-circle"), tr("add_to_panel")),
                     class = "btn btn-success"
                   )
                 ),
               DTOutput('statsTable'),
               hr(),
               tags$div(
                  style = "font-size:16px; text-align:center; color:#555;",
                  tr("comments_label"),
                  tags$a("bioszenf@gmail.com",
                         href = "mailto:bioszenf@gmail.com",
                          style = "font-weight:bold;")
                   
                 )   # ← comentario dice “FIN sidebarLayout”, pero en realidad cierra sólo <div>
               ) # ← este cierra <mainPanel>
             )
    ),
    tabPanel(title = HTML(as.character(tr("tab_growth"))), value = "tab_growth",
             sidebarLayout(
               sidebarPanel(style="overflow-y:auto;max-height:95vh;position:sticky;top:0;",
                 fileInput(
                   "growthFiles",
                   tr("growth_files"),
                   accept = ".xlsx",
                   multiple = TRUE
                 ),
                 numericInput(
                   "maxTime",
                   tr("growth_max_time"),
                   value = 48,
                   min = 0
                 ),
                 numericInput(
                   "timeInterval",
                   tr("growth_interval"),
                   value = 0.5,
                   min = 0.01
                 ),
                  actionButton(
                    "runGrowth",
                    tr("growth_run"),
                    class = "btn btn-primary"
                  ),
                  br(), br(),
                  downloadButton(
                    "downloadGrowthZip",
                    tr("growth_download"),
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
  )
)
