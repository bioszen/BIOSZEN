# --- Main UI ---
# --- UI ----------------------------------------------------------------------
bioszen_dir_button <- function(id, label, title, icon_name = "folder-open") {
  button <- shinyFiles::shinyDirButton(
    id = id,
    label = "",
    title = title,
    icon = icon(icon_name)
  )
  button[[2]]$children <- list(icon(icon_name), label)
  button
}

ui <- fluidPage(
  theme = theme_light,          # por defecto; el server lo cambia a oscuro/claro
  useShinyjs(),
  usei18n(i18n),

  tags$head(
    tags$title("BIOSZEN"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "icon", type = "image/x-icon", href = "bioszen.ico")
  ),

  tags$head(
    tags$style(HTML("
      :root {
        --bz-radius: 14px;
        --bz-gap: 14px;
      }

      html {
        font-size: 16px;
      }

      html, body {
        min-height: 100%;
        -webkit-text-size-adjust: 100%;
        text-size-adjust: 100%;
      }

      body,
      .form-control,
      .selectize-input,
      .selectize-dropdown,
      .btn,
      .checkbox label,
      .radio label {
        font-size: 1rem;
      }

      .bioszen-topbar {
        position: sticky;
        top: 0;
        z-index: 1100;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        padding: 10px 16px;
        margin: 0 -12px 8px -12px;
        background: rgba(var(--bs-body-bg-rgb, 248, 249, 250), 0.93);
        border-bottom: 1px solid var(--bs-border-color, rgba(0, 0, 0, 0.12));
        backdrop-filter: blur(6px);
      }

      .bioszen-brand {
        display: flex;
        align-items: center;
        gap: 10px;
        min-width: 0;
      }

      .bioszen-brand img {
        height: 58px !important;
        width: auto;
      }

      .bioszen-brand-title {
        font-size: clamp(18px, 2.2vw, 28px);
        font-weight: 700;
        line-height: 1.1;
      }

      .bioszen-topbar-actions {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
        justify-content: flex-end;
      }

      .bioszen-topbar-actions .btn {
        white-space: nowrap;
      }

      .bioszen-main-tabs {
        margin-top: 4px;
        position: relative;
      }

      #mainTabs > .nav-tabs {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        align-items: center;
        padding-right: 280px;
      }

      #mainTabs > .nav-tabs > li > a {
        white-space: normal;
        line-height: 1.2;
      }

      .bioszen-sidebar-content,
      .bioszen-main-content {
        background: var(--bs-body-bg);
        border: 1px solid var(--bs-border-color, rgba(0, 0, 0, 0.12));
        border-radius: var(--bz-radius);
        padding: 16px;
      }

      .bioszen-top-quickbar {
        position: fixed;
        top: 92px;
        right: 20px;
        z-index: 1250;
        margin: 0;
        display: flex;
        justify-content: flex-end;
      }

      .bioszen-top-quickbar .btn {
        width: auto;
        white-space: nowrap;
      }

      .bioszen-top-quickbar .bioszen-quick-fab-btn {
        border: 0;
        border-radius: 999px;
        padding: 10px 20px;
        min-height: 44px;
        font-weight: 700;
        color: #fff;
        background: linear-gradient(135deg, #006bb3, #00945f);
        box-shadow: 0 8px 22px rgba(0, 0, 0, 0.24);
      }

      .bioszen-top-quickbar .bioszen-quick-fab-btn:hover,
      .bioszen-top-quickbar .bioszen-quick-fab-btn:focus,
      .bioszen-top-quickbar .bioszen-quick-fab-btn:active {
        color: #fff;
        filter: brightness(1.05);
      }

      .bioszen-top-quickbar .bioszen-quick-fab-btn .caret {
        margin-left: 8px;
        border-top-color: #fff;
      }

      .bioszen-top-quickbar .dropdown-menu {
        min-width: 240px;
        border-radius: 12px;
        border: 1px solid var(--bs-border-color, rgba(0, 0, 0, 0.14));
        box-shadow: 0 10px 28px rgba(0, 0, 0, 0.18);
        margin-top: 8px;
        padding: 6px 0;
      }

      .bioszen-top-quickbar .dropdown-item {
        white-space: normal;
      }

      .bioszen-guided-core {
        position: static;
        background: var(--bs-body-bg);
        border: 1px solid var(--bs-border-color, rgba(0, 0, 0, 0.12));
        border-radius: 12px;
        padding: 12px;
        margin: 8px 0 14px 0;
      }

      .bioszen-guided-core h4 {
        margin: 0 0 8px 0;
      }

      .bioszen-guided-core .form-group:last-child {
        margin-bottom: 0;
      }

      .bioszen-sidebar-content hr {
        margin: 14px 0;
      }

      .bioszen-sidebar-content .form-group {
        margin-bottom: 10px;
      }

      .bioszen-axis-input-row .form-group > label {
        min-height: 2.45em;
        display: flex;
        align-items: flex-end;
        line-height: 1.22;
      }

      .bioszen-sidebar-content .btn {
        width: 100%;
        word-break: normal;
        overflow-wrap: break-word;
      }

      .bioszen-datafile-row {
        display: flex;
        align-items: flex-start;
        gap: 8px;
      }

      .bioszen-datafile-main {
        flex: 1 1 auto;
        min-width: 0;
      }

      .bioszen-datafile-main .form-group {
        margin-bottom: 0;
      }

      .bioszen-datafile-arrow {
        flex: 0 0 auto;
        margin-top: 34px;
      }

      .bioszen-datafile-arrow .btn {
        width: auto;
        min-width: 52px;
        padding: 8px 14px;
      }

      .bioszen-sidebar-content .dropdown > .btn {
        width: 100%;
      }

      .bioszen-sidebar-content .dropdown-menu {
        min-width: 100%;
      }

      .bioszen-sidebar-content .accordion {
        margin-top: 8px;
      }

      .bioszen-main-tabs .tab-pane .col-sm-4[style*='overflow-y'],
      .bioszen-main-tabs .tab-pane .col-sm-3[style*='overflow-y'] {
        min-width: 320px;
        scrollbar-width: thin;
      }

      #statsTabs .checkbox {
        margin-top: 6px;
      }

      .bioszen-main-content .dataTables_wrapper .dataTables_length,
      .bioszen-main-content .dataTables_wrapper .dataTables_filter {
        margin-bottom: 8px;
      }

      .bioszen-main-content .dataTables_wrapper .dataTables_filter input {
        margin-left: 6px;
      }

      /* Sidebar tables need their controls stacked at every desktop width. */
      .bioszen-sidebar-content .dataTables_wrapper {
        box-sizing: border-box;
        width: 100% !important;
        max-width: 100%;
        min-width: 0;
        overflow: hidden;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_length,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_filter,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_info,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_paginate {
        float: none !important;
        clear: both;
        box-sizing: border-box;
        width: 100%;
        max-width: 100%;
        text-align: left !important;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_length,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_filter {
        margin-bottom: 10px;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_length label,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_filter label {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 6px;
        width: 100%;
        margin: 0;
        white-space: normal;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_filter input {
        box-sizing: border-box;
        width: min(240px, 100%);
        max-width: 100%;
        margin-left: 0;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_scroll,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_scrollHead,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_scrollBody {
        box-sizing: border-box;
        width: 100% !important;
        max-width: 100% !important;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_scrollBody {
        overflow-x: auto !important;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_info {
        padding-top: 10px;
        white-space: normal;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_paginate,
      .bioszen-sidebar-content .dataTables_wrapper .dataTables_paginate > span {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: flex-start;
        gap: 4px;
        white-space: normal;
      }

      .bioszen-sidebar-content .dataTables_wrapper .dataTables_paginate .paginate_button {
        box-sizing: border-box;
        margin: 0 !important;
      }

      /*
       * Shiny adds the `recalculating` class while an output is updating and
       * its default CSS fades that output. For BIOSZEN plots and filter panels
       * this looked like a reload loop during rapid checkbox changes, even
       * when the final plot was correct. Keep the previous UI fully visible
       * while the debounced final render is prepared.
       */
      .shiny-bound-output.recalculating,
      .shiny-bound-output.recalculating *,
      #plotInteractivo.recalculating,
      #plotInteractivo.recalculating *,
      #plotInteractivoUI.recalculating,
      #plotInteractivoUI.recalculating *,
      #showMediosUI.recalculating,
      #showMediosUI.recalculating *,
      #groupSel.recalculating,
      #groupSel.recalculating *,
      #rmRepsGlobalUI.recalculating,
      #rmRepsGlobalUI.recalculating *,
      #repsStrainUI.recalculating,
      #repsStrainUI.recalculating *,
      #repsGrpUI.recalculating,
      #repsGrpUI.recalculating *,
      #repSelCurvas.recalculating,
      #repSelCurvas.recalculating * {
        opacity: 1 !important;
      }

      #statsPanel .accordion-header .accordion-button,
      #curveStatsPanel .accordion-header .accordion-button,
      #qcPanel .accordion-header .accordion-button,
      #plotTextStylePanel .accordion-header .accordion-button,
      #comboTextStylePanel .accordion-header .accordion-button,
      #repsGlobalPanel .accordion-header .accordion-button,
      #repsPanel .accordion-header .accordion-button,
      #repsGrpPanel .accordion-header .accordion-button {
        font-size: 16px;
        font-weight: 700;
      }

      .bundle-action-bar {
        margin-top: 12px;
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        align-items: center;
        justify-content: flex-end;
      }

      .bundle-action-bar .bundle-label-input {
        min-width: 220px;
        flex: 1 1 220px;
      }

      .bundle-action-bar .bundle-label-input input {
        height: 40px;
      }

      .qc-help {
        font-size: 13px;
        color: var(--bs-secondary-color);
        margin-bottom: 8px;
      }

      .qc-tabs-layout .nav-tabs {
        display: flex;
        flex-direction: column;
        align-items: stretch;
        gap: 6px;
        border-bottom: 0;
        margin-bottom: 12px;
        width: 100%;
        min-width: 0;
      }

      .qc-tabs-layout .nav-tabs > li {
        float: none;
        width: 100%;
        min-width: 0;
        margin-bottom: 0;
      }

      .qc-tabs-layout .nav-tabs > li > a {
        display: block;
        width: 100%;
        min-width: 0;
        white-space: normal;
        word-break: normal;
        overflow-wrap: break-word;
        line-height: 1.25;
        min-height: 3.25em;
        padding: 10px 12px;
      }

      .bioszen-pane-fab {
        display: none;
        position: fixed;
        right: 16px;
        bottom: 16px;
        z-index: 1300;
      }

      .bioszen-pane-fab .btn {
        border: 0;
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        color: #fff;
        background: linear-gradient(135deg, #006bb3, #00945f);
        box-shadow: 0 8px 22px rgba(0, 0, 0, 0.28);
      }

      .bioszen-pane-fab .btn:hover,
      .bioszen-pane-fab .btn:focus {
        color: #fff;
        filter: brightness(1.05);
      }

      @media (max-width: 1200px) {
        .bioszen-main-content .dataTables_wrapper .dataTables_length,
        .bioszen-main-content .dataTables_wrapper .dataTables_filter {
          float: none !important;
          width: 100%;
          text-align: left !important;
        }
        .bioszen-main-content .dataTables_wrapper .dataTables_length label,
        .bioszen-main-content .dataTables_wrapper .dataTables_filter label {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 6px;
        }
        .bioszen-main-content .dataTables_wrapper .dataTables_filter input {
          margin-left: 0;
          width: min(260px, 100%);
        }
        .bioszen-pane-fab {
          display: block;
        }
        .bioszen-scroll-layout {
          overflow: visible;
        }
        .bioszen-scroll-layout > .row > .col-sm-4,
        .bioszen-scroll-layout > .row > .col-sm-3,
        .bioszen-scroll-layout > .row > .col-sm-8,
        .bioszen-scroll-layout > .row > .col-sm-9 {
          float: none;
          width: 100%;
          max-width: 100%;
          flex: none;
        }
        .bioszen-scroll-layout.show-sidebar > .row > .col-sm-8,
        .bioszen-scroll-layout.show-sidebar > .row > .col-sm-9 {
          display: none;
        }
        .bioszen-scroll-layout.show-main > .row > .col-sm-4,
        .bioszen-scroll-layout.show-main > .row > .col-sm-3 {
          display: none;
        }
        .bioszen-scroll-layout.show-main > .row > .col-sm-8,
        .bioszen-scroll-layout.show-main > .row > .col-sm-9 {
          display: block;
        }
        .bioszen-scroll-layout .col-sm-4[style*='overflow-y'],
        .bioszen-scroll-layout .col-sm-3[style*='overflow-y'] {
          position: static !important;
          top: auto !important;
          height: auto !important;
          max-height: none !important;
          overflow-y: visible !important;
        }
      }

      @media (max-width: 991px) {
        .bioszen-topbar {
          flex-direction: column;
          align-items: stretch;
        }
        .bioszen-topbar-actions {
          justify-content: flex-start;
        }
        .bioszen-main-content {
          margin-top: 12px;
        }
        .bioszen-scroll-layout .col-sm-4[style*='overflow-y'],
        .bioszen-scroll-layout .col-sm-3[style*='overflow-y'] {
          position: static !important;
          top: auto !important;
          height: auto !important;
          max-height: none !important;
          overflow-y: visible !important;
        }
      }

      @media (max-width: 768px) {
        .bioszen-pane-fab {
          right: 12px;
          bottom: 12px;
        }
        .bioszen-pane-fab .btn {
          font-size: 14px;
          padding: 9px 14px;
        }
        .bioszen-brand img {
          height: 44px !important;
        }
        .bioszen-brand-title {
          font-size: 18px;
        }
        .bundle-action-bar {
          justify-content: stretch;
        }
        .bioszen-datafile-arrow {
          margin-top: 30px;
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
    (function(){
      function selectCurrentShinyFilesDirectory(modal){
        if (!modal || !window.jQuery) return false;
        var $modal = window.jQuery(modal);
        if ($modal.find('.sF-dirList .selected').length === 1) return true;
        var data = $modal.data('currentData') || {};
        var path = Array.isArray(data.contentPath) ? data.contentPath.slice() : [''];
        var $node = $modal.find('.sF-dirList > .sF-directory').first();
        if (!$node.length) return false;
        for (var i = 1; i < path.length; i++) {
          var part = String(path[i] || '');
          if (!part) continue;
          var $next = $node.children('.sF-content').children('.sF-directory').filter(function(){
            return window.jQuery(this).children('.sF-file-name').children().text() === part;
          }).first();
          if (!$next.length) break;
          $node = $next;
        }
        $modal.find('.sF-dirList .selected').removeClass('selected');
        $node.addClass('selected');
        return true;
      }

      function submitGrowthFolder(modal){
        if (!modal || modal.id !== 'browseGrowthOutputDir-modal' || !window.jQuery) return false;
        var $modal = window.jQuery(modal);
        var name = String($modal.find('.sF-newDir input').val() || '').trim();
        if (!name) return false;
        var data = $modal.data('currentData') || {};
        var path = Array.isArray(data.contentPath) ? data.contentPath.slice() : [''];
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('browseGrowthOutputDir-newDir', {
            name: name,
            path: path,
            root: data.selectedRoot || '',
            id: Date.now()
          }, {priority:'event'});
        }
        $modal.find('.sF-newDir').removeClass('open').find('#sF-btn-newDir').removeClass('active');
        return true;
      }

      document.addEventListener('click', function(event){
        var button = event.target && event.target.closest ?
          event.target.closest('.sF-newDir ul button') : null;
        if (!button) return;
        var modal = button.closest('.sF-modalContainer');
        selectCurrentShinyFilesDirectory(modal);
        if (submitGrowthFolder(modal)) {
          event.preventDefault();
          event.stopImmediatePropagation();
        }
      }, true);

      document.addEventListener('keydown', function(event){
        var input = event.target;
        if (!input || event.key !== 'Enter' || !input.closest('.sF-newDir')) return;
        var modal = input.closest('.sF-modalContainer');
        selectCurrentShinyFilesDirectory(modal);
        if (submitGrowthFolder(modal)) {
          event.preventDefault();
          event.stopImmediatePropagation();
        }
      }, true);

      Shiny.addCustomMessageHandler('bioszenGrowthFolderCreated', function(msg){
        var modal = document.getElementById('browseGrowthOutputDir-modal');
        if (modal) {
          var menu = modal.querySelector('.sF-newDir');
          if (menu) menu.classList.remove('open');
          var refresh = modal.querySelector('#sF-btn-refresh');
          if (refresh) refresh.click();
        }
      });
    })();
  ")),
  tags$script(HTML("
    (function () {
      function ensureI18nTranslations() {
        if (typeof window.i18n_translations === 'undefined' || window.i18n_translations === null) {
          window.i18n_translations = [];
        }
      }

      function i18nRows() {
        ensureI18nTranslations();
        if (Array.isArray(window.i18n_translations)) {
          return window.i18n_translations;
        }
        return Object.keys(window.i18n_translations || {}).map(function (key) {
          return window.i18n_translations[key];
        });
      }

      function lookupTranslation(key, lang) {
        if (!key || !lang) return null;
        var rows = i18nRows();
        for (var i = 0; i < rows.length; i++) {
          var row = rows[i];
          if (!row || String(row._row) !== String(key)) continue;
          var value = row[lang];
          if (value !== undefined && value !== null && String(value).length) {
            return String(value);
          }
          var fallback = row.en;
          if (fallback !== undefined && fallback !== null && String(fallback).length) {
            return String(fallback);
          }
        }
        return null;
      }

      function translateFileInputs(lang) {
        var browseLabel = lookupTranslation('file_browse', lang) || 'Browse...';
        var emptyLabel = lookupTranslation('file_no_selection', lang) || '';
        var completeLabel = lookupTranslation('file_upload_complete', lang) || 'Upload complete';
        var inputs = document.querySelectorAll('input[type=\"file\"].shiny-input-file');
        for (var i = 0; i < inputs.length; i++) {
          var input = inputs[i];
          var container = input.closest('.form-group, .shiny-input-container') || input.parentElement;
          var button = input.closest('.btn-file') || (container ? container.querySelector('.btn-file') : null);
          if (button) {
            var updated = false;
            for (var j = 0; j < button.childNodes.length; j++) {
              var node = button.childNodes[j];
              if (node && node.nodeType === 3 && node.textContent.trim().length) {
                if (node.textContent !== browseLabel + ' ') {
                  node.textContent = browseLabel + ' ';
                }
                updated = true;
              }
            }
            if (!updated) {
              button.insertBefore(document.createTextNode(browseLabel + ' '), input);
            }
          }
          if (container) {
            var display = container.querySelector('input[type=\"text\"][readonly], input.form-control[readonly]');
            if (display && emptyLabel) {
              if (display.getAttribute('placeholder') !== emptyLabel) {
              display.setAttribute('placeholder', emptyLabel);
              }
            }
            var bars = container.querySelectorAll('.progress-bar');
            for (var k = 0; k < bars.length; k++) {
              var barText = (bars[k].textContent || '').trim();
              if (/^(Upload complete|Carga completa)$/.test(barText) && barText !== completeLabel) {
                bars[k].textContent = completeLabel;
              }
            }
          }
        }
      }

      function setInputLabelText(input, text) {
        if (!input || !text) return;
        var label = input.closest('label');
        if (!label && input.id) {
          label = document.querySelector('label[for=\"' + input.id.replace(/\"/g, '\\\\\"') + '\"]');
        }
        if (!label) return;

        var updated = false;
        for (var i = 0; i < label.childNodes.length; i++) {
          var node = label.childNodes[i];
          if (!node) continue;
          if (node.nodeType === 3 && node.textContent.trim().length) {
            if (node.textContent !== ' ' + text) node.textContent = ' ' + text;
            updated = true;
          } else if (
            node.nodeType === 1 &&
            node.tagName &&
            node.tagName.toLowerCase() !== 'input' &&
            !node.querySelector('input')
          ) {
            if (node.textContent !== text) node.textContent = text;
            updated = true;
          }
        }
        if (!updated) {
          label.appendChild(document.createTextNode(' ' + text));
        }
      }

      function translateChoiceLabels(lang) {
        var maps = {
          scope: {
            'Por Cepa': 'scope_by_strain',
            Combinado: 'scope_combined'
          },
          tipo: {
            Boxplot: 'plot_boxplot',
            Barras: 'plot_bars',
            Violin: 'plot_violin',
            Curvas: 'plot_curves',
            Apiladas: 'plot_stacked',
            Correlacion: 'plot_correlation',
            Heatmap: 'plot_heatmap',
            MatrizCorrelacion: 'plot_corr_matrix'
          },
          sig_mode: {
            bars: 'sig_mode_bars',
            labels: 'sig_mode_labels'
          },
          cur_ci_style: {
            ribbon: 'curves_ci_ribbon',
            errorbar: 'curves_ci_errorbar'
          },
          curve_geom: {
            line_points: 'curves_geom_line_points',
            line_only: 'curves_geom_line_only'
          },
          curve_color_mode: {
            by_group: 'curves_color_by_group',
            single: 'curves_color_single'
          }
        };

        Object.keys(maps).forEach(function (name) {
          var inputs = document.querySelectorAll('input[name=\"' + name + '\"]');
          for (var i = 0; i < inputs.length; i++) {
            var input = inputs[i];
            var key = maps[name][input.value];
            var value = lookupTranslation(key, lang);
            if (value !== null) setInputLabelText(input, value);
          }
        });
      }

      function translateDynamicHeadings(lang) {
        var mediaText = lang === 'en' ? 'Media' : 'Media';
        var filterPrefix = lang === 'en' ? 'Filter ' : 'Filtro de ';
        var repsPrefix = lang === 'en' ? 'Replicates by ' : 'Replicas por ';
        var nodes = document.querySelectorAll('h4, button.accordion-button, .accordion-button');
        for (var i = 0; i < nodes.length; i++) {
          var node = nodes[i];
          var text = (node.textContent || '').trim();
          if (/^(Filter|Filtro de)\\s+/.test(text)) {
            if (text !== filterPrefix + mediaText) node.textContent = filterPrefix + mediaText;
          } else if (/^(Replicates by|Replicas por)\\s+/.test(text)) {
            if (text !== repsPrefix + mediaText) node.textContent = repsPrefix + mediaText;
          }
        }
      }

      function replaceTextNodes(node, replacements) {
        if (!node) return;
        for (var i = 0; i < node.childNodes.length; i++) {
          var child = node.childNodes[i];
          if (!child || child.nodeType !== 3) continue;
          var text = child.textContent;
          var next = text;
          for (var j = 0; j < replacements.length; j++) {
            next = next.replace(replacements[j][0], replacements[j][1]);
          }
          if (next !== text) child.textContent = next;
        }
      }

      function translateDataTables(lang) {
        var show = lookupTranslation('datatable_show', lang) || (lang === 'en' ? 'Show' : 'Mostrar');
        var entries = lookupTranslation('datatable_entries', lang) || (lang === 'en' ? 'entries' : 'entradas');
        var search = lookupTranslation('datatable_search', lang) || (lang === 'en' ? 'Search:' : 'Buscar:');
        var previous = lookupTranslation('datatable_previous', lang) || (lang === 'en' ? 'Previous' : 'Anterior');
        var next = lookupTranslation('datatable_next', lang) || (lang === 'en' ? 'Next' : 'Siguiente');

        var lengthLabels = document.querySelectorAll('.dataTables_length label');
        for (var i = 0; i < lengthLabels.length; i++) {
          replaceTextNodes(lengthLabels[i], [
            [/Show/g, show],
            [/Mostrar/g, show],
            [/entries/g, entries],
            [/entradas/g, entries]
          ]);
        }

        var filterLabels = document.querySelectorAll('.dataTables_filter label');
        for (var f = 0; f < filterLabels.length; f++) {
          replaceTextNodes(filterLabels[f], [
            [/Search:/g, search],
            [/Buscar:/g, search]
          ]);
        }

        var infoNodes = document.querySelectorAll('.dataTables_info');
        for (var n = 0; n < infoNodes.length; n++) {
          var info = (infoNodes[n].textContent || '').trim();
          var match = info.match(/^(Showing|Mostrando)\\s+(\\d+)\\s+(to|a)\\s+(\\d+)\\s+(of|de)\\s+(\\d+)\\s+(entries|entradas)$/);
          if (match) {
            var text = lang === 'en'
              ? 'Showing ' + match[2] + ' to ' + match[4] + ' of ' + match[6] + ' entries'
              : 'Mostrando ' + match[2] + ' a ' + match[4] + ' de ' + match[6] + ' entradas';
            if (infoNodes[n].textContent !== text) infoNodes[n].textContent = text;
          }
        }

        var prevNodes = document.querySelectorAll('.dataTables_paginate .previous');
        for (var p = 0; p < prevNodes.length; p++) {
          if ((prevNodes[p].textContent || '').trim() !== previous) prevNodes[p].textContent = previous;
        }
        var nextNodes = document.querySelectorAll('.dataTables_paginate .next');
        for (var q = 0; q < nextNodes.length; q++) {
          if ((nextNodes[q].textContent || '').trim() !== next) nextNodes[q].textContent = next;
        }
      }

      function setTextAfterIcons(node, value) {
        if (!node || value === null || value === undefined) return;
        var textNodes = [];
        for (var i = 0; i < node.childNodes.length; i++) {
          if (node.childNodes[i].nodeType === 3) textNodes.push(node.childNodes[i]);
        }
        var next = ' ' + String(value);
        if (textNodes.length) {
          if (textNodes[0].nodeValue !== next) textNodes[0].nodeValue = next;
          for (var j = 1; j < textNodes.length; j++) textNodes[j].nodeValue = '';
        } else {
          node.appendChild(document.createTextNode(next));
        }
      }

      function translateShinyFiles(lang) {
        var modals = document.querySelectorAll('.sF-modalContainer');
        for (var i = 0; i < modals.length; i++) {
          var modal = modals[i];
          var isGrowth = modal.id === 'browseGrowthOutputDir-modal';
          var title = modal.querySelector('.sF-title');
          if (isGrowth && title) {
            var titleText = lookupTranslation('growth_browse_dir_caption', lang);
            if (titleText !== null && title.textContent !== titleText) title.textContent = titleText;
          }

          setTextAfterIcons(
            modal.querySelector('#sF-btn-newDir'),
            lookupTranslation('shinyfiles_create_folder', lang) || 'Create new folder'
          );
          setTextAfterIcons(
            modal.querySelector('#sF-btn-sort'),
            lookupTranslation('shinyfiles_sort_content', lang) || 'Sort content'
          );

          var folderInput = modal.querySelector('.sF-newDir input');
          if (folderInput) {
            folderInput.placeholder = lookupTranslation('shinyfiles_folder_name', lang) || 'Folder name';
          }
          var pathInput = modal.querySelector('.sF-textChoice input');
          if (pathInput) pathInput.placeholder = lookupTranslation('shinyfiles_enter_path', lang) || 'Enter path';

          var sortKeys = [
            'shinyfiles_name', 'shinyfiles_type', 'shinyfiles_size',
            'shinyfiles_created', 'shinyfiles_modified', 'shinyfiles_sort_direction'
          ];
          var sortItems = modal.querySelectorAll('.sF-sort li a');
          for (var s = 0; s < sortItems.length && s < sortKeys.length; s++) {
            setTextAfterIcons(sortItems[s], lookupTranslation(sortKeys[s], lang));
          }

          var headings = modal.querySelectorAll('.sF-dirInfo h6');
          if (headings[0]) headings[0].textContent =
            lookupTranslation('shinyfiles_directories', lang) || 'Directories';
          if (headings[1]) headings[1].textContent =
            lookupTranslation('shinyfiles_content', lang) || 'Content';

          var groups = modal.querySelectorAll('.sF-breadcrumps optgroup');
          for (var g = 0; g < groups.length; g++) {
            groups[g].label = lookupTranslation('shinyfiles_volumes', lang) || 'Volumes';
          }
          var cancel = modal.querySelector('#sF-cancelButton');
          var select = modal.querySelector('#sF-selectButton');
          if (cancel) cancel.textContent = lookupTranslation('shinyfiles_cancel', lang) || 'Cancel';
          if (select) select.textContent = lookupTranslation('shinyfiles_select', lang) || 'Select';

          var content = modal.querySelector('.sF-dirContent');
          if (content) {
            var contentTarget = content;
            if (content.children.length === 1 && content.children[0].children.length === 0) {
              contentTarget = content.children[0];
            }
            var current = (contentTarget.textContent || '').trim();
            if (/^(No folder selected|Ninguna carpeta seleccionada)$/i.test(current)) {
              contentTarget.textContent = lookupTranslation('shinyfiles_no_selection', lang) || 'No folder selected';
            } else if (/^(No folders|No directories|Sin carpetas)$/i.test(current)) {
              contentTarget.textContent = lookupTranslation('shinyfiles_no_folder', lang) || 'No folders';
            } else if (/^(Empty folder|Carpeta vacía)$/i.test(current)) {
              contentTarget.textContent = lookupTranslation('shinyfiles_empty_folder', lang) || 'Empty folder';
            }
          }
        }
      }

      var translationObserverStarted = false;
      var translationPending = false;

      function scheduleTranslateStatic(lang) {
        if (translationPending) return;
        translationPending = true;
        setTimeout(function () {
          translationPending = false;
          translateStatic(lang || window.BIOSZEN_LANG || localStorage.getItem('appLang') || 'en');
        }, 100);
      }

      function startTranslationObserver() {
        if (translationObserverStarted || !document.body || !window.MutationObserver) return;
        translationObserverStarted = true;
        var observer = new MutationObserver(function () {
          scheduleTranslateStatic();
        });
        observer.observe(document.body, {
          childList: true,
          subtree: true,
          characterData: true
        });
      }

      function translateStatic(lang) {
        lang = String(lang || window.BIOSZEN_LANG || localStorage.getItem('appLang') || 'en').toLowerCase();
        window.BIOSZEN_LANG = lang;
        document.documentElement.setAttribute('lang', lang);
        var nodes = document.querySelectorAll('.i18n[data-key], .i18n[data-i18n]');
        for (var i = 0; i < nodes.length; i++) {
          var node = nodes[i];
          var key = node.getAttribute('data-key') || node.getAttribute('data-i18n');
          var value = lookupTranslation(key, lang);
          if (value !== null && node.textContent !== value) node.textContent = value;
        }
        translateFileInputs(lang);
        translateChoiceLabels(lang);
        translateDynamicHeadings(lang);
        translateDataTables(lang);
        translateShinyFiles(lang);
        startTranslationObserver();
      }

      window.BIOSZEN_translateStatic = translateStatic;
      window.BIOSZEN_lookupTranslation = lookupTranslation;

      ensureI18nTranslations();
      document.addEventListener('bioszen:lang-changed', function () {
        translateStatic();
      });
      document.addEventListener('DOMContentLoaded', function () {
        translateStatic();
      });
      if (window.jQuery) {
        $(document).on('shiny:connected', function () {
          translateStatic();
        });
      }
    })();
  ")),
  tags$script(HTML("
    (function () {
      function findSidebarScrollBox(el) {
        if (!el) return null;
        return el.closest('.col-sm-4[style*=\\'overflow-y\\']') ||
               el.closest('.col-sm-3[style*=\\'overflow-y\\']') ||
               null;
      }

      function scrollSidebarTo(targetId) {
        if (!targetId) return;
        var target = document.getElementById(targetId);
        if (!target) return;
        var scrollBox = findSidebarScrollBox(target);
        if (!scrollBox) {
          target.scrollIntoView({ behavior: 'smooth', block: 'start' });
          return;
        }
        var top = target.getBoundingClientRect().top - scrollBox.getBoundingClientRect().top + scrollBox.scrollTop - 8;
        scrollBox.scrollTo({ top: Math.max(0, top), behavior: 'smooth' });
      }

      function bindQuickNav() {
        if (window.__bioszen_quick_nav_bound) return;
        window.__bioszen_quick_nav_bound = true;
        document.addEventListener('click', function (ev) {
          var btn = ev.target && ev.target.closest ? ev.target.closest('.bioszen-quick-btn') : null;
          if (!btn) return;
          ev.preventDefault();
          var targetId = btn.getAttribute('data-target');
          if (!targetId) return;
          scrollSidebarTo(targetId);
        });
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bindQuickNav);
      } else {
        bindQuickNav();
      }
      if (window.jQuery) {
        $(document).on('shiny:connected', bindQuickNav);
      }
    })();
  ")),
  tags$head(
    tags$style(HTML("
      #startup-loader {
        position: fixed;
        right: 14px;
        bottom: 14px;
        background: rgba(11, 11, 11, 0.84);
        border: 1px solid rgba(255, 255, 255, 0.18);
        border-radius: 12px;
        padding: 10px 12px;
        display: flex;
        align-items: center;
        justify-content: flex-start;
        flex-direction: row;
        gap: 10px;
        z-index: 9999;
        pointer-events: none;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.35);
      }
      #startup-loader img {
        width: 38px;
        max-width: 38px;
        height: auto;
        filter: drop-shadow(0 6px 20px rgba(0,0,0,0.45));
      }
      #startup-loader .startup-text {
        color: #f3f3f3;
        font-family: Helvetica, Arial, sans-serif;
        letter-spacing: 0.03em;
        font-size: 12px;
      }
      #startup-loader.startup-hidden {
        opacity: 0;
        transition: opacity 0.35s ease;
        pointer-events: none;
      }
      .bioszen-error-report {
        margin: 0 0 16px;
        border-radius: 6px;
      }
      .bioszen-error-report-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 12px;
      }
      .bioszen-error-report h4 {
        margin: 0 0 6px;
        font-size: 18px;
      }
      .bioszen-error-report-message,
      .bioszen-error-report-intro {
        margin-bottom: 8px;
      }
      .bioszen-error-report details {
        margin: 8px 0 12px;
      }
      .bioszen-error-report summary {
        cursor: pointer;
        font-weight: 600;
      }
      .bioszen-error-report-text {
        max-height: 300px;
        overflow: auto;
        white-space: pre-wrap;
        overflow-wrap: anywhere;
        margin-top: 8px;
        padding: 10px;
        background: rgba(255, 255, 255, 0.75);
        color: #1f1f1f;
        border: 1px solid rgba(130, 0, 0, 0.18);
      }
      .bioszen-error-report-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
      }
    "))
  ),
  tags$script(HTML("
    (function () {
      var autoHideMs = 4500;
      function hideLoader() {
        var el = document.getElementById('startup-loader');
        if (!el || el.classList.contains('startup-hidden')) return;
        el.classList.add('startup-hidden');
        setTimeout(function () {
          if (el) el.style.display = 'none';
        }, 400);
      }
      document.addEventListener('DOMContentLoaded', function () {
        setTimeout(hideLoader, 1200);
      });
      document.addEventListener('shiny:connected', hideLoader);
      document.addEventListener('shiny:error', hideLoader);
      window.addEventListener('load', function () {
        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
            typeof Shiny.shinyapp.isConnected === 'function' &&
            Shiny.shinyapp.isConnected()) {
          hideLoader();
        }
        setTimeout(hideLoader, 300);
      });
      setTimeout(hideLoader, autoHideMs);
    })();
  ")),
  tags$div(
    id = "startup-loader",
    tags$img(src = "logo_light.png", alt = "BIOSZEN"),
    tags$div(class = "startup-text", tr("startup_loading"))
  ),

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
  tags$header(
    class = "bioszen-topbar",
    tags$div(
      class = "bioszen-brand",
      uiOutput("logo_img"),
      tags$span(class = "bioszen-brand-title", tr("app_title"))
    ),
    tags$div(
      class = "bioszen-topbar-actions",
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
          tags$li(actionLink(
            "lang_es",
            tr("lang_es"),
            class = "dropdown-item",
            onclick = "if(window.BIOSZEN_applyLang){window.BIOSZEN_applyLang('es');} return false;"
          )),
          tags$li(actionLink(
            "lang_en",
            tr("lang_en"),
            class = "dropdown-item",
            onclick = "if(window.BIOSZEN_applyLang){window.BIOSZEN_applyLang('en');} return false;"
          ))
        )
      ),
      actionButton("btn_light", label = tr("theme_light"),
                   class = "btn btn-light", style = "color:#000;"),
      actionButton("btn_dark", label = tr("theme_dark"),
                   class = "btn btn-dark", style = "color:#fff;")
    )
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
        window.BIOSZEN_LANG = lang;
        localStorage.setItem('appLang', lang);
        if (window.BIOSZEN_translateStatic) {
          window.BIOSZEN_translateStatic(lang);
        }
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
        setTimeout(function () {
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('app_lang', lang, {priority: 'event'});
            Shiny.setInputValue('manual_lang', lang, {priority: 'event'});
          }
          if (window.BIOSZEN_translateStatic) {
            window.BIOSZEN_translateStatic(lang);
          }
          document.dispatchEvent(new Event('bioszen:lang-changed'));
        }, 350);
      }

      window.BIOSZEN_applyLang = applyLang;

      document.addEventListener('DOMContentLoaded', function(){
        var lg = localStorage.getItem('appLang') || 'en';
        applyLang(lg);
      });
    })();
  ")),
  tags$script(HTML("
    (function () {
      function getActiveLayout() {
        var panes = document.querySelectorAll('.bioszen-main-tabs .tab-content > .tab-pane.active');
        if (panes && panes.length) {
          for (var i = 0; i < panes.length; i++) {
            var layout = panes[i].querySelector('.bioszen-scroll-layout');
            if (layout) return layout;
          }
        }
        var allLayouts = document.querySelectorAll('.bioszen-scroll-layout');
        if (allLayouts && allLayouts.length) {
          for (var j = 0; j < allLayouts.length; j++) {
            var node = allLayouts[j];
            if (node.offsetParent !== null) return node;
          }
        }
        return null;
      }

      function getLabelText(id, fallback) {
        var el = document.getElementById(id);
        if (!el || !el.textContent) return fallback || '';
        var txt = el.textContent.trim();
        return txt || fallback || '';
      }

      function setSwitchLabel(state) {
        var btn = document.getElementById('mobile_switch_panel');
        if (!btn) return;
        var sidebarLbl = getLabelText('mobile_switch_label_sidebar', btn.textContent || '');
        var mainLbl = getLabelText('mobile_switch_label_main', sidebarLbl);
        btn.textContent = (state === 'main') ? mainLbl : sidebarLbl;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('mobile_panel_state', state, { priority: 'event' });
        }
      }

      function setPaneLabels(sidebarLabel, mainLabel) {
        var sidebarSpan = document.getElementById('mobile_switch_label_sidebar');
        var mainSpan = document.getElementById('mobile_switch_label_main');
        if (sidebarSpan && typeof sidebarLabel === 'string' && sidebarLabel.length) {
          sidebarSpan.textContent = sidebarLabel;
        }
        if (mainSpan && typeof mainLabel === 'string' && mainLabel.length) {
          mainSpan.textContent = mainLabel;
        }
        setTimeout(syncSwitchFromLayout, 10);
      }

      function refreshPlotAfterSwitch() {
        setTimeout(function () {
          var container = document.getElementById('plotInteractivo');
          var plotElem = null;
          if (container) {
            plotElem = container.querySelector('.js-plotly-plot') ||
              (container.classList && container.classList.contains('js-plotly-plot') ? container : null) ||
              container.querySelector('.plotly');
          }
          if (plotElem && window.Plotly && Plotly.Plots && typeof Plotly.Plots.resize === 'function') {
            try { Plotly.Plots.resize(plotElem); } catch (e) {}
          }
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('mobile_plot_refresh', Date.now(), { priority: 'event' });
          }
          window.dispatchEvent(new Event('resize'));
        }, 80);
      }

      function syncSwitchFromLayout() {
        var layout = getActiveLayout();
        if (!layout) return;
        var state = layout.classList.contains('show-main') ? 'main' : 'sidebar';
        setSwitchLabel(state);
      }

      function togglePane() {
        var layout = getActiveLayout();
        if (!layout) return false;
        if (layout.classList.contains('show-main')) {
          layout.classList.remove('show-main');
          layout.classList.add('show-sidebar');
          setSwitchLabel('sidebar');
        } else {
          layout.classList.remove('show-sidebar');
          layout.classList.add('show-main');
          setSwitchLabel('main');
          refreshPlotAfterSwitch();
        }
        window.scrollTo({ top: 0, behavior: 'smooth' });
        return false;
      }

      window.BIOSZEN_togglePane = togglePane;
      window.BIOSZEN_setPaneLabels = setPaneLabels;

      function bindSwitchButton() {
        var btn = document.getElementById('mobile_switch_panel');
        if (!btn) return;
        if (btn.dataset && btn.dataset.bioszenBound === '1') return;
        if (btn.dataset) btn.dataset.bioszenBound = '1';
        btn.addEventListener('click', function (ev) {
          ev.preventDefault();
          togglePane();
        });
      }

      document.addEventListener('shown.bs.tab', function () {
        setTimeout(syncSwitchFromLayout, 10);
      });

      document.addEventListener('bioszen:lang-changed', function () {
        bindSwitchButton();
        setTimeout(syncSwitchFromLayout, 420);
      });

      document.addEventListener('DOMContentLoaded', function () {
        bindSwitchButton();
        var prefLang = (localStorage.getItem('appLang') || 'en').toLowerCase();
        if (prefLang === 'es') {
          setPaneLabels('Ver gráficos', 'Panel de configuración');
        } else {
          setPaneLabels('View charts', 'Configuration panel');
        }
        setTimeout(syncSwitchFromLayout, 120);
      });
    })();
  ")),
  tags$script(HTML("
    (function(){
      var bioszenCheckboxGroupSyncRegistered = false;
      function registerBioszenCheckboxGroupSync(){
        if (bioszenCheckboxGroupSyncRegistered) return true;
        if (!window.Shiny || typeof Shiny.addCustomMessageHandler !== 'function') return false;
        bioszenCheckboxGroupSyncRegistered = true;
        Shiny.addCustomMessageHandler('bioszen-set-checkbox-group-values', function(payload){
          if (!payload || !payload.id) return;
          var id = String(payload.id);
          var selected = Array.isArray(payload.selected) ? payload.selected.map(String) : [];
          if (window.BIOSZEN_debouncedCheckboxGroups &&
              typeof window.BIOSZEN_debouncedCheckboxGroups.setFinal === 'function' &&
              window.BIOSZEN_debouncedCheckboxGroups.setFinal(id, selected)) {
            return;
          }
          var container = document.getElementById(id);
          var boxes = [];
          if (container) {
            boxes = Array.prototype.slice.call(container.querySelectorAll('input[type=\"checkbox\"]'));
          }
          if (!boxes.length) {
            boxes = Array.prototype.slice.call(document.getElementsByName(id));
          }
          boxes.forEach(function(box){
            var shouldCheck = selected.indexOf(String(box.value)) !== -1;
            if (box.checked !== shouldCheck) {
              box.checked = shouldCheck;
            }
          });
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue(id, selected, {priority: 'event'});
          }
        });
        return true;
      }
      if (!registerBioszenCheckboxGroupSync()) {
        document.addEventListener('shiny:connected', registerBioszenCheckboxGroupSync, {once: true});
        var attempts = 0;
        var retry = window.setInterval(function(){
          attempts += 1;
          if (registerBioszenCheckboxGroupSync() || attempts >= 40) {
            window.clearInterval(retry);
          }
        }, 250);
      }
      window.BIOSZEN_registerCheckboxGroupSync = registerBioszenCheckboxGroupSync;
    })();
  ")),
  tags$script(HTML("
    (function(){
      if (window.BIOSZEN_debouncedCheckboxGroups &&
          window.BIOSZEN_debouncedCheckboxGroups.version >= 5) {
        return;
      }
      var pendingTimers = {};
      var pendingSelections = {};
      var pendingKinds = {};
      var pendingStartedAt = {};
      var releasedSelections = {};
      var releasedKinds = {};
      var releasedExpiresAt = {};
      var mutationFrame = null;
      var reapplyInterval = null;
      var protectedChangeCount = 0;
      var protectedLastChange = null;
      var debounceMs = 280;
      var minHoldMs = 900;
      var holdMs = 3000;
      var maxHoldMs = 120000;
      document.documentElement.setAttribute('data-bioszen-selector-guard', '5');

      function isDebouncedGroup(name) {
        if (!name) return false;
        return name === 'showMedios' ||
          name === 'showGroups' ||
          name === 'rm_reps_all' ||
          name.indexOf('reps_') === 0 ||
          name.indexOf('reps_grp_') === 0 ||
          name.indexOf('reps_cur_') === 0 ||
          name.indexOf('qc_tech_rep_') === 0;
      }

      var protectedSelectorKeys = {
        scope: true,
        strain: true,
        tipo: true,
        param: true,
        ctrlMedium: true,
        corr_norm_target: true,
        corr_param_x: true,
        corr_param_y: true,
        corr_method: true,
        corrm_params: true,
        corrm_method: true,
        corrm_adjust: true,
        heat_params: true,
        heat_scale_mode: true,
        heat_orientation: true,
        heat_hclust_method: true,
        stackParams: true,
        errbar_stat: true,
        colorMode: true,
        adv_pal_type: true,
        adv_pal_filters: true,
        normTests: true,
        sigTest: true,
        postHoc: true,
        compMode: true,
        multitest_method: true,
        sig_mode: true,
        sig_auto_include: true,
        sig_auto_label_mode: true,
        sig_param: true,
        controlGroup: true,
        group1: true,
        group2: true,
        sig_group1: true,
        sig_group2: true
      };

      function isProtectedKey(key) {
        key = String(key || '');
        if (!key) return false;
        return !!protectedSelectorKeys[key] || isDebouncedGroup(key);
      }

      function isSelectControl(target) {
        if (!target || String(target.tagName || '').toLowerCase() !== 'select') return false;
        var key = String(target.id || target.name || '');
        if (!key) return false;
        if (target.type === 'file') return false;
        if (!isProtectedKey(key)) return false;
        return true;
      }

      function isRadioGroup(target) {
        return target && target.type === 'radio' && !!target.name && isProtectedKey(target.name);
      }

      function isCheckboxGroupControl(target) {
        if (!target || target.type !== 'checkbox' || !target.name) return false;
        if (!isProtectedKey(target.name)) return false;
        return isDebouncedGroup(String(target.name || '')) || checkboxesFor(target.name).length > 1;
      }

      function controlKey(target) {
        if (!target) return '';
        if (target.type === 'checkbox' || target.type === 'radio') {
          return String(target.name || '');
        }
        return String(target.id || target.name || '');
      }

      function controlKind(target) {
        if (isCheckboxGroupControl(target)) return 'checkbox-group';
        if (isRadioGroup(target)) return 'radio';
        if (isSelectControl(target)) return 'select';
        return '';
      }

      function controlKindFromKey(key, value) {
        key = String(key || '');
        if (!key) return '';
        var directEl = document.getElementById(key);
        if (directEl && directEl.type === 'file') return '';
        if (!isProtectedKey(key)) return '';
        if (isDebouncedGroup(key) || checkboxesFor(key).length > 1) return 'checkbox-group';
        if (radiosFor(key).length > 0) return 'radio';
        if (selectFor(key)) return 'select';
        if (Array.isArray(value)) return 'checkbox-group';
        return '';
      }

      function checkboxesFor(name) {
        return Array.prototype.slice.call(document.querySelectorAll('input[type=\"checkbox\"]'))
          .filter(function(box){ return String(box.name || '') === name; });
      }

      function radiosFor(name) {
        return Array.prototype.slice.call(document.querySelectorAll('input[type=\"radio\"]'))
          .filter(function(box){ return String(box.name || '') === name; });
      }

      function selectFor(key) {
        return document.getElementById(key) ||
          document.querySelector('select[name=\"' + String(key).replace(/\"/g, '\\\\\"') + '\"]');
      }

      function currentValueFor(key, kind) {
        if (kind === 'checkbox-group') return selectedValues(key);
        if (kind === 'radio') {
          var checked = radiosFor(key).filter(function(box){ return box.checked; })[0];
          return checked ? String(checked.value || '') : null;
        }
        if (kind === 'select') {
          var sel = selectFor(key);
          if (!sel) return null;
          var values = Array.prototype.slice.call(sel.options)
            .filter(function(opt){ return opt.selected; })
            .map(function(opt){ return String(opt.value || ''); });
          return sel.multiple ? values : (values[0] || '');
        }
        return null;
      }

      function selectedValues(name) {
        return checkboxesFor(name)
          .filter(function(box){ return box.checked; })
          .map(function(box){ return String(box.value || ''); });
      }

      function normalizeProtectedValue(value, kind) {
        if (kind === 'checkbox-group') {
          if (Array.isArray(value)) return value.map(String);
          if (value === null || typeof value === 'undefined' || value === false) return [];
          return [String(value)];
        }
        if (kind === 'select') {
          if (Array.isArray(value)) return value.map(String);
          return value === null || typeof value === 'undefined' ? '' : String(value);
        }
        return value === null || typeof value === 'undefined' ? '' : String(value);
      }

      function applySelection(key, value, kind) {
        if (kind === 'checkbox-group') {
          value = Array.isArray(value) ? value.map(String) : [];
          var lookup = {};
          value.forEach(function(item){ lookup[String(item)] = true; });
          checkboxesFor(key).forEach(function(box){
            var shouldCheck = !!lookup[String(box.value || '')];
            if (box.checked !== shouldCheck) {
              box.checked = shouldCheck;
            }
          });
          return;
        }
        if (kind === 'radio') {
          radiosFor(key).forEach(function(box){
            var shouldCheck = String(box.value || '') === String(value || '');
            if (box.checked !== shouldCheck) {
              box.checked = shouldCheck;
            }
          });
          return;
        }
        if (kind === 'select') {
          var sel = selectFor(key);
          if (!sel) return;
          var values = Array.isArray(value) ? value.map(String) : [String(value || '')];
          var selectLookup = {};
          values.forEach(function(item){ selectLookup[String(item)] = true; });
          Array.prototype.slice.call(sel.options).forEach(function(opt){
            var shouldSelect = !!selectLookup[String(opt.value || '')];
            if (opt.selected !== shouldSelect) {
              opt.selected = shouldSelect;
            }
          });
        }
      }

      function valuesEqual(a, b) {
        if (Array.isArray(a) || Array.isArray(b)) {
          a = Array.isArray(a) ? a.map(String).sort() : [String(a || '')];
          b = Array.isArray(b) ? b.map(String).sort() : [String(b || '')];
          return a.length === b.length && a.every(function(value, index){ return value === b[index]; });
        }
        return String(a || '') === String(b || '');
      }

      function shinyInputMatches(key, value) {
        if (!window.Shiny || !Shiny.shinyapp || !Shiny.shinyapp.$inputValues) return false;
        var commit = Shiny.shinyapp.$inputValues.bioszen_selector_commit;
        if (commit && String(commit.key || '') === String(key || '') &&
            valuesEqual(commit.value, value)) {
          return true;
        }
        if (!Object.prototype.hasOwnProperty.call(Shiny.shinyapp.$inputValues, key)) return false;
        return valuesEqual(Shiny.shinyapp.$inputValues[key], value);
      }

      function elementIsVisible(el) {
        if (!el || !el.isConnected) return false;
        var style = window.getComputedStyle(el);
        if (!style || style.display === 'none' || style.visibility === 'hidden' || style.opacity === '0') {
          return false;
        }
        var rect = el.getBoundingClientRect();
        return rect.width > 0 && rect.height > 0;
      }

      function pageIsBusy() {
        return document.documentElement.classList.contains('shiny-busy') ||
          document.body.classList.contains('shiny-busy') ||
          Array.prototype.slice.call(document.querySelectorAll(
            '.shiny-output-error, ' +
            '#plotInteractivo.recalculating, ' +
            '#plotInteractivoUI.recalculating, ' +
            '#showMediosUI.recalculating, ' +
            '#groupSel.recalculating, ' +
            '#repsStrainUI.recalculating, ' +
            '#repsGrpUI.recalculating, ' +
            '#qcTechSelectorsUI.recalculating'
          )).some(elementIsVisible);
      }

      function startReapplyLoop() {
        if (reapplyInterval !== null) return;
        reapplyInterval = window.setInterval(reapplyPendingSelections, 120);
      }

      function activeReleasedKeys() {
        var now = Date.now();
        Object.keys(releasedSelections).forEach(function(key){
          if ((releasedExpiresAt[key] || 0) <= now) {
            delete releasedSelections[key];
            delete releasedKinds[key];
            delete releasedExpiresAt[key];
          }
        });
        return Object.keys(releasedSelections);
      }

      function stopReapplyLoopIfIdle() {
        if ((Object.keys(pendingSelections).length || activeReleasedKeys().length) ||
            reapplyInterval === null) return;
        window.clearInterval(reapplyInterval);
        reapplyInterval = null;
      }

      function maybeRelease(key) {
        var kind = pendingKinds[key];
        if (!kind) return;
        var ageMs = Date.now() - (pendingStartedAt[key] || Date.now());
        var current = currentValueFor(key, kind);
        var settled = valuesEqual(current, pendingSelections[key]);
        if ((ageMs >= minHoldMs && settled && shinyInputMatches(key, pendingSelections[key])) ||
            (ageMs >= holdMs && !pageIsBusy() && settled) ||
            ageMs >= maxHoldMs) {
          var released = Array.isArray(pendingSelections[key]) ? pendingSelections[key].slice() : pendingSelections[key];
          applySelection(key, released, kind);
          releasedSelections[key] = Array.isArray(released) ? released.slice() : released;
          releasedKinds[key] = kind;
          releasedExpiresAt[key] = Date.now() + holdMs;
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('bioszen_selector_release', {
              key: key,
              kind: kind,
              value: released,
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
          delete pendingSelections[key];
          delete pendingKinds[key];
          delete pendingStartedAt[key];
          stopReapplyLoopIfIdle();
          return;
        }
        window.setTimeout(function(){ maybeRelease(key); }, 150);
      }

      function flushControl(key) {
        delete pendingTimers[key];
        var kind = pendingKinds[key];
        if (!kind) return;
        var selected = Object.prototype.hasOwnProperty.call(pendingSelections, key) ?
          pendingSelections[key] :
          currentValueFor(key, kind);
        if (Array.isArray(selected)) selected = selected.slice();
        applySelection(key, selected, kind);
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('bioszen_selector_commit', {
            key: key,
            kind: kind,
            value: selected,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
          Shiny.setInputValue(key, selected, {priority: 'event'});
        }
        var ageMs = Date.now() - (pendingStartedAt[key] || Date.now());
        var firstReleaseCheckMs = Math.max(50, minHoldMs - ageMs);
        window.setTimeout(function(){ maybeRelease(key); }, firstReleaseCheckMs);
      }

      function reapplyPendingSelections() {
        mutationFrame = null;
        Object.keys(pendingSelections).forEach(function(key){
          applySelection(key, pendingSelections[key], pendingKinds[key]);
        });
        activeReleasedKeys().forEach(function(key){
          if (!Object.prototype.hasOwnProperty.call(pendingSelections, key)) {
            applySelection(key, releasedSelections[key], releasedKinds[key]);
          }
        });
        stopReapplyLoopIfIdle();
      }

      function scheduleReapply() {
        if (mutationFrame !== null) return;
        mutationFrame = window.requestAnimationFrame(reapplyPendingSelections);
      }

      function queueProtectedSelection(key, value, kind) {
        if (!key) return;
        protectedChangeCount += 1;
        delete releasedSelections[key];
        delete releasedKinds[key];
        delete releasedExpiresAt[key];
        pendingKinds[key] = kind;
        pendingSelections[key] = normalizeProtectedValue(value, kind);
        pendingStartedAt[key] = Date.now();
        document.documentElement.setAttribute('data-bioszen-selector-change-count', String(protectedChangeCount));
        document.documentElement.setAttribute('data-bioszen-selector-last-key', key);
        protectedLastChange = {
          key: key,
          kind: kind,
          value: Array.isArray(pendingSelections[key]) ? pendingSelections[key].slice() : pendingSelections[key]
        };
        applySelection(key, pendingSelections[key], kind);
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('bioszen_selector_pending', {
            key: key,
            kind: kind,
            value: Array.isArray(pendingSelections[key]) ? pendingSelections[key].slice() : pendingSelections[key],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
        if (pendingTimers[key]) window.clearTimeout(pendingTimers[key]);
        pendingTimers[key] = window.setTimeout(function(){ flushControl(key); }, debounceMs);
        startReapplyLoop();
        scheduleReapply();
      }

      function handleProtectedControlChange(ev) {
        var target = ev.target;
        if (!target || target.type === 'file') return;
        var kind = controlKind(target);
        if (!kind) return;
        var key = controlKey(target);
        if (!key) return;
        ev.stopImmediatePropagation();
        queueProtectedSelection(key, currentValueFor(key, kind), kind);
      }

      window.addEventListener('change', handleProtectedControlChange, true);
      document.addEventListener('change', handleProtectedControlChange, true);
      if (window.MutationObserver) {
        new MutationObserver(function(){
          if (Object.keys(pendingSelections).length || activeReleasedKeys().length) scheduleReapply();
        }).observe(document.documentElement, {
          childList: true,
          subtree: true,
          attributes: true,
          attributeFilter: ['checked', 'class', 'disabled']
        });
      }

      window.BIOSZEN_debouncedCheckboxGroups = {
        version: 5,
        flush: function(name) {
          name = String(name || '');
          if (!pendingKinds[name] && !isDebouncedGroup(name)) return false;
          if (pendingTimers[name]) window.clearTimeout(pendingTimers[name]);
          flushControl(name);
          return true;
        },
        pending: function(name) {
          name = String(name || '');
          return Object.prototype.hasOwnProperty.call(pendingSelections, name) ?
            pendingSelections[name].slice() :
            null;
        },
        setFinal: function(name, selected) {
          name = String(name || '');
          if (!name || !isDebouncedGroup(name)) return false;
          selected = normalizeProtectedValue(selected, 'checkbox-group');
          if (pendingTimers[name]) window.clearTimeout(pendingTimers[name]);
          delete pendingTimers[name];
          delete pendingSelections[name];
          delete pendingKinds[name];
          delete pendingStartedAt[name];
          delete releasedSelections[name];
          delete releasedKinds[name];
          delete releasedExpiresAt[name];
          applySelection(name, selected, 'checkbox-group');
          releasedSelections[name] = selected.slice();
          releasedKinds[name] = 'checkbox-group';
          releasedExpiresAt[name] = Date.now() + holdMs;
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('bioszen_selector_commit', {
              key: name,
              kind: 'checkbox-group',
              value: selected.slice(),
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
            Shiny.setInputValue(name, selected, {priority: 'event'});
          }
          startReapplyLoop();
          scheduleReapply();
          return true;
        },
        debug: function() {
          return {
            changeCount: protectedChangeCount,
            lastChange: protectedLastChange,
            pendingKeys: Object.keys(pendingSelections),
            pendingKinds: Object.assign({}, pendingKinds),
            releasedKeys: activeReleasedKeys()
          };
        },
        selectedValues: selectedValues
      };
    })();
  ")),
  tags$script(HTML("
    (function(){
      document.addEventListener('change', function(ev){
        var target = ev.target;
        if (!target || (target.id !== 'toggleMedios' && target.id !== 'toggleGroups')) return;
        if (!ev.isTrusted) return;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue(target.id + '_user_change', {
            value: !!target.checked,
            nonce: Date.now()
          }, {priority: 'event'});
        }
      }, true);
    })();
  ")),
  # ------ Explicit save-location handling ----------------------------------
  tags$script(HTML("
    (function(){
      function cleanName(value){
        value = String(value || '').trim().replace(/[\\/:*?\"<>|]+/g, '_');
        return value || 'BIOSZEN_download';
      }

      function extensionFor(link){
        var id = String(link.id || '').toLowerCase();
        var text = String(link.textContent || '').toLowerCase();
        if (/pptx|ppt/.test(id + ' ' + text)) return '.pptx';
        if (/pdf|manual/.test(id + ' ' + text)) return '.pdf';
        if (/png/.test(id + ' ' + text)) return '.png';
        if (/zip|bundle|refs|growth/.test(id + ' ' + text)) return '.zip';
        if (/csv/.test(id + ' ' + text)) return '.csv';
        return '.xlsx';
      }

      function plotSuggestedName(link, ext){
        if (!/^downloadPlot/i.test(link.id || '')) return null;
        var scope = document.getElementById('scope');
        var strain = document.getElementById('strain');
        var type = document.getElementById('tipo');
        var prefix = scope && scope.value === 'Combinado' ? 'Combinado' :
          (strain && strain.value ? strain.value : 'Grafico');
        return cleanName(prefix + '_' + (type && type.value ? type.value : 'plot')) + ext;
      }

      function suggestedName(link){
        var ext = extensionFor(link);
        var plotName = plotSuggestedName(link, ext);
        if (plotName) return plotName;
        var names = {
          downloadExcel: 'BIOSZEN_data.xlsx',
          downloadMetadata: 'BIOSZEN_metadata.xlsx',
          downloadStats: 'BIOSZEN_statistics.xlsx',
          downloadBundleZip: 'BIOSZEN_bundle.zip',
          downloadGrowthZip: 'BIOSZEN_growth_results.zip',
          download_refs: 'BIOSZEN_reference_files.zip',
          downloadHeatClusters: 'BIOSZEN_heatmap_clusters.xlsx',
          download_corr_adv: 'BIOSZEN_correlations.xlsx',
          downloadMergedPlatemap: 'BIOSZEN_merged_platemap.xlsx',
          downloadMergedPlatemapLatest: 'BIOSZEN_merged_platemap.xlsx',
          downloadMergedCurves: 'BIOSZEN_merged_curves.xlsx',
          downloadMergedCurvesLatest: 'BIOSZEN_merged_curves.xlsx',
          dl_combo_png: 'BIOSZEN_composition.png',
          dl_combo_pdf: 'BIOSZEN_composition.pdf',
          dl_combo_pptx: 'BIOSZEN_composition.pptx',
          dl_combo_meta: 'BIOSZEN_composition_metadata.xlsx'
        };
        if (names[link.id]) return names[link.id];
        return cleanName(link.textContent || link.id || 'BIOSZEN_download') + ext;
      }

      function pickerOptions(link){
        var name = suggestedName(link);
        var ext = name.slice(name.lastIndexOf('.')).toLowerCase();
        var mime = {
          '.png':'image/png', '.pdf':'application/pdf',
          '.pptx':'application/vnd.openxmlformats-officedocument.presentationml.presentation',
          '.xlsx':'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
          '.zip':'application/zip', '.csv':'text/csv'
        }[ext] || 'application/octet-stream';
        var accept = {};
        accept[mime] = [ext];
        var lang = window.BIOSZEN_LANG || 'en';
        var description = window.BIOSZEN_lookupTranslation &&
          window.BIOSZEN_lookupTranslation('download_file_description', lang);
        return {suggestedName:name, types:[{description:description || 'BIOSZEN file', accept:accept}]};
      }

      function triggerNativeDownload(link){
        link.dataset.bioszenDownloadBypass = '1';
        link.click();
        setTimeout(function(){ delete link.dataset.bioszenDownloadBypass; }, 0);
      }

      function canUseSaveFilePicker(){
        if (typeof window.showSaveFilePicker !== 'function' || !window.isSecureContext) {
          return false;
        }
        try {
          // The File System Access API is forbidden from cross-origin frames,
          // including the embedded RStudio Viewer on Windows and macOS.
          return window.self === window.top;
        } catch (error) {
          return false;
        }
      }

      function shouldUseNativeDownload(error){
        if (!error) return false;
        var name = String(error.name || '');
        var message = String(error.message || '').toLowerCase();
        return name === 'SecurityError' || name === 'NotAllowedError' ||
          message.indexOf('cross origin') >= 0 ||
          message.indexOf('cross-origin') >= 0 ||
          message.indexOf('not allowed to show a file picker') >= 0;
      }

      function fetchDownloadBlob(link, attempts){
        attempts = Math.max(1, Number(attempts || 1));
        function fetchOnce(){
          var href = String(link.getAttribute('href') || '').trim();
          if (!href || href === '#') {
            return Promise.reject(new Error('Download link is not ready.'));
          }
          return fetch(href, {credentials:'same-origin', cache:'no-store'}).then(function(response){
            if (!response.ok) throw new Error('HTTP ' + response.status);
            return response.blob();
          }).then(function(blob){
            if (!blob || !Number.isFinite(blob.size) || blob.size <= 0) {
              throw new Error('The generated file is empty.');
            }
            return blob;
          });
        }
        function attempt(remaining){
          return fetchOnce().catch(function(error){
            if (remaining <= 1) throw error;
            return new Promise(function(resolve){ setTimeout(resolve, 500); })
              .then(function(){ return attempt(remaining - 1); });
          });
        }
        return attempt(attempts);
      }

      function saveDownloadLink(link, pickerPromise){
        var blobPromise = fetchDownloadBlob(link, 3).then(
          function(blob){ return {blob:blob, error:null}; },
          function(error){ return {blob:null, error:error}; }
        );
        return pickerPromise.then(function(handle){
          if (!handle) return false;
          return blobPromise.then(function(result){
            if (result.error) throw result.error;
            var blob = result.blob;
            return handle.createWritable().then(function(writer){
              return writer.write(blob)
                .then(function(){ return writer.close(); })
                .catch(function(error){
                  var abortResult = typeof writer.abort === 'function' ? writer.abort() : null;
                  return Promise.resolve(abortResult)
                    .catch(function(){ return null; })
                    .then(function(){ throw error; });
                });
            });
          }).then(function(){ return true; });
        });
      }

      document.addEventListener('click', function(event){
        var link = event.target && event.target.closest ?
          event.target.closest('a.shiny-download-link') : null;
        if (!link || link.dataset.bioszenDownloadBypass === '1' || !event.isTrusted) return;
        var canPick = canUseSaveFilePicker();
        if (!canPick) return;

        event.preventDefault();
        event.stopImmediatePropagation();

        var pickerPromise;
        try {
          pickerPromise = window.showSaveFilePicker(pickerOptions(link))
            .catch(function(error){
              if (error && error.name === 'AbortError') return null;
              if (shouldUseNativeDownload(error)) {
                triggerNativeDownload(link);
                return null;
              }
              throw error;
            });
        } catch (error) {
          triggerNativeDownload(link);
          return;
        }

        saveDownloadLink(link, pickerPromise).catch(function(error){
          var lang = window.BIOSZEN_LANG || 'en';
          var message = window.BIOSZEN_lookupTranslation &&
            window.BIOSZEN_lookupTranslation('download_save_failed', lang);
          window.alert((message || 'The file could not be saved.') + '\\n' + error.message);
        });
      }, true);
    })();
  ")),
  tags$script(HTML("
    (function(){
      function fallbackCopy(text){
        return new Promise(function(resolve, reject){
          try {
            var area = document.createElement('textarea');
            area.value = text;
            area.setAttribute('readonly', '');
            area.style.position = 'fixed';
            area.style.left = '-9999px';
            document.body.appendChild(area);
            area.select();
            var ok = document.execCommand('copy');
            document.body.removeChild(area);
            if (ok) resolve(); else reject(new Error('Clipboard copy was not accepted'));
          } catch (error) {
            reject(error);
          }
        });
      }

      function copyDiagnostic(text){
        text = String(text || '');
        if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
          return navigator.clipboard.writeText(text).catch(function(){
            return fallbackCopy(text);
          });
        }
        return fallbackCopy(text);
      }

      function notifyCopy(ok, detail){
        if (!window.Shiny || typeof Shiny.setInputValue !== 'function') return;
        Shiny.setInputValue('bioszen_error_copy_result', {
          ok: !!ok,
          detail: String(detail || ''),
          ts: Date.now()
        }, {priority: 'event'});
      }

      Shiny.addCustomMessageHandler('bioszen-copy-error-report', function(msg){
        copyDiagnostic(msg && msg.text).then(function(){
          notifyCopy(true, 'copied');
        }).catch(function(error){
          notifyCopy(false, error && error.message ? error.message : String(error));
        });
      });

      Shiny.addCustomMessageHandler('bioszen-email-error-report', function(msg){
        msg = msg || {};
        var recipient = String(msg.to || 'bioszenf+bugs@gmail.com');
        var subject = String(msg.subject || 'BIOSZEN error report');
        var report = String(msg.report || '');
        var prompt = String(msg.prompt || 'What were you doing before the error occurred?\\n[Please describe here]\\n\\n').replace(/\\\\n/g, '\\n');
        var body = prompt + '\\nDiagnostic report:\\n' + report;
        var encoded = 'mailto:' + recipient + '?subject=' + encodeURIComponent(subject) + '&body=' + encodeURIComponent(body);
        var useFallback = encoded.length > 1800;
        var openMail = function(mailBody){
          window.location.href = 'mailto:' + recipient + '?subject=' + encodeURIComponent(subject) + '&body=' + encodeURIComponent(mailBody);
        };

        if (!useFallback) {
          openMail(body);
          return;
        }

        copyDiagnostic(report).then(function(){
          notifyCopy(true, 'copied-for-email');
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('bioszen_error_mail_fallback', {used: true, ts: Date.now()}, {priority: 'event'});
          }
          openMail(prompt + '\\n' + String(msg.fallback || 'The complete diagnostic report was copied. Please paste it below.').replace(/\\\\n/g, '\\n'));
        }).catch(function(error){
          notifyCopy(false, error && error.message ? error.message : String(error));
          openMail(prompt + '\\n' + String(msg.fallback || 'Please copy and paste the diagnostic report from BIOSZEN below.').replace(/\\\\n/g, '\\n'));
        });
      });
    })();
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
        scale:  msg.scale || 1
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
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyStaticPlotToClipboard', function(msg){
      var elementId = msg.elementId || 'comboPreview';
      var failId    = msg.fail    || 'combo_copy_error';
      var successId = msg.success || 'combo_copy_success';
      var node = document.getElementById(elementId);
      if (!node) {
        Shiny.setInputValue(failId, {message:'Static plot container not found', ts: Date.now()}, {priority:'event'});
        return;
      }

      var img = null;
      if (node.tagName && node.tagName.toLowerCase() === 'img') {
        img = node;
      } else {
        img = node.querySelector('img');
      }

      if (!img || !img.src) {
        Shiny.setInputValue(failId, {message:'Static image source not found', ts: Date.now()}, {priority:'event'});
        return;
      }

      var downloadNode = document.getElementById(msg.downloadId || 'dl_combo_png');
      var downloadHref = downloadNode && downloadNode.href ? downloadNode.href : '';
      var src = downloadHref || img.src;
      var notifySuccess = function(mode) {
        Shiny.setInputValue(successId, {message: mode || 'copied', ts: Date.now()}, {priority:'event'});
      };
      var notifyFail = function(err) {
        var message = (err && err.message) ? err.message : String(err || 'Unknown clipboard error');
        Shiny.setInputValue(failId, {message: message, ts: Date.now()}, {priority:'event'});
      };

      fetch(src, { credentials: 'same-origin' })
        .then(function(res){
          if (!res.ok) throw new Error('Could not read rendered image');
          return res.blob();
        })
        .then(function(blob){
          if (navigator.clipboard && window.ClipboardItem) {
            return navigator.clipboard.write([new ClipboardItem({ [blob.type || 'image/png']: blob })])
              .then(function(){ notifySuccess('copied'); })
              .catch(function(err){ notifyFail(err); });
          }

          var a = document.createElement('a');
          a.href = src;
          a.download = 'composition.png';
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          notifySuccess('downloaded');
        })
        .catch(function(err){
          notifyFail(err);
        });
    });
  ")),
  tags$head(
    tags$style(HTML("
      .plot-loading-wrap {
        position: relative;
      }
      .plot-loading-indicator {
        display: none;
        position: absolute;
        top: 10px;
        right: 10px;
        z-index: 25;
        align-items: center;
        gap: 8px;
        padding: 7px 10px;
        border-radius: 10px;
        background: rgba(18, 24, 32, 0.78);
        color: #fff;
        font-size: 12px;
        pointer-events: none;
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.24);
      }
      .plot-loading-wrap.is-loading .plot-loading-indicator {
        display: inline-flex;
      }
    "))
  ),
  tags$script(HTML("
    (function () {
      var plotLoadingTimer = null;
      function setPlotLoading(on) {
        var wrap = document.getElementById('plot-loading-wrap');
        if (!wrap) return;
        if (on) wrap.classList.add('is-loading');
        else wrap.classList.remove('is-loading');
      }
      function schedulePlotLoading() {
        if (plotLoadingTimer) clearTimeout(plotLoadingTimer);
        plotLoadingTimer = setTimeout(function () {
          plotLoadingTimer = null;
          setPlotLoading(true);
        }, 3000);
      }
      function clearPlotLoading() {
        if (plotLoadingTimer) {
          clearTimeout(plotLoadingTimer);
          plotLoadingTimer = null;
        }
        setPlotLoading(false);
      }

      $(document).on('shiny:outputinvalidated', function (ev) {
        if ((ev && ev.name) === 'plotInteractivo') schedulePlotLoading();
      });

      $(document).on('shiny:value shiny:error', function (ev) {
        if ((ev && ev.name) === 'plotInteractivo') clearPlotLoading();
      });

      $(document).on('shiny:disconnected', function () {
        clearPlotLoading();
      });
    })();
  ")),

  uiOutput("app_announcement"),
  tags$div(
    class = "bioszen-pane-fab",
    actionButton(
      "mobile_switch_panel",
      label = tr("mobile_switch_view_graphics"),
      class = "btn bioszen-pane-switch-btn"
    ),
    tags$span(id = "mobile_switch_label_sidebar", style = "display:none;", tr("mobile_switch_view_graphics")),
    tags$span(id = "mobile_switch_label_main", style = "display:none;", tr("mobile_switch_config_panel"))
  ),
  # --- Inicio de pesta?as principales ---
  tags$div(
    class = "bioszen-main-tabs",
    tabsetPanel(id = "mainTabs", type = "tabs",
    tabPanel(title = HTML(as.character(tr("tab_plots"))), value = "tab_plots",
             # --------------------------------------------------------------------------
             #  Layout principal: barra lateral + área de trazado/tablas
             # --------------------------------------------------------------------------
             tags$div(
               class = "bioszen-top-quickbar",
               tags$div(
                 class = "dropdown",
                  actionButton(
                    "quick_options_btn",
                    tr("quick_options_title"),
                    class = "btn bioszen-quick-fab-btn dropdown-toggle",
                    `data-bs-toggle` = "dropdown",
                    `aria-expanded` = "false"
                  ),
                 tags$ul(
                   class = "dropdown-menu dropdown-menu-end",
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "plot_setup_core", tr("quick_setup"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "quick_target_plot_type", tr("quick_plot_type"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "quick_target_normalization", tr("quick_normalization"))),
                   tags$li(tags$hr(class = "dropdown-divider")),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "section_chart_options", tr("quick_chart_options"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "section_appearance", tr("quick_appearance"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "section_filters", tr("quick_filters"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "section_analysis", tr("quick_analysis"))),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "section_export", tr("quick_export"))),
                   tags$li(tags$hr(class = "dropdown-divider")),
                   tags$li(tags$a(href = "#", class = "dropdown-item bioszen-quick-btn", `data-target` = "plot_setup_core", tr("quick_top")))
                 )
               )
             ),
             tags$div(
               class = "bioszen-scroll-layout show-sidebar",
               sidebarLayout(
               
               ###########################################################################
               #  SIDEBAR ----------------------------------------------------------------
               ###########################################################################
              sidebarPanel(
                width = 4,
                style = "position:sticky; top:86px; height:calc(100vh - 98px); overflow-y:auto;",
                tags$div(
                  class = "bioszen-sidebar-content",
                  tags$div(
                    class = "bioszen-datafile-row",
                    tags$div(
                      class = "bioszen-datafile-main",
                      fileInput('dataFile',  tr("file_data"), accept = c('.xlsx', '.xls', '.csv'))
                    ),
                    tags$div(
                      class = "bioszen-datafile-arrow",
                      actionButton(
                        "openMergeModal",
                        label = NULL,
                        icon = icon("chevron-right"),
                        class = "btn btn-outline-secondary",
                        title = label_to_text(tr("merge_open")),
                        `aria-label` = label_to_text(tr("merge_open"))
                      )
                    )
                  ),
                 tags$div(
                   id = "curve_upload_section",
                   tags$div(
                     class = "bioszen-datafile-row",
                     tags$div(
                       class = "bioszen-datafile-main",
                       fileInput('curveFile', tr("file_curves"), accept = c('.xlsx', '.xls', '.csv'))
                     ),
                     tags$div(
                       class = "bioszen-datafile-arrow",
                       uiOutput("curveMergeArrowUI")
                     )
                   ),
                   uiOutput("mergedCurveDownloadUI")
                 ),
                 fileInput('metaFiles', tr("file_meta"),    accept = '.xlsx', multiple = TRUE),
                 uiOutput("mergedPlatemapDownloadUI"),

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

                 # ---- Guided Core Setup ------------------------------------------------
                 tags$div(
                   id = "plot_setup_core",
                   class = "bioszen-guided-core",
                   h4(tr("guided_setup")),
                   radioButtons(
                     "scope",
                     tr("scope_label"),
                     choices = named_choices(
                       c("Por Cepa", "Combinado"),
                       list(tr("scope_by_strain"), tr("scope_combined"))
                     ),
                     selected = "Por Cepa"
                   ),
                   conditionalPanel(
                     condition = "input.scope=='Por Cepa'",
                     selectizeInput("strain", tr("strain_label"), choices = NULL, selected = NULL)
                   ),
                   uiOutput("paramSel"),
                   tags$div(
                     id = "quick_target_plot_type",
                     radioButtons(
                       "tipo",
                       tr("plot_type_label"),
                        choices = named_choices(
                          c("Boxplot", "Barras", "Violin", "Curvas", "Apiladas", "Correlacion", "Heatmap", "MatrizCorrelacion"),
                          list(
                            tr("plot_boxplot"),
                            tr("plot_bars"),
                            tr("plot_violin"),
                            tr("plot_curves"),
                            tr("plot_stacked"),
                            tr("plot_correlation"),
                            tr("plot_heatmap"),
                            tr("plot_corr_matrix")
                          )
                        ),
                       selected = "Boxplot"
                     )
                   ),
                   tags$div(
                     id = "quick_target_normalization",
                     checkboxInput('doNorm', tr("norm_by_control"), FALSE),
                     uiOutput('ctrlSelUI'),
                     conditionalPanel(
                       condition = "input.tipo == 'Correlacion' && input.doNorm",
                       radioButtons(
                         "corr_norm_target", tr("corr_norm_target"),
                         choices = named_choices(
                           c("both", "x_only", "y_only"),
                           tr_text(c("corr_norm_both", "corr_norm_x", "corr_norm_y"), i18n_lang)
                         ),
                         selected = "both", inline = TRUE
                       )
                     )
                   )
                 ),
                  
                 hr(), tags$div(id = "section_chart_options", h4(tr("guided_chart_options"))),
                 # ---------- SECCION "Apiladas" ----------------------------------------
                  conditionalPanel(
                    condition = "input.tipo == 'Apiladas'",
                    h4(tr("stack_settings")),
                    uiOutput("stackParamsUI"),
                    textInput("orderStack", tr("stack_order"), ""),
                    checkboxInput("showErrBars", tr("stack_show_errbars"), TRUE),
                    checkboxInput("errbar_param_color", tr("stack_errbar_param_color"), FALSE),
                    checkboxInput("stack_outline_only", tr("stack_outline_only"), FALSE)
                 ),
                 
                 # ---------- SECCION "Correlacion" -------------------------------------
                 conditionalPanel(
                   condition = "input.tipo == 'Correlacion'",
                   h4(tr("corr_settings")),
                    fluidRow(
                      column(
                        6,
                        selectizeInput(
                          "corr_param_x",
                          tr("corr_param_x"),
                          choices = NULL,
                          selected = NULL
                        )
                      ),
                      column(
                        6,
                        selectizeInput(
                          "corr_param_y",
                          tr("corr_param_y"),
                          choices = NULL,
                          selected = NULL
                        )
                      )
                    ),
                   radioButtons(
                     "corr_method", tr("corr_method_label"),
                    choices = named_choices(
                      c("pearson", "spearman", "kendall"),
                      list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
                    ),
                     selected = "pearson", inline = TRUE
                   ),
                   checkboxInput("corr_show_line",   tr("corr_show_line"), TRUE),
                   checkboxInput("corr_show_labels", tr("corr_show_labels"), TRUE),
                   checkboxInput("corr_show_r",      tr("corr_show_r"), TRUE),
                   checkboxInput("corr_show_p",      tr("corr_show_p"), TRUE),
                   checkboxInput("corr_show_r2",     tr("corr_show_r2"), FALSE),
                   checkboxInput("corr_show_eq",     tr("corr_show_eq"), FALSE),
                   checkboxInput("corr_show_ci",     tr("corr_show_ci"), FALSE),
                   radioButtons(
                     "corr_ci_style", tr("corr_ci_style"),
                     choices = named_choices(
                       c("band", "dashed"),
                       list(tr("corr_ci_band"), tr("corr_ci_dashed"))
                     ),
                     selected = "band", inline = TRUE
                   ),
                   numericInput(
                     "corr_ci_level",
                     tr("corr_ci_level"),
                     value = 0.95,
                     min = 0.5,
                     max = 0.999,
                     step = 0.01
                   ),
                   
                    textInput("corr_xlab", tr("corr_xlab"), value = ""),
                    textInput("corr_ylab", tr("corr_ylab"), value = ""),
                    
                    fluidRow(
                      column(6, numericInput("xmin_corr", tr("corr_xmin"), value = 0)),
                      column(6, numericInput("xmax_corr", tr("corr_xmax"), value = 0))
                    ),
                    fluidRow(
                      column(6, numericInput("xbreak_corr", tr("corr_xbreak"), value = 1,  min = 0.001)),
                      column(6, numericInput("ybreak_corr", tr("corr_ybreak"), value = 1,  min = 0.001))
                    ),
                    tags$p(class = "qc-help", tr("interval_hint_general")),
                    fluidRow(
                      column(6, numericInput("ymin_corr", tr("corr_ymin"), value = 0)),
                      column(6, numericInput("ymax_corr", tr("corr_ymax"), value = 0)),
                      column(6, numericInput("corr_label_size",
                                             tr("corr_label_size"), value = 4, min = 1))
                    ),
                    accordion(
                      id = "corrAdvancedPanel",
                      open = FALSE,
                      multiple = TRUE,
                      accordion_panel_safe(
                        tr("corr_adv_title"),
                        tags$p(class = "qc-help", tr("corr_adv_hint_click")),
                        selectizeInput(
                          "corr_adv_anchor",
                          tr("corr_adv_anchor"),
                          choices = NULL,
                          selected = NULL
                        ),
                        radioButtons(
                          "corr_adv_method",
                          tr("corr_method_label"),
                          choices = named_choices(
                            c("pearson", "spearman", "kendall"),
                            list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
                          ),
                          selected = "pearson",
                          inline = TRUE
                        ),
                        radioButtons(
                          "corr_adv_data_mode",
                          tr("corr_adv_data_mode"),
                          choices = named_choices(
                            c("raw", "norm_both", "norm_x", "norm_y"),
                            list(
                              tr("corr_adv_data_raw"),
                              tr("corr_adv_data_norm_both"),
                              tr("corr_adv_data_norm_x"),
                              tr("corr_adv_data_norm_y")
                            )
                          ),
                          selected = "raw",
                          inline = TRUE
                        ),
                        checkboxInput("corr_adv_sig_only", tr("corr_adv_sig_only"), FALSE),
                        numericInput(
                          "corr_adv_pvalue_max",
                          tr("corr_adv_pvalue_max"),
                          value = 0.05,
                          min = 0,
                          max = 1,
                          step = 0.001
                        ),
                        selectInput(
                          "corr_adv_direction",
                          tr("corr_adv_direction"),
                          choices = named_choices(
                            c("all", "positive", "negative"),
                            list(
                              tr("corr_adv_direction_all"),
                              tr("corr_adv_direction_pos"),
                              tr("corr_adv_direction_neg")
                            )
                          ),
                          selected = "all"
                        ),
                        sliderInput(
                          "corr_adv_r_filter",
                          tr("corr_adv_r_filter"),
                          min = -1,
                          max = 1,
                          value = c(-1, 1),
                          step = 0.01
                        ),
                        actionButton(
                          "corr_adv_run",
                          tr("corr_adv_run"),
                          class = "btn btn-primary w-100"
                        ),
                        br(),
                        uiOutput("corr_adv_summary"),
                        conditionalPanel(
                          condition = "output.corrAdvDownloadReady == 'true'",
                          downloadButton(
                            "download_corr_adv",
                            tr("corr_adv_download_all"),
                            class = "btn btn-default w-100"
                          )
                        ),
                        br(), br(),
                        tags$strong(tr("corr_adv_results")),
                        DTOutput("corr_adv_table"),
                        style = "info"
                      )
                    )
                  ),

                 # ---------- SECCION "Matriz de Correlacion" ---------------------------
                 conditionalPanel(
                   condition = "input.tipo == 'MatrizCorrelacion'",
                   h4(tr("corr_matrix_settings")),
                   selectizeInput(
                     "corrm_params",
                     tr("corr_matrix_params"),
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options = list(plugins = list("remove_button"))
                   ),
                   radioButtons(
                     "corrm_method",
                     tr("corr_method_label"),
                     choices = named_choices(
                       c("pearson", "spearman", "kendall"),
                       list(tr("corr_method_pearson"), tr("corr_method_spearman"), tr("corr_method_kendall"))
                     ),
                     selected = "spearman",
                     inline = TRUE
                   ),
                   radioButtons(
                     "corrm_adjust",
                     tr("multitest_method"),
                     choices = named_choices(
                       c("holm", "fdr", "bonferroni", "none"),
                       list(
                         tr("multitest_holm"),
                         tr("multitest_fdr"),
                         tr("multitest_bonferroni"),
                         tr("multitest_none")
                       )
                     ),
                    selected = "none",
                     inline = TRUE
                   ),
                   checkboxInput("corrm_show_sig", tr("corr_matrix_show_sig"), TRUE),
                   checkboxInput("corrm_order_profile", tr("corr_matrix_order_profile"), FALSE)
                 ),

                 # ---------- SECCION "Heatmap" -----------------------------------------
                 heatmap_controls_ui(),
                 
                
                 # ---------- Eje Y para Boxplot/Barras ---------------------------------
                  conditionalPanel(
                    condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                    h4(tr("y_axis_settings")),
                    tags$div(
                      class = "bioszen-axis-input-row",
                      fluidRow(
                        column(6, numericInput('ymax',   tr("y_max"),  value = 0, min = 0)),
                        column(6, numericInput('ybreak', tr("y_interval"),  value = 1, min = 0.001))
                      )
                    ),
                    tags$p(class = "qc-help", tr("interval_hint_general"))
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
                   tags$p(class = "qc-help", tr("interval_hint_general")),
                   fluidRow(
                     column(6, textInput('cur_xlab', tr("curves_xlab"), '')),
                     column(6, textInput('cur_ylab', tr("curves_ylab"), ''))
                   ),
                  numericInput('curve_lwd', tr("curves_linewidth"), value = 1.5, min = 0.5, step = 0.1),
                  numericInput('curve_pt_size', tr("curves_point_size"), value = 3.3, min = 0.1, step = 0.1),
                  checkboxInput("cur_show_ci", tr("curves_show_ci"), FALSE),
                  radioButtons(
                    "cur_ci_style", tr("curves_ci_style"),
                    choices = named_choices(
                      c("ribbon", "errorbar"),
                      list(tr("curves_ci_ribbon"), tr("curves_ci_errorbar"))
                    ),
                    selected = "ribbon",
                    inline = TRUE
                  ),
                  checkboxInput("cur_show_reps", tr("curves_show_reps"), FALSE),
                  sliderInput("cur_rep_alpha", tr("curves_rep_alpha"),
                              min = 0.05, max = 1, value = 0.25, step = 0.05),
                  radioButtons(
                    "curve_geom", tr("curves_geom"),
                    choices = named_choices(
                      c("line_points", "line_only"),
                      list(tr("curves_geom_line_points"), tr("curves_geom_line_only"))
                    ),
                    selected = "line_points",
                    inline = TRUE
                  ),
                  radioButtons(
                    "curve_color_mode", tr("curves_color_mode"),
                    choices = named_choices(
                      c("by_group", "single"),
                      list(tr("curves_color_by_group"), tr("curves_color_single"))
                    ),
                    selected = "by_group",
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.curve_color_mode == 'single'",
                    tags$label(`for` = "curve_single_color", tr("curves_single_color")),
                    tags$input(
                      id = "curve_single_color",
                      type = "color",
                      value = "#000000",
                      class = "form-control form-control-color",
                      oninput = "Shiny.setInputValue('curve_single_color', this.value, {priority: 'event'});"
                    )
                  )
                ),
                 
                 # Réplicas a mostrar para Curvas
                 conditionalPanel(
                   "input.tipo == 'Curvas'",
                   uiOutput("repSelCurvas")
                 ),
                hr(), tags$div(id = "section_appearance", h4(tr("guided_appearance"))),
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
                    list(
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
                          list(tr("palette_type_seq"), tr("palette_type_div"), tr("palette_type_qual"))
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
                          list(
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
                    hr(),
                    checkboxInput(
                      "adv_pal_group_enable",
                      tr("palette_group_enable"),
                      FALSE
                    ),
                    conditionalPanel(
                      condition = "input.adv_pal_group_enable == true",
                      uiOutput("adv_pal_group_ui")
                    ),
                    style = "default"
                  )
                ),
                 
                 ## ─── Boxplot ──────────────────────────────────────────────
                 conditionalPanel(
                   condition = "input.tipo == 'Boxplot'",
                   numericInput("box_w",   tr("box_width"),     value = 0.8,
                                min = 0.1, max = 1.5, step = 0.05)
                 ),

                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin'].indexOf(input.tipo) >= 0",
                   numericInput("pt_jit",  tr("point_jitter"), value = 0.1,
                                min = 0,   max = 0.5, step = 0.01)
                 ),

                 conditionalPanel(
                   condition = "input.tipo == 'Violin'",
                   numericInput("violin_width", tr("violin_width"), value = 0.45,
                                min = 0.1, max = 1.5, step = 0.05),
                   numericInput("violin_linewidth", tr("violin_linewidth"), value = 0.6,
                                min = 0.1, max = 2.5, step = 0.1),
                   radioButtons(
                     "violin_inner",
                     tr("violin_inner"),
                     choices = named_choices(
                       c("box", "points"),
                       list(tr("violin_inner_box"), tr("violin_inner_points"))
                     ),
                     selected = bioszen_visual_defaults$violin_inner,
                     inline = TRUE
                   )
                 ),
                  
                 ## ─── Boxplot *y* Barras (tamaño de puntos) ────────────────
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                   conditionalPanel(
                     condition = "['Boxplot','Barras','Apiladas'].indexOf(input.tipo) >= 0",
                     uiOutput("errbarStatUI"),
                   ),
                   numericInput("pt_size", tr("point_size"), value = 3,
                                min = 0.5, max = 20,  step = 0.5),
                   numericInput("errbar_size", tr("errbar_size"),
                                value = 0.6, min = 0.1, step = 0.1),
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
                     "plot_flip",
                     tr("plot_flip"),
                     FALSE
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
                numericInput("base_size", tr("base_size"), bioszen_visual_defaults$base_size, min = 8),
                numericInput("fs_title",  tr("title_size"), bioszen_visual_defaults$title_size, min = 6),
                numericInput("fs_axis",   tr("axis_size"), bioszen_visual_defaults$axis_size, min = 6),
                numericInput("fs_legend", tr("legend_size"), bioszen_visual_defaults$legend_size, min = 6),
                tags$div(
                  class = "plot-text-style-section",
                  accordion(
                    id = "plotTextStylePanel",
                    open = FALSE,
                    multiple = TRUE,
                    accordion_panel_safe(
                      tr("plot_text_style_section"),
                      selectInput(
                        "plot_font_family",
                        tr("plot_font_family"),
                        choices = bioszen_plot_font_choices(),
                        selected = "Helvetica"
                      ),
                      h5(tr("plot_text_styles_by_target")),
                      checkboxGroupInput(
                        "plot_text_style_title",
                        tr("plot_text_target_title"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      checkboxGroupInput(
                        "plot_text_style_axis_titles",
                        tr("plot_text_target_axis_titles"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      checkboxInput(
                        "plot_axis_xy_custom",
                        tr("plot_axis_xy_custom"),
                        value = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.plot_axis_xy_custom == true",
                        checkboxGroupInput(
                          "plot_text_style_axis_title_x",
                          tr("plot_text_target_axis_title_x"),
                          choices = named_choices(
                            bioszen_plot_text_styles(),
                            list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                          ),
                          selected = character(0),
                          inline = TRUE
                        ),
                        checkboxGroupInput(
                          "plot_text_style_axis_title_y",
                          tr("plot_text_target_axis_title_y"),
                          choices = named_choices(
                            bioszen_plot_text_styles(),
                            list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                          ),
                          selected = character(0),
                          inline = TRUE
                        )
                      ),
                      checkboxGroupInput(
                        "plot_text_style_axis_text",
                        tr("plot_text_target_axis_text"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      conditionalPanel(
                        condition = "input.plot_axis_xy_custom == true",
                        checkboxGroupInput(
                          "plot_text_style_axis_text_x",
                          tr("plot_text_target_axis_text_x"),
                          choices = named_choices(
                            bioszen_plot_text_styles(),
                            list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                          ),
                          selected = character(0),
                          inline = TRUE
                        ),
                        checkboxGroupInput(
                          "plot_text_style_axis_text_y",
                          tr("plot_text_target_axis_text_y"),
                          choices = named_choices(
                            bioszen_plot_text_styles(),
                            list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                          ),
                          selected = character(0),
                          inline = TRUE
                        )
                      ),
                      checkboxGroupInput(
                        "plot_text_style_legend",
                        tr("plot_text_target_legend"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      checkboxGroupInput(
                        "plot_text_style_data_labels",
                        tr("plot_text_target_data_labels"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      conditionalPanel(
                        condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                        uiOutput("groupLabelStyleUI")
                      ),
                      checkboxGroupInput(
                        "plot_text_style_significance",
                        tr("plot_text_target_significance"),
                        choices = named_choices(
                          bioszen_plot_text_styles(),
                          list(tr("plot_text_style_bold"), tr("plot_text_style_italic"), tr("plot_text_style_underline"))
                        ),
                        selected = character(0),
                        inline = TRUE
                      ),
                      value = "plot_text_style_section"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "['Boxplot','Barras','Violin'].indexOf(input.tipo) >= 0",
                  checkboxInput("legend_right", tr("legend_right"), FALSE)
                ),
                numericInput("axis_line_size", tr("axis_line_size"),
                             value = bioszen_visual_defaults$axis_line_size, min = .1, step = .1),
                numericInput(
                  "axis_title_spacing_x",
                  tr("axis_title_spacing_x"),
                  value = bioszen_visual_defaults$axis_title_spacing_x,
                  min = 0,
                  max = 100,
                  step = 1
                ),
                numericInput(
                  "axis_title_spacing_y",
                  tr("axis_title_spacing_y"),
                  value = bioszen_visual_defaults$axis_title_spacing_y,
                  min = 0,
                  max = 100,
                  step = 1
                ),
                
                 # ---------- Parámetro a graficar --------------------------------------
                 textInput('yLab', tr("y_label"), ''),
                 
                 conditionalPanel(
                   condition = "input.scope=='Por Cepa'",
                   textInput('orderMedios', tr("order_csv"), '')
                 ),
                
                textInput('plotTitle', tr("plot_title"), ''),
                
                hr(), tags$div(id = "section_filters", h4(tr("guided_filters"))),
                uiOutput('rmRepsGlobalUI'),
                
                
                # ---------- Filtro de medios (Por Cepa) -------------------------------
                conditionalPanel(
                  condition = "input.scope=='Por Cepa'",
                  uiOutput("filterMediaHeader"),
                   checkboxInput('toggleMedios', tr("toggle_all"), TRUE),
                   uiOutput('showMediosUI'),
                   
                   accordion(                     # <-- en lugar de bsCollapse
                     id       = 'repsPanel',
                     open     = FALSE,
                     multiple = TRUE,
                    accordion_panel_safe(
                      uiOutput("repsByMediaTitle"),
                      uiOutput('repsStrainUI'),
                      value = "reps_by_media",
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
                 
                 
                 hr(), tags$div(id = "section_analysis", h4(tr("guided_analysis"))),
                 # ---------- Estadística para Boxplot/Barras/Apiladas ---------------------------
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
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
                             list(tr("norm_shapiro"), tr("norm_ks"), tr("norm_ad"))
                           ),
                           selected = c('shapiro','ks','ad')),
                           actionButton('runNorm',
                                        label = tagList(icon('vial'), tr("run_normality")),
                                        class = "btn btn-primary w-100",
                                        style = "white-space: normal;"),
                           br(), br(),
                           DTOutput('normTable')
                         ),
                         tabPanel(
                           title = tr("stats_tab_significance"),
                           radioButtons('sigTest', NULL,
                                        choices = named_choices(
                                          c('ANOVA', 'Kruskal-Wallis', 'ttest', 'wilcox'),
                                          list(
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
                                          list(tr("comp_all"), tr("comp_control"), tr("comp_pair"))
                                        ),
                                        selected = 'all'),
                           radioButtons(
                             "multitest_method",
                             tr("multitest_method"),
                             choices = named_choices(
                               c("holm", "fdr", "bonferroni", "none"),
                               list(
                                 tr("multitest_holm"),
                                 tr("multitest_fdr"),
                                 tr("multitest_bonferroni"),
                                 tr("multitest_none")
                               )
                             ),
                            selected = "none"
                           ),
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
                                        class = "btn btn-primary w-100",
                                        style = "white-space: normal;"),
                           br(), br(),
                           DTOutput('sigTable')
                         )
                       ),
                       style = 'primary'
                     )
                 )),
                 conditionalPanel(
                   condition = "input.tipo == 'Curvas'",
                   accordion(
                     id = "curveStatsPanel",
                     open = FALSE,
                     multiple = TRUE,
                     accordion_panel_safe(
                       tr("curves_stats_title"),
                       checkboxGroupInput(
                         "curve_stats_methods",
                         NULL,
                         choices = named_choices(
                           c("S1", "S2", "S3", "S4"),
                           list(
                             tr("curves_stats_s1"),
                             tr("curves_stats_s2"),
                             tr("curves_stats_s3"),
                             tr("curves_stats_s4")
                           )
                         ),
                         selected = c("S1", "S2", "S3", "S4")
                       ),
                       actionButton(
                         "runCurveStats",
                         tr("curves_stats_run"),
                         class = "btn btn-primary w-100"
                       ),
                       br(), br(),
                       DTOutput("curveStatsTable"),
                       style = "info"
                     )
                   )
                 ),
                 accordion(
                   id = "qcPanel",
                   open = FALSE,
                   multiple = TRUE,
                   accordion_panel_safe(
                     tr("qc_title"),
                     tags$p(class = "qc-help", tr("qc_subtitle")),
                     tags$p(
                       tags$strong(tr("qc_what_is")),
                       tags$span(" "),
                       tr("qc_what_is_desc")
                     ),
                       tags$div(
                        class = "qc-tabs-layout",
                        tabsetPanel(
                          id = "qcTabs",
                          tabPanel(
                            value = "qc_missing",
                            title = tr("qc_missing_table"),
                            tags$p(class = "qc-help", tr("qc_missing_help")),
                            DTOutput("qcMissingTable")
                          ),
                          tabPanel(
                            value = "qc_outliers",
                            title = tr("qc_outlier_table"),
                            tags$p(class = "qc-help", tr("qc_outlier_help")),
                            numericInput(
                              "qc_outlier_iqr_mult",
                              tr("qc_outlier_iqr_multiplier_label"),
                              value = 1.5,
                              min = 0.1,
                              step = 0.1
                            ),
                            tags$p(class = "qc-help", tr("qc_outlier_iqr_multiplier_help")),
                            actionButton(
                              "qc_apply_outlier_exclusion",
                              tr("qc_outlier_apply_button"),
                              class = "btn btn-outline-primary w-100",
                              style = "white-space: normal;"
                            ),
                            br(),
                            numericInput(
                              "qc_keep_top_n",
                              tr("qc_outlier_keep_n_label"),
                              value = 3,
                              min = 1,
                              step = 1
                            ),
                            tags$p(class = "qc-help", tr("qc_outlier_keep_n_help")),
                            actionButton(
                              "qc_apply_top_n",
                              tr("qc_outlier_apply_topn_button"),
                              class = "btn btn-outline-secondary w-100",
                              style = "white-space: normal;"
                            ),
                            br(), br(),
                            DTOutput("qcOutlierTable"),
                            tags$hr(),
                            tags$h5(tr("qc_outlier_group_counts")),
                            tags$p(class = "qc-help", tr("qc_outlier_group_counts_help")),
                            DTOutput("qcOutlierGroupTable")
                          ),
                          tabPanel(
                            value = "qc_sample",
                            title = tr("qc_sample_table"),
                            tags$p(class = "qc-help", tr("qc_sample_help")),
                            DTOutput("qcSampleTable")
                          )
                        )
                      ),
                     style = "secondary"
                    )
                 ),
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
                   hr(), h4(tr("sig_bars_title")),
                         radioButtons('sig_mode', NULL,
                               choices = named_choices(
                                 c('bars', 'labels'),
                                 list(tr("sig_mode_bars"), tr("sig_mode_labels"))
                               ),
                               selected = 'bars', inline = TRUE),
                  conditionalPanel(
                    condition = "input.sig_mode == 'labels'",
                    conditionalPanel(
                      condition = "input.tipo == 'Apiladas'",
                      selectizeInput(
                        'sig_param',
                        tr("sig_param_label"),
                        choices = NULL,
                        selected = NULL
                      ),
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
                    column(6, actionButton('add_sig',
                                            label = tagList(icon('plus'), tr("sig_add_bar")),
                                            class = 'btn btn-primary w-100', style='white-space: normal; margin-bottom: 5px;')),
                    column(6, actionButton('sig_update_label',
                                           label = tagList(icon('edit'), tr("sig_update_label")),
                                           class = 'btn btn-outline-primary w-100', style='white-space: normal; margin-bottom: 5px;'))
                  ),
                  fluidRow(
                    column(6, actionButton('remove_sig',
                                           label = tagList(icon('minus'), tr("sig_remove")),
                                           class = 'btn btn-secondary w-100', style='white-space: normal;')),
                    column(6, actionButton('clear_sig',
                                           label = tagList(icon('eraser'), tr("sig_clear")),
                                           class = 'btn btn-secondary w-100', style='white-space: normal;'))
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
                  conditionalPanel(
                    condition = "input.runSig > 0",
                    hr(),
                    h5(tr("sig_auto_title")),
                    tags$p(class = "qc-help", tr("sig_auto_help")),
                    radioButtons(
                      "sig_auto_include",
                      tr("sig_auto_include"),
                      choices = named_choices(
                        c("significant", "all"),
                        list(tr("sig_auto_significant"), tr("sig_auto_all"))
                      ),
                      selected = "significant",
                      inline = TRUE
                    ),
                    radioButtons(
                      "sig_auto_label_mode",
                      tr("sig_auto_label_mode"),
                      choices = named_choices(
                        c("stars", "pvalue"),
                        list(tr("sig_auto_label_stars"), tr("sig_auto_label_p"))
                      ),
                      selected = "stars",
                      inline = TRUE
                    ),
                    checkboxInput("sig_auto_replace", tr("sig_auto_replace"), TRUE),
                    actionButton(
                      "sig_auto_apply",
                      label = tagList(icon("magic"), tr("sig_auto_apply")),
                      class = "btn btn-primary w-100",
                      style = "white-space: normal;"
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput('sig_linewidth', tr("sig_linewidth"),
                                        .4,  min = .2, step = .2)),
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
                 hr(), tags$div(id = "section_export", h4(tr("guided_export"))),
                 fluidRow(
                   column(6, numericInput('plot_w', tr("plot_width"), 1000, min = 100)),
                   column(6, numericInput('plot_h', tr("plot_height"),  700, min = 100))
                 ),
                 numericInput(
                   "export_dpi",
                   tr("export_dpi"),
                   bioszen_visual_defaults$export_dpi,
                   min = BIOSZEN_MIN_DPI,
                   max = BIOSZEN_MAX_DPI,
                   step = 1
                 ),
                 helpText(tr("export_dpi_help")),
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
                       tags$li(downloadLink("downloadPlotly_png", "PNG", class="dropdown-item")),
                       tags$li(downloadLink("downloadPlotly_pdf", "PDF", class="dropdown-item")),
                       tags$li(downloadLink("downloadPlotly_pptx", "PPT", class="dropdown-item"))
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
                       tags$li(downloadLink("downloadPlot_pdf", "PDF", class="dropdown-item")),
                       tags$li(downloadLink("downloadPlot_pptx", "PPT", class="dropdown-item"))
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
                 hr()
                )
               ),  # ---- FIN sidebarPanel -----------------------------------------------
               
               ###########################################################################
               #  MAIN PANEL -------------------------------------------------------------
               ###########################################################################
                mainPanel(
                   width = 8,
                  tags$div(
                    class = "bioszen-main-content",
                    uiOutput('appErrorReportUI'),
                    uiOutput('plotInteractivoUI'),
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
                    uiOutput("statsFilteredTechTableUI"),
                    hr(),
                   tags$div(
                      style = "font-size:16px; text-align:center; color:#555;",
                      tr("comments_label"),
                      tags$a("bioszenf@gmail.com",
                             href = "mailto:bioszenf@gmail.com",
                              style = "font-weight:bold;")
                   )
               )
               ) # ← este cierra <mainPanel>
             )
             )
    ),
    tabPanel(title = HTML(as.character(tr("tab_growth"))), value = "tab_growth",
             tags$div(
               class = "bioszen-scroll-layout show-sidebar",
               sidebarLayout(
                sidebarPanel(
                   width = 4,
                   style = "position:sticky; top:86px; height:calc(100vh - 98px); overflow-y:auto;",
                  tags$div(
                    class = "bioszen-sidebar-content",
                    fileInput(
                     "growthFiles",
                     tr("growth_files"),
                     accept = ".xlsx",
                    multiple = TRUE
                  ),
                  uiOutput("growthSelectedFilesUI"),
                  actionButton(
                    "clearGrowthFiles",
                    tr("growth_clear_files"),
                    class = "btn btn-default"
                  ),
                  br(), br(),
                  textInput(
                    "growthOutputDir",
                    tr("growth_output_dir"),
                    value = "",
                    placeholder = tr_text("growth_output_dir_placeholder", i18n_lang)
                  ),
                  bioszen_dir_button(
                    "browseGrowthOutputDir",
                    label = tr("growth_browse_dir"),
                    title = tr_text("growth_browse_dir_caption", i18n_lang)
                  ),
                  helpText(tr("growth_output_dir_help")),
                  br(),
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
                  br(),
                  shinyjs::disabled(
                    actionButton(
                      "stopGrowth",
                      tr("growth_stop"),
                      class = "btn btn-warning"
                    )
                  ),
                  br(), br(),
                  downloadButton(
                    "downloadGrowthZip",
                    tr("growth_download"),
                    class = "btn btn-success"
                  ),
                  tags$div(
                    style = "margin-top: 10px; font-size: 13px; color: #444;",
                    textOutput("growthStatus")
                  ),
                  tags$script(HTML("
                    (function () {
                      if (window.__bioszen_growth_close_hook_installed) return;
                      window.__bioszen_growth_close_hook_installed = true;
                      window.__bioszen_growth_running = false;

                      function clearGrowthFileInput() {
                        var input = document.getElementById('growthFiles');
                        if (!input) return;
                        input.value = '';
                        var label = input.closest('.shiny-input-container');
                        if (!label) return;
                        var text = label.querySelector('.control-label');
                        var files = label.querySelector('.btn-file + input');
                        if (files) files.value = '';
                        var nameNode = label.querySelector('.shiny-file-input-progress, .shiny-file-input-name');
                        if (nameNode) nameNode.textContent = '';
                      }

                      function registerGrowthHandlers() {
                        if (!window.Shiny || typeof Shiny.addCustomMessageHandler !== 'function') return false;
                        Shiny.addCustomMessageHandler('bioszen-clear-growth-files', function (payload) {
                          clearGrowthFileInput();
                        });
                        Shiny.addCustomMessageHandler('bioszen-growth-running', function (payload) {
                          var running = !!(payload && payload.running);
                          window.__bioszen_growth_running = running;
                          var runButton = document.getElementById('runGrowth');
                          var stopButton = document.getElementById('stopGrowth');
                          if (runButton) {
                            runButton.disabled = running;
                            runButton.classList.toggle('disabled', running);
                            runButton.setAttribute('aria-disabled', running ? 'true' : 'false');
                          }
                          if (stopButton) {
                            stopButton.disabled = !running;
                            stopButton.classList.toggle('disabled', !running);
                            stopButton.setAttribute('aria-disabled', running ? 'false' : 'true');
                          }
                        });
                        return true;
                      }

                      if (!registerGrowthHandlers()) {
                        $(document).one('shiny:connected', function () {
                          registerGrowthHandlers();
                        });
                      }

                    })();
                  ")),
                  # solo mostramos cuando el usuario haya subido exactamente 1 archivo
                  uiOutput("showImportBtn")
                 )
                ),
                mainPanel(
                  width = 8,
                  tags$div(
                    class = "bioszen-main-content",
                    DTOutput("growthTable")
                  )
               )
             )
             )
    )  # ← cierre de tabPanel Growth Rates (sin coma si es la última)
    )
  )
)
