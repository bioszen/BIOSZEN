# --- Main UI ---
# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = theme_light,          # por defecto; el server lo cambia a oscuro/claro
  useShinyjs(),
  usei18n(i18n),

  tags$head(
    tags$title("BIOSZEN"),
    tags$link(rel = "icon", type = "image/x-icon", href = "bioszen.ico")
  ),

  tags$head(
    tags$style(HTML("
      :root {
        --bz-radius: 14px;
        --bz-gap: 14px;
      }

      html, body {
        min-height: 100%;
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

      #statsPanel .accordion-header .accordion-button,
      #curveStatsPanel .accordion-header .accordion-button,
      #qcPanel .accordion-header .accordion-button,
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
    (function () {
      function ensureI18nTranslations() {
        if (typeof window.i18n_translations === 'undefined' || window.i18n_translations === null) {
          window.i18n_translations = [];
        }
      }

      ensureI18nTranslations();
      $(document).one('shiny:connected', ensureI18nTranslations);
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
    tags$div(class = "startup-text", "Loading...")
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
          tags$li(actionLink("lang_es", tr("lang_es"), class = "dropdown-item")),
          tags$li(actionLink("lang_en", tr("lang_en"), class = "dropdown-item"))
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
        setTimeout(function () {
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

      var src = img.src;
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
      function setPlotLoading(on) {
        var wrap = document.getElementById('plot-loading-wrap');
        if (!wrap) return;
        if (on) wrap.classList.add('is-loading');
        else wrap.classList.remove('is-loading');
      }

      $(document).on('shiny:outputinvalidated', function (ev) {
        if ((ev && ev.name) === 'plotInteractivo') setPlotLoading(true);
      });

      $(document).on('shiny:value shiny:error', function (ev) {
        if ((ev && ev.name) === 'plotInteractivo') setPlotLoading(false);
      });

      $(document).on('shiny:disconnected', function () {
        setPlotLoading(false);
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
                        title = tr("merge_open"),
                        `aria-label` = as.character(tr("merge_open"))
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
                  numericInput('curve_lwd', tr("curves_linewidth"), value = 1.5, min = 0.5, step = 0.1),
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
                                min = 0.1, max = 2.5, step = 0.1)
                 ),
                  
                 ## ─── Boxplot *y* Barras (tamaño de puntos) ────────────────
                 conditionalPanel(
                   condition = "['Boxplot','Barras','Violin','Apiladas'].indexOf(input.tipo) >= 0",
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
                conditionalPanel(
                  condition = "['Boxplot','Barras','Violin'].indexOf(input.tipo) >= 0",
                  checkboxInput("legend_right", tr("legend_right"), FALSE)
                ),
                numericInput("axis_line_size", tr("axis_line_size"),
                             value = 1.2, min = .1, step = .1),
                
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
                 hr(), tags$div(id = "section_export", h4(tr("guided_export"))),
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
                          window.__bioszen_growth_running = !!(payload && payload.running);
                        });
                        return true;
                      }

                      if (!registerGrowthHandlers()) {
                        $(document).one('shiny:connected', function () {
                          registerGrowthHandlers();
                        });
                      }

                      $(document).on('click', '.shiny-notification .close, .shiny-notification .btn-close', function () {
                        var box = $(this).closest('.shiny-notification');
                        var hasProgress = box.find('.progress, .shiny-progress').length > 0;
                        if (hasProgress && window.__bioszen_growth_running && window.Shiny && typeof Shiny.setInputValue === 'function') {
                          Shiny.setInputValue('growth_progress_closed', Date.now(), {priority: 'event'});
                        }
                      });
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
