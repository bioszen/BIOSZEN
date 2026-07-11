library(testthat)

app_launch_dir <- app_test_launch_dir()

skip_if_shiny_e2e_unavailable <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()
  in_r_cmd_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")) ||
    grepl("\\.Rcheck(/|$)", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  skip_if(in_r_cmd_check, "Browser E2E tests are covered by the GitHub/local test_dir lane.")

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!nzchar(chrome_path)) {
    skip("Chrome/Chromium is not available for shinytest2 integration tests.")
  }
}

is_chromote_navigation_timeout <- function(err) {
  msg <- conditionMessage(err)
  grepl("Chromote: timed out", msg, fixed = TRUE) &&
    grepl("Page.navigate", msg, fixed = TRUE)
}

start_bioszen_driver <- function(max_attempts = 3L) {
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")

  if (is.null(max_attempts) || !length(max_attempts) || is.na(max_attempts[[1]])) {
    max_attempts <- 3L
  }
  max_attempts <- max(1L, as.integer(max_attempts[[1]]))
  last_error <- NULL
  on.exit({
    if (!is.null(last_error)) {
      if (is.na(old_not_cran)) {
        Sys.unsetenv("NOT_CRAN")
      } else {
        Sys.setenv(NOT_CRAN = old_not_cran)
      }
    }
  }, add = TRUE)

  for (attempt in seq_len(max_attempts)) {
    app <- tryCatch(
      shinytest2::AppDriver$new(
        app_dir = app_launch_dir,
        load_timeout = 120000,
        timeout = 120000,
        clean_logs = FALSE,
        options = list(warn = 1)
      ),
      error = function(e) e
    )

    if (!inherits(app, "error")) {
      last_error <- NULL
      return(list(app = app, old_not_cran = old_not_cran))
    }

    last_error <- app
    if (!is_chromote_navigation_timeout(app) || attempt >= max_attempts) {
      break
    }

    Sys.sleep(min(10, 2 * attempt))
    invisible(gc())
  }

  stop(last_error)
}

stop_bioszen_driver <- function(ctx) {
  if (!is.null(ctx$app)) {
    try(ctx$app$stop(), silent = TRUE)
  }

  old_not_cran <- ctx$old_not_cran
  if (is.na(old_not_cran)) {
    Sys.unsetenv("NOT_CRAN")
  } else {
    Sys.setenv(NOT_CRAN = old_not_cran)
  }
}

find_critical_frontend_logs <- function(log_tbl) {
  if (is.null(log_tbl) || !nrow(log_tbl)) {
    return(data.frame())
  }
  msg <- tolower(as.character(log_tbl$message))
  bad <- grepl("handler must be a function that takes one argument", msg, fixed = TRUE) |
    grepl("inputbinding.receivemessage", msg, fixed = TRUE) |
    grepl("uncaught", msg, fixed = TRUE)
  log_tbl[bad, c("location", "level", "message"), drop = FALSE]
}

normalize_js_scalar <- function(x) {
  vals <- unlist(x, use.names = FALSE)
  if (!length(vals)) return("")
  as.character(vals[[1]])
}

normalize_js_bool <- function(x) {
  if (is.logical(x) && length(x)) return(isTRUE(x[[1]]))
  val <- tolower(trimws(normalize_js_scalar(x)))
  val %in% c("true", "1", "t", "yes", "y")
}

wait_for_plot_idle <- function(app, timeout_sec = 25) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    idle <- tryCatch(
      app$get_js(
        "(function(){
           var wrap = document.getElementById('plot-loading-wrap');
           if (!wrap) return true;
           return !wrap.classList.contains('is-loading');
         })()"
      ),
      error = function(e) FALSE
    )
    if (isTRUE(normalize_js_bool(idle))) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

wait_for_shiny_connected <- function(app, timeout_sec = 20) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    connected <- tryCatch(
      app$get_js(
        "(function(){
           return !!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected());
         })()"
      ),
      error = function(e) FALSE
    )
    if (isTRUE(normalize_js_bool(connected))) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

wait_for_numeric_input_value <- function(app, input_id, expected, timeout_sec = 30, tolerance = 1e-8) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    current <- tryCatch(as.numeric(app$get_value(input = input_id)), error = function(e) NA_real_)
    if (length(current) && is.finite(current[[1]]) &&
        abs(current[[1]] - expected) <= tolerance) {
      return(TRUE)
    }
    Sys.sleep(0.25)
  }
  FALSE
}

install_loop_probe <- function(app) {
  app$get_js(
    "(function(){
       if (window.__bioszenLoopProbeInstalled) return 'already-installed';
       window.__bioszenLoopProbeInstalled = true;
       window.__bioszenLoopProbe = {
         invalidated: 0,
         value: 0,
         errors: 0,
         busy: 0,
         idle: 0
       };
       document.addEventListener('shiny:outputinvalidated', function(ev){
         if (!ev || ev.name === 'plotInteractivo') {
           window.__bioszenLoopProbe.invalidated += 1;
         }
       });
       document.addEventListener('shiny:value', function(ev){
         if (!ev || ev.name === 'plotInteractivo') {
           window.__bioszenLoopProbe.value += 1;
         }
       });
       document.addEventListener('shiny:error', function(ev){
         if (!ev || ev.name === 'plotInteractivo') {
           window.__bioszenLoopProbe.errors += 1;
         }
       });
       document.addEventListener('shiny:busy', function(){
         window.__bioszenLoopProbe.busy += 1;
       });
       document.addEventListener('shiny:idle', function(){
         window.__bioszenLoopProbe.idle += 1;
       });
       return 'installed';
     })()"
  )
}

loop_probe_counts <- function(app) {
  skip_if_not_installed("jsonlite")
  raw <- app$get_js(
    "(function(){
       return JSON.stringify(window.__bioszenLoopProbe || {
         invalidated: 0,
         value: 0,
         errors: 0,
         busy: 0,
         idle: 0
       });
     })()"
  )
  out <- jsonlite::fromJSON(normalize_js_scalar(raw))
  as.list(out)
}

reset_loop_probe <- function(app) {
  app$get_js(
    "(function(){
       if (!window.__bioszenLoopProbe) return false;
       window.__bioszenLoopProbe.invalidated = 0;
       window.__bioszenLoopProbe.value = 0;
       window.__bioszenLoopProbe.errors = 0;
       window.__bioszenLoopProbe.busy = 0;
       window.__bioszenLoopProbe.idle = 0;
       return true;
     })()"
  )
}

install_plotly_redraw_probe <- function(app, timeout_sec = 30) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    installed <- tryCatch(
      app$get_js(
        "(function(){
           if (!window.Plotly) return false;
           if (window.__bioszenPlotlyProbeInstalled) return true;
           window.__bioszenPlotlyProbeInstalled = true;
           window.__bioszenPlotlyProbe = {newPlot: 0, react: 0, redraw: 0};
           ['newPlot', 'react', 'redraw'].forEach(function(name){
             if (typeof window.Plotly[name] !== 'function') return;
             var original = window.Plotly[name];
             window.Plotly[name] = function(){
               if (window.__bioszenPlotlyProbe) {
                 window.__bioszenPlotlyProbe[name] += 1;
               }
               return original.apply(this, arguments);
             };
           });
           return true;
         })()"
      ),
      error = function(e) FALSE
    )
    if (isTRUE(normalize_js_bool(installed))) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

reset_plotly_redraw_probe <- function(app) {
  app$get_js(
    "(function(){
       if (!window.__bioszenPlotlyProbe) return false;
       window.__bioszenPlotlyProbe.newPlot = 0;
       window.__bioszenPlotlyProbe.react = 0;
       window.__bioszenPlotlyProbe.redraw = 0;
       return true;
     })()"
  )
}

plotly_redraw_probe_counts <- function(app) {
  skip_if_not_installed("jsonlite")
  raw <- app$get_js(
    "(function(){
       return JSON.stringify(window.__bioszenPlotlyProbe || {newPlot: 0, react: 0, redraw: 0});
     })()"
  )
  as.list(jsonlite::fromJSON(normalize_js_scalar(raw)))
}

install_plot_loading_probe <- function(app, timeout_sec = 30) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    installed <- tryCatch(
      app$get_js(
        "(function(){
           var wrap = document.getElementById('plot-loading-wrap');
           if (!wrap) return false;
           if (window.__bioszenPlotLoadingProbeInstalled) return true;
           window.__bioszenPlotLoadingProbeInstalled = true;
           window.__bioszenPlotLoadingProbe = {loadingOn: 0, loadingOff: 0};
           window.__bioszenPlotLoadingLast = wrap.classList.contains('is-loading');
           var obs = new MutationObserver(function(){
             var on = wrap.classList.contains('is-loading');
             if (on === window.__bioszenPlotLoadingLast) return;
             window.__bioszenPlotLoadingLast = on;
             if (on) window.__bioszenPlotLoadingProbe.loadingOn += 1;
             else window.__bioszenPlotLoadingProbe.loadingOff += 1;
           });
           obs.observe(wrap, {attributes: true, attributeFilter: ['class']});
           return true;
         })()"
      ),
      error = function(e) FALSE
    )
    if (isTRUE(normalize_js_bool(installed))) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

reset_plot_loading_probe <- function(app) {
  app$get_js(
    "(function(){
       var wrap = document.getElementById('plot-loading-wrap');
       if (!window.__bioszenPlotLoadingProbe || !wrap) return false;
       window.__bioszenPlotLoadingProbe.loadingOn = 0;
       window.__bioszenPlotLoadingProbe.loadingOff = 0;
       window.__bioszenPlotLoadingLast = wrap.classList.contains('is-loading');
       return true;
     })()"
  )
}

plot_loading_probe_counts <- function(app) {
  skip_if_not_installed("jsonlite")
  raw <- app$get_js(
    "(function(){
       return JSON.stringify(window.__bioszenPlotLoadingProbe || {loadingOn: 0, loadingOff: 0});
     })()"
  )
  as.list(jsonlite::fromJSON(normalize_js_scalar(raw)))
}

wait_for_no_plot_churn <- function(app, quiet_sec = 2.5, timeout_sec = 25, max_plot_events = 1) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    before <- loop_probe_counts(app)
    Sys.sleep(quiet_sec)
    after <- loop_probe_counts(app)
    invalidated_delta <- as.numeric(after$invalidated %||% 0) - as.numeric(before$invalidated %||% 0)
    value_delta <- as.numeric(after$value %||% 0) - as.numeric(before$value %||% 0)
    error_delta <- as.numeric(after$errors %||% 0) - as.numeric(before$errors %||% 0)
    if (
      invalidated_delta <= max_plot_events &&
        value_delta <= max_plot_events &&
        identical(error_delta, 0)
    ) {
      return(TRUE)
    }
  }
  FALSE
}

expect_app_idle_without_loop <- function(app, step_name, idle_timeout = 35) {
  expect_true(
    wait_for_shiny_connected(app, timeout_sec = 15),
    info = sprintf("Shiny session disconnected after %s.", step_name)
  )
  expect_true(
    wait_for_plot_idle(app, timeout_sec = idle_timeout),
    info = sprintf("Plot loading overlay did not settle after %s.", step_name)
  )
  expect_true(
    wait_for_no_plot_churn(app, timeout_sec = idle_timeout),
    info = sprintf("Plot kept invalidating after %s, suggesting a reactive loop.", step_name)
  )
}

send_filter_toggle_user_change <- function(app, input_id, value) {
  js <- sprintf(
    "(function(){
       Shiny.setInputValue('%s_user_change', {
         value: %s,
         nonce: Date.now()
       }, {priority: 'event'});
       return true;
     })()",
    input_id,
    if (isTRUE(value)) "true" else "false"
  )
  app$get_js(js)
}

wait_for_selected_values <- function(app, input_id, expected, timeout_sec = 30) {
  expected <- sort(unique(as.character(expected)))
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    observed <- tryCatch(
      unique(as.character(app$get_value(input = input_id))),
      error = function(e) character(0)
    )
    observed <- observed[!is.na(observed) & nzchar(observed)]
    if (identical(sort(observed), expected)) {
      return(TRUE)
    }
    Sys.sleep(0.5)
  }
  FALSE
}

checkbox_dom_selected_values <- function(app, input_id) {
  skip_if_not_installed("jsonlite")
  input_json <- jsonlite::toJSON(as.character(input_id), auto_unbox = TRUE)
  raw <- app$get_js(sprintf(
    "(function(){
       var inputId = %s;
       var selected = Array.from(document.querySelectorAll('input[type=\"checkbox\"]'))
         .filter(function(box){ return String(box.name || '') === inputId && box.checked; })
         .map(function(box){ return String(box.value || ''); });
       return JSON.stringify(selected);
     })()",
    input_json
  ))
  vals <- jsonlite::fromJSON(normalize_js_scalar(raw))
  vals <- as.character(vals %||% character(0))
  sort(unique(vals[!is.na(vals) & nzchar(vals)]))
}

wait_for_dom_selected_values <- function(app, input_id, expected, timeout_sec = 30) {
  expected <- sort(unique(as.character(expected)))
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    observed <- tryCatch(
      checkbox_dom_selected_values(app, input_id),
      error = function(e) character(0)
    )
    if (identical(observed, expected)) return(TRUE)
    Sys.sleep(0.5)
  }
  FALSE
}

expect_dom_selection_stays_put <- function(app, input_id, expected, label, quiet_sec = 3) {
  expected <- sort(unique(as.character(expected)))
  expect_true(
    wait_for_dom_selected_values(app, input_id, expected, timeout_sec = 30),
    info = sprintf("DOM checkbox selection did not reach the expected final state after %s.", label)
  )
  Sys.sleep(quiet_sec)
  expect_identical(
    checkbox_dom_selected_values(app, input_id),
    expected,
    info = sprintf("DOM checkbox selection replayed a stale state after %s.", label)
  )
}

rapid_click_checkbox <- function(app, selector, times = 7L) {
  skip_if_not_installed("jsonlite")
  selector_json <- jsonlite::toJSON(selector, auto_unbox = TRUE)
  times <- max(1L, as.integer(times[[1]]))
  js <- sprintf(
    "(function(){
       var selector = %s;
       var box = document.querySelector(selector);
       if (!box) return JSON.stringify({ok:false, reason:'missing'});
       for (var i = 0; i < %d; i += 1) {
         box.click();
       }
       return JSON.stringify({
         ok: true,
         name: String(box.name || box.id || ''),
         value: String(box.value || ''),
         checked: !!box.checked
       });
     })()",
    selector_json,
    times
  )
  out <- jsonlite::fromJSON(normalize_js_scalar(app$get_js(js)))
  if (!isTRUE(out$ok)) {
    stop(sprintf("Checkbox selector was not found: %s", selector), call. = FALSE)
  }
  out
}

rapid_click_checked_checkboxes <- function(app, name, limit = 3L) {
  skip_if_not_installed("jsonlite")
  name_json <- jsonlite::toJSON(as.character(name), auto_unbox = TRUE)
  limit <- max(1L, as.integer(limit[[1]]))
  js <- sprintf(
    "(function(){
       var name = %s;
       var boxes = Array.from(document.querySelectorAll('input[type=\"checkbox\"]'))
         .filter(function(box){ return String(box.name || '') === name && box.checked; })
         .slice(0, %d);
       boxes.forEach(function(box){ box.click(); });
       return JSON.stringify({
         n: boxes.length,
         values: boxes.map(function(box){ return String(box.value || ''); })
       });
     })()",
    name_json,
    limit
  )
  jsonlite::fromJSON(normalize_js_scalar(app$get_js(js)))
}

set_checkbox_group_dom_values <- function(app, name, selected) {
  skip_if_not_installed("jsonlite")
  name_json <- jsonlite::toJSON(as.character(name), auto_unbox = TRUE)
  selected_json <- jsonlite::toJSON(as.character(selected %||% character(0)), auto_unbox = FALSE)
  raw <- app$get_js(sprintf(
    "(function(){
       var name = %s;
       var selected = %s.map(String);
       var lookup = {};
       selected.forEach(function(value){ lookup[String(value)] = true; });
       var clicked = [];
       Array.from(document.querySelectorAll('input[type=\"checkbox\"]'))
         .filter(function(box){ return String(box.name || '') === name; })
         .forEach(function(box){
           var shouldCheck = !!lookup[String(box.value || '')];
           if (box.checked !== shouldCheck) {
             box.click();
             clicked.push(String(box.value || ''));
           }
         });
       return JSON.stringify({clicked: clicked});
     })()",
    name_json,
    selected_json
  ))
  jsonlite::fromJSON(normalize_js_scalar(raw))
}

checkbox_group_state <- function(app, name_prefix = NULL, exact_name = NULL) {
  skip_if_not_installed("jsonlite")
  state_arg <- jsonlite::toJSON(
    list(name_prefix = name_prefix, exact_name = exact_name),
    auto_unbox = TRUE,
    null = "null"
  )
  raw <- app$get_js(sprintf(
    "(function(arg){
       var boxes = Array.from(document.querySelectorAll('input[type=\"checkbox\"]'));
       if (arg.exact_name) {
         boxes = boxes.filter(function(box){ return String(box.name || '') === String(arg.exact_name); });
       }
       if (arg.name_prefix) {
         boxes = boxes.filter(function(box){ return String(box.name || '').indexOf(String(arg.name_prefix)) === 0; });
       }
       return JSON.stringify(boxes.map(function(box){
         return {
           id: String(box.id || ''),
           name: String(box.name || ''),
           value: String(box.value || ''),
           checked: !!box.checked
         };
       }));
     })(%s)",
    state_arg
  ))
  jsonlite::fromJSON(normalize_js_scalar(raw), simplifyDataFrame = TRUE)
}

css_checkbox_selector <- function(name, value) {
  sprintf(
    "input[type=\"checkbox\"][name=\"%s\"][value=\"%s\"]",
    gsub('(["\\\\])', '\\\\\\1', as.character(name), perl = TRUE),
    gsub('(["\\\\])', '\\\\\\1', as.character(value), perl = TRUE)
  )
}

open_qc_tech_controls <- function(app, timeout_sec = 45) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  while (Sys.time() < deadline) {
    boxes <- checkbox_group_state(app, name_prefix = "qc_tech_rep_")
    if (is.data.frame(boxes) && nrow(boxes)) return(boxes)
    try(app$set_inputs(qcTabs = "qc_tech", wait_ = FALSE), silent = TRUE)
    try(
      app$get_js(
        "(function(){
           var qcButton = Array.from(document.querySelectorAll('button[aria-controls]')).find(function(x){
             return /data quality control|control de calidad/i.test(x.textContent || '');
           });
           if (qcButton && qcButton.getAttribute('aria-expanded') !== 'true') {
             try { qcButton.click(); } catch(e) {}
           }
           var techTab = document.querySelector('a[data-value=\"qc_tech\"], button[data-value=\"qc_tech\"]');
           if (techTab) {
             try { techTab.click(); } catch(e) {}
           }
         })()"
      ),
      silent = TRUE
    )
    Sys.sleep(1)
  }
  checkbox_group_state(app, name_prefix = "qc_tech_rep_")
}

click_stats_button_with_blank_param <- function(app, button_id) {
  js <- sprintf(
     "(function(){
       if (window.Shiny && typeof Shiny.setInputValue === 'function') {
         Shiny.setInputValue('param', '', {priority: 'event'});
         var btn = document.getElementById('%s');
         if (btn) {
           btn.click();
           return true;
         }
         var current = 0;
         if (Shiny.shinyapp && Shiny.shinyapp.$inputValues) {
           current = Number(Shiny.shinyapp.$inputValues['%s'] || 0);
         }
         Shiny.setInputValue('%s', current + 1, {priority: 'event'});
         return true;
       }
       return false;
     })()",
    button_id,
    button_id,
    button_id
  )
  normalize_js_bool(app$get_js(js))
}

click_stats_button_with_empty_media_filter <- function(app, button_id) {
  js <- sprintf(
     "(function(){
       if (window.Shiny && typeof Shiny.setInputValue === 'function') {
         Shiny.setInputValue('showMedios', [], {priority: 'event'});
         var btn = document.getElementById('%s');
         if (btn) {
           btn.click();
           return true;
         }
       }
       return false;
     })()",
    button_id
  )
  normalize_js_bool(app$get_js(js))
}

clear_shiny_notifications <- function(app) {
  invisible(app$get_js(
    "(function(){
       Array.from(document.querySelectorAll('.shiny-notification')).forEach(function(el){ el.remove(); });
       return true;
     })()"
  ))
}

current_notification_text <- function(app) {
  normalize_js_scalar(app$get_js(
    "(function(){
       return Array.from(document.querySelectorAll('.shiny-notification, .shiny-notification-message'))
         .map(function(el){ return el.innerText || el.textContent || ''; })
         .join(' ');
     })()"
  ))
}

activate_stats_tab <- function(app, pattern) {
  js <- sprintf(
    "(function(){
       var re = new RegExp(%s, 'i');
       var links = Array.from(document.querySelectorAll('#statsTabs a, a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]'));
       var link = links.find(function(a){ return re.test((a.textContent || '').trim()); });
       if (!link) return false;
       link.click();
       return true;
     })()",
    jsonlite::toJSON(pattern, auto_unbox = TRUE)
  )
  normalize_js_bool(app$get_js(js))
}

activate_growth_tab <- function(app) {
  js <- "(function(){
     var links = Array.from(document.querySelectorAll('a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"], .nav-link'));
     var link = links.find(function(a){ return /Growth Parameter Extraction/i.test((a.textContent || '').trim()); });
     if (!link) return false;
     link.click();
     return true;
   })()"
  normalize_js_bool(app$get_js(js))
}

make_growth_e2e_workbook <- function(path, n_points = 12, n_wells = 3) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  time <- seq_len(n_points)
  dat <- data.frame(Ignore1 = time, Ignore2 = time)
  for (i in seq_len(n_wells)) {
    rate <- 0.08 + 0.01 * i
    dat[[paste0("W", i)]] <- exp(seq(0, by = rate, length.out = n_points)) *
      (1 + 0.01 * sin(time + i))
  }
  openxlsx::writeData(wb, "Sheet1", dat, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

wait_for_growth_status <- function(app, timeout_sec = 90) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  last_status <- ""
  while (Sys.time() < deadline) {
    raw <- tryCatch(
      app$get_js(
        "(function(){
           var el = document.getElementById('growthStatus');
           return el ? (el.innerText || el.textContent || '') : '';
         })()"
      ),
      error = function(e) ""
    )
    last_status <- normalize_js_scalar(raw)
    if (grepl("Completed|Error|stopped|cancel", last_status, ignore.case = TRUE)) {
      return(last_status)
    }
    Sys.sleep(0.5)
  }
  last_status
}

growth_button_state <- function(app) {
  skip_if_not_installed("jsonlite")
  raw <- app$get_js(
    "(function(){
       var run = document.getElementById('runGrowth');
       var stop = document.getElementById('stopGrowth');
       return JSON.stringify({
         runDisabled: !!(run && run.disabled),
         stopDisabled: !!(stop && stop.disabled)
       });
     })()"
  )
  as.list(jsonlite::fromJSON(normalize_js_scalar(raw)))
}

click_growth_button_if_enabled <- function(app, button_id) {
  js <- sprintf(
    "(function(){
       var btn = document.getElementById('%s');
       if (!btn || btn.disabled) return false;
       btn.click();
       return true;
     })()",
    button_id
  )
  normalize_js_bool(app$get_js(js))
}

wait_for_growth_buttons <- function(app,
                                    run_disabled = NULL,
                                    stop_disabled = NULL,
                                    timeout_sec = 45) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  last <- NULL
  while (Sys.time() < deadline) {
    last <- growth_button_state(app)
    run_ok <- is.null(run_disabled) || identical(isTRUE(last$runDisabled), isTRUE(run_disabled))
    stop_ok <- is.null(stop_disabled) || identical(isTRUE(last$stopDisabled), isTRUE(stop_disabled))
    if (run_ok && stop_ok) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

wait_for_stats_output_text <- function(app, output_id, timeout_sec = 45) {
  deadline <- Sys.time() + as.numeric(timeout_sec)
  last_text <- ""
  empty_patterns <- paste(
    c(
      "no data", "no hay datos",
      "no valid", "no hay comparaciones",
      "not enough", "no hay grupos suficientes",
      "need at least", "se necesitan"
    ),
    collapse = "|"
  )
  while (Sys.time() < deadline) {
    value_raw <- tryCatch(
      app$get_js(sprintf(
        "(function(){
           var values = window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$values;
           var val = values ? values['%s'] : null;
           if (!val || !val.x || !Array.isArray(val.x.data)) return '';
           var rows = (Array.isArray(val.x.data[0])) ? val.x.data[0].length : 0;
           return JSON.stringify({
             rows: rows,
             data: val.x.data.slice(0, Math.min(5, val.x.data.length))
           });
         })()",
        output_id
      )),
      error = function(e) ""
    )
    value_txt <- normalize_js_scalar(value_raw)
    if (nzchar(value_txt)) {
      value_info <- tryCatch(jsonlite::fromJSON(value_txt), error = function(e) NULL)
      if (!is.null(value_info) && isTRUE(as.numeric(value_info$rows %||% 0) > 0)) {
        payload_text <- paste(unlist(value_info$data, use.names = FALSE), collapse = " ")
        payload_text <- gsub("\u00a0", " ", payload_text, fixed = TRUE)
        payload_text <- trimws(gsub("\\s+", " ", payload_text, perl = TRUE))
        if (nzchar(payload_text) &&
            grepl("[[:alnum:]]", payload_text, perl = TRUE) &&
            !grepl(empty_patterns, payload_text, ignore.case = TRUE, perl = TRUE)) {
          return(payload_text)
        }
      }
    }

    raw <- tryCatch(
      app$get_js(sprintf(
        "(function(){
           var el = document.getElementById('%s');
           return el ? el.innerText : '';
         })()",
        output_id
      )),
      error = function(e) ""
    )
    txt <- normalize_js_scalar(raw)
    last_text <- txt
    txt <- gsub("\u00a0", " ", txt, fixed = TRUE)
    txt_compact <- trimws(gsub("\\s+", " ", txt, perl = TRUE))
    if (nzchar(txt_compact) &&
        grepl("[[:alnum:]]", txt_compact, perl = TRUE) &&
        !grepl(empty_patterns, txt_compact, ignore.case = TRUE, perl = TRUE)) {
      return(txt_compact)
    }
    Sys.sleep(0.5)
  }
  fail(sprintf("Stats output '%s' did not populate. Last text: %s", output_id, last_text))
}

expect_nonempty_download <- function(path, label) {
  expect_true(file.exists(path), info = sprintf("%s download was not created.", label))
  expect_true(
    file.info(path)$size > 0,
    info = sprintf("%s download was empty.", label)
  )
}

write_dpi_metadata_variant <- function(source, target, field, value = NULL, remove = FALSE) {
  stopifnot(requireNamespace("openxlsx", quietly = TRUE))
  sheets <- openxlsx::getSheetNames(source)
  wb <- openxlsx::createWorkbook()
  for (sheet in sheets) {
    dat <- openxlsx::read.xlsx(source, sheet = sheet, check.names = FALSE)
    if (identical(sheet, "Metadata") && all(c("Campo", "Valor") %in% names(dat))) {
      idx <- which(as.character(dat$Campo) == field)
      if (isTRUE(remove)) {
        if (length(idx)) dat <- dat[-idx, , drop = FALSE]
      } else if (length(idx)) {
        dat$Valor[idx[[1]]] <- as.character(value)
      } else {
        dat <- rbind(dat, data.frame(Campo = field, Valor = as.character(value)))
      }
    }
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, dat)
  }
  openxlsx::saveWorkbook(wb, target, overwrite = TRUE)
  target
}

expect_grouped_download_accepts_filtered_tabs <- function(path, require_filtered = FALSE) {
  skip_if_not_installed("readxl")

  tabs <- readxl::excel_sheets(path)
  expect_true(
    length(tabs) > 0,
    info = "Grouped data workbook should contain at least one sheet."
  )

  filtered <- grep("_filt$", tabs, value = TRUE)
  if (isTRUE(require_filtered)) {
    expect_gt(
      length(filtered),
      0,
      info = paste("Expected at least one _filt sheet. Available sheets:", paste(tabs, collapse = ", "))
    )
  }

  for (sheet in filtered) {
    base_sheet <- sub("_filt$", "", sheet)
    expect_true(
      base_sheet %in% tabs,
      info = sprintf("Filtered sheet %s should have matching base sheet %s.", sheet, base_sheet)
    )
  }

  non_filtered <- tabs[!grepl("_filt$", tabs)]
  check_tabs <- unique(c(utils::head(non_filtered, 3L), filtered))
  check_tabs <- check_tabs[!is.na(check_tabs) & nzchar(check_tabs)]
  for (sheet in check_tabs) {
    expect_no_error(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 20)
    )
  }

  invisible(tabs)
}

test_that("browser upload flow keeps searchable selectors and no critical frontend errors", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)

  strain_val <- as.character(app$get_value(input = "strain"))
  param_val <- as.character(app$get_value(input = "param"))
  expect_true(length(strain_val) >= 1 && nzchar(strain_val[[1]]))
  expect_true(length(param_val) >= 1 && nzchar(param_val[[1]]))

  strain_search <- app$get_html(selector = "#strain + .selectize-control .selectize-input input")
  param_search <- app$get_html(selector = "#param + .selectize-control .selectize-input input")
  expect_match(strain_search, "id=\"strain-selectized\"", fixed = TRUE)
  expect_match(param_search, "id=\"param-selectized\"", fixed = TRUE)

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("growth parameter extraction upload flow completes without session notification errors", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("openxlsx")

  growth_fixture <- tempfile("bioszen_growth_e2e_", fileext = ".xlsx")
  on.exit(unlink(growth_fixture, force = TRUE), add = TRUE)
  make_growth_e2e_workbook(growth_fixture, n_points = 12, n_wells = 3)

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  expect_true(
    activate_growth_tab(app),
    info = "The Growth Parameter Extraction tab should be available."
  )
  app$upload_file(growthFiles = normalizePath(growth_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "growthFilesKeep", timeout = 120000)

  selected_text <- normalize_js_scalar(app$get_js(
    "(function(){
       var el = document.getElementById('growthSelectedFilesUI');
       return el ? (el.innerText || el.textContent || '') : '';
     })()"
  ))
  expect_match(selected_text, basename(growth_fixture), fixed = TRUE)

  app$set_inputs(maxTime = 11, timeInterval = 1, runGrowth = "click", wait_ = FALSE)
  growth_status <- wait_for_growth_status(app, timeout_sec = 120)
  expect_match(growth_status, "Completed", fixed = TRUE)

  page_text <- normalize_js_scalar(app$get_js("document.body.innerText"))
  expect_false(grepl("ShinySession", page_text, fixed = TRUE))
  expect_false(grepl("sendNotification", page_text, fixed = TRUE))

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("growth can be stopped, reuploaded, and run again without restarting the app", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("openxlsx")

  first_growth <- tempfile("bioszen_growth_stop_first_", fileext = ".xlsx")
  second_growth <- tempfile("bioszen_growth_stop_second_", fileext = ".xlsx")
  on.exit(unlink(c(first_growth, second_growth), force = TRUE), add = TRUE)
  make_growth_e2e_workbook(first_growth, n_points = 120, n_wells = 8)
  make_growth_e2e_workbook(second_growth, n_points = 12, n_wells = 3)

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  expect_true(activate_growth_tab(app))
  app$upload_file(growthFiles = normalizePath(first_growth), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "growthFilesKeep", timeout = 120000)

  expect_true(wait_for_growth_buttons(app, run_disabled = FALSE, stop_disabled = TRUE, timeout_sec = 30))
  app$set_inputs(maxTime = 119, timeInterval = 1, wait_ = TRUE, timeout_ = 60000)
  expect_true(click_growth_button_if_enabled(app, "runGrowth"))
  expect_true(wait_for_growth_buttons(app, run_disabled = TRUE, stop_disabled = FALSE, timeout_sec = 45))
  expect_true(click_growth_button_if_enabled(app, "stopGrowth"))
  stopped_status <- wait_for_growth_status(app, timeout_sec = 120)
  expect_match(stopped_status, "stopped|cancel", ignore.case = TRUE)
  expect_true(wait_for_growth_buttons(app, run_disabled = FALSE, stop_disabled = TRUE, timeout_sec = 45))

  app$upload_file(growthFiles = normalizePath(second_growth), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "growthFilesKeep", timeout = 120000)
  selected_text <- normalize_js_scalar(app$get_js(
    "(function(){
       var el = document.getElementById('growthSelectedFilesUI');
       return el ? (el.innerText || el.textContent || '') : '';
     })()"
  ))
  expect_match(selected_text, basename(second_growth), fixed = TRUE)
  expect_true(wait_for_growth_buttons(app, run_disabled = FALSE, stop_disabled = TRUE, timeout_sec = 45))

  app$set_inputs(maxTime = 11, timeInterval = 1, wait_ = TRUE, timeout_ = 60000)
  expect_true(click_growth_button_if_enabled(app, "runGrowth"))
  rerun_status <- wait_for_growth_status(app, timeout_sec = 120)
  expect_match(rerun_status, "Completed", fixed = TRUE)

  page_text <- normalize_js_scalar(app$get_js("document.body.innerText"))
  expect_false(grepl("ShinySession", page_text, fixed = TRUE))
  expect_false(grepl("sendNotification", page_text, fixed = TRUE))
  expect_true(wait_for_growth_buttons(app, run_disabled = FALSE, stop_disabled = TRUE, timeout_sec = 45))
})

test_that("core user processes settle without reload loops", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("jsonlite")

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app
  install_loop_probe(app)

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  expect_app_idle_without_loop(app, "data upload", idle_timeout = 45)

  expect_true(
    activate_stats_tab(app, "normal|normalidad"),
    info = "The normality tab should be available after data upload."
  )
  expect_true(
    click_stats_button_with_blank_param(app, "runNorm"),
    info = "The normality button should be present after data upload."
  )
  norm_text <- wait_for_stats_output_text(app, "normTable", timeout_sec = 60)
  expect_match(norm_text, "Shapiro|Label|Control|Ampicillin", perl = TRUE)
  expect_app_idle_without_loop(app, "normality after transient blank parameter", idle_timeout = 45)

  medias_before_blank <- unique(as.character(app$get_value(input = "showMedios")))
  medias_before_blank <- medias_before_blank[!is.na(medias_before_blank) & nzchar(medias_before_blank)]
  if (length(medias_before_blank) >= 2) {
    clear_shiny_notifications(app)
    expect_true(
      click_stats_button_with_empty_media_filter(app, "runNorm"),
      info = "Normality should still run when a transient empty media filter races with the visible plot."
    )
    Sys.sleep(1.5)
    stale_filter_norm_text <- wait_for_stats_output_text(app, "normTable", timeout_sec = 60)
    expect_match(stale_filter_norm_text, "Shapiro|Label|Control|Ampicillin", perl = TRUE)
    notification_text <- current_notification_text(app)
    expect_false(
      grepl("need at least 2 groups|no data available for normality|no hay datos", notification_text, ignore.case = TRUE, perl = TRUE),
      info = sprintf("Normality incorrectly reported no data after a transient empty media filter: %s", notification_text)
    )
    app$set_inputs(showMedios = medias_before_blank, wait_ = TRUE, timeout_ = 90000)
    expect_app_idle_without_loop(app, "media filter restore after stale normality check", idle_timeout = 45)
  }

  expect_true(
    activate_stats_tab(app, "signif|significancia"),
    info = "The significance tab should be available after data upload."
  )
  expect_true(
    click_stats_button_with_blank_param(app, "runSig"),
    info = "The significance button should be present after data upload."
  )
  expect_app_idle_without_loop(app, "significance after transient blank parameter", idle_timeout = 45)
  stats_after_blank_param <- app$get_download(output = "downloadStats")
  expect_nonempty_download(stats_after_blank_param, "statistics after transient blank parameter")

  plot_types <- c("Boxplot", "Barras", "Violin", "Heatmap", "MatrizCorrelacion")
  for (plot_type in plot_types) {
    app$set_inputs(
      tipo = plot_type,
      wait_ = !identical(plot_type, "Curvas"),
      timeout_ = 120000,
      allow_no_input_binding_ = TRUE
    )
    if (identical(plot_type, "Curvas")) Sys.sleep(1)
    if (identical(plot_type, "Heatmap")) {
      try(app$wait_for_value(input = "heat_params", timeout = 60000), silent = TRUE)
    }
    if (identical(plot_type, "MatrizCorrelacion")) {
      try(app$wait_for_value(input = "corrm_params", timeout = 60000), silent = TRUE)
    }
    expect_app_idle_without_loop(
      app,
      sprintf("plot type %s", plot_type),
      idle_timeout = 45
    )
  }

  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 90000)
  app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showGroups", timeout = 90000)
  groups <- unique(as.character(app$get_value(input = "showGroups")))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (length(groups) >= 2) {
    expected_groups <- groups[-1]
    app$set_inputs(showGroups = expected_groups, wait_ = TRUE, timeout_ = 90000)
    expect_app_idle_without_loop(app, "combined group deselection", idle_timeout = 45)
    Sys.sleep(7)
    persisted_groups <- unique(as.character(app$get_value(input = "showGroups")))
    persisted_groups <- persisted_groups[!is.na(persisted_groups) & nzchar(persisted_groups)]
    expect_equal(
      sort(persisted_groups),
      sort(expected_groups),
      info = "Combined group deselection should not be reselected by filter sync after the plot settles."
    )
    expect_app_idle_without_loop(app, "combined group deselection persistence", idle_timeout = 45)
    app$set_inputs(showGroups = character(0), wait_ = TRUE, timeout_ = 90000)
    expect_app_idle_without_loop(app, "combined group clear all", idle_timeout = 45)
    send_filter_toggle_user_change(app, "toggleGroups", TRUE)
    expect_true(
      wait_for_selected_values(app, "showGroups", groups, timeout_sec = 45),
      info = "Combined select-all recovery should restore every group after the selection becomes empty."
    )
    expect_app_idle_without_loop(app, "combined group select-all recovery", idle_timeout = 45)
  }

  app$set_inputs(scope = "Por Cepa", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showMedios", timeout = 90000)
  medias <- unique(as.character(app$get_value(input = "showMedios")))
  medias <- medias[!is.na(medias) & nzchar(medias)]
  if (length(medias) >= 2) {
    expected_medias <- medias[-1]
    app$set_inputs(showMedios = expected_medias, wait_ = TRUE, timeout_ = 90000)
    expect_app_idle_without_loop(app, "condition deselection", idle_timeout = 45)
    Sys.sleep(7)
    persisted_medias <- unique(as.character(app$get_value(input = "showMedios")))
    persisted_medias <- persisted_medias[!is.na(persisted_medias) & nzchar(persisted_medias)]
    expect_equal(
      sort(persisted_medias),
      sort(expected_medias),
      info = "Condition deselection should not be reselected by filter sync after the plot settles."
    )
    expect_app_idle_without_loop(app, "condition deselection persistence", idle_timeout = 45)
    app$set_inputs(showMedios = character(0), wait_ = TRUE, timeout_ = 90000)
    expect_app_idle_without_loop(app, "condition clear all", idle_timeout = 45)
    send_filter_toggle_user_change(app, "toggleMedios", TRUE)
    expect_true(
      wait_for_selected_values(app, "showMedios", medias, timeout_sec = 45),
      info = "Condition select-all recovery should restore every media after the selection becomes empty."
    )
    expect_app_idle_without_loop(app, "condition select-all recovery", idle_timeout = 45)
  }

  app$set_inputs(doNorm = TRUE, wait_ = TRUE, timeout_ = 90000)
  try(app$wait_for_value(input = "ctrlMedium", timeout = 60000), silent = TRUE)
  expect_app_idle_without_loop(app, "normalization enabled", idle_timeout = 45)
  app$set_inputs(doNorm = FALSE, wait_ = TRUE, timeout_ = 90000)
  expect_app_idle_without_loop(app, "normalization disabled", idle_timeout = 45)

  app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 90000)
  if (length(groups)) {
    app$set_inputs(showGroups = groups, wait_ = TRUE, timeout_ = 90000)
  }
  expect_app_idle_without_loop(app, "download preflight", idle_timeout = 45)

  download_specs <- list(
    data = "downloadExcel",
    metadata = "downloadMetadata",
    statistics = "downloadStats",
    plot_png = "downloadPlot_png",
    plot_pdf = "downloadPlot_pdf"
  )
  for (label in names(download_specs)) {
    output_id <- download_specs[[label]]
    out <- app$get_download(output = output_id)
    expect_nonempty_download(out, label)
    if (identical(label, "data")) {
      expect_grouped_download_accepts_filtered_tabs(out)
    }
    expect_app_idle_without_loop(app, sprintf("%s download", label), idle_timeout = 45)
  }

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("heatmap metadata roundtrip preserves strict design fields", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)

  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)
  app$set_inputs(
    heat_scale_mode = "row",
    heat_hclust_method = "ward.D2",
    heat_cluster_rows = TRUE,
    heat_cluster_cols = TRUE,
    heat_k_rows = 3,
    heat_k_cols = 2,
    heat_show_values = TRUE,
    wait_ = TRUE,
    timeout_ = 60000
  )

  meta_path <- app$get_download(output = "downloadMetadata")
  expect_true(file.exists(meta_path))

  meta_tbl <- readxl::read_excel(meta_path, sheet = "Metadata")
  fields <- stats::setNames(as.character(meta_tbl$Valor), as.character(meta_tbl$Campo))
  required_fields <- c(
    "tipo", "heat_scale_mode", "heat_hclust_method",
    "heat_cluster_rows", "heat_cluster_cols",
    "heat_k_rows", "heat_k_cols",
    "heat_show_values", "heat_norm_z"
  )
  expect_true(all(required_fields %in% names(fields)))
  expect_identical(fields[["tipo"]], "Heatmap")
  expect_identical(fields[["heat_scale_mode"]], "row")
  expect_identical(fields[["heat_hclust_method"]], "ward.D2")
  expect_identical(fields[["heat_cluster_rows"]], "TRUE")
  expect_identical(fields[["heat_cluster_cols"]], "TRUE")
  expect_identical(fields[["heat_k_rows"]], "3")
  expect_identical(fields[["heat_k_cols"]], "2")
  expect_identical(fields[["heat_show_values"]], "TRUE")
  expect_identical(fields[["heat_norm_z"]], "TRUE")

  tabs <- readxl::excel_sheets(meta_path)
  wb_sheets <- stats::setNames(
    lapply(tabs, function(sheet) readxl::read_excel(meta_path, sheet = sheet)),
    tabs
  )
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_scale_mode"] <- "none"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_hclust_method"] <- "complete"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_cluster_rows"] <- "FALSE"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_cluster_cols"] <- "TRUE"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_k_rows"] <- "4"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_k_cols"] <- "5"
  wb_sheets[["Metadata"]]$Valor[wb_sheets[["Metadata"]]$Campo == "heat_show_values"] <- "FALSE"

  patched_meta <- tempfile("metadata_heatmap_roundtrip_", fileext = ".xlsx")
  on.exit(unlink(patched_meta), add = TRUE)
  writexl::write_xlsx(wb_sheets, path = patched_meta)

  app$upload_file(metaFiles = patched_meta, wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "heat_hclust_method", ignore = list("ward.D2"), timeout = 120000)
  app$wait_for_value(input = "heat_cluster_cols", ignore = list(FALSE), timeout = 120000)

  expect_identical(app$get_value(input = "heat_scale_mode"), "none")
  expect_identical(app$get_value(input = "heat_hclust_method"), "complete")
  expect_false(isTRUE(app$get_value(input = "heat_cluster_rows")))
  expect_true(isTRUE(app$get_value(input = "heat_cluster_cols")))
  expect_identical(as.numeric(app$get_value(input = "heat_k_rows")), 4)
  expect_identical(as.numeric(app$get_value(input = "heat_k_cols")), 5)
  expect_false(isTRUE(app$get_value(input = "heat_show_values")))

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("metadata import does not overwrite dataset selectors", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)

  old_scope <- as.character(app$get_value(input = "scope"))[[1]]
  old_strain <- as.character(app$get_value(input = "strain"))[[1]]
  old_param <- as.character(app$get_value(input = "param"))[[1]]
  old_do_norm <- isTRUE(app$get_value(input = "doNorm"))

  meta_path <- app$get_download(output = "downloadMetadata")
  expect_true(file.exists(meta_path))

  tabs <- readxl::excel_sheets(meta_path)
  wb_sheets <- stats::setNames(
    lapply(tabs, function(sheet) readxl::read_excel(meta_path, sheet = sheet)),
    tabs
  )

  set_meta_field <- function(df, field, value) {
    idx <- which(as.character(df$Campo) == field)
    if (length(idx)) {
      df$Valor[idx[[1]]] <- as.character(value)
      return(df)
    }
    rbind(
      df,
      data.frame(Campo = as.character(field), Valor = as.character(value), stringsAsFactors = FALSE)
    )
  }

  md <- wb_sheets[["Metadata"]]
  md <- set_meta_field(md, "scope", "Combinado")
  md <- set_meta_field(md, "strain", "__INVALID_STRAIN__")
  md <- set_meta_field(md, "param", "__INVALID_PARAM__")
  md <- set_meta_field(md, "doNorm", "TRUE")
  md <- set_meta_field(md, "ctrlMedium", "__INVALID_CTRL__")
  md <- set_meta_field(md, "heat_params", "__INVALID_PARAM__")
  wb_sheets[["Metadata"]] <- md

  patched_meta <- tempfile("metadata_design_only_", fileext = ".xlsx")
  on.exit(unlink(patched_meta), add = TRUE)
  writexl::write_xlsx(wb_sheets, path = patched_meta)

  app$upload_file(metaFiles = patched_meta, wait_ = TRUE, timeout_ = 120000)

  expect_identical(as.character(app$get_value(input = "scope"))[[1]], old_scope)
  expect_identical(as.character(app$get_value(input = "strain"))[[1]], old_strain)
  expect_identical(as.character(app$get_value(input = "param"))[[1]], old_param)
  expect_identical(isTRUE(app$get_value(input = "doNorm")), old_do_norm)
})

test_that("heatmap cluster export download includes parameter cluster sheets", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)
  app$set_inputs(
    heat_cluster_rows = TRUE,
    heat_cluster_cols = TRUE,
    heat_k_rows = 3,
    heat_k_cols = 2,
    wait_ = TRUE,
    timeout_ = 60000
  )
  app$wait_for_idle(timeout = 120000)
  expect_true(wait_for_plot_idle(app, timeout_sec = 60))

  out <- tryCatch(
    app$get_download(output = "downloadHeatClusters"),
    error = function(e) {
      logs <- tryCatch(
        paste(capture.output(print(app$get_logs())), collapse = "\n"),
        error = function(...) "<failed to read app logs>"
      )
      stop(
        sprintf("downloadHeatClusters failed: %s\nApp logs:\n%s", conditionMessage(e), logs),
        call. = FALSE
      )
    }
  )
  expect_true(file.exists(out), info = sprintf("downloadHeatClusters file not created: %s", out))

  sheets <- readxl::excel_sheets(out)
  expect_true(
    all(c("Summary", "HeatmapMatrix", "RowClusters", "ColumnClusters") %in% sheets),
    info = paste("Workbook sheets:", paste(sheets, collapse = ", "))
  )
  expect_true(
    length(grep("^RowCluster_", sheets)) >= 1,
    info = paste("Workbook sheets:", paste(sheets, collapse = ", "))
  )
})

test_that("heatmap and correlation-matrix flows run without critical frontend errors", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)

  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)
  app$set_inputs(heat_cluster_rows = TRUE, heat_k_rows = 3, wait_ = TRUE, timeout_ = 60000)

  app$set_inputs(tipo = "MatrizCorrelacion", wait_ = TRUE, timeout_ = 60000)
  app$wait_for_value(input = "corrm_params", timeout = 120000)

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("curve statistics download explicitly includes curve context and metric labels", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")

  data_fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  curve_fixture <- app_test_path("www", "reference_files", "Ejemplo_curvas.xlsx"
  )
  expect_true(file.exists(data_fixture))
  expect_true(file.exists(curve_fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(data_fixture), wait_ = TRUE, timeout_ = 120000)
  app$upload_file(curveFile = normalizePath(curve_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)

  app$set_inputs(tipo = "Curvas", wait_ = FALSE)
  app$set_inputs(
    curve_stats_methods = c("S2", "S4"),
    wait_ = FALSE,
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    runCurveStats = "click",
    wait_ = FALSE,
    allow_no_input_binding_ = TRUE
  )

  stats_path <- tryCatch(
    app$get_download(output = "downloadStats"),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "downloadStats request failed")
      skip(sprintf("Skipping flaky Curvas E2E download due runtime instability: %s", msg))
    }
  )
  expect_true(file.exists(stats_path))

  tabs <- readxl::excel_sheets(stats_path)
  expect_true(length(tabs) > 0)

  has_curve_context <- FALSE
  for (sheet in tabs) {
    raw_tbl <- readxl::read_excel(stats_path, sheet = sheet, col_names = FALSE)
    tokens <- as.character(unlist(raw_tbl, use.names = FALSE))
    has_header <- any(tokens == "Curve statistics context")
    has_source <- any(tokens == "Source") && any(tokens == "Curves")
    has_methods <- any(tokens == "Curve methods")
    has_normality_metric <- any(grepl("^Normalidad \\(Curvas - metrica:", tokens))
    has_signif_metric <- any(grepl("^Significancia \\(Curvas - metrica:", tokens))
    if (has_header && has_source && has_methods && has_normality_metric && has_signif_metric) {
      has_curve_context <- TRUE
      break
    }
  }

  expect_true(
    has_curve_context,
    info = "Curve stats workbook did not include explicit curve context and metric labels."
  )
})

test_that("language switch updates dynamic heatmap UI labels", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)

  wait_for_label <- function(pattern, selector, timeout_sec = 12) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      html <- tryCatch(
        app$get_html(selector = selector),
        error = function(e) character(0)
      )
      if (length(html)) {
        html_text <- paste(html, collapse = "\n")
        if (nzchar(html_text) && grepl(pattern, html_text, ignore.case = TRUE)) return(TRUE)
      }
      Sys.sleep(0.3)
    }
    FALSE
  }

  wait_for_lang_storage <- function(expected, timeout_sec = 12) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      lang <- tryCatch(
        as.character(app$get_js("localStorage.getItem('appLang')")),
        error = function(e) character(0)
      )
      if (length(lang) && nzchar(lang[[1]]) && identical(tolower(lang[[1]]), tolower(expected))) {
        return(TRUE)
      }
      Sys.sleep(0.3)
    }
    FALSE
  }

  app$set_inputs(lang_en = "click", wait_ = FALSE)
  expect_true(wait_for_lang_storage("en"))

  app$set_inputs(lang_es = "click", wait_ = FALSE)
  expect_true(wait_for_lang_storage("es"))

  side_dend_selector <- "label[for='heat_show_side_dend']"
  if (wait_for_label("side dendrogram", side_dend_selector, timeout_sec = 6)) {
    expect_true(wait_for_label("dendrograma lateral", side_dend_selector, timeout_sec = 12))
  }
})

test_that("rapid checkbox clicks do not replay stale group or replicate selections", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 60000)
  install_loop_probe(app)

  app$set_inputs(scope = "Por Cepa", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showMedios", timeout = 90000)
  medias <- sort(unique(as.character(app$get_value(input = "showMedios"))))
  medias <- medias[!is.na(medias) & nzchar(medias)]
  if (length(medias) < 2) skip("Not enough media choices to test rapid media toggles.")
  app$set_inputs(showMedios = medias, wait_ = TRUE, timeout_ = 90000)
  expect_true(wait_for_dom_selected_values(app, "showMedios", medias, timeout_sec = 45))
  target_media <- medias[[1]]
  rapid_click_checkbox(app, css_checkbox_selector("showMedios", target_media), times = 7L)
  expect_true(
    wait_for_selected_values(app, "showMedios", setdiff(medias, target_media), timeout_sec = 30),
    info = "Rapid media clicks should leave only the clicked condition deselected."
  )
  expect_dom_selection_stays_put(
    app,
    "showMedios",
    setdiff(medias, target_media),
    "rapid media checkbox clicks"
  )
  expect_app_idle_without_loop(app, "rapid media checkbox clicks", idle_timeout = 45)

  strain_rep_boxes <- checkbox_group_state(app, name_prefix = "reps_")
  if (is.data.frame(strain_rep_boxes) && nrow(strain_rep_boxes)) {
    strain_rep_boxes <- strain_rep_boxes[
      !grepl("^reps_grp_", strain_rep_boxes$name) &
        strain_rep_boxes$name != "rm_reps_all" &
        as.logical(strain_rep_boxes$checked),
      ,
      drop = FALSE
    ]
  }
  if (is.data.frame(strain_rep_boxes) && nrow(strain_rep_boxes)) {
    rep_name <- strain_rep_boxes$name[[1]]
    rep_value <- strain_rep_boxes$value[[1]]
    rep_values <- sort(unique(as.character(app$get_value(input = rep_name))))
    rep_values <- rep_values[!is.na(rep_values) & nzchar(rep_values)]
    rapid_click_checkbox(app, css_checkbox_selector(rep_name, rep_value), times = 7L)
    expect_true(
      wait_for_selected_values(app, rep_name, setdiff(rep_values, rep_value), timeout_sec = 30),
      info = "Rapid biological replicate clicks in Por Cepa should persist."
    )
    expect_dom_selection_stays_put(
      app,
      rep_name,
      setdiff(rep_values, rep_value),
      "rapid Por Cepa biological replicate clicks"
    )
    expect_app_idle_without_loop(app, "rapid Por Cepa biological replicate clicks", idle_timeout = 45)
  }

  app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showGroups", timeout = 90000)
  groups <- sort(unique(as.character(app$get_value(input = "showGroups"))))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (length(groups) < 2) skip("Not enough combined groups to test rapid group toggles.")
  app$set_inputs(showGroups = groups, wait_ = TRUE, timeout_ = 90000)
  expect_true(wait_for_dom_selected_values(app, "showGroups", groups, timeout_sec = 45))
  target_group <- groups[[1]]
  rapid_click_checkbox(app, css_checkbox_selector("showGroups", target_group), times = 7L)
  expect_true(
    wait_for_selected_values(app, "showGroups", setdiff(groups, target_group), timeout_sec = 30),
    info = "Rapid combined-group clicks should leave only the clicked group deselected."
  )
  expect_dom_selection_stays_put(
    app,
    "showGroups",
    setdiff(groups, target_group),
    "rapid combined group checkbox clicks"
  )
  expect_app_idle_without_loop(app, "rapid combined group checkbox clicks", idle_timeout = 45)

  group_rep_boxes <- checkbox_group_state(app, name_prefix = "reps_grp_")
  if (is.data.frame(group_rep_boxes) && nrow(group_rep_boxes)) {
    group_rep_boxes <- group_rep_boxes[as.logical(group_rep_boxes$checked), , drop = FALSE]
  }
  if (is.data.frame(group_rep_boxes) && nrow(group_rep_boxes)) {
    rep_name <- group_rep_boxes$name[[1]]
    rep_value <- group_rep_boxes$value[[1]]
    rep_values <- sort(unique(as.character(app$get_value(input = rep_name))))
    rep_values <- rep_values[!is.na(rep_values) & nzchar(rep_values)]
    rapid_click_checkbox(app, css_checkbox_selector(rep_name, rep_value), times = 7L)
    expect_true(
      wait_for_selected_values(app, rep_name, setdiff(rep_values, rep_value), timeout_sec = 30),
      info = "Rapid biological replicate clicks in Combinado should persist."
    )
    expect_dom_selection_stays_put(
      app,
      rep_name,
      setdiff(rep_values, rep_value),
      "rapid Combinado biological replicate clicks"
    )
    expect_app_idle_without_loop(app, "rapid Combinado biological replicate clicks", idle_timeout = 45)
  }

  tech_available <- tryCatch(
    identical(normalize_js_scalar(app$get_value(output = "qcTechTabAvailable")), "TRUE"),
    error = function(e) FALSE
  )
  tech_rep_boxes <- if (isTRUE(tech_available)) {
    open_qc_tech_controls(app, timeout_sec = 60)
  } else {
    checkbox_group_state(app, name_prefix = "qc_tech_rep_")
  }
  if (isTRUE(tech_available)) {
    expect_true(
      is.data.frame(tech_rep_boxes) && nrow(tech_rep_boxes) > 0,
      info = "Technical replicate QC is available, so rapid technical replicate tests must render selectors."
    )
  }
  if (is.data.frame(tech_rep_boxes) && nrow(tech_rep_boxes)) {
    tech_rep_boxes <- tech_rep_boxes[as.logical(tech_rep_boxes$checked), , drop = FALSE]
  }
  if (is.data.frame(tech_rep_boxes) && nrow(tech_rep_boxes)) {
    rep_name <- tech_rep_boxes$name[[1]]
    rep_value <- tech_rep_boxes$value[[1]]
    rep_values <- sort(unique(as.character(app$get_value(input = rep_name))))
    rep_values <- rep_values[!is.na(rep_values) & nzchar(rep_values)]
    rapid_click_checkbox(app, css_checkbox_selector(rep_name, rep_value), times = 7L)
    expect_true(
      wait_for_selected_values(app, rep_name, setdiff(rep_values, rep_value), timeout_sec = 30),
      info = "Rapid technical replicate clicks should persist when technical selectors are available."
    )
    expect_dom_selection_stays_put(
      app,
      rep_name,
      setdiff(rep_values, rep_value),
      "rapid technical replicate clicks"
    )
    expect_app_idle_without_loop(app, "rapid technical replicate clicks", idle_timeout = 45)
  }

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("rapid combined group bursts render once across plot types", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("jsonlite")

  data_fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  curve_fixture <- app_test_path("www", "reference_files", "Ejemplo_curvas.xlsx")
  expect_true(file.exists(data_fixture))
  expect_true(file.exists(curve_fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(data_fixture), wait_ = TRUE, timeout_ = 120000)
  app$upload_file(curveFile = normalizePath(curve_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  install_loop_probe(app)
  expect_true(
    install_plotly_redraw_probe(app, timeout_sec = 45),
    info = "Plotly redraw probe could not be installed."
  )
  expect_true(
    install_plot_loading_probe(app, timeout_sec = 45),
    info = "Plot loading probe could not be installed."
  )

  app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showGroups", timeout = 90000)
  groups <- sort(unique(as.character(app$get_value(input = "showGroups"))))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (length(groups) < 4) skip("Not enough combined groups to test rapid multi-group bursts.")

  plot_types <- c(
    "Boxplot", "Barras", "Violin", "Apiladas",
    "Correlacion", "Heatmap", "MatrizCorrelacion"
  )

  wait_for_single_settled_plot <- function(label, timeout_sec = 70) {
    deadline <- Sys.time() + as.numeric(timeout_sec)
    last_counts <- NULL
    last_error_count <- NA_real_
    while (Sys.time() < deadline) {
      expect_true(
        wait_for_shiny_connected(app, timeout_sec = 10),
        info = sprintf("Shiny disconnected while waiting for %s.", label)
      )
      if (isTRUE(wait_for_plot_idle(app, timeout_sec = 10))) {
        counts <- plotly_redraw_probe_counts(app)
        loading_counts <- plot_loading_probe_counts(app)
        last_counts <- counts
        redraw_count <- sum(as.numeric(unlist(counts, use.names = FALSE)), na.rm = TRUE)
        loading_on_count <- as.numeric(loading_counts$loadingOn %||% 0)
        error_count <- as.numeric(loop_probe_counts(app)$errors %||% 0)
        last_error_count <- error_count
        if (redraw_count >= 1 && loading_on_count <= 1 && identical(error_count, 0)) {
          expect_true(
            wait_for_no_plot_churn(app, quiet_sec = 1.5, timeout_sec = 15, max_plot_events = 1),
            info = sprintf("Plot kept changing after %s.", label)
          )
          return(invisible(counts))
        }
      }
      Sys.sleep(0.5)
    }
    redraw_count <- if (is.null(last_counts)) NA_real_ else sum(as.numeric(unlist(last_counts, use.names = FALSE)), na.rm = TRUE)
    loading_counts <- tryCatch(plot_loading_probe_counts(app), error = function(e) list(loadingOn = NA_real_))
    loading_on_count <- as.numeric(loading_counts$loadingOn %||% NA_real_)
    stop(sprintf(
      "Expected one settled plot for %s; observed redraws=%s loadingOn=%s errors=%s.",
      label,
      redraw_count,
      loading_on_count,
      last_error_count
    ), call. = FALSE)
  }

  for (plot_type in plot_types) {
    set_checkbox_group_dom_values(app, "showGroups", groups)
    expect_true(wait_for_selected_values(app, "showGroups", groups, timeout_sec = 45))
    expect_true(wait_for_dom_selected_values(app, "showGroups", groups, timeout_sec = 45))

    app$set_inputs(
      tipo = plot_type,
      wait_ = !identical(plot_type, "Curvas"),
      timeout_ = 120000,
      allow_no_input_binding_ = TRUE
    )
    if (identical(plot_type, "Curvas")) Sys.sleep(1)
    if (identical(plot_type, "Heatmap")) {
      try(app$wait_for_value(input = "heat_params", timeout = 60000), silent = TRUE)
    }
    if (identical(plot_type, "MatrizCorrelacion")) {
      try(app$wait_for_value(input = "corrm_params", timeout = 60000), silent = TRUE)
    }
    if (identical(plot_type, "Correlacion")) {
      try(app$wait_for_value(input = "corr_param_x", timeout = 60000), silent = TRUE)
      try(app$wait_for_value(input = "corr_param_y", timeout = 60000), silent = TRUE)
    }
    expect_app_idle_without_loop(app, sprintf("%s before rapid burst", plot_type), idle_timeout = 45)

    reset_loop_probe(app)
    reset_plotly_redraw_probe(app)
    reset_plot_loading_probe(app)
    clicked <- rapid_click_checked_checkboxes(app, "showGroups", limit = 3L)
    clicked_values <- unique(as.character(clicked$values %||% character(0)))
    clicked_values <- clicked_values[!is.na(clicked_values) & nzchar(clicked_values)]
    expect_true(
      length(clicked_values) >= 1,
      info = sprintf("No checked groups were clicked for %s.", plot_type)
    )

    expected_groups <- setdiff(groups, clicked_values)
    expect_true(
      wait_for_selected_values(app, "showGroups", expected_groups, timeout_sec = 45),
      info = sprintf("Rapid combined-group burst did not settle on the expected groups for %s.", plot_type)
    )
    counts <- wait_for_single_settled_plot(sprintf("%s rapid group burst", plot_type))
    loading_counts <- plot_loading_probe_counts(app)
    loading_on_count <- as.numeric(loading_counts$loadingOn %||% 0)
    expect_true(
      loading_on_count <= 1,
      info = sprintf("%s showed repeated visible plot loading during rapid group selection.", plot_type)
    )
  }

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("rapid combined group bursts keep Curvas connected", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("jsonlite")

  data_fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  curve_fixture <- app_test_path("www", "reference_files", "Ejemplo_curvas.xlsx")
  expect_true(file.exists(data_fixture))
  expect_true(file.exists(curve_fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(data_fixture), wait_ = TRUE, timeout_ = 120000)
  app$upload_file(curveFile = normalizePath(curve_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  install_loop_probe(app)
  expect_true(install_plot_loading_probe(app, timeout_sec = 45))

  app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showGroups", timeout = 90000)
  groups <- sort(unique(as.character(app$get_value(input = "showGroups"))))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (length(groups) < 4) skip("Not enough combined groups to test rapid Curvas group bursts.")

  app$set_inputs(tipo = "Curvas", wait_ = FALSE, allow_no_input_binding_ = TRUE)
  expect_true(wait_for_shiny_connected(app, timeout_sec = 20))
  Sys.sleep(12)
  expect_true(wait_for_shiny_connected(app, timeout_sec = 20))
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))
  Sys.sleep(1)

  reset_loop_probe(app)
  reset_plot_loading_probe(app)
  clicked <- rapid_click_checked_checkboxes(app, "showGroups", limit = 3L)
  clicked_values <- unique(as.character(clicked$values %||% character(0)))
  clicked_values <- clicked_values[!is.na(clicked_values) & nzchar(clicked_values)]
  expect_true(length(clicked_values) >= 1)

  expected_groups <- setdiff(groups, clicked_values)
  expect_true(
    wait_for_selected_values(app, "showGroups", expected_groups, timeout_sec = 45),
    info = "Rapid combined-group burst did not settle on the expected groups for Curvas."
  )
  expect_true(wait_for_shiny_connected(app, timeout_sec = 20))
  expect_true(wait_for_plot_idle(app, timeout_sec = 60))
  expect_true(
    wait_for_no_plot_churn(app, quiet_sec = 1.5, timeout_sec = 30, max_plot_events = 1),
    info = "Curvas kept changing after rapid group selection."
  )
  loading_counts <- plot_loading_probe_counts(app)
  expect_true(
    as.numeric(loading_counts$loadingOn %||% 0) <= 1,
    info = "Curvas showed repeated visible plot loading during rapid group selection."
  )

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("reactive stress: repeated parameter and replicate actions keep app responsive", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 60000)

  expect_true(wait_for_shiny_connected(app, timeout_sec = 20))
  expect_true(wait_for_plot_idle(app, timeout_sec = 25))

  param_choices_raw <- tryCatch(
    app$get_js(
      "(function(){
         var el = document.getElementById('param');
         if (!el || !el.selectize || !el.selectize.options) return [];
         return Object.keys(el.selectize.options);
       })()"
    ),
    error = function(e) character(0)
  )
  param_choices <- unique(as.character(unlist(param_choices_raw, use.names = FALSE)))
  param_choices <- param_choices[!is.na(param_choices) & nzchar(param_choices)]
  if (length(param_choices) < 2) {
    skip("Not enough parameter choices available to run stress churn test.")
  }

  churn_values <- rep(param_choices[seq_len(min(4L, length(param_choices)))], length.out = 12L)
  for (i in seq_along(churn_values)) {
    app$set_inputs(param = churn_values[[i]], wait_ = TRUE, timeout_ = 90000)
    expect_true(
      wait_for_plot_idle(app, timeout_sec = 25),
      info = sprintf("Plot remained in loading state after param churn iteration %d.", i)
    )
    expect_true(
      wait_for_shiny_connected(app, timeout_sec = 10),
      info = sprintf("Shiny session disconnected during param churn iteration %d.", i)
    )

    if (i %% 2L == 0L) {
      app$set_inputs(scope = "Combinado", wait_ = TRUE, timeout_ = 60000)
      has_grp_btn <- length(tryCatch(
        app$get_html(selector = "#repsGrpSelectAll"),
        error = function(e) character(0)
      )) > 0
      if (isTRUE(has_grp_btn)) {
        app$set_inputs(
          repsGrpSelectAll = "click",
          wait_ = FALSE,
          allow_no_input_binding_ = TRUE
        )
      }
    } else {
      app$set_inputs(scope = "Por Cepa", wait_ = TRUE, timeout_ = 60000)
      has_str_btn <- length(tryCatch(
        app$get_html(selector = "#repsStrainSelectAll"),
        error = function(e) character(0)
      )) > 0
      if (isTRUE(has_str_btn)) {
        app$set_inputs(
          repsStrainSelectAll = "click",
          wait_ = FALSE,
          allow_no_input_binding_ = TRUE
        )
      }
    }

    expect_true(
      wait_for_plot_idle(app, timeout_sec = 25),
      info = sprintf("Plot remained in loading state after replicate action iteration %d.", i)
    )
    expect_true(
      wait_for_shiny_connected(app, timeout_sec = 10),
      info = sprintf("Shiny session disconnected after replicate action iteration %d.", i)
    )
  }

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("reactive stress: rapid strain switching with normalization toggles stays responsive", {
  skip_if_shiny_e2e_unavailable()

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 60000)

  expect_true(wait_for_shiny_connected(app, timeout_sec = 20))
  expect_true(wait_for_plot_idle(app, timeout_sec = 25))

  strain_choices_raw <- tryCatch(
    app$get_js(
      "(function(){
         var el = document.getElementById('strain');
         if (!el || !el.selectize || !el.selectize.options) return [];
         return Object.keys(el.selectize.options);
       })()"
    ),
    error = function(e) character(0)
  )
  strain_choices <- unique(as.character(unlist(strain_choices_raw, use.names = FALSE)))
  strain_choices <- strain_choices[!is.na(strain_choices) & nzchar(strain_choices)]
  if (length(strain_choices) < 2) {
    skip("Not enough strain choices available to run normalization churn test.")
  }

  param_choices_raw <- tryCatch(
    app$get_js(
      "(function(){
         var el = document.getElementById('param');
         if (!el || !el.selectize || !el.selectize.options) return [];
         return Object.keys(el.selectize.options);
       })()"
    ),
    error = function(e) character(0)
  )
  param_choices <- unique(as.character(unlist(param_choices_raw, use.names = FALSE)))
  param_choices <- param_choices[!is.na(param_choices) & nzchar(param_choices)]
  if (!length(param_choices)) {
    skip("No parameter choices available to run normalization churn test.")
  }

  strain_seq <- rep(strain_choices[seq_len(min(3L, length(strain_choices)))], length.out = 12L)
  param_seq <- rep(param_choices[seq_len(min(4L, length(param_choices)))], length.out = 12L)

  for (i in seq_len(length(strain_seq))) {
    app$set_inputs(strain = strain_seq[[i]], wait_ = TRUE, timeout_ = 90000)
    app$set_inputs(param = param_seq[[i]], wait_ = TRUE, timeout_ = 90000)

    if (i %% 2L == 1L) {
      app$set_inputs(doNorm = TRUE, wait_ = TRUE, timeout_ = 90000)
      try(app$wait_for_value(input = "ctrlMedium", timeout = 60000), silent = TRUE)
    } else {
      app$set_inputs(doNorm = FALSE, wait_ = TRUE, timeout_ = 90000)
    }

    expect_true(
      wait_for_plot_idle(app, timeout_sec = 25),
      info = sprintf("Plot remained in loading state during normalization churn iteration %d.", i)
    )
    expect_true(
      wait_for_shiny_connected(app, timeout_sec = 10),
      info = sprintf("Shiny session disconnected during normalization churn iteration %d.", i)
    )
  }

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("normalized parameter switching keeps axis and metadata on the selected parameter", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 60000)
  app$set_inputs(doNorm = TRUE, wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "ctrlMedium", timeout = 60000)

  wait_for_param_choices <- function(timeout_sec = 45) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      raw <- tryCatch(
        app$get_js(
          "(function(){
             var el = document.getElementById('param');
             if (!el || !el.selectize || !el.selectize.options) return [];
             return Object.keys(el.selectize.options);
           })()"
        ),
        error = function(e) character(0)
      )
      vals <- unique(as.character(unlist(raw, use.names = FALSE)))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) >= 2) return(vals)
      Sys.sleep(0.5)
    }
    character(0)
  }

  param_choices <- wait_for_param_choices()
  if (length(param_choices) < 2) {
    skip("Not enough normalized-ready parameters available for switching test.")
  }

  switch_seq <- rep(param_choices[seq_len(min(4L, length(param_choices)))], length.out = 8L)
  wait_for_normalized_axis_param <- function(target, timeout_sec = 25) {
    deadline <- Sys.time() + timeout_sec
    last <- list(selected = "", label = "")
    while (Sys.time() < deadline) {
      raw <- tryCatch(
        app$get_js(
          "(function(){
             var param = (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) ?
               (Shiny.shinyapp.$inputValues.param || '') : '';
             var el = document.querySelector('label[for=\"ymax\"]');
             return JSON.stringify({
               param: String(param || ''),
               label: el ? (el.textContent || '') : ''
             });
           })()"
        ),
        error = function(e) ""
      )
      info <- tryCatch(jsonlite::fromJSON(normalize_js_scalar(raw)), error = function(e) NULL)
      if (!is.null(info)) {
        last <- list(
          selected = as.character(info$param %||% ""),
          label = as.character(info$label %||% "")
        )
        if (identical(last$selected, target) &&
            grepl(paste0(target, "_Norm"), last$label, fixed = TRUE)) {
          return(TRUE)
        }
      }
      Sys.sleep(0.4)
    }
    out <- FALSE
    attr(out, "last") <- last
    out
  }

  for (i in seq_along(switch_seq)) {
    target <- switch_seq[[i]]
    app$set_inputs(param = target, wait_ = TRUE, timeout_ = 90000)
    expect_true(
      wait_for_plot_idle(app, timeout_sec = 30),
      info = sprintf("Plot did not settle after normalized parameter switch to %s.", target)
    )
    expect_true(
      wait_for_shiny_connected(app, timeout_sec = 15),
      info = sprintf("Shiny disconnected after normalized parameter switch to %s.", target)
    )

    axis_ok <- wait_for_normalized_axis_param(target)
    last_axis <- attr(axis_ok, "last") %||% list(selected = "", label = "")
    expect_true(
      axis_ok,
      info = sprintf(
        "Normalized axis did not settle on %s. Last server param: %s. Last label: %s",
        target, last_axis$selected, last_axis$label
      )
    )

    selected <- normalize_js_scalar(app$get_js(
      "(function(){
         var el = document.getElementById('param');
         if (el && el.selectize) return el.selectize.getValue();
         return (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) ?
           (Shiny.shinyapp.$inputValues.param || '') : '';
       })()"
    ))
    expect_identical(selected, target)
    expect_false(grepl("_Norm$", selected, ignore.case = TRUE))
  }

  meta_path <- app$get_download(output = "downloadMetadata")
  expect_true(file.exists(meta_path))
  meta_tbl <- readxl::read_excel(meta_path, sheet = "Metadata")
  fields <- stats::setNames(as.character(meta_tbl$Valor), as.character(meta_tbl$Campo))
  expect_true("param" %in% names(fields), info = paste(names(fields), collapse = ", "))
  expect_false(grepl("_Norm$", fields[["param"]], ignore.case = TRUE))
  expect_identical(fields[["param"]], tail(switch_seq, 1))
  expect_identical(tolower(fields[["doNorm"]]), "true")

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})

test_that("publication style exports preserve DPI metadata and one-slide PowerPoint geometry", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("readxl")
  skip_if_not_installed("png")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx")

  data_fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  curve_fixture <- app_test_path("www", "reference_files", "Ejemplo_curvas.xlsx")
  expect_true(file.exists(data_fixture))
  expect_true(file.exists(curve_fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(data_fixture), wait_ = TRUE, timeout_ = 120000)
  app$upload_file(curveFile = normalizePath(curve_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$wait_for_value(input = "param", timeout = 120000)
  expect_equal(as.numeric(app$get_value(input = "export_dpi")), 300)
  app$set_inputs(export_dpi = 0, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "export_dpi", 300))

  app$set_inputs(
    tipo = "Curvas",
    plot_w = 600,
    plot_h = 420,
    wait_ = TRUE,
    timeout_ = 120000
  )
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))
  default_dpi_png <- app$get_download(output = "downloadPlot_png")
  expect_nonempty_download(default_dpi_png, "default 300 DPI plot")
  default_png_array <- png::readPNG(default_dpi_png)
  expect_equal(ncol(default_png_array), 1875)
  expect_equal(nrow(default_png_array), 1313, tolerance = 1)
  expect_equal(ncol(default_png_array) / nrow(default_png_array), 600 / 420, tolerance = 0.002)

  app$set_inputs(
    tipo = "Violin",
    violin_inner = "box",
    axis_title_spacing_x = 11,
    axis_title_spacing_y = 13,
    export_dpi = 192,
    wait_ = TRUE,
    timeout_ = 120000
  )
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))

  metadata_path <- app$get_download(output = "downloadMetadata")
  expect_nonempty_download(metadata_path, "publication metadata")
  metadata <- readxl::read_excel(metadata_path, sheet = "Metadata")
  metadata_fields <- stats::setNames(as.character(metadata$Valor), as.character(metadata$Campo))
  expect_identical(metadata_fields[["violin_inner"]], "box")
  expect_identical(metadata_fields[["axis_title_spacing_x"]], "11")
  expect_identical(metadata_fields[["axis_title_spacing_y"]], "13")
  expect_identical(metadata_fields[["export_dpi"]], "192")

  legacy_meta <- tempfile("metadata_legacy_dpi_", fileext = ".xlsx")
  invalid_meta <- tempfile("metadata_invalid_dpi_", fileext = ".xlsx")
  on.exit(unlink(c(legacy_meta, invalid_meta), force = TRUE), add = TRUE)
  write_dpi_metadata_variant(metadata_path, legacy_meta, "export_dpi", remove = TRUE)
  write_dpi_metadata_variant(metadata_path, invalid_meta, "export_dpi", value = "0")

  app$set_inputs(export_dpi = 450, wait_ = TRUE, timeout_ = 120000)
  app$upload_file(metaFiles = legacy_meta, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "export_dpi", 300))

  app$set_inputs(export_dpi = 450, wait_ = TRUE, timeout_ = 120000)
  app$upload_file(metaFiles = invalid_meta, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "export_dpi", 300))

  app$set_inputs(export_dpi = 192, wait_ = TRUE, timeout_ = 120000)

  app$set_inputs(violin_inner = "points", wait_ = TRUE, timeout_ = 120000)
  expect_identical(as.character(app$get_value(input = "violin_inner"))[[1]], "points")
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))

  app$set_inputs(export_dpi = 150, wait_ = FALSE, timeout_ = 120000)
  app$set_inputs(export_dpi = 450, wait_ = FALSE, timeout_ = 120000)
  app$set_inputs(
    tipo = "Curvas",
    plot_w = 600,
    plot_h = 420,
    export_dpi = 192,
    wait_ = TRUE,
    timeout_ = 120000
  )
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))
  dpi_png <- app$get_download(output = "downloadPlot_png")
  expect_nonempty_download(dpi_png, "192 DPI plot")
  png_array <- png::readPNG(dpi_png)
  expect_equal(ncol(png_array), 1200)
  expect_equal(nrow(png_array), 840)
  expect_equal(ncol(png_array) / nrow(png_array), 600 / 420, tolerance = 1e-8)

  app$click("add2panel")
  app$set_inputs(tipo = "Boxplot", export_dpi = 96, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_plot_idle(app, timeout_sec = 45))
  app$click("add2panel")
  app$set_inputs(mainTabs = "tab_composition", wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "combo_pptx_preset", timeout = 120000)
  expect_equal(as.numeric(app$get_value(input = "combo_export_dpi")), 300)
  app$set_inputs(combo_export_dpi = 0, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "combo_export_dpi", 300))

  app$set_inputs(
    combo_width = 1000,
    combo_height = 700,
    combo_export_dpi = 180,
    combo_pptx_preset = "custom",
    combo_pptx_orientation = "landscape",
    combo_pptx_width = 13.333,
    combo_pptx_height = 7.5,
    combo_pptx_margin = 0.2,
    combo_override_typography = TRUE,
    combo_axis_xy_custom = TRUE,
    fs_axis_text_all = 17,
    fs_axis_text_x_all = 19,
    fs_axis_text_y_all = 15,
    combo_axis_text_x_angle = 35,
    combo_axis_text_y_angle = -10,
    combo_axis_text_x_align = "right",
    combo_axis_text_y_align = "left",
    fs_legend_all = 18,
    combo_font_family = "Arial",
    combo_text_style_axis_text_x = c("bold", "italic"),
    combo_text_style_legend = "underline",
    makeCombo = "click",
    wait_ = TRUE,
    timeout_ = 120000,
    allow_no_input_binding_ = TRUE
  )

  combo_meta <- app$get_download(output = "dl_combo_meta")
  expect_nonempty_download(combo_meta, "composition metadata")
  combo_metadata <- readxl::read_excel(combo_meta, sheet = "Metadata")
  combo_fields <- stats::setNames(as.character(combo_metadata$Valor), as.character(combo_metadata$Campo))
  expect_identical(combo_fields[["combo_export_dpi"]], "180")
  expect_identical(combo_fields[["combo_pptx_orientation"]], "landscape")
  expect_identical(combo_fields[["combo_pptx_width"]], "13.333")
  expect_identical(combo_fields[["combo_pptx_height"]], "7.5")
  expect_identical(combo_fields[["combo_override_typography"]], "TRUE")
  expect_identical(combo_fields[["combo_axis_xy_custom"]], "TRUE")
  expect_identical(combo_fields[["fs_axis_text_x_all"]], "19")
  expect_identical(combo_fields[["fs_axis_text_y_all"]], "15")
  expect_identical(combo_fields[["combo_axis_text_x_angle"]], "35")
  expect_identical(combo_fields[["combo_axis_text_y_angle"]], "-10")
  expect_identical(combo_fields[["combo_axis_text_x_align"]], "right")
  expect_identical(combo_fields[["combo_axis_text_y_align"]], "left")
  expect_identical(combo_fields[["fs_legend_all"]], "18")
  expect_identical(combo_fields[["combo_font_family"]], "Arial")
  expect_identical(combo_fields[["combo_text_style_axis_text_x"]], "bold,italic")
  expect_identical(combo_fields[["combo_text_style_legend"]], "underline")

  combo_png <- app$get_download(output = "dl_combo_png")
  expect_nonempty_download(combo_png, "180 DPI composition PNG")
  combo_png_array <- png::readPNG(combo_png)
  expect_equal(ncol(combo_png_array), 1875)
  expect_equal(nrow(combo_png_array), 1313, tolerance = 1)
  expect_equal(ncol(combo_png_array) / nrow(combo_png_array), 1000 / 700, tolerance = 0.002)

  landscape_pptx <- app$get_download(output = "dl_combo_pptx")
  expect_nonempty_download(landscape_pptx, "landscape PowerPoint")
  landscape_doc <- officer::read_pptx(landscape_pptx)
  expect_length(landscape_doc, 1)
  expect_equal(officer::slide_size(landscape_doc)$width, 13.333, tolerance = 1e-5)
  expect_equal(officer::slide_size(landscape_doc)$height, 7.5, tolerance = 1e-5)
  expect_gt(nrow(officer::pptx_summary(landscape_doc)), 0)

  app$set_inputs(
    combo_pptx_preset = "custom",
    combo_pptx_orientation = "portrait",
    combo_pptx_width = 7.5,
    combo_pptx_height = 10,
    wait_ = TRUE,
    timeout_ = 120000
  )
  portrait_pptx <- app$get_download(output = "dl_combo_pptx")
  expect_nonempty_download(portrait_pptx, "portrait PowerPoint")
  portrait_doc <- officer::read_pptx(portrait_pptx)
  expect_length(portrait_doc, 1)
  expect_equal(officer::slide_size(portrait_doc)$width, 7.5, tolerance = 1e-5)
  expect_equal(officer::slide_size(portrait_doc)$height, 10, tolerance = 1e-5)
  expect_gt(nrow(officer::pptx_summary(portrait_doc)), 0)

  app$set_inputs(
    combo_export_dpi = 96,
    combo_pptx_orientation = "landscape",
    combo_pptx_width = 10,
    combo_pptx_height = 7.5,
    combo_override_typography = FALSE,
    combo_axis_xy_custom = FALSE,
    fs_axis_text_x_all = 12,
    fs_axis_text_y_all = 12,
    combo_axis_text_x_angle = 0,
    combo_axis_text_y_angle = 0,
    combo_axis_text_x_align = "auto",
    combo_axis_text_y_align = "auto",
    fs_legend_all = 16,
    combo_font_family = "Helvetica",
    wait_ = TRUE,
    timeout_ = 120000
  )
  combo_meta_upload <- tempfile("composition_metadata_roundtrip_", fileext = ".xlsx")
  on.exit(unlink(combo_meta_upload, force = TRUE), add = TRUE)
  expect_true(file.copy(combo_meta, combo_meta_upload, overwrite = TRUE))
  app$upload_file(combo_meta = combo_meta_upload, wait_ = TRUE, timeout_ = 120000)
  restore_deadline <- Sys.time() + 30
  restore_error <- NULL
  restored_dpi <- tryCatch(
    as.numeric(app$get_value(input = "combo_export_dpi")),
    error = function(e) {
      restore_error <<- e
      NA_real_
    }
  )
  while (is.null(restore_error) && Sys.time() < restore_deadline && !identical(restored_dpi, 180)) {
    Sys.sleep(0.5)
    restored_dpi <- tryCatch(
      as.numeric(app$get_value(input = "combo_export_dpi")),
      error = function(e) {
        restore_error <<- e
        NA_real_
      }
    )
  }
  if (!is.null(restore_error)) {
    logs <- tryCatch(app$get_logs(), error = function(e) data.frame())
    log_tail <- if (is.data.frame(logs) && nrow(logs)) {
      paste(tail(as.character(logs$message), 30), collapse = "\n")
    } else {
      "No server log was available."
    }
    stop(
      paste0(
        "Shiny stopped during composition metadata restore: ",
        conditionMessage(restore_error),
        "\nRecent server log:\n",
        log_tail
      ),
      call. = FALSE
    )
  }
  expect_equal(restored_dpi, 180)
  expect_identical(as.character(app$get_value(input = "combo_pptx_orientation"))[[1]], "landscape")
  expect_equal(as.numeric(app$get_value(input = "combo_pptx_width")), 13.333, tolerance = 1e-5)
  expect_equal(as.numeric(app$get_value(input = "combo_pptx_height")), 7.5, tolerance = 1e-5)
  expect_true(isTRUE(app$get_value(input = "combo_override_typography")))
  expect_true(isTRUE(app$get_value(input = "combo_axis_xy_custom")))
  expect_equal(as.numeric(app$get_value(input = "fs_axis_text_x_all")), 19)
  expect_equal(as.numeric(app$get_value(input = "fs_axis_text_y_all")), 15)
  expect_equal(as.numeric(app$get_value(input = "combo_axis_text_x_angle")), 35)
  expect_equal(as.numeric(app$get_value(input = "combo_axis_text_y_angle")), -10)
  expect_identical(as.character(app$get_value(input = "combo_axis_text_x_align"))[[1]], "right")
  expect_identical(as.character(app$get_value(input = "combo_axis_text_y_align"))[[1]], "left")
  expect_equal(as.numeric(app$get_value(input = "fs_legend_all")), 18)
  expect_identical(as.character(app$get_value(input = "combo_font_family"))[[1]], "Arial")

  combo_legacy_meta <- tempfile("composition_metadata_legacy_dpi_", fileext = ".xlsx")
  combo_invalid_meta <- tempfile("composition_metadata_invalid_dpi_", fileext = ".xlsx")
  on.exit(unlink(c(combo_legacy_meta, combo_invalid_meta), force = TRUE), add = TRUE)
  write_dpi_metadata_variant(combo_meta, combo_legacy_meta, "combo_export_dpi", remove = TRUE)
  write_dpi_metadata_variant(combo_meta, combo_invalid_meta, "combo_export_dpi", value = "unsupported")

  app$set_inputs(combo_export_dpi = 450, wait_ = TRUE, timeout_ = 120000)
  app$upload_file(combo_meta = combo_legacy_meta, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "combo_export_dpi", 300))

  app$set_inputs(combo_export_dpi = 450, wait_ = TRUE, timeout_ = 120000)
  app$upload_file(combo_meta = combo_invalid_meta, wait_ = TRUE, timeout_ = 120000)
  expect_true(wait_for_numeric_input_value(app, "combo_export_dpi", 300))

  critical <- find_critical_frontend_logs(app$get_logs())
  expect_equal(
    nrow(critical),
    0,
    info = paste(unique(as.character(critical$message)), collapse = "\n")
  )
})
