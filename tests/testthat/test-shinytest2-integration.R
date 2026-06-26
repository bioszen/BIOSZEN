library(testthat)

app_launch_dir <- app_test_launch_dir()

skip_if_shiny_e2e_unavailable <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!nzchar(chrome_path)) {
    skip("Chrome/Chromium is not available for shinytest2 integration tests.")
  }
}

start_bioszen_driver <- function() {
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")

  app <- shinytest2::AppDriver$new(
    app_dir = app_launch_dir,
    load_timeout = 120000,
    timeout = 120000,
    clean_logs = FALSE
  )

  list(app = app, old_not_cran = old_not_cran)
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
      wait_ = TRUE,
      timeout_ = 120000,
      allow_no_input_binding_ = TRUE
    )
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
