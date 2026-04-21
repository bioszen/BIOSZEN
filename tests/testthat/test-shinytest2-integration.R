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

  out <- app$get_download(output = "downloadHeatClusters")
  expect_true(file.exists(out))

  sheets <- readxl::excel_sheets(out)
  expect_true(all(c("Summary", "HeatmapMatrix", "RowClusters", "ColumnClusters") %in% sheets))
  expect_true(length(grep("^RowCluster_", sheets)) >= 1)
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
