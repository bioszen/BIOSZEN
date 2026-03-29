library(testthat)

app_root <- normalizePath(testthat::test_path("..", ".."))

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
    app_dir = app_root,
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

test_that("browser upload flow keeps searchable selectors and no critical frontend errors", {
  skip_if_shiny_e2e_unavailable()

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
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

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
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

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
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

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
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

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
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

  data_fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  curve_fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_curvas.xlsx"
  )
  expect_true(file.exists(data_fixture))
  expect_true(file.exists(curve_fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(data_fixture), wait_ = TRUE, timeout_ = 120000)
  app$upload_file(curveFile = normalizePath(curve_fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)

  app$set_inputs(tipo = "Curvas", wait_ = TRUE, timeout_ = 60000)
  app$set_inputs(curve_stats_methods = c("S2", "S4"), wait_ = TRUE, timeout_ = 120000)

  wait_for_curve_methods <- function(expected, timeout_sec = 15) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      current <- tryCatch(
        as.character(app$get_value(input = "curve_stats_methods")),
        error = function(e) character(0)
      )
      if (length(current) && setequal(current, expected)) return(TRUE)
      Sys.sleep(0.3)
    }
    FALSE
  }

  wait_for_curve_stats_table <- function(timeout_sec = 20) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      html <- tryCatch(
        app$get_html(selector = "#curveStatsTable"),
        error = function(e) character(0)
      )
      if (length(html)) {
        html_text <- paste(html, collapse = "\n")
        if (nzchar(html_text) && grepl("table|dataTable|datatable", html_text, ignore.case = TRUE)) {
          return(TRUE)
        }
      }
      Sys.sleep(0.3)
    }
    FALSE
  }

  expect_true(wait_for_curve_methods(c("S2", "S4")))
  app$set_inputs(runCurveStats = 1, wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "runCurveStats", ignore = list(0), timeout = 120000)
  expect_true(wait_for_curve_stats_table())

  stats_path <- app$get_download(output = "downloadStats", timeout_ = 180000)
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

  fixture <- file.path(
    app_root, "inst", "app", "www", "reference_files", "Ejemplo_platemap_parametros.xlsx"
  )
  expect_true(file.exists(fixture))

  ctx <- start_bioszen_driver()
  on.exit(stop_bioszen_driver(ctx), add = TRUE)
  app <- ctx$app

  app$upload_file(dataFile = normalizePath(fixture), wait_ = TRUE, timeout_ = 120000)
  app$wait_for_value(input = "strain", timeout = 120000)
  app$set_inputs(tipo = "Heatmap", wait_ = TRUE, timeout_ = 60000)

  wait_for_label <- function(pattern, timeout_sec = 12) {
    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
      html <- tryCatch(
        app$get_html(selector = "label[for='heat_cluster_rows']"),
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

  expect_true(wait_for_label("Show side dendrogram"))

  app$set_inputs(app_lang = "es", wait_ = TRUE, timeout_ = 60000)
  app$wait_for_value(input = "app_lang", ignore = list("en"), timeout = 120000)
  expect_true(wait_for_label("dendrograma lateral"))
})
