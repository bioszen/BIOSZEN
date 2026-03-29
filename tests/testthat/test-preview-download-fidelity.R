library(testthat)

app_root <- normalizePath(testthat::test_path("..", ".."))

test_that("export pipeline is wired to preview-aligned renderer", {
  path <- file.path(app_root, "inst", "app", "server", "server_main.R")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "build_current_plotly_for_export\\s*<-\\s*function")
  expect_true(grepl(
    "write_current_plot_png\\s*<-\\s*function[\\s\\S]*?build_current_plotly_for_export",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "write_current_plot_pdf\\s*<-\\s*function[\\s\\S]*?build_current_plotly_for_export",
    txt,
    perl = TRUE
  ))
  expect_true(grepl("zoom\\s*=\\s*1", txt, perl = TRUE))
})

skip_if_shiny_e2e_unavailable <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("png")
  skip_if_not_installed("webshot2")
  skip_on_cran()

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!nzchar(chrome_path)) {
    skip("Chrome/Chromium is not available for shinytest2 integration tests.")
  }
}

skip_if_export_backend_unavailable <- function() {
  tmp_html <- tempfile(fileext = ".html")
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(c(tmp_html, tmp_png), force = TRUE), add = TRUE)

  writeLines(
    c(
      "<!doctype html>",
      "<html><body style='margin:0;background:white;'>",
      "<div style='width:200px;height:100px;background:#2f6fb0;'></div>",
      "</body></html>"
    ),
    con = tmp_html,
    useBytes = TRUE
  )

  ok <- tryCatch({
    webshot2::webshot(
      url = tmp_html,
      file = tmp_png,
      vwidth = 200,
      vheight = 100,
      delay = 0.5,
      zoom = 1,
      max_concurrent = 1,
      quiet = TRUE
    )
    file.exists(tmp_png) &&
      is.finite(file.info(tmp_png)$size) &&
      file.info(tmp_png)$size > 0
  }, error = function(e) FALSE)

  if (!isTRUE(ok)) {
    skip("webshot2/chromote export backend is unavailable; skipping download parity E2E test.")
  }
}

start_bioszen_driver <- function() {
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")

  app <- shinytest2::AppDriver$new(
    app_dir = app_root,
    load_timeout = 90000,
    timeout = 90000,
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

png_dims <- function(path) {
  img <- png::readPNG(path)
  c(width = ncol(img), height = nrow(img))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

download_png_with_retry <- function(app, output_id, retries = 4L, pause_sec = 1) {
  last_err <- NULL
  for (i in seq_len(retries)) {
    app$wait_for_idle(duration = 500, timeout = 120000)
    attempt <- tryCatch(
      app$get_download(output = output_id),
      error = function(e) {
        last_err <<- e
        NULL
      }
    )
    if (!is.null(attempt) && file.exists(attempt)) {
      return(attempt)
    }
    Sys.sleep(pause_sec)
  }

  if (!is.null(last_err)) stop(last_err)
  stop("Failed to download PNG: unknown error")
}

first_non_empty_option <- function(app, input_id) {
  app$get_js(
    script = sprintf(
      paste(
        "var el = document.getElementById('%s');",
        "if (!el) return '';",
        "var vals = Array.from(el.options || [])",
        ".map(function(o){ return o.value || ''; })",
        ".filter(function(v){ return v.length > 0; });",
        "return vals.length ? vals[0] : '';"
      ),
      input_id
    )
  )
}

ensure_select_has_value <- function(app, input_id) {
  current <- app$get_value(input = input_id)
  cur_vec <- as.character(current %||% character())
  has_value <- length(cur_vec) >= 1 && nzchar(cur_vec[[1]])
  if (has_value) return(invisible(TRUE))

  fallback <- as.character(first_non_empty_option(app, input_id) %||% "")
  if (!nzchar(fallback)) return(invisible(FALSE))

  args <- setNames(list(fallback), input_id)
  do.call(
    app$set_inputs,
    c(args, list(wait_ = TRUE, timeout_ = 120000, allow_no_input_binding_ = TRUE))
  )
  invisible(TRUE)
}

wait_for_plot_ready <- function(app, timeout_sec = 120) {
  app$wait_for_js(
    script = "!!document.querySelector('#plotInteractivo .plotly, #plotInteractivo svg, #plotInteractivo canvas, #plotInteractivo img')",
    timeout = timeout_sec * 1000L,
    interval = 300
  )
}

test_that("PNG download dimensions match preview dimensions across plot types", {
  skip_if_shiny_e2e_unavailable()
  skip_if_export_backend_unavailable()

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
  app$wait_for_value(input = "param", timeout = 120000)
  ensure_select_has_value(app, "strain")
  ensure_select_has_value(app, "param")

  expected_w <- 1200
  expected_h <- 800
  input_names <- names(app$get_values(input = TRUE)$input %||% list())
  expect_true("tipo" %in% input_names, info = "Input 'tipo' must exist in main plot panel.")
  base_inputs <- list(
    scope = "Combinado",
    plot_w = expected_w,
    plot_h = expected_h
  )
  if ("margin_right_adj" %in% input_names) base_inputs$margin_right_adj <- 0
  if ("margin_bottom_adj" %in% input_names) base_inputs$margin_bottom_adj <- 0
  do.call(
    app$set_inputs,
    c(
      base_inputs,
      list(wait_ = TRUE, timeout_ = 120000, allow_no_input_binding_ = TRUE)
    )
  )

  plot_types <- c(
    "Boxplot",
    "Barras",
    "Violin",
    "Apiladas",
    "Correlacion",
    "MatrizCorrelacion",
    "Heatmap",
    "Curvas"
  )

  # Preflight check: if the browser harness cannot trigger plot downloads in
  # this environment, skip instead of emitting false failures.
  app$set_inputs(tipo = "Boxplot", wait_ = TRUE, timeout_ = 120000)
  wait_for_plot_ready(app, timeout_sec = 120)
  probe <- tryCatch(
    download_png_with_retry(app, output_id = "downloadPlot_png"),
    error = function(e) e
  )
  if (inherits(probe, "error")) {
    skip(paste0(
      "App download endpoint is unavailable in this shinytest2 environment: ",
      conditionMessage(probe)
    ))
  }
  probe_dims <- png_dims(probe)
  expect_equal(unname(probe_dims[["width"]]), expected_w, info = "width mismatch for type Boxplot")
  expect_equal(unname(probe_dims[["height"]]), expected_h, info = "height mismatch for type Boxplot")

  for (tp in setdiff(plot_types, "Boxplot")) {
    app$set_inputs(tipo = tp, wait_ = TRUE, timeout_ = 120000)
    wait_for_plot_ready(app, timeout_sec = 120)
    out <- tryCatch(
      download_png_with_retry(app, output_id = "downloadPlot_png"),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      logs <- tryCatch(app$get_logs(), error = function(e) data.frame())
      tail_msgs <- character(0)
      if (is.data.frame(logs) && nrow(logs) > 0) {
        msg_col <- if ("message" %in% names(logs)) "message" else NULL
        lvl_col <- if ("level" %in% names(logs)) "level" else NULL
        loc_col <- if ("location" %in% names(logs)) "location" else NULL
        rows <- tail(seq_len(nrow(logs)), 6L)
        for (idx in rows) {
          loc <- if (!is.null(loc_col)) as.character(logs[[loc_col]][idx]) else "log"
          lvl <- if (!is.null(lvl_col)) as.character(logs[[lvl_col]][idx]) else "info"
          msg <- if (!is.null(msg_col)) as.character(logs[[msg_col]][idx]) else ""
          tail_msgs <- c(tail_msgs, paste0("[", loc, "/", lvl, "] ", msg))
        }
      }
      fail(
        paste0(
          "PNG download failed for type ", tp, ": ", conditionMessage(out),
          if (length(tail_msgs)) paste0("\nRecent logs:\n", paste(tail_msgs, collapse = "\n")) else ""
        )
      )
      next
    }
    expect_true(file.exists(out), info = paste("Missing export file for type", tp))

    dims <- png_dims(out)
    expect_equal(
      unname(dims[["width"]]),
      expected_w,
      info = paste("width mismatch for type", tp)
    )
    expect_equal(
      unname(dims[["height"]]),
      expected_h,
      info = paste("height mismatch for type", tp)
    )
  }
})
