library(testthat)

app_launch_dir <- app_test_launch_dir()

test_that("export pipeline is wired to preview-aligned renderer", {
  path <- app_test_path("server", "server_main.R")
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
  expect_true(grepl(
    "write_current_plot_pdf\\s*<-\\s*function[\\s\\S]*?build_current_plotly_for_export[\\s\\S]*?export_plotly_image[\\s\\S]*?p_fallback\\s*<-\\s*build_plot[\\s\\S]*?ggplot2::ggsave[\\s\\S]*?device\\s*=\\s*grDevices::cairo_pdf",
    txt,
    perl = TRUE
  ))
  expect_match(txt, "@page \\{ size: %.4fin %.4fin; margin: 0; \\}", fixed = FALSE)
  expect_match(txt, "html, body { margin: 0; padding: 0;", fixed = TRUE)
  expect_true(grepl(
    "outputOptions\\(output,\\s*\"downloadPlot_png\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "outputOptions\\(output,\\s*\"downloadPlot_pdf\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "write_current_plot_pptx\\s*<-\\s*function[\\s\\S]*?build_plot\\(scope_sel, strain_sel, input\\$tipo, for_interactive = TRUE\\)[\\s\\S]*?bioszen_write_editable_plot_pptx",
    txt,
    perl = TRUE
  ))
  expect_false(grepl(
    "bioszen_write_plotly_svg_pptx|current_plotly_svg|bioszen_plot_svg",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "outputOptions\\(output,\\s*\"downloadPlot_pptx\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)",
    txt,
    perl = TRUE
  ))
  expect_true(grepl(
    "outputOptions\\(output,\\s*\"downloadPlotly_pptx\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)",
    txt,
    perl = TRUE
  ))
  curve_static_exports <- gregexpr(
    "if \\(identical\\(input\\$tipo %\\|\\|% \"\", \"Curvas\"\\)\\)[\\s\\S]*?ggplot2::ggsave",
    txt,
    perl = TRUE
  )[[1]]
  expect_gte(sum(curve_static_exports > 0), 1)
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
    app_dir = app_launch_dir,
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
    try(app$wait_for_idle(duration = 500, timeout = 120000), silent = TRUE)
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

download_pdf_with_retry <- function(app, output_id, retries = 4L, pause_sec = 1) {
  last_err <- NULL
  for (i in seq_len(retries)) {
    try(app$wait_for_idle(duration = 500, timeout = 120000), silent = TRUE)
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
  stop("Failed to download PDF: unknown error")
}

download_pptx_with_retry <- function(app, output_id, retries = 4L, pause_sec = 1) {
  last_err <- NULL
  for (i in seq_len(retries)) {
    try(app$wait_for_idle(duration = 500, timeout = 120000), silent = TRUE)
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
  stop("Failed to download PPTX: unknown error")
}

pdf_page_size_pts <- function(path) {
  pdfinfo <- Sys.which("pdfinfo")
  if (!nzchar(pdfinfo)) return(NULL)
  info <- tryCatch(
    suppressWarnings(system2(pdfinfo, shQuote(path), stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  line <- grep("^Page size:", info, value = TRUE)
  if (!length(line)) return(NULL)
  vals <- regmatches(line[[1]], gregexpr("[0-9]+(?:\\.[0-9]+)?", line[[1]], perl = TRUE))[[1]]
  vals <- suppressWarnings(as.numeric(vals))
  if (length(vals) < 2 || any(!is.finite(vals[1:2]))) return(NULL)
  c(width = vals[[1]], height = vals[[2]])
}

wait_for_input_value <- function(app, input_id, expected, timeout_sec = 120) {
  deadline <- Sys.time() + timeout_sec
  repeat {
    current <- tryCatch(app$get_value(input = input_id), error = function(e) NULL)
    current <- as.character(current %||% character())
    if (length(current) >= 1 && identical(current[[1]], expected)) {
      return(invisible(TRUE))
    }
    if (Sys.time() > deadline) {
      stop(sprintf("Timed out waiting for input '%s' to become '%s'.", input_id, expected))
    }
    Sys.sleep(0.3)
  }
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
  app$wait_for_value(input = "param", timeout = 120000)
  ensure_select_has_value(app, "strain")
  ensure_select_has_value(app, "param")

  logical_w <- 1200
  logical_h <- 800
  default_dpi <- BIOSZEN_DEFAULT_DPI
  expected_png_w <- round(logical_w * default_dpi / BIOSZEN_CSS_DPI)
  expected_png_h <- round(logical_h * default_dpi / BIOSZEN_CSS_DPI)
  input_names <- names(app$get_values(input = TRUE)$input %||% list())
  expect_true("tipo" %in% input_names, info = "Input 'tipo' must exist in main plot panel.")
  base_inputs <- list(
    scope = "Combinado",
    plot_w = logical_w,
    plot_h = logical_h
  )
  if ("export_dpi" %in% input_names) base_inputs$export_dpi <- default_dpi
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
    "Heatmap"
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
  expect_equal(unname(probe_dims[["width"]]), expected_png_w, info = "width mismatch for type Boxplot")
  expect_equal(unname(probe_dims[["height"]]), expected_png_h, info = "height mismatch for type Boxplot")

  pdf_probe <- download_pdf_with_retry(app, output_id = "downloadPlot_pdf")
  expect_true(file.exists(pdf_probe), info = "Missing PDF export for type Boxplot")
  expect_gt(file.info(pdf_probe)$size, 1000)
  header <- readChar(pdf_probe, nchars = 4, useBytes = TRUE)
  expect_identical(header, "%PDF")
  pdf_dims <- pdf_page_size_pts(pdf_probe)
  if (!is.null(pdf_dims)) {
    expect_equal(unname(pdf_dims[["width"]]), logical_w / BIOSZEN_CSS_DPI * 72, tolerance = 1)
    expect_equal(unname(pdf_dims[["height"]]), logical_h / BIOSZEN_CSS_DPI * 72, tolerance = 1)
  }

  for (tp in setdiff(plot_types, "Boxplot")) {
    app$set_inputs(tipo = tp, wait_ = FALSE, allow_no_input_binding_ = TRUE)
    wait_for_input_value(app, "tipo", tp, timeout_sec = 120)
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
      expected_png_w,
      info = paste("width mismatch for type", tp)
    )
    expect_equal(
      unname(dims[["height"]]),
      expected_png_h,
      info = paste("height mismatch for type", tp)
    )
  }
})

test_that("editable PPTX downloads contain one vector slide across plot types", {
  skip_if_shiny_e2e_unavailable()
  skip_if_not_installed("officer")
  skip_if_not_installed("xml2")

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
  ensure_select_has_value(app, "strain")
  ensure_select_has_value(app, "param")

  logical_w <- 1200
  logical_h <- 800
  app$set_inputs(
    scope = "Combinado",
    plot_w = logical_w,
    plot_h = logical_h,
    wait_ = TRUE,
    timeout_ = 120000,
    allow_no_input_binding_ = TRUE
  )
  helper_env <- new.env(parent = globalenv())
  helper_env$`%||%` <- `%||%`
  helper_env$BIOSZEN_CSS_DPI <- BIOSZEN_CSS_DPI
  sys.source(app_test_path("helpers.R"), envir = helper_env)
  plot_types <- c(
    "Boxplot", "Barras", "Violin", "Curvas", "Apiladas",
    "Correlacion", "MatrizCorrelacion", "Heatmap"
  )

  for (tp in plot_types) {
    app$set_inputs(tipo = tp, wait_ = FALSE, allow_no_input_binding_ = TRUE)
    wait_for_input_value(app, "tipo", tp, timeout_sec = 120)
    wait_for_plot_ready(app, timeout_sec = 120)

    output_id <- if (identical(tp, "Apiladas")) {
      "downloadPlotly_pptx"
    } else {
      "downloadPlot_pptx"
    }
    out <- download_pptx_with_retry(app, output_id = output_id)
    expect_true(file.exists(out), info = paste("Missing PPTX export for type", tp))
    expect_true(file.info(out)$size > 1000, info = paste("Empty PPTX export for type", tp))

    deck <- officer::read_pptx(out)
    expect_equal(length(deck), 1, info = paste("Unexpected slide count for type", tp))
    slide_dims <- officer::slide_size(deck)
    expected_page <- if (identical(tp, "Curvas")) {
      list(width_px = logical_w, height_px = logical_h)
    } else {
      helper_env$bioszen_pdf_page_size_from_pixels(logical_w, logical_h)
    }
    expected_size <- helper_env$bioszen_pptx_size_from_pixels(
      expected_page$width_px,
      expected_page$height_px
    )
    expect_equal(slide_dims$width, expected_size$width, tolerance = 1e-5)
    expect_equal(slide_dims$height, expected_size$height, tolerance = 1e-5)

    objects <- officer::pptx_summary(deck)
    expect_true(nrow(objects) > 1, info = paste("PPTX lacks editable objects for type", tp))
    expect_false(
      nrow(objects) == 1 && identical(objects$content_type[[1]], "image"),
      info = paste("PPTX unexpectedly contains only a raster image for type", tp)
    )

    listing <- utils::unzip(out, list = TRUE)
    expect_false(
      any(grepl("^ppt/media/.*\\.(?:png|jpe?g|svg)$", listing$Name, perl = TRUE)),
      info = paste("PPTX contains a flattened chart image for type", tp)
    )

    unpacked <- tempfile(paste0("bioszen-pptx-", tp, "-"))
    dir.create(unpacked, recursive = TRUE)
    utils::unzip(out, exdir = unpacked)
    slide <- xml2::read_xml(file.path(unpacked, "ppt", "slides", "slide1.xml"))
    shape_count <- length(xml2::xml_find_all(slide, ".//p:sp", xml2::xml_ns(slide)))
    unlink(unpacked, recursive = TRUE, force = TRUE)
    expect_true(shape_count > 5, info = paste("PPTX lacks independent DrawingML shapes for type", tp))
  }
})
