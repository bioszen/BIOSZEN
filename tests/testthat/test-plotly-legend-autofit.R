testthat::test_that("plotly legend autofit avoids clipping", {
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("htmlwidgets")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("chromote")

  root <- normalizePath(testthat::test_path("..", ".."))
  helper_path <- file.path(root, "inst", "app", "graficos", "plotly_autofit.R")
  source(helper_path, local = TRUE)

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_html <- file.path(tmp_dir, "plot.html")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  label <- "Rapa-U18"
  df <- data.frame(x = c(1, 2), y = c(1, 2), g = c(label, "Control"))
  colors <- c("Control" = "#2c7fb8", "#41ab5d")
  names(colors)[2] <- label
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, color = g)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_minimal()
  p <- plotly::ggplotly(p, width = 260, height = 220)
  p <- plotly::layout(
    p,
    showlegend = TRUE,
    legend = list(
      x = 1, xref = "paper", xanchor = "left",
      y = 1, yanchor = "top",
      font = list(color = "rgb(255,0,0)", size = 16)
    ),
    margin = list(l = 30, r = 0, t = 20, b = 30),
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  )

  p <- plotly_autofit_widget(p, force_legend_right = TRUE)
  htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)

  chrome_candidates <- c(
    Sys.getenv("CHROMOTE_CHROME"),
    Sys.getenv("CHROME_PATH"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]
  testthat::skip_if(is.na(chrome_path) || !nzchar(chrome_path), "Chrome/Edge not found")
  Sys.setenv(CHROMOTE_CHROME = chrome_path)

  session <- chromote::ChromoteSession$new()
  on.exit(session$close(), add = TRUE)
  page_url <- paste0("file:///", normalizePath(tmp_html, winslash = "/", mustWork = TRUE))
  session$default_timeout <- 60000
  session$go_to(page_url)
  js <- paste(
    "(async () => {",
    "  const deadline = Date.now() + 30000;",
    "  while (Date.now() < deadline) {",
    "    if (!window.__bioszenRendered && window.HTMLWidgets && window.HTMLWidgets.staticRender) {",
    "      window.HTMLWidgets.staticRender();",
    "      window.__bioszenRendered = true;",
    "    }",
    "    const gd = document.querySelector('.js-plotly-plot') || document.querySelector('.plotly');",
    "    const legend = gd ? (gd.querySelector('.legend') || gd.querySelector('g.legend'))",
    "                     : (document.querySelector('.legend') || document.querySelector('g.legend'));",
    "    const svg = gd ? (gd.querySelector('.main-svg') || gd.querySelector('svg'))",
    "                  : (document.querySelector('.main-svg') || document.querySelector('svg'));",
    "    if (gd && window.Plotly && gd._fullLayout && !gd.__bioszenResized) {",
    "      gd.__bioszenResized = true;",
    "      try { Plotly.Plots.resize(gd); } catch (e) {}",
    "    }",
    "    if (legend && svg) {",
    "      const clips = legend.querySelectorAll('[clip-path]').length;",
    "      const legendRect = legend.getBoundingClientRect();",
    "      const svgRect = svg.getBoundingClientRect();",
    "      return {",
    "        clipCount: clips,",
    "        legendRight: legendRect ? legendRect.right : null,",
    "        svgRight: svgRect ? svgRect.right : null",
    "      };",
    "    }",
    "    await new Promise(r => setTimeout(r, 100));",
    "  }",
    "  return null;",
    "})()",
    sep = "\\n"
  )
  res <- session$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)
  vals <- res$result$value
  testthat::skip_if(is.null(vals), "Legend DOM unavailable")
  testthat::expect_true(is.finite(vals$clipCount))
  testthat::expect_true(
    vals$clipCount == 0,
    info = paste("legend clip paths:", vals$clipCount)
  )
  testthat::expect_true(is.finite(vals$legendRight))
  testthat::expect_true(is.finite(vals$svgRight))
  testthat::expect_true(
    vals$legendRight <= vals$svgRight + 1,
    info = paste("legend overflow:", vals$legendRight - vals$svgRight)
  )
})

testthat::test_that("plotly autofit injects legend padding", {
  testthat::skip_if_not_installed("plotly")
  root <- normalizePath(testthat::test_path("..", ".."))
  helper_path <- file.path(root, "inst", "app", "graficos", "plotly_autofit.R")
  source(helper_path, local = TRUE)

  p <- plotly::plot_ly(x = c(1, 2), y = c(1, 2), type = "scatter", mode = "markers")
  w <- plotly_autofit_widget(p, force_legend_right = TRUE)
  js <- w$jsHooks$render[[1]]$code
  testthat::expect_true(grepl("legendPadFixed", js, fixed = TRUE))
  testthat::expect_true(grepl("legendBuffer = 6", js, fixed = TRUE))
})
