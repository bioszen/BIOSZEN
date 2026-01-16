library(testthat)

# Verify that whisker caps stay present in builds/exports even with near-constant data.

test_that("boxplot whisker caps are drawn for export", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  source(testthat::test_path("..", "..", "inst", "app", "graficos", "boxplot_caps.R"))

  df <- data.frame(
    Group = "A",
    Value = c(rep(1, 6), 2, 0.6)
  )

  stats_df <- df %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      q1     = stats::quantile(Value, 0.25, na.rm = TRUE, names = FALSE),
      median = stats::median(Value, na.rm = TRUE),
      q3     = stats::quantile(Value, 0.75, na.rm = TRUE, names = FALSE),
      lower  = min(Value, na.rm = TRUE),
      upper  = max(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(group = as.character(Group)) %>%
    dplyr::select(group, q1, median, q3, lower, upper)

  p <- ggplot2::ggplot(df, ggplot2::aes(Group, Value)) +
    ggplot2::geom_boxplot(coef = 1e6, outlier.shape = NA)
  p_cap <- draw_whisker_caps(p, stats_df, levels(df$Group), box_width = 0.8, line_width = 0.6)

  build <- ggplot2::ggplot_build(p_cap)
  seg_layers <- vapply(build$data, function(d) all(c("xend", "yend") %in% names(d)), logical(1))
  expect_true(any(seg_layers))
  seg_df <- build$data[[which(seg_layers)[1]]]
  expect_true(any(seg_df$x != seg_df$xend))

  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = p_cap, width = 3, height = 3, dpi = 300, bg = "white")
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})
