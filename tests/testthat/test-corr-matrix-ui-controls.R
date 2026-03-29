test_that("correlation matrix controls are gated by matrix plot type", {
  ui_file <- test_path("..", "..", "inst", "app", "ui", "ui_main.R")
  expect_true(file.exists(ui_file))

  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    txt,
    "condition\\s*=\\s*\"input\\.tipo\\s*==\\s*'MatrizCorrelacion'\"",
    perl = TRUE
  )

  expect_match(
    txt,
    paste0(
      "condition\\s*=\\s*\"input\\.tipo\\s*==\\s*'MatrizCorrelacion'\"",
      "[\\s\\S]{0,2500}?\"corrm_params\"",
      "[\\s\\S]{0,1200}?\"corrm_method\"",
      "[\\s\\S]{0,1200}?\"corrm_adjust\"",
      "[\\s\\S]{0,1200}?\"corrm_show_sig\"",
      "[\\s\\S]{0,1200}?\"corrm_order_profile\""
    ),
    perl = TRUE
  )

  count_literal <- function(needle, haystack) {
    pos <- gregexpr(needle, haystack, fixed = TRUE)[[1]]
    if (length(pos) == 1L && identical(pos[[1]], -1L)) return(0L)
    length(pos)
  }

  expect_equal(count_literal("\"corrm_params\"", txt), 1L)
  expect_equal(count_literal("\"corrm_method\"", txt), 1L)
  expect_equal(count_literal("\"corrm_adjust\"", txt), 1L)
  expect_equal(count_literal("\"corrm_show_sig\"", txt), 1L)
  expect_equal(count_literal("\"corrm_order_profile\"", txt), 1L)
})

test_that("initial plot-type choices include correlation matrix", {
  ui_file <- test_path("..", "..", "inst", "app", "ui", "ui_main.R")
  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    txt,
    paste0(
      "c\\(\\s*\"Boxplot\"\\s*,\\s*\"Barras\"\\s*,\\s*\"Violin\"\\s*,\\s*\"Curvas\"\\s*,\\s*",
      "\"Apiladas\"\\s*,\\s*\"Correlacion\"\\s*,\\s*\"Heatmap\"\\s*,\\s*\"MatrizCorrelacion\"\\s*\\)"
    ),
    perl = TRUE
  )
})
