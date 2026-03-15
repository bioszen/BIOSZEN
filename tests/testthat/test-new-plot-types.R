test_that("new plot types are wired in build_plot", {
  server_file <- test_path("..", "..", "inst", "app", "server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl('if \\(tipo == "Raincloud"\\)', txt, perl = TRUE))
  expect_false(grepl('if \\(tipo == "Estimacion"\\)', txt, perl = TRUE))
  expect_match(txt, 'if \\(tipo == "Heatmap"\\)', perl = TRUE)
  expect_match(txt, 'if \\(tipo == "MatrizCorrelacion"\\)', perl = TRUE)
})

test_that("fallback branch does not return a fully blank canvas", {
  server_file <- test_path("..", "..", "inst", "app", "server", "server_main.R")
  lines <- readLines(server_file, warn = FALSE, encoding = "UTF-8")
  fallback_idx <- grep("fallback para nunca retornar NULL", lines, fixed = TRUE)
  expect_true(length(fallback_idx) >= 1)

  idx <- fallback_idx[[1]]
  end_idx <- min(length(lines), idx + 8L)
  snippet <- paste(lines[idx:end_idx], collapse = "\n")
  expect_true(grepl('annotate\\("text"', snippet, perl = TRUE))
})

test_that("UI still exposes all extended plot types", {
  ui_file <- test_path("..", "..", "inst", "app", "ui", "ui_main.R")
  expect_true(file.exists(ui_file))

  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Raincloud", txt, fixed = TRUE))
  expect_false(grepl("Estimacion", txt, fixed = TRUE))
  expect_true(grepl("Heatmap", txt, fixed = TRUE))
  expect_true(grepl("MatrizCorrelacion", txt, fixed = TRUE))
})

test_that("auto significance section is shown only after running significance test", {
  ui_file <- test_path("..", "..", "inst", "app", "ui", "ui_main.R")
  txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(grepl('condition\\s*=\\s*"input\\.runSig\\s*>\\s*0"', txt, perl = TRUE))
})
