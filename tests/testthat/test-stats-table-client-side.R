test_that("stats table uses client-side DT rendering", {
  server_file <- app_test_path("server", "server_main.R")
  expect_true(file.exists(server_file))

  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    txt,
    "output\\$statsTable\\s*<-\\s*renderDT\\([\\s\\S]*?\\},\\s*server\\s*=\\s*FALSE\\)",
    perl = TRUE
  )
})
