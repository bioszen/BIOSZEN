library(testthat)

test_that("announcement state write failures do not interrupt the app", {
  server_txt <- paste(
    readLines(app_test_path("server", "server_main.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  function_txt <- regmatches(
    server_txt,
    regexpr(
      "(?s)(mark_seen_announcement <- function\\(.*?\\n\\})(?=\\n\\nshould_show_announcement <- function)",
      server_txt,
      perl = TRUE
    )
  )
  expect_length(function_txt, 1L)

  env <- new.env(parent = baseenv())
  env$announcements_state_path <- function(pkg = "BIOSZEN") "blocked/seen_announcements.rds"
  env$saveRDS <- function(...) stop("permission denied")
  eval(parse(text = function_txt), envir = env)

  expect_false(env$mark_seen_announcement("release-2.0.4"))
})
