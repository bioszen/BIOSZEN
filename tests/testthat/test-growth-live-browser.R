library(testthat)

.bioszen_live_growth_fixture <- function(path, n_points = 120L, n_wells = 24L) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  time <- seq_len(n_points)
  data <- data.frame(Ignore1 = time, Ignore2 = time)
  for (index in seq_len(n_wells)) {
    rate <- 0.025 + (index %% 7L) * 0.002
    data[[sprintf("W%02d", index)]] <-
      0.08 + 1.2 / (1 + exp(-rate * (time - 55))) + 0.002 * sin(time + index)
  }
  openxlsx::writeData(wb, "Sheet1", data, startRow = 3, colNames = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

.bioszen_live_growth_table_rows <- function(app) {
  raw <- app$get_js(
    "(function(){
       var table = document.querySelector('#growthTable table');
       if (!table) return 0;
       return Array.from(table.querySelectorAll('tbody tr')).filter(function(row){
         var text = (row.innerText || row.textContent || '').trim();
         return row.querySelectorAll('td').length > 1 &&
           !/No data available|No hay datos disponibles/i.test(text);
       }).length;
     })()"
  )
  values <- suppressWarnings(as.integer(unlist(raw, use.names = FALSE)))
  if (!length(values) || is.na(values[[1L]])) 0L else values[[1L]]
}

test_that("growth rows reach the browser before a long calculation finishes", {
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")
  on.exit({
    if (is.na(old_not_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_not_cran)
  }, add = TRUE)

  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("openxlsx")
  skip_on_cran()

  in_r_cmd_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")) ||
    grepl("\\.Rcheck(/|$)", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  skip_if(in_r_cmd_check, "Browser E2E tests run in the GitHub/local test_dir lane.")

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!nzchar(chrome_path)) skip("Chrome/Chromium is not available.")

  fixture <- tempfile("bioszen_growth_live_", fileext = ".xlsx")
  .bioszen_live_growth_fixture(fixture)
  on.exit(unlink(fixture, force = TRUE), add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = app_test_launch_dir(),
    load_timeout = 240000,
    timeout = 240000,
    clean_logs = FALSE,
    options = list(warn = 1)
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  activated <- app$get_js(
    "(function(){
       var links = Array.from(document.querySelectorAll(
         'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"], .nav-link'
       ));
       var link = links.find(function(item){
         return /Growth Parameter Extraction/i.test((item.textContent || '').trim());
       });
       if (!link) return false;
       link.click();
       return true;
     })()"
  )
  expect_true(isTRUE(as.logical(unlist(activated, use.names = FALSE)[[1L]])))

  app$upload_file(
    growthFiles = normalizePath(fixture),
    wait_ = TRUE,
    timeout_ = 240000
  )
  app$wait_for_value(input = "growthFilesKeep", timeout = 240000)
  app$set_inputs(maxTime = 119, timeInterval = 1, wait_ = TRUE, timeout_ = 60000)

  started <- app$get_js(
    "(function(){
       var button = document.getElementById('runGrowth');
       if (!button || button.disabled) return false;
       button.click();
       return true;
     })()"
  )
  expect_true(isTRUE(as.logical(unlist(started, use.names = FALSE)[[1L]])))

  deadline <- Sys.time() + 60
  live_rows <- 0L
  completed <- FALSE
  while (Sys.time() < deadline && live_rows < 1L && !completed) {
    live_rows <- tryCatch(.bioszen_live_growth_table_rows(app), error = function(e) 0L)
    status <- tryCatch(
      paste(unlist(app$get_js(
        "(function(){
           var el = document.getElementById('growthStatus');
           return el ? (el.innerText || el.textContent || '') : '';
         })()"
      ), use.names = FALSE), collapse = ""),
      error = function(e) ""
    )
    completed <- grepl("Completed", status, ignore.case = TRUE)
    if (live_rows < 1L && !completed) Sys.sleep(0.2)
  }

  expect_gte(live_rows, 1L, info = "A final per-well row should be visible before completion.")
  expect_false(completed, info = "The assertion must observe a live update, not only final output.")

  app$get_js(
    "(function(){
       var button = document.getElementById('stopGrowth');
       if (button && !button.disabled) button.click();
       return true;
     })()"
  )
})
