library(testthat)

skip_if_significance_e2e_unavailable <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_on_cran()
  in_r_cmd_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")) ||
    grepl("\\.Rcheck(/|$)", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  skip_if(in_r_cmd_check, "Browser E2E tests run in the dedicated workflow lane.")
  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!nzchar(chrome_path)) skip("Chrome/Chromium is unavailable.")
}

normalize_browser_values <- function(value) {
  out <- as.character(unlist(value, use.names = FALSE))
  unique(out[!is.na(out) & nzchar(out)])
}

selectize_option_labels <- function(app, input_id) {
  js <- sprintf(
    "(function(){
       var el = document.getElementById(%s);
       if (!el) return [];
       var rendered = Array.prototype.map.call(el.options || [], function(item){
         return String(item.text || item.value || '');
       });
       if (rendered.length) return rendered;
       if (el.selectize && el.selectize.options) {
         return Object.keys(el.selectize.options).map(function(key){
           var item = el.selectize.options[key] || {};
           return String(item.text == null ? key : item.text);
         });
       }
       return [];
     })()",
    jsonlite::toJSON(as.character(input_id), auto_unbox = TRUE)
  )
  normalize_browser_values(app$get_js(js))
}

selectize_option_values <- function(app, input_id) {
  js <- sprintf(
    "(function(){
       var el = document.getElementById(%s);
       if (!el) return [];
       if (el.selectize && el.selectize.options) return Object.keys(el.selectize.options);
       return Array.prototype.map.call(el.options || [], function(item){
         return String(item.value || '');
       });
     })()",
    jsonlite::toJSON(as.character(input_id), auto_unbox = TRUE)
  )
  normalize_browser_values(app$get_js(js))
}

wait_for_selectize_labels <- function(app, input_id, predicate, timeout_sec = 30) {
  deadline <- Sys.time() + timeout_sec
  repeat {
    labels <- tryCatch(selectize_option_labels(app, input_id), error = function(e) character(0))
    if (isTRUE(predicate(labels))) return(labels)
    if (Sys.time() >= deadline) return(labels)
    Sys.sleep(0.25)
  }
}

test_that("significance bars remain independent when switching parameters", {
  skip_if_significance_e2e_unavailable()
  skip_if_not_installed("jsonlite")

  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")
  on.exit({
    if (is.na(old_not_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_not_cran)
  }, add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = app_test_launch_dir(),
    load_timeout = 120000,
    timeout = 120000,
    clean_logs = FALSE,
    options = list(warn = 1)
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  fixture <- app_test_path("www", "reference_files", "Ejemplo_platemap_parametros.xlsx")
  expect_true(file.exists(fixture))
  app$upload_file(dataFile = fixture)
  app$wait_for_value(input = "param", timeout = 90000)
  app$set_inputs(tipo = "Boxplot", scope = "Por Cepa", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "showMedios", timeout = 90000)

  params <- wait_for_selectize_labels(app, "param", function(x) length(x) >= 2L, timeout_sec = 45)
  param_values <- selectize_option_values(app, "param")
  app$wait_for_value(input = "sig_group1", timeout = 90000)
  app$wait_for_value(input = "sig_group2", timeout = 90000)
  group_values <- selectize_option_values(app, "sig_group1")
  if (length(param_values) < 2L || length(group_values) < 2L) {
    skip("The reference workbook did not expose enough parameters or groups.")
  }

  param_a <- param_values[[1]]
  param_b <- param_values[[2]]
  group_a <- group_values[[1]]
  group_b <- group_values[[2]]
  app$set_inputs(param = param_a, wait_ = TRUE, timeout_ = 90000)
  app$set_inputs(
    sig_group1 = group_a,
    sig_group2 = group_b,
    sig_label = "bar-for-param-a",
    wait_ = TRUE,
    timeout_ = 90000
  )
  expect_identical(normalize_browser_values(app$get_value(input = "param")), param_a)
  expect_identical(normalize_browser_values(app$get_value(input = "sig_group1")), group_a)
  expect_identical(normalize_browser_values(app$get_value(input = "sig_group2")), group_b)
  expect_identical(normalize_browser_values(app$get_value(input = "sig_label")), "bar-for-param-a")
  app$click("add_sig")
  labels_a <- wait_for_selectize_labels(
    app, "sig_current", function(x) any(grepl("bar-for-param-a", x, fixed = TRUE))
  )
  sig_html_a <- tryCatch(app$get_html("#sig_current"), error = function(e) conditionMessage(e))
  expect_true(
    any(grepl("bar-for-param-a", labels_a, fixed = TRUE)),
    info = paste("labels:", paste(labels_a, collapse = " | "), "html:", sig_html_a)
  )

  app$set_inputs(param = param_b, wait_ = TRUE, timeout_ = 90000)
  labels_b_empty <- wait_for_selectize_labels(app, "sig_current", function(x) !length(x))
  expect_length(labels_b_empty, 0L)

  app$set_inputs(
    sig_group1 = group_a,
    sig_group2 = group_b,
    sig_label = "bar-for-param-b",
    wait_ = TRUE,
    timeout_ = 90000
  )
  app$click("add_sig")
  labels_b <- wait_for_selectize_labels(
    app, "sig_current", function(x) any(grepl("bar-for-param-b", x, fixed = TRUE))
  )
  expect_true(any(grepl("bar-for-param-b", labels_b, fixed = TRUE)))
  expect_false(any(grepl("bar-for-param-a", labels_b, fixed = TRUE)))

  app$set_inputs(param = param_a, wait_ = TRUE, timeout_ = 90000)
  labels_a_restored <- wait_for_selectize_labels(
    app, "sig_current", function(x) any(grepl("bar-for-param-a", x, fixed = TRUE))
  )
  expect_true(any(grepl("bar-for-param-a", labels_a_restored, fixed = TRUE)))
  expect_false(any(grepl("bar-for-param-b", labels_a_restored, fixed = TRUE)))

  app$set_inputs(tipo = "Curvas", wait_ = TRUE, timeout_ = 90000)
  app$wait_for_value(input = "curve_pt_size", timeout = 30000)
  expect_equal(as.numeric(app$get_value(input = "curve_pt_size")), 3.3)
})
