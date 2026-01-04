library(testthat)

test_that("xlsx generation writes and reads minimal dataset", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  path <- tempfile("bioszen_min_", fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  sheet1 <- data.frame(
    Time = 0:2,
    A = c(0.1, 0.2, 0.3),
    B = c(0.15, 0.25, 0.35)
  )
  params <- data.frame(
    X_Max = 50,
    Interval_X = 10,
    Y_Max = 1.5,
    Interval_Y = 0.5,
    X_Title = "Tiempo (h)",
    Y_Title = "OD620",
    stringsAsFactors = FALSE
  )

  writexl::write_xlsx(list(Sheet1 = sheet1, Params = params), path = path)

  expect_true(file.exists(path))
  read_sheet1 <- readxl::read_excel(path, sheet = "Sheet1")
  read_params <- readxl::read_excel(path, sheet = "Params")

  expect_equal(names(read_sheet1), names(sheet1))
  expect_equal(nrow(read_sheet1), nrow(sheet1))
  expect_equal(as.data.frame(read_sheet1), sheet1)
  expect_equal(as.data.frame(read_params), params)
})
