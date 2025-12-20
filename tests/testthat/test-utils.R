library(testthat)

source(testthat::test_path('..', '..', 'inst', 'app', 'helpers.R'))

test_that('matrix_to_tibble works', {
  skip_if_not_installed('tibble')
  mat <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2,
                dimnames = list(c('A','B'), c('C','D')))
  tb <- matrix_to_tibble(mat, colname = 'p')
  expect_s3_class(tb, 'tbl')
  expect_equal(nrow(tb), 4)
  expect_equal(names(tb), c('grupo1', 'grupo2', 'p'))
})

test_that('pmcmr_to_tibble works', {
  skip_if_not_installed('tibble')
  obj <- list(p.value = matrix(c(0.05, 0.2, 0.3, 0.1), nrow = 2,
                               dimnames = list(c('A','B'), c('C','D'))))
  tb <- pmcmr_to_tibble(obj)
  expect_s3_class(tb, 'tbl')
  expect_equal(nrow(tb), 4)
  expect_true(all(c('grupo1','grupo2','p.adj') %in% names(tb)))
})
