library(testthat)

root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(root, "inst", "app", "helpers.R"))

test_that("normalize_replicate_selection uses natural numeric order", {
  expect_identical(
    normalize_replicate_selection(c("1", "10", "2", "100", "11", "3")),
    c("1", "2", "3", "10", "11", "100")
  )

  expect_identical(
    normalize_replicate_selection(c("R2", "R10", "R1")),
    c("R1", "R2", "R10")
  )
})

test_that("replicate map sync is idempotent for the same inputs", {
  groups <- c("G1", "G2")
  available_map <- list(
    G1 = c("1", "10", "100"),
    G2 = c("1", "2", "3")
  )
  input_map <- list(
    G1 = c("1", "100"),
    G2 = c("1", "3")
  )

  first <- sync_replicate_selection_map(
    current_map = list(),
    groups = groups,
    available_map = available_map,
    input_map = input_map,
    drop_all = character(0)
  )
  second <- sync_replicate_selection_map(
    current_map = first$map,
    groups = groups,
    available_map = available_map,
    input_map = input_map,
    drop_all = character(0)
  )

  expect_true(isTRUE(first$changed))
  expect_false(isTRUE(second$changed))
  expect_identical(second$map, first$map)
})

test_that("replicate map sync keeps stored selection when no input event arrives", {
  groups <- c("G1")
  available_map <- list(G1 = c("1", "10", "100"))
  current_map <- list(G1 = c("1", "100"))

  synced <- sync_replicate_selection_map(
    current_map = current_map,
    groups = groups,
    available_map = available_map,
    input_map = list(),
    drop_all = character(0)
  )

  expect_false(isTRUE(synced$changed))
  expect_identical(synced$map$G1, c("1", "100"))
})

test_that("replicate map sync applies global exclusions deterministically", {
  groups <- c("G1")
  available_map <- list(G1 = c("1", "10", "100"))
  input_map <- list(G1 = c("1", "10", "100"))

  synced <- sync_replicate_selection_map(
    current_map = list(),
    groups = groups,
    available_map = available_map,
    input_map = input_map,
    drop_all = c("10")
  )

  expect_true(isTRUE(synced$changed))
  expect_identical(synced$map$G1, c("1", "100"))
})
