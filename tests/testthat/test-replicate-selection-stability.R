library(testthat)

root <- app_test_root()
source(app_test_path( "helpers.R"))

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

test_that("replicate selections stay independent across groups", {
  groups <- c("S1-M1", "S2-M1")
  available_map <- list(
    "S1-M1" = c("1", "2", "3"),
    "S2-M1" = c("1", "2", "3")
  )

  synced <- sync_replicate_selection_map(
    current_map = list(),
    groups = groups,
    available_map = available_map,
    input_map = list(
      "S1-M1" = c("1", "2"),
      "S2-M1" = c("1", "2", "3")
    ),
    drop_all = character(0)
  )

  expect_true(isTRUE(synced$changed))
  expect_identical(synced$map[["S1-M1"]], c("1", "2"))
  expect_identical(synced$map[["S2-M1"]], c("1", "2", "3"))
})

test_that("updating one group does not overwrite other group selections", {
  groups <- c("S1-M1", "S2-M1")
  available_map <- list(
    "S1-M1" = c("1", "2", "3"),
    "S2-M1" = c("1", "2", "3")
  )
  current_map <- list(
    "S1-M1" = c("1", "2", "3"),
    "S2-M1" = c("1", "2", "3")
  )

  synced <- sync_replicate_selection_map(
    current_map = current_map,
    groups = groups,
    available_map = available_map,
    input_map = list("S1-M1" = c("1", "2")),
    drop_all = character(0)
  )

  expect_true(isTRUE(synced$changed))
  expect_identical(synced$map[["S1-M1"]], c("1", "2"))
  expect_identical(synced$map[["S2-M1"]], c("1", "2", "3"))
})

test_that("synced selection setters update strain and group maps together", {
  synced <- replicate_selection_set_synced(
    reps_strain_map = list(),
    reps_group_map = list(),
    strain = "S1",
    media = "M1",
    selected = c("1", "10", "2")
  )

  expect_identical(
    replicate_selection_get_strain_media(synced$reps_strain_map, "S1", "M1"),
    c("1", "2", "10")
  )
  expect_identical(
    replicate_selection_get_group(synced$reps_group_map, "S1", "M1"),
    c("1", "2", "10")
  )
})

test_that("synced selection getter falls back to group map when strain map is missing", {
  strain_map <- list()
  group_map <- list("S2-M2" = c("1", "3"))

  expect_identical(
    replicate_selection_get_synced(strain_map, group_map, "S2", "M2"),
    c("1", "3")
  )
})

test_that("cross-scope sync is idempotent after repeated strain-scope updates", {
  simulate_strain_pass <- function(reps_strain_map, reps_group_map) {
    strain_sel <- "S1"
    medias <- c("M1", "M2")
    available_map <- list(
      M1 = c("1", "2", "3"),
      M2 = c("1", "2", "3")
    )
    input_map <- list(
      M1 = c("1", "2"),
      M2 = c("1", "2", "3")
    )

    current_sub <- reps_strain_map[[strain_sel]]
    if (!is.list(current_sub)) current_sub <- list()
    sync_res <- sync_replicate_selection_map(
      current_map = current_sub,
      groups = medias,
      available_map = available_map,
      input_map = input_map,
      drop_all = character(0)
    )

    for (m in medias) {
      reps_strain_map <- replicate_selection_set_strain_media(
        reps_strain_map, strain_sel, m, sync_res$map[[m]]
      )
      reps_group_map <- replicate_selection_set_group(
        reps_group_map, strain_sel, m, sync_res$map[[m]]
      )
    }

    list(
      reps_strain_map = reps_strain_map,
      reps_group_map = reps_group_map,
      changed = isTRUE(sync_res$changed)
    )
  }

  first <- simulate_strain_pass(list(), list())
  second <- simulate_strain_pass(first$reps_strain_map, first$reps_group_map)

  expect_true(first$changed)
  expect_false(second$changed)
  expect_identical(second$reps_strain_map, first$reps_strain_map)
  expect_identical(second$reps_group_map, first$reps_group_map)
})

test_that("cross-scope sync is idempotent after repeated group-scope updates", {
  simulate_group_pass <- function(reps_strain_map, reps_group_map) {
    groups <- c("S1-M1", "S2-M1")
    available_map <- list(
      "S1-M1" = c("1", "2", "3"),
      "S2-M1" = c("1", "2", "3")
    )
    input_map <- list(
      "S1-M1" = c("1", "2"),
      "S2-M1" = c("1", "2", "3")
    )
    group_meta <- list(
      "S1-M1" = list(strain = "S1", media = "M1"),
      "S2-M1" = list(strain = "S2", media = "M1")
    )

    sync_res <- sync_replicate_selection_map(
      current_map = reps_group_map,
      groups = groups,
      available_map = available_map,
      input_map = input_map,
      drop_all = character(0)
    )
    reps_group_map <- sync_res$map

    for (g in groups) {
      reps_strain_map <- replicate_selection_set_strain_media(
        reps_strain_map,
        group_meta[[g]]$strain,
        group_meta[[g]]$media,
        reps_group_map[[g]]
      )
    }

    list(
      reps_strain_map = reps_strain_map,
      reps_group_map = reps_group_map,
      changed = isTRUE(sync_res$changed)
    )
  }

  first <- simulate_group_pass(list(), list())
  second <- simulate_group_pass(first$reps_strain_map, first$reps_group_map)

  expect_true(first$changed)
  expect_false(second$changed)
  expect_identical(second$reps_strain_map, first$reps_strain_map)
  expect_identical(second$reps_group_map, first$reps_group_map)
})
