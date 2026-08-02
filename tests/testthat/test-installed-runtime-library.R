library(testthat)

runtime_env <- new.env(parent = globalenv())
sys.source(file.path(app_test_root(), "R", "app_startup.R"), envir = runtime_env)

write_fake_package <- function(library, package, built_r, compiled = TRUE) {
  package_dir <- file.path(library, package)
  dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      paste0("Package: ", package),
      "Version: 1.0.0",
      paste0("Built: R ", built_r, ".0; test-platform; test-date; test-os"),
      paste0("NeedsCompilation: ", if (compiled) "yes" else "no")
    ),
    file.path(package_dir, "DESCRIPTION")
  )
  invisible(package_dir)
}

write_fake_runtime <- function(library, built_r) {
  pure_r <- c("officer")
  for (package in runtime_env$bioszen_pptx_runtime_packages()) {
    write_fake_package(
      library,
      package,
      built_r = built_r,
      compiled = !package %in% pure_r
    )
  }
  invisible(library)
}

test_that("versioned R user libraries map to the running R version on every platform", {
  current <- runtime_env$bioszen_r_version_key()

  expect_identical(
    runtime_env$bioszen_current_library_from_stale("C:\\Users\\name\\Documents\\R\\win-library\\4.4"),
    paste0("C:/Users/name/Documents/R/win-library/", current)
  )
  expect_identical(
    runtime_env$bioszen_current_library_from_stale("/home/name/R/x86_64-pc-linux-gnu-library/4.4"),
    paste0("/home/name/R/x86_64-pc-linux-gnu-library/", current)
  )
  expect_identical(
    runtime_env$bioszen_current_library_from_stale("/Users/name/Library/R/arm64/4.4/library"),
    paste0("/Users/name/Library/R/arm64/", current, "/library")
  )
  expect_identical(
    runtime_env$bioszen_current_library_from_stale("/Users/name/Library/R/x86_64/4.4/library"),
    paste0("/Users/name/Library/R/x86_64/", current, "/library")
  )
  expect_null(
    runtime_env$bioszen_current_library_from_stale(
      paste0("C:/Users/name/Documents/R/win-library/", current)
    )
  )
  expect_true(runtime_env$bioszen_is_current_versioned_library(
    paste0("C:/Users/name/Documents/R/win-library/", current)
  ))
  expect_true(runtime_env$bioszen_is_current_versioned_library(
    paste0("/Users/name/Library/R/arm64/", current, "/library")
  ))
  expect_false(runtime_env$bioszen_is_current_versioned_library(
    "C:/Program Files/R/R-4.6.0/library"
  ))
})

test_that("the managed runtime path is private, versioned, and platform-specific", {
  root <- tempfile("bioszen-managed-root-")
  path <- runtime_env$bioszen_managed_runtime_library(
    version_key = "4.6",
    platform = "x86_64-w64-mingw32",
    root = root
  )

  expect_identical(
    path,
    file.path(root, "runtime-library", "4.6", "x86_64-w64-mingw32")
  )
  expect_false(dir.exists(path))
})

test_that("the graphics cache is writable and process-local", {
  root <- tempfile("bioszen-gdtools-cache-")
  old_option <- getOption("GDTOOLS_CACHE_DIR")
  old_env <- Sys.getenv("GDTOOLS_CACHE_DIR", unset = NA_character_)
  on.exit({
    options(GDTOOLS_CACHE_DIR = old_option)
    if (is.na(old_env)) {
      Sys.unsetenv("GDTOOLS_CACHE_DIR")
    } else {
      Sys.setenv(GDTOOLS_CACHE_DIR = old_env)
    }
    unlink(root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  selected <- runtime_env$bioszen_configure_graphics_cache(root)
  expect_true(dir.exists(selected))
  expect_identical(unname(file.access(selected, 2)), 0L)
  expect_identical(getOption("GDTOOLS_CACHE_DIR"), selected)
  expect_identical(Sys.getenv("GDTOOLS_CACHE_DIR"), selected)
})

test_that("runtime library selection skips an unusable candidate", {
  root <- tempfile("bioszen-library-selection-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  blocked <- file.path(root, "not-a-directory")
  writeLines("occupied", blocked)
  available <- file.path(root, "available")

  selected <- runtime_env$bioszen_select_writable_library(c(blocked, available))
  expect_identical(
    selected,
    normalizePath(available, winslash = "/", mustWork = TRUE)
  )
})

test_that("native PowerPoint packages must be built for the running R version", {
  root <- tempfile("bioszen-runtime-build-")
  current_library <- file.path(root, "win-library", runtime_env$bioszen_r_version_key())
  dir.create(current_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  write_fake_runtime(current_library, runtime_env$bioszen_r_version_key())
  expect_true(runtime_env$bioszen_pptx_runtime_compatible(
    current_library,
    required_library = current_library
  ))

  write_fake_package(current_library, "rvg", built_r = "4.4", compiled = TRUE)
  expect_false(runtime_env$bioszen_pptx_runtime_compatible(
    current_library,
    required_library = current_library
  ))
})

test_that("installed launches repair a stale runtime once and reuse it afterwards", {
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-runtime-repair-")
  stale_library <- file.path(root, "win-library", old)
  dir.create(stale_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_fake_runtime(stale_library, old)

  installed <- 0L
  selected_libraries <- character()
  fake_install <- function(packages, lib, repos) {
    installed <<- installed + 1L
    expect_setequal(packages, runtime_env$bioszen_pptx_runtime_packages())
    write_fake_runtime(lib, current)
  }
  fake_libpaths <- function(paths) {
    selected_libraries <<- paths
    invisible(paths)
  }

  repaired <- runtime_env$bioszen_prepare_installed_runtime(
    libraries = stale_library,
    repos = "https://example.invalid",
    install_fun = fake_install,
    set_library_paths = fake_libpaths,
    loaded_namespaces = character()
  )
  expected_library <- normalizePath(
    file.path(root, "win-library", current),
    winslash = "/",
    mustWork = TRUE
  )

  expect_true(repaired$repaired)
  expect_identical(repaired$library, expected_library)
  expect_identical(installed, 1L)
  expect_identical(selected_libraries[[1]], expected_library)
  expect_true(runtime_env$bioszen_pptx_runtime_compatible(
    c(expected_library, stale_library),
    required_library = expected_library
  ))

  reused <- runtime_env$bioszen_prepare_installed_runtime(
    libraries = c(expected_library, stale_library),
    repos = "https://example.invalid",
    install_fun = function(...) stop("the compatible runtime must not be reinstalled"),
    set_library_paths = fake_libpaths,
    loaded_namespaces = character()
  )
  expect_false(reused$repaired)
  expect_null(reused$library)
})

test_that("a failed runtime repair never prevents the app from starting", {
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-runtime-failure-")
  stale_library <- file.path(root, "win-library", old)
  dir.create(stale_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_fake_runtime(stale_library, old)

  selected_libraries <- character()
  expect_warning(
    result <- runtime_env$bioszen_prepare_installed_runtime(
      libraries = stale_library,
      repos = "https://example.invalid",
      install_fun = function(...) stop("offline"),
      set_library_paths = function(paths) {
        selected_libraries <<- paths
        invisible(paths)
      },
      loaded_namespaces = character()
    ),
    "app will continue"
  )

  expect_false(result$repaired)
  expect_false(result$vector_available)
  expect_identical(result$error, "offline")
  expect_identical(selected_libraries, stale_library)
})

test_that("an incompatible binary already in the current user library is rebuilt", {
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-current-runtime-")
  current_library <- file.path(root, "win-library", current)
  dir.create(current_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_fake_runtime(current_library, old)

  installed <- 0L
  result <- runtime_env$bioszen_prepare_installed_runtime(
    libraries = current_library,
    repos = "https://example.invalid",
    install_fun = function(packages, lib, repos) {
      installed <<- installed + 1L
      write_fake_runtime(lib, current)
    },
    set_library_paths = function(paths) invisible(paths),
    loaded_namespaces = character()
  )

  expect_true(result$repaired)
  expect_identical(installed, 1L)
  expect_true(runtime_env$bioszen_pptx_runtime_compatible(
    current_library,
    required_library = current_library
  ))
})

test_that("system-only and custom libraries use a managed per-user runtime", {
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-system-only-")
  system_library <- file.path(root, "custom-system-library")
  managed_root <- file.path(root, "user-data")
  dir.create(system_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_fake_runtime(system_library, old)

  old_runtime_root <- getOption("BIOSZEN.runtime_root")
  options(BIOSZEN.runtime_root = managed_root)
  on.exit(options(BIOSZEN.runtime_root = old_runtime_root), add = TRUE)

  selected_libraries <- character()
  result <- runtime_env$bioszen_prepare_installed_runtime(
    libraries = system_library,
    repos = "https://example.invalid",
    install_fun = function(packages, lib, repos) {
      write_fake_runtime(lib, current)
    },
    set_library_paths = function(paths) {
      selected_libraries <<- paths
      invisible(paths)
    },
    loaded_namespaces = character()
  )
  expected <- normalizePath(
    file.path(
      managed_root,
      "runtime-library",
      current,
      R.version$platform
    ),
    winslash = "/",
    mustWork = TRUE
  )

  expect_true(result$repaired)
  expect_identical(result$library, expected)
  expect_identical(selected_libraries[[1]], expected)
  expect_true(runtime_env$bioszen_pptx_runtime_compatible(
    c(expected, system_library),
    required_library = expected
  ))
})

test_that("the updater targets the running R library instead of a stale package library", {
  update_env <- new.env(parent = runtime_env)
  sys.source(file.path(app_test_root(), "R", "update.R"), envir = update_env)

  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-update-library-")
  stale_library <- file.path(root, "win-library", old)
  package_path <- file.path(stale_library, "BIOSZEN")
  dir.create(package_path, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  selected <- update_env$.bioszen_update_library(
    package_path = package_path,
    libraries = stale_library
  )
  expect_identical(
    selected,
    normalizePath(file.path(root, "win-library", current), winslash = "/", mustWork = TRUE)
  )
})
