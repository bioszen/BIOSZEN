library(testthat)

runtime_env <- new.env(parent = globalenv())
runtime_source <- file.path(app_test_root(), "R", "app_startup.R")
if (file.exists(runtime_source)) {
  sys.source(runtime_source, envir = runtime_env)
} else {
  runtime_namespace <- asNamespace("BIOSZEN")
  runtime_symbols <- ls(runtime_namespace, pattern = "^bioszen_")
  list2env(
    mget(runtime_symbols, envir = runtime_namespace, inherits = FALSE),
    envir = runtime_env
  )
}

write_fake_package <- function(library, package, built_r, compiled = TRUE,
                               depends = NULL, imports = NULL,
                               linking_to = NULL) {
  package_dir <- file.path(library, package)
  dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)
  fields <- c(
    paste0("Package: ", package),
    "Version: 1.0.0",
    paste0("Built: R ", built_r, ".0; test-platform; test-date; test-os"),
    paste0("NeedsCompilation: ", if (compiled) "yes" else "no")
  )
  if (!is.null(depends)) fields <- c(fields, paste0("Depends: ", depends))
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  if (!is.null(linking_to)) fields <- c(fields, paste0("LinkingTo: ", linking_to))
  writeLines(fields, file.path(package_dir, "DESCRIPTION"))
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

test_that("installed runtime repairs use CRAN binaries for the active R version", {
  repos <- runtime_env$bioszen_runtime_repositories()

  expect_identical(names(repos), "CRAN")
  expect_identical(unname(repos), "https://cloud.r-project.org")
  expect_false(any(grepl("r-universe", repos, fixed = TRUE)))
  expect_identical(
    eval(formals(runtime_env$bioszen_prepare_installed_runtime)$repos,
         envir = runtime_env),
    repos
  )
})

test_that("installed runtime audit covers all imported compiled dependencies", {
  root <- tempfile("bioszen-complete-runtime-")
  library <- file.path(root, "win-library", runtime_env$bioszen_r_version_key())
  dir.create(library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"

  write_fake_runtime(library, current)
  write_fake_package(
    library,
    "BIOSZEN",
    current,
    compiled = FALSE,
    imports = "CompiledParent, PureOld, MissingDirect"
  )
  write_fake_package(
    library,
    "CompiledParent",
    current,
    compiled = FALSE,
    imports = "NativeLeaf"
  )
  write_fake_package(library, "NativeLeaf", old, compiled = TRUE)
  write_fake_package(library, "PureOld", old, compiled = FALSE)

  expect_setequal(
    runtime_env$bioszen_runtime_repair_packages(library),
    c("NativeLeaf", "MissingDirect")
  )

  write_fake_package(library, "NativeLeaf", current, compiled = TRUE)
  write_fake_package(library, "MissingDirect", current, compiled = FALSE)
  expect_length(runtime_env$bioszen_runtime_repair_packages(library), 0L)
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

test_that("custom writable libraries are reused without a separate runtime folder", {
  current <- runtime_env$bioszen_r_version_key()
  old <- if (identical(current, "4.4")) "4.3" else "4.4"
  root <- tempfile("bioszen-system-only-")
  custom_library <- file.path(root, "custom-library")
  dir.create(custom_library, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_fake_runtime(custom_library, old)

  selected_libraries <- character()
  result <- runtime_env$bioszen_prepare_installed_runtime(
    libraries = custom_library,
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
  expected <- normalizePath(custom_library, winslash = "/", mustWork = TRUE)

  expect_true(result$repaired)
  expect_identical(result$library, expected)
  expect_identical(selected_libraries[[1]], expected)
  expect_true(runtime_env$bioszen_pptx_runtime_compatible(
    expected,
    required_library = expected
  ))
  expect_false(dir.exists(file.path(root, "runtime-library")))
})

test_that("the updater targets the running R library instead of a stale package library", {
  update_env <- new.env(parent = runtime_env)
  update_source <- file.path(app_test_root(), "R", "update.R")
  if (file.exists(update_source)) {
    sys.source(update_source, envir = update_env)
  } else {
    update_env$.bioszen_update_library <- get(
      ".bioszen_update_library",
      envir = asNamespace("BIOSZEN"),
      inherits = FALSE
    )
  }

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
