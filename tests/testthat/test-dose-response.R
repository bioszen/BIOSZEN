load_dose_response_helpers <- function() {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(
    app_test_path("graficos", "graficos_dose_response.R"),
    envir = env
  )
  env
}

dose_response_test_fixture <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    env <- load_dose_response_helpers()
    doses <- c(0, 1, 3, 10, 30, 100)
    strains <- c(A = 10, B = 30)
    rows <- lapply(names(strains), function(strain) {
      do.call(rbind, lapply(doses, function(dose) {
        response <- 20 + 80 / (1 + (dose / strains[[strain]])^1.4)
        data.frame(
          Strain = strain,
          Media = if (dose == 0) "Control" else paste0(dose, " uM Rapa"),
          BiologicalReplicate = c("1", "2", "3"),
          muMax = response + c(-0.35, 0, 0.35),
          stringsAsFactors = FALSE
        )
      }))
    })
    data <- do.call(rbind, rows)
    series_key <- env$bioszen_dose_series_table(data$Media)$SeriesKey[[1]]
    analysis <- env$bioszen_analyze_dose_response(
      data,
      response_col = "muMax",
      parameter_label = "muMax",
      series_key = series_key,
      selected_strains = names(strains),
      include_linear_slope = TRUE
    )
    cached <<- list(env = env, data = data, strains = strains, analysis = analysis)
    cached
  }
})

test_that("concentrations are parsed from units rather than compound digits", {
  env <- load_dose_response_helpers()
  parsed <- env$bioszen_parse_concentration_labels(c(
    "20   uM Rapa",
    "200ug/ml Compound42",
    "1.23uM Compound42",
    "Compound42 0.5 mM",
    "Control",
    "Compound42"
  ))

  expect_equal(parsed$Dose[1:5], c(20, 200, 1.23, 500, 0))
  expect_equal(parsed$Compound[1:4], c("Rapa", "Compound42", "Compound42", "Compound42"))
  expect_equal(parsed$ConcentrationUnit[1:4], c("µM", "µg/mL", "µM", "µM"))
  expect_true(parsed$IsControl[[5]])
  expect_false(parsed$Parsed[[6]])
})

test_that("editable mapping keeps unitless compound digits blank", {
  env <- load_dose_response_helpers()
  media <- c("Control", "1 uM Compound42", "Compound42", "10 uM Compound42", "20 uM Rapa")
  series_key <- env$bioszen_dose_series_table(media)$SeriesKey[
    env$bioszen_dose_series_table(media)$Compound == "Compound42"
  ][[1]]
  mapping <- env$bioszen_dose_mapping_defaults(media, series_key)

  expect_equal(mapping$Media, c("Control", "1 uM Compound42", "Compound42", "10 uM Compound42"))
  expect_equal(mapping$DoseDefault[c(1, 2, 4)], c(0, 1, 10))
  expect_true(is.na(mapping$DoseDefault[[3]]))
  expect_equal(unique(mapping$UnitDefault), "µM")
})

test_that("mapping validation requires numeric doses and one identical unit", {
  env <- load_dose_response_helpers()
  valid <- data.frame(
    Media = c("Control", "1 uM Compound42", "10 uM Compound42"),
    DoseInput = c("0", "1", "10"),
    UnitInput = c("µM", "µM", "µM"),
    stringsAsFactors = FALSE
  )
  expect_true(env$bioszen_validate_dose_mapping(valid)$valid)

  missing <- valid
  missing$DoseInput[[2]] <- ""
  expect_equal(env$bioszen_validate_dose_mapping(missing)$errors$Code, "missing_concentration")

  mixed <- valid
  mixed$UnitInput[[3]] <- "mM"
  expect_equal(env$bioszen_validate_dose_mapping(mixed)$code, "mixed_units")

  custom <- valid
  custom$UnitInput <- "arbitrary fluorescence units"
  custom_status <- env$bioszen_validate_dose_mapping(custom)
  expect_true(custom_status$valid)
  expect_equal(unique(custom_status$mapping$Unit), "arbitrary fluorescence units")

  custom_data <- data.frame(
    Strain = "A",
    Media = custom$Media,
    BiologicalReplicate = c("1", "2", "3"),
    muMax = c(1, 0.8, 0.5),
    stringsAsFactors = FALSE
  )
  prepared <- env$bioszen_prepare_dose_response_data(
    custom_data,
    response_col = "muMax",
    series_key = "__MANUAL__",
    selected_strains = "A",
    concentration_map = custom
  )
  expect_equal(prepared$Dose, c(0, 1, 10))
  expect_equal(unique(prepared$UnitFamily), "custom")
  expect_equal(unique(prepared$ConcentrationUnit), "arbitrary fluorescence units")
})

test_that("apply to all propagates recognized or arbitrary units without changing doses", {
  env <- load_dose_response_helpers()
  mapping <- data.frame(
    Media = c("Control", "Treatment 1", "Treatment 2"),
    DoseInput = c("0", "1", "2.5"),
    UnitInput = c("", "", ""),
    stringsAsFactors = FALSE
  )
  recognized <- env$bioszen_dose_apply_unit_all(mapping, " ug / mL ")
  custom <- env$bioszen_dose_apply_unit_all(mapping, "custom activity units")

  expect_equal(recognized$DoseInput, mapping$DoseInput)
  expect_equal(unique(recognized$UnitInput), "µg/mL")
  expect_equal(unique(custom$UnitInput), "custom activity units")
})

test_that("strain selection initializes once and preserves a deliberate empty selection", {
  env <- load_dose_response_helpers()
  initial <- env$bioszen_dose_resolve_strain_selection(NULL, c("A", "B"), initialized = FALSE)
  deliberate_empty <- env$bioszen_dose_resolve_strain_selection(character(0), c("A", "B"), initialized = TRUE)
  stale <- env$bioszen_dose_resolve_strain_selection("old", c("A", "B"), initialized = TRUE)

  expect_equal(initial$selected, c("A", "B"))
  expect_length(deliberate_empty$selected, 0L)
  expect_equal(stale$selected, c("A", "B"))

  delayed_initial <- env$bioszen_dose_resolve_strain_selection(
    character(0), c("A", "B"), initialized = FALSE
  )
  expect_equal(delayed_initial$selected, c("A", "B"))
})

test_that("unit conversion groups compatible concentration series", {
  env <- load_dose_response_helpers()
  parsed <- env$bioszen_parse_concentration_labels(c(
    "500 nM Rapa", "1 uM Rapa", "0.002 mM Rapa"
  ))

  expect_equal(parsed$Dose, c(0.5, 1, 2))
  expect_length(unique(parsed$SeriesKey), 1L)
  expect_equal(unique(parsed$ConcentrationUnit), "µM")
})

test_that("normalized response mode scales biological observations without changing selection", {
  env <- load_dose_response_helpers()
  data <- data.frame(
    Strain = rep(c("A", "B"), each = 4L),
    Media = rep(c("Control", "1 uM Drug42", "10 uM Drug42", "100 uM Drug42"), 2L),
    BiologicalReplicate = rep("1", 8L),
    muMax_Norm = c(1, 0.9, 0.5, 0.2, 1, 0.8, 0.4, 0.1),
    stringsAsFactors = FALSE
  )
  series_key <- env$bioszen_dose_series_table(data$Media)$SeriesKey[[1]]
  prepared <- env$bioszen_prepare_dose_response_data(
    data,
    response_col = "muMax_Norm",
    series_key = series_key,
    selected_strains = "A",
    normalized = TRUE
  )

  expect_equal(unique(prepared$Strain), "A")
  expect_equal(prepared$Response, c(100, 90, 50, 20))
})

test_that("normalized axis labels preserve literal percent signs", {
  env <- load_dose_response_helpers()

  expect_equal(
    env$bioszen_dose_format_label("%s (% of control)", "muMax"),
    "muMax (% of control)"
  )
  expect_equal(
    env$bioszen_dose_format_label("%s (% del control)", "muMax"),
    "muMax (% del control)"
  )
})

test_that("insufficient and flat datasets return clear non-estimable statuses", {
  skip_if_not_installed("drc")
  env <- load_dose_response_helpers()
  insufficient <- data.frame(Dose = c(0, 1, 10), Response = c(1, 0.8, 0.4))
  flat <- data.frame(
    Dose = rep(c(0, 1, 10, 100), each = 2L),
    Response = 1
  )

  expect_equal(env$bioszen_fit_dose_response_strain(insufficient)$status, "insufficient_doses")
  expect_equal(env$bioszen_fit_dose_response_strain(flat)$status, "flat_response")
  expect_null(env$bioszen_fit_dose_response_strain(flat)$fit)
})

test_that("a failed nonlinear optimizer is handled without propagating an error", {
  skip_if_not_installed("drc")
  env <- load_dose_response_helpers()
  data <- data.frame(
    Dose = rep(c(0, 1, 10, 100), each = 2L),
    Response = c(1, 0.99, 0.9, 0.88, 0.5, 0.48, 0.2, 0.19)
  )
  result <- env$bioszen_fit_dose_response_strain(
    data,
    fit_function = function(data) stop("simulated optimizer failure")
  )

  expect_equal(result$status, "fit_failed")
  expect_null(result$fit)
  expect_equal(result$n_observations, nrow(data))
})

test_that("IC50 beyond the tested range is reported as not reached", {
  skip_if_not_installed("drc")
  env <- load_dose_response_helpers()
  doses <- rep(c(0, 1, 3, 10, 30, 100), each = 3L)
  response <- 20 + 80 / (1 + (doses / 1000)^1.4) + rep(c(-0.01, 0, 0.01), 6L)
  result <- env$bioszen_fit_dose_response_strain(
    data.frame(Dose = doses, Response = response)
  )

  expect_equal(result$status, "not_reached")
  expect_false(result$comparable)
  expect_gt(result$ic50, max(doses))
})

test_that("four-parameter fits report IC50 and susceptibility rank by strain", {
  skip_if_not_installed("drc")
  fixture <- dose_response_test_fixture()
  result <- fixture$analysis

  expect_equal(result$parameters$Status, c("ok", "ok"))
  expect_equal(result$parameters$SusceptibilityRank, c(1L, 2L))
  expect_equal(result$parameters$IC50, unname(fixture$strains), tolerance = 0.15)
  expect_true(all(result$parameters$CI_Lower < result$parameters$IC50))
  expect_true(all(result$parameters$CI_Upper > result$parameters$IC50))
  expect_equal(result$parameters$ED50, result$parameters$IC50)
  expect_true(all(is.finite(result$parameters$HillSlope)))
  expect_true(all(result$parameters$ResponseRange > 0))
  expect_true(all(result$parameters$MaximumSlope < 0))
  expect_equal(
    result$parameters$MaximumSlopeMagnitude,
    abs(result$parameters$MaximumSlope)
  )
  expect_equal(result$parameters$InflectionPoint, result$parameters$IC50, tolerance = 0.15)
  expect_true(all(result$diagnostics$Converged))
  expect_true(all(result$diagnostics$R_Squared > 0.99))
  expect_true(all(is.finite(result$diagnostics$LinearSlope)))
  expect_equal(nrow(result$comparisons), 1L)
  expect_true(is.finite(result$comparisons$P_Adjusted[[1]]))
  expect_equal(result$comparisons$LowerIC50Strain[[1]], "A")
})

test_that("dose-response plot builds multiple strain curves with assigned colors", {
  skip_if_not_installed("drc")
  fixture <- dose_response_test_fixture()
  colors <- c(A = "#3366CC", B = "#AA66CC")
  input <- list(
    dose_log_x = FALSE,
    dose_show_ci = TRUE,
    dose_point_display = "individual",
    errbar_stat = "SD",
    yLab = "",
    plotTitle = "Concentration-response test",
    base_size = 11
  )
  labels <- c(
    dose_no_valid_data = "No valid data",
    dose_table_strain = "Strain",
    dose_table_concentration = "Concentration",
    dose_table_response = "Response",
    dose_table_biorep = "Biological replicate",
    dose_x_label = "Concentration",
    dose_strains = "Strains to compare"
  )
  plot <- fixture$env$build_dose_response_plot_impl(
    analysis = fixture$analysis,
    input = input,
    lang = "en",
    tr_text = function(key, lang) {
      value <- labels[[key]]
      if (is.null(value)) key else unname(value)
    },
    palette_for_levels = function(levels) colors[levels],
    margin_adj = function(top, right, bottom, left) ggplot2::margin(top, right, bottom, left),
    fs_title = 14,
    fs_axis = 11,
    fs_legend = 10,
    axis_size = 0.5
  )

  expect_s3_class(plot, "ggplot")
  expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))))
  point_layers <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), plot$layers)
  expect_length(point_layers, 1L)
  expect_equal(point_layers[[1]]$aes_params$shape, 21)
  expect_equal(point_layers[[1]]$aes_params$colour, "black")
  expect_gt(point_layers[[1]]$aes_params$stroke, 0)
  expect_true("fill" %in% names(point_layers[[1]]$mapping))
  expect_false("text" %in% names(point_layers[[1]]$mapping))
  expect_equal(plot$labels$x, "Concentration (µM)")
  expect_equal(unname(plot$scales$get_scales("colour")$palette(2)), unname(colors))
})

test_that("replicate display defaults to individuals and supports mean SD or SEM", {
  skip_if_not_installed("drc")
  fixture <- dose_response_test_fixture()
  observations <- fixture$analysis$observations

  individual <- fixture$env$bioszen_dose_display_data(observations)
  mean_sd <- fixture$env$bioszen_dose_display_data(observations, "mean_error", "SD")
  mean_sem <- fixture$env$bioszen_dose_display_data(observations, "mean_error", "SEM")

  expect_equal(nrow(individual), nrow(observations))
  expect_true(all(individual$DisplayMode == "individual"))
  expect_equal(nrow(mean_sd), 2L * 6L)
  expect_true(all(mean_sd$N == 3L))
  expect_true(all(mean_sd$Error == mean_sd$SD))
  expect_equal(mean_sem$Error, mean_sd$SD / sqrt(mean_sd$N))

  colors <- c(A = "#3366CC", B = "#AA66CC")
  input <- list(
    dose_log_x = FALSE, dose_show_ci = TRUE,
    dose_point_display = "mean_error", errbar_stat = "SEM",
    yLab = "", plotTitle = "Mean response", base_size = 11
  )
  plot <- fixture$env$build_dose_response_plot_impl(
    analysis = fixture$analysis,
    input = input,
    lang = "en",
    tr_text = function(key, lang) key,
    palette_for_levels = function(levels) colors[levels],
    margin_adj = function(top, right, bottom, left) ggplot2::margin(top, right, bottom, left),
    fs_title = 14, fs_axis = 11, fs_legend = 10, axis_size = 0.5
  )
  expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomErrorbar"), logical(1))))
})

test_that("dose-response presentation controls change the plot without changing results", {
  skip_if_not_installed("drc")
  fixture <- dose_response_test_fixture()
  parameters_before <- fixture$analysis$parameters
  colors <- c(A = "#3366CC", B = "#AA66CC")
  input <- list(
    dose_log_x = FALSE, dose_show_ci = TRUE,
    dose_point_display = "individual", errbar_stat = "SD",
    dose_xmin = 0, dose_xmax = 100, dose_xbreak = 20,
    dose_ymin = 10, dose_ymax = 110, dose_ybreak = 10,
    dose_xlab = "Treatment concentration", dose_ylab = "Selected response",
    dose_line_width = 1.7, dose_point_size = 4.2,
    dose_point_stroke = 0.9, dose_ci_alpha = 0.31,
    yLab = "", plotTitle = "Styled response", base_size = 11
  )
  plot <- fixture$env$build_dose_response_plot_impl(
    analysis = fixture$analysis,
    input = input,
    lang = "en",
    tr_text = function(key, lang) key,
    palette_for_levels = function(levels) colors[levels],
    margin_adj = function(top, right, bottom, left) ggplot2::margin(top, right, bottom, left),
    fs_title = 14, fs_axis = 11, fs_legend = 10, axis_size = 0.5
  )

  point_layer <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), plot$layers)[[1]]
  line_layer <- Filter(function(layer) inherits(layer$geom, "GeomLine"), plot$layers)[[1]]
  ribbon_layer <- Filter(function(layer) inherits(layer$geom, "GeomRibbon"), plot$layers)[[1]]
  expect_equal(plot$labels$x, "Treatment concentration")
  expect_equal(plot$labels$y, "Selected response")
  expect_equal(plot$scales$get_scales("x")$limits, c(0, 100))
  expect_equal(plot$scales$get_scales("y")$limits, c(10, 110))
  expect_equal(
    plot$scales$get_scales("x")$expand,
    ggplot2::expansion(mult = 0)
  )
  expect_equal(
    plot$scales$get_scales("y")$expand,
    ggplot2::expansion(mult = 0)
  )
  expect_null(plot$scales$get_scales("colour")$name)
  expect_null(plot$scales$get_scales("fill")$name)
  expect_true(all(c(0, 100) %in% plot$scales$get_scales("x")$breaks))
  expect_true(all(c(10, 110) %in% plot$scales$get_scales("y")$breaks))
  expect_equal(line_layer$aes_params$linewidth, 1.7)
  expect_equal(point_layer$aes_params$size, 4.2)
  expect_equal(point_layer$aes_params$stroke, 0.9)
  expect_equal(ribbon_layer$aes_params$alpha, 0.31)
  expect_identical(fixture$analysis$parameters, parameters_before)
})

test_that("configured decimal axis endpoints are always labelled", {
  env <- load_dose_response_helpers()
  y_breaks <- env$bioszen_dose_axis_breaks(c(0, 0.3), interval = 0, n = 5)
  x_breaks <- env$bioszen_dose_axis_breaks(c(0, 200), interval = 0, n = 7)

  expect_true(all(c(0, 0.3) %in% y_breaks))
  expect_true(all(c(0, 200) %in% x_breaks))
})

test_that("replicate-level table preserves selected technical rows and normalized values", {
  env <- load_dose_response_helpers()
  raw <- data.frame(
    Strain = c("A", "A", "A", "B"),
    Media = c("Control", "10 uM Drug42", "10 uM Drug42", "10 uM Drug42"),
    BiologicalReplicate = c("1", "1", "2", "1"),
    TechnicalReplicate = c("T1", "T2", "T1", "T1"),
    muMax = c(1, 0.8, 0.75, 0.7),
    stringsAsFactors = FALSE
  )
  normalized <- data.frame(
    Strain = c("A", "A", "A"),
    Media = c("Control", "10 uM Drug42", "10 uM Drug42"),
    BiologicalReplicate = c("1", "1", "2"),
    muMax_Norm = c(1, 0.8, 0.75),
    stringsAsFactors = FALSE
  )
  mapping <- data.frame(
    Media = c("Control", "10 uM Drug42"),
    DoseInput = c("0", "10"),
    UnitInput = c("µM", "µM"),
    stringsAsFactors = FALSE
  )
  table <- env$bioszen_prepare_dose_replicate_values(
    raw,
    response_col = "muMax",
    parameter_label = "muMax",
    concentration_map = mapping,
    selected_strains = "A",
    normalized_df = normalized,
    normalized_col = "muMax_Norm"
  )

  expect_equal(nrow(table), 3L)
  expect_equal(unique(table$Strain), "A")
  expect_equal(table$TechnicalReplicate, c("T1", "T2", "T1"))
  expect_equal(table$RawValue, c(1, 0.8, 0.75))
  expect_equal(table$NormalizedValue, c(100, 80, 75))
  expect_equal(table$ModelValue, c(100, 80, 75))
  expect_equal(unique(table$Parameter), "muMax")
})

test_that("dose-response statistics workbook contains the complete reproducible analysis", {
  skip_if_not_installed("drc")
  skip_if_not_installed("openxlsx")
  fixture <- dose_response_test_fixture()
  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)

  analysis <- fixture$analysis
  analysis$replicate_values <- data.frame(
    Strain = "A", Condition = "Control", Concentration = 0,
    ConcentrationUnit = "µM", BiologicalReplicate = "1",
    TechnicalReplicate = "T1", Parameter = "muMax", RawValue = 1,
    NormalizedValue = NA_real_, stringsAsFactors = FALSE
  )
  fixture$env$bioszen_write_dose_response_workbook(analysis, file)
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
  expect_equal(
    openxlsx::getSheetNames(file),
    c(
      "Replicate values", "Curve parameters", "IC50 results",
      "Strain comparisons", "Model diagnostics", "Analysis settings"
    )
  )
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = "Replicate values")), 1L)
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = "Curve parameters")), 2L)
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = "IC50 results")), 2L)
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = "Strain comparisons")), 1L)
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = "Model diagnostics")), 2L)
  expect_gt(nrow(openxlsx::read.xlsx(file, sheet = "Analysis settings")), 0L)
})

test_that("dose-response controls and exports are wired into the app", {
  ui <- paste(readLines(app_test_path("ui", "ui_main.R"), warn = FALSE), collapse = "\n")
  server <- paste(readLines(app_test_path("server", "server_main.R"), warn = FALSE), collapse = "\n")
  description <- read.dcf(file.path(app_test_root(), "DESCRIPTION"))

  expect_match(ui, "input.tipo == 'DoseResponse'", fixed = TRUE)
  expect_match(ui, '"dose_series"', fixed = TRUE)
  expect_match(ui, '"dose_strains"', fixed = TRUE)
  expect_match(ui, '"dose_unit_all"', fixed = TRUE)
  expect_match(ui, '"dose_apply_unit_all"', fixed = TRUE)
  expect_match(ui, 'uiOutput("doseConcentrationMappingUI")', fixed = TRUE)
  expect_match(ui, '"dose_point_display"', fixed = TRUE)
  expect_match(ui, 'selected = "individual"', fixed = TRUE)
  expect_match(ui, '"dose_xmin"', fixed = TRUE)
  expect_match(ui, '"dose_xmax"', fixed = TRUE)
  expect_match(ui, '"dose_xbreak"', fixed = TRUE)
  expect_match(ui, '"dose_ymin"', fixed = TRUE)
  expect_match(ui, '"dose_ymax"', fixed = TRUE)
  expect_match(ui, '"dose_ybreak"', fixed = TRUE)
  expect_match(ui, '"dose_xlab"', fixed = TRUE)
  expect_match(ui, '"dose_ylab"', fixed = TRUE)
  expect_match(ui, '"dose_line_width"', fixed = TRUE)
  expect_match(ui, '"dose_point_size"', fixed = TRUE)
  expect_match(ui, '"dose_point_stroke"', fixed = TRUE)
  expect_match(ui, '"dose_ci_alpha"', fixed = TRUE)
  expect_match(ui, 'DTOutput("doseCurveParametersTable")', fixed = TRUE)
  expect_match(ui, 'DTOutput("doseDiagnosticsTable")', fixed = TRUE)
  expect_match(server, 'if (tipo == "DoseResponse")', fixed = TRUE)
  expect_match(server, "bioszen_write_dose_response_workbook", fixed = TRUE)
  expect_match(server, '"dose_mapping"', fixed = TRUE)
  expect_match(server, 'shiny::freezeReactiveValue(input, "dose_series")', fixed = TRUE)
  expect_match(server, 'shiny::freezeReactiveValue(input, "dose_strains")', fixed = TRUE)
  expect_match(server, 'observeEvent(input$dose_apply_unit_all', fixed = TRUE)
  unit_apply_observer <- regexpr(
    'observeEvent(input$dose_apply_unit_all', server, fixed = TRUE
  )[[1]]
  unit_apply_call <- paste0(
    'applied_mapping <- bioszen_dose_apply_unit_all(',
    'dose_mapping_values(), unit_value)'
  )
  expect_gt(unit_apply_observer, 0L)
  expect_false(grepl(
    unit_apply_call,
    substr(server, 1L, unit_apply_observer - 1L),
    fixed = TRUE
  ))
  expect_equal(
    length(regmatches(server, gregexpr(unit_apply_call, server, fixed = TRUE))[[1]]),
    1L
  )
  expect_match(server, 'dose_response_raw_scope_df', fixed = TRUE)
  expect_match(server, 'apply_qc_tech_filter_raw(datos_combinados())', fixed = TRUE)
  expect_match(server, 'dose_point_display = as.character', fixed = TRUE)
  expect_match(server, 'dose_line_width = as.character', fixed = TRUE)
  expect_match(server, 'updateNumericInput(session, "dose_line_width"', fixed = TRUE)
  expect_match(server, '"dose_linear_slope"', fixed = TRUE)
  expect_match(server, 'updateRadioButtons(session, "dose_point_display"', fixed = TRUE)
  expect_match(server, 'updateCheckboxInput(session, "dose_linear_slope"', fixed = TRUE)
  expect_match(server, 'textInput(\n            bioszen_dose_mapping_input_id(media, "unit")', fixed = TRUE)
  expect_false(grepl('selectInput(\n            bioszen_dose_mapping_input_id(media, "unit")', server, fixed = TRUE))
  expect_match(server, 'if (identical(tipo, "DoseResponse"))', fixed = TRUE)
  expect_match(description[[1, "Imports"]], "drc", fixed = TRUE)

  global_ui <- paste(readLines(app_test_path("global.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("export_dpi_help", ui, fixed = TRUE))
  expect_false(grepl("export_dpi_help", global_ui, fixed = TRUE))

  public_text <- paste(
    ui,
    paste(readLines(app_test_path("i18n", "translation_en.csv"), warn = FALSE), collapse = "\n"),
    paste(readLines(app_test_path("i18n", "translation_es.csv"), warn = FALSE), collapse = "\n")
  )
  expect_false(grepl(paste0("U", "18"), public_text, fixed = TRUE))
})

test_that("dose-response bundle uses the same complete statistics workbook", {
  server <- paste(readLines(app_test_path("server", "server_main.R"), warn = FALSE), collapse = "\n")
  compact_server <- gsub("\\s+", " ", server)

  expect_match(compact_server, "analysis <- dose_response_export_analysis() bioszen_write_dose_response_workbook(analysis, file)", fixed = TRUE)
  expect_match(server, "stats_hash", fixed = TRUE)
  expect_match(server, "dataset_stats_paths", fixed = TRUE)
})

test_that("dose-response selector refreshes do not chase transient empty values", {
  server <- paste(readLines(app_test_path("server", "server_main.R"), warn = FALSE), collapse = "\n")
  compact_server <- gsub("\\s+", " ", server)

  expect_match(compact_server, "control_needs_update <- !identical", fixed = TRUE)
  expect_match(compact_server, "series_needs_update <- !identical", fixed = TRUE)
  expect_match(compact_server, "strain_needs_update <- !identical", fixed = TRUE)
  expect_false(grepl("control_signature) || !isTRUE(control_is_valid)", compact_server, fixed = TRUE))
  expect_false(grepl("series_signature) || !length(input$dose_series)", compact_server, fixed = TRUE))
  expect_false(grepl("strain_selection$missing", compact_server, fixed = TRUE))
  expect_false(grepl("strain_selection$invalid", compact_server, fixed = TRUE))
})
