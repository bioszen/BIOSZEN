library(testthat)

test_that("statistics actions open the bslib accordion panel by stable value", {
  server_file <- app_test_path("server", "server_main.R")
  txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl("updateCollapse\\(session,\\s*\"statsPanel\"", txt, perl = TRUE))
  expect_true(grepl(
    "bslib::accordion_panel_open\\(\\s*\"statsPanel\"\\s*,\\s*values\\s*=\\s*\"stats_title\"\\s*,\\s*session\\s*=\\s*session\\s*\\)",
    txt,
    perl = TRUE
  ))
})

test_that("server string literals do not contain mojibake text", {
  server_file <- app_test_path("server", "server_main.R")
  expr <- parse(server_file, keep.source = TRUE)
  parsed <- utils::getParseData(expr)
  string_literals <- parsed$text[parsed$token == "STR_CONST"]

  mojibake_pattern <- paste(c("\u00c3", "\u0192", "\u00c2", "\ufffd"), collapse = "|")
  bad_literals <- string_literals[grepl(mojibake_pattern, string_literals)]

  expect_equal(
    bad_literals,
    character(0),
    info = paste("Mojibake string literals:", paste(bad_literals, collapse = ", "))
  )
})

test_that("technical QC selection is wired to filtered final table and export cache keys", {
  server_file <- app_test_path("server", "server_main.R")
  ui_file <- app_test_path("ui", "ui_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(ui_txt, 'uiOutput\\("statsFilteredTechTableUI"\\)')
  expect_match(server_txt, "output\\$statsFilteredTechTableUI\\s*<-\\s*renderUI")
  expect_match(server_txt, "output\\$statsTechFilteredTable\\s*<-\\s*renderDT")
  expect_match(server_txt, "build_technical_filtered_detail_table")
  expect_match(server_txt, "stable_key_value\\(qc_tech_selected\\(\\)\\)")
  expect_match(server_txt, "stable_key_value\\(qc_tech_selected_by_param\\(\\)\\)")
})

test_that("automatic technical QC selections persist while browser inputs catch up", {
  server_file <- app_test_path("server", "server_main.R")
  ui_file <- app_test_path("ui", "ui_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    server_txt,
    "qc_tech_render_from_store <- reactiveVal\\(FALSE\\)",
    info = "Automatic technical QC updates need a temporary stored-state render guard."
  )
  expect_match(
    server_txt,
    "prefer_stored <- isTRUE\\(qc_tech_render_from_store\\(\\)\\)",
    info = "Selector rendering must prefer stored values only during programmatic technical QC updates."
  )
  expect_match(
    server_txt,
    "if \\(isTRUE\\(qc_tech_render_from_store\\(\\)\\)\\)",
    info = "Stale browser checkbox values must not overwrite a just-applied technical QC map."
  )
  expect_match(
    server_txt,
    "qc_tech_set_param_map\\(current_store, param_key, next_map\\)",
    info = "Automatic technical QC actions must persist into the per-parameter map used by downloads."
  )
  expect_match(
    server_txt,
    "input_id = info\\$id,\\s*choices = info\\$choices,\\s*selected = next_sel,\\s*freeze_input = FALSE",
    perl = TRUE,
    info = "Automatic technical QC checkbox updates must let the client send the new selected values."
  )
  expect_match(
    server_txt,
    'sendCustomMessage\\(\\s*"bioszen-set-checkbox-group-values"',
    perl = TRUE,
    info = "Automatic technical QC updates must force dynamic checkbox DOM values to match the stored map."
  )
  expect_match(
    ui_txt,
    "Shiny\\.addCustomMessageHandler\\('bioszen-set-checkbox-group-values'",
    info = "The UI must listen for server-side technical QC checkbox synchronization."
  )
  expect_match(
    ui_txt,
    "document\\.addEventListener\\('shiny:connected', registerBioszenCheckboxGroupSync",
    info = "The technical QC checkbox sync handler must register even if Shiny loads after the script block."
  )
  expect_match(
    server_txt,
    "qc_build_technical_outlier_selection",
    info = "Outlier exclusion should build selections from canonical source rows instead of display labels only."
  )
  expect_match(
    server_txt,
    "stable_key_value\\(qc_tech_selected_by_param\\(\\)\\)",
    info = "Parameter download cache keys must include per-parameter technical replicate selections."
  )
  expect_match(
    server_txt,
    "active_tech_param = isolate\\(qc_tech_param_key\\(\\)\\)",
    info = "Parameter workbook exports must fall back to the current technical selection for the active parameter."
  )
})

test_that("upload and filter updates are batched before plot redraws", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    server_txt,
    "begin_dataset_update\\s*<-\\s*function",
    info = "Dataset loads should keep the loading guard active through the UI flush."
  )
  expect_match(
    server_txt,
    "begin_deferred_reactive_flag\\(\\s*dataset_loading,[\\s\\S]*?flush_cycles = 3L[\\s\\S]*?timeout_ms = 850L",
    perl = TRUE,
    info = "Dataset uploads should hold the plot gate long enough for upload-driven UI updates to settle."
  )
  expect_match(
    server_txt,
    "begin_filter_selection_sync\\s*<-\\s*function",
    info = "Filter changes need their own short sync guard before plotting."
  )
  expect_match(
    server_txt,
    "filter_selection_sync_inflight <- reactiveVal\\(FALSE\\)",
    info = "The plot output must be able to observe filter sync state."
  )
  expect_match(
    server_txt,
    "plot_settle_tick <- reactiveVal\\(0L\\)",
    info = "Clearing upload/filter gates should force one final plot render."
  )
  expect_match(
    server_txt,
    "plot_settle_tick\\(isolate\\(plot_settle_tick\\(\\)\\) \\+ 1L\\)",
    perl = TRUE,
    info = "The plot should be invalidated when a deferred loading gate clears."
  )
  expect_match(
    server_txt,
    "output\\$plotInteractivo <- renderPlotly\\(\\{\\s*input\\$mobile_plot_refresh\\s*plot_settle_tick\\(\\)",
    perl = TRUE,
    info = "The interactive plot must depend on the settle tick."
  )
  plot_output <- sub(
    "^[\\s\\S]*?output\\$plotInteractivo <- renderPlotly\\(\\{",
    "output$plotInteractivo <- renderPlotly({",
    server_txt,
    perl = TRUE
  )
  plot_output <- sub(
    "outputOptions\\(output, \"plotInteractivoUI\"[\\s\\S]*$",
    "",
    plot_output,
    perl = TRUE
  )
  expect_match(
    plot_output,
    "req\\(input\\$dataFile, cancelOutput = TRUE\\)",
    perl = TRUE,
    info = "Upload-time plot invalidations should keep the current output instead of drawing a loading placeholder."
  )
  expect_match(
    plot_output,
    "identical\\(isolate\\(input\\$tipo %\\|\\|% \"\"\\), \"Curvas\"\\)",
    perl = TRUE,
    info = "Plot type changes should flow through the debounced plot request."
  )
  expect_match(
    plot_output,
    "req\\(FALSE, cancelOutput = TRUE\\)",
    perl = TRUE,
    info = "Deferred plot gates should cancel intermediate renders."
  )
  expect_false(grepl(
    "validate\\(need\\(FALSE, tr_text\\(\"loading_plot_data\"",
    plot_output,
    perl = TRUE
  ))
  expect_match(
    plot_output,
    "if \\(isTRUE\\(isolate\\(filter_selection_sync_inflight\\(\\)\\)\\)\\) req\\(FALSE, cancelOutput = TRUE\\)",
    perl = TRUE,
    info = "Plot rendering should wait while selected groups/conditions settle."
  )
  expect_match(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$scope, input\\$strain, input\\$showMedios, input\\$showGroups\\)",
    perl = TRUE,
    info = "Filter selector changes should enter the short sync guard."
  )
  data_file_observers <- gregexpr(
    "observeEvent\\(input\\$dataFile,",
    server_txt,
    perl = TRUE
  )[[1]]
  expect_equal(
    sum(data_file_observers > 0),
    1L,
    info = "Only the main data-file loader should observe input$dataFile."
  )
  expect_false(grepl(
    "observeEvent\\(input\\$dataFile, \\{\\s*req\\(plot_settings\\(\\)\\)",
    server_txt,
    perl = TRUE
  ))
  expect_false(grepl(
    "on\\.exit\\(dataset_loading\\(FALSE\\)",
    server_txt,
    perl = TRUE
  ))
})

test_that("core reactive controls keep loop guards around programmatic updates", {
  server_file <- app_test_path("server", "server_main.R")
  ui_file <- app_test_path("ui", "ui_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  extract_section <- function(txt, start_pattern, end_pattern) {
    start <- regexpr(start_pattern, txt, perl = TRUE)
    expect_true(start[[1]] > 0L, info = start_pattern)
    tail <- substring(txt, start[[1]])
    end <- regexpr(end_pattern, substring(tail, 2), perl = TRUE)
    if (end[[1]] > 0L) {
      substring(tail, 1L, end[[1]])
    } else {
      tail
    }
  }

  helper_section <- extract_section(
    server_txt,
    "update_checkbox_group_input_if_changed <- function",
    "update_radio_buttons_if_changed <- function"
  )
  expect_match(helper_section, "freezeReactiveValue\\(input, input_id\\)", perl = TRUE)
  expect_match(helper_section, "update_signature <- list", fixed = TRUE)
  expect_match(helper_section, "previous_signature <-", fixed = TRUE)
  expect_match(helper_section, "identical\\(update_signature, previous_signature\\)", perl = TRUE)
  expect_match(helper_section, "return\\(invisible\\(FALSE\\)\\)", perl = TRUE)

  expect_match(
    server_txt,
    "filter_toggle_sync_inflight <- reactiveVal\\(FALSE\\)",
    perl = TRUE,
    info = "Programmatic select-all toggle updates need a guard so partial filters do not reselect all choices."
  )
  expect_match(
    server_txt,
    "sync_filter_toggle_input <- function",
    fixed = TRUE,
    info = "The select-all checkbox state should be synced from the actual selected filters."
  )
  expect_match(ui_txt, "target.id + '_user_change'", fixed = TRUE)
  expect_match(ui_txt, "target.id !== 'toggleMedios' && target.id !== 'toggleGroups'", fixed = TRUE)
  expect_match(ui_txt, "ev.isTrusted", fixed = TRUE)
  expect_false(
    grepl("observeEvent\\(input\\$toggleMedios,", server_txt, perl = TRUE),
    info = "Raw select-all checkbox changes must not drive bulk filter updates; programmatic syncs can change those inputs."
  )
  expect_false(
    grepl("observeEvent\\(input\\$toggleGroups,", server_txt, perl = TRUE),
    info = "Raw select-all checkbox changes must not drive bulk filter updates; programmatic syncs can change those inputs."
  )

  deferred_section <- extract_section(
    server_txt,
    "begin_deferred_reactive_flag <- function",
    "begin_dataset_update <- function"
  )
  expect_match(deferred_section, "flag_key", fixed = TRUE)
  expect_match(deferred_section, "deferred_flag_tokens", fixed = TRUE)
  expect_match(deferred_section, "flush_cycles", fixed = TRUE)
  expect_match(deferred_section, "timeout_ms", fixed = TRUE)
  expect_match(deferred_section, "flag_rv\\(TRUE\\)", perl = TRUE)
  expect_match(deferred_section, "flag_rv\\(FALSE\\)", perl = TRUE)
  expect_match(deferred_section, "session\\$onFlushed\\(", perl = TRUE)
  expect_match(deferred_section, "once = TRUE", fixed = TRUE)
  expect_match(deferred_section, "requireNamespace\\(\"later\", quietly = TRUE\\)", perl = TRUE)
  expect_match(deferred_section, "getExportedValue\\(\"later\", \"later\"\\)", perl = TRUE)

  toggle_medios <- extract_section(
    server_txt,
    "observeEvent\\(input\\$toggleMedios_user_change",
    "observeEvent\\(input\\$toggleGroups_user_change"
  )
  expect_match(toggle_medios, "identical\\(current_sel, target_sel\\)", perl = TRUE)
  expect_match(toggle_medios, "update_checkbox_group_input_if_changed", fixed = TRUE)
  expect_match(toggle_medios, "freeze_input = TRUE", fixed = TRUE)
  expect_match(toggle_medios, "toggle_payload\\$value", perl = TRUE)
  expect_match(toggle_medios, "ignoreInit = TRUE", fixed = TRUE)
  expect_false(grepl("updateCheckboxGroupInput", toggle_medios, fixed = TRUE))

  toggle_groups <- extract_section(
    server_txt,
    "observeEvent\\(input\\$toggleGroups_user_change",
    "observeEvent\\(\\s*list\\(input\\$scope"
  )
  expect_match(toggle_groups, "identical\\(current_sel, target_sel\\)", perl = TRUE)
  expect_match(toggle_groups, "update_checkbox_group_input_if_changed", fixed = TRUE)
  expect_match(toggle_groups, "freeze_input = TRUE", fixed = TRUE)
  expect_match(toggle_groups, "toggle_payload\\$value", perl = TRUE)
  expect_match(toggle_groups, "ignoreInit = TRUE", fixed = TRUE)
  expect_false(grepl("updateCheckboxGroupInput", toggle_groups, fixed = TRUE))

  filter_toggle_sync <- extract_section(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$showMedios, input\\$showGroups, datos_agrupados\\(\\)\\)",
    "observeEvent\\(\\s*list\\(input\\$scope"
  )
  expect_match(filter_toggle_sync, "sync_filter_toggle_input", fixed = TRUE)
  expect_match(filter_toggle_sync, "toggleMedios", fixed = TRUE)
  expect_match(filter_toggle_sync, "toggleGroups", fixed = TRUE)
  expect_match(filter_toggle_sync, "ignoreInit = TRUE", fixed = TRUE)

  filter_guard <- extract_section(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$scope, input\\$strain, input\\$showMedios, input\\$showGroups\\)",
    "output\\$repsStrainUI <- renderUI"
  )
  expect_match(filter_guard, "begin_filter_selection_sync\\(\\)", perl = TRUE)
  expect_match(filter_guard, "ignoreInit = TRUE", fixed = TRUE)
  expect_match(filter_guard, "priority = 100", fixed = TRUE)

  group_select_all <- extract_section(
    server_txt,
    "observeEvent\\(input\\$repsGrpSelectAll",
    "observeEvent\\(input\\$repsStrainSelectAll"
  )
  strain_select_all <- extract_section(
    server_txt,
    "observeEvent\\(input\\$repsStrainSelectAll",
    "output\\$plotInteractivo <- renderPlotly"
  )
  expect_match(group_select_all, "begin_bulk_update\\(replicate_bulk_updating, \"replicate_bulk_updating\"\\)", perl = TRUE)
  expect_match(strain_select_all, "begin_bulk_update\\(replicate_bulk_updating, \"replicate_bulk_updating\"\\)", perl = TRUE)
  expect_match(group_select_all, "update_checkbox_group_input_if_changed", fixed = TRUE)
  expect_match(strain_select_all, "update_checkbox_group_input_if_changed", fixed = TRUE)
})

test_that("statistics builders resolve scoped data and tolerate transient blank selectors", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  extract_section <- function(txt, start_pattern, end_pattern) {
    start <- regexpr(start_pattern, txt, perl = TRUE)
    expect_true(start[[1]] > 0L, info = start_pattern)
    tail <- substring(txt, start[[1]])
    end <- regexpr(end_pattern, substring(tail, 2), perl = TRUE)
    if (end[[1]] > 0L) {
      substring(tail, 1L, end[[1]])
    } else {
      tail
    }
  }

  expect_match(server_txt, "current_stats_scope_df <- function", fixed = TRUE)
  expect_match(server_txt, "resolve_stats_param <- function", fixed = TRUE)
  expect_match(server_txt, "finalize_stats_test_df <- function", fixed = TRUE)
  expect_match(server_txt, "last_stats_scope_snapshot <- reactiveVal\\(NULL\\)", perl = TRUE)
  expect_match(server_txt, "last_stats_scope_snapshot\\(NULL\\)", perl = TRUE)
  expect_match(server_txt, "record_stats_scope_snapshot <- function", fixed = TRUE)
  expect_match(server_txt, "clear_stats_scope_snapshot <- function", fixed = TRUE)
  expect_match(server_txt, "stats_snapshot_df <- function", fixed = TRUE)
  expect_match(server_txt, "record_stats_scope_snapshot\\(scope, strain, scope_df, param_sel\\)", perl = TRUE)
  expect_match(server_txt, "clear_stats_scope_snapshot\\(scope, strain\\)", perl = TRUE)

  build_section <- extract_section(
    server_txt,
    "build_stats_observation_df <- function",
    "make_test_df <- function"
  )
  expect_match(build_section, "finalize_stats_test_df\\(out\\)", perl = TRUE)
  expect_false(
    grepl("active_plot_data\\(\\)", build_section, fixed = TRUE),
    info = "The observation builder should consume a resolved scoped dataframe, not rebuild its own plot data path."
  )

  stats_section <- extract_section(
    server_txt,
    "make_test_df <- function",
    "build_summary_stats_df <- function"
  )
  expect_match(stats_section, "current_stats_scope_df\\(\\)", perl = TRUE)
  expect_match(stats_section, "resolve_stats_param\\(src\\)\\$value", perl = TRUE)
  expect_match(stats_section, "stats_snapshot_df\\(scope_sel, strain_sel\\)", perl = TRUE)
  expect_match(stats_section, "build_stats_observation_df\\(snap_src, p_snap, scope_sel\\)", perl = TRUE)
  expect_false(
    grepl("active_plot_data\\(\\)", stats_section, fixed = TRUE),
    info = "On-screen stats should use the same scoped, input-tolerant dataframe resolver as the plot."
  )

  summary_section <- extract_section(
    server_txt,
    "make_summary_test_df <- function",
    "summary_welch_pair <- function"
  )
  expect_match(summary_section, "stats_snapshot_df\\(scope_sel, strain_sel\\)", perl = TRUE)
  expect_match(summary_section, "build_summary_stats_df\\(snap_src, p_snap, scope_sel\\)", perl = TRUE)

  export_section <- extract_section(
    server_txt,
    "download_stats_content <- function",
    "output\\$downloadStats <- downloadHandler"
  )
  expect_match(export_section, "get_scope_df", fixed = TRUE)
  expect_false(
    grepl("datos_agrupados\\(\\) \\|>", export_section, fixed = TRUE),
    info = "Statistics downloads should not rebuild a separate filter path from raw grouped data."
  )
})
