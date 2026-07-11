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

test_that("global Shiny error notifications are defensive", {
  global_file <- app_test_path("global.R")
  txt <- paste(readLines(global_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(txt, "bioszen_safe_show_notification <- function", fixed = TRUE)
  expect_match(txt, "sendNotification", fixed = TRUE)
  expect_match(txt, 'inherits\\(notify_session, "ShinySession"\\)', perl = TRUE)
  expect_match(txt, "options\\(shiny\\.error[\\s\\S]*bioszen_safe_show_notification", perl = TRUE)
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
    "current <- selector_committed_or_input\\(\\s*input_id,\\s*input\\[\\[input_id\\]\\],\\s*cached = stored\\s*\\)",
    perl = TRUE,
    info = "Manual rapid technical QC clicks must render from the committed final selector state, not stale raw input."
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
    "begin_reactive_stabilizer\\(\\s*flag_key = \"dataset_loading\",[\\s\\S]*?flush_cycles = 8L[\\s\\S]*?timeout_ms = 2500L",
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
    "begin_filter_selection_sync <- function\\(\\) \\{[\\s\\S]*?begin_quiet_reactive_flag\\([\\s\\S]*?flag_key = \"filter_selection_sync_inflight\"[\\s\\S]*?quiet_ms = 1400L",
    perl = TRUE,
    info = "Filter changes should wait for selected groups/conditions to settle before redraw."
  )
  expect_match(
    server_txt,
    "begin_plot_input_sync\\s*<-\\s*function",
    info = "Programmatic selector refreshes need a short sync guard before plotting."
  )
  expect_match(
    server_txt,
    "begin_reactive_stabilizer\\(\\s*flag_key = \"plot_input_sync_inflight\",[\\s\\S]*?flush_cycles = 5L[\\s\\S]*?timeout_ms = 1400L",
    perl = TRUE,
    info = "Programmatic selector refreshes should hide intermediate plot states."
  )
  expect_match(
    server_txt,
    "filter_selection_sync_inflight <- reactiveVal\\(FALSE\\)",
    info = "Dependent selector synchronization should be able to observe filter sync state."
  )
  expect_match(
    server_txt,
    "plot_input_sync_inflight <- reactiveVal\\(FALSE\\)",
    info = "The plot output must be able to observe selector sync state."
  )
  expect_match(
    server_txt,
    "replicate_selection_settling <- reactiveVal\\(FALSE\\)",
    info = "Biological/technical replicate maps should coalesce rapid selection bursts."
  )
  expect_match(
    server_txt,
    "last_plotly_render <- new\\.env\\(parent = emptyenv\\(\\)\\)",
    perl = TRUE,
    info = "The app should cache the last good plotly render for burst-loop fallback."
  )
  expect_match(
    server_txt,
    "begin_reactive_stabilizer\\s*<-\\s*function",
    info = "Loop-prone UI phases should use one stabilizer helper instead of scattered guards."
  )
  expect_match(
    server_txt,
    "begin_quiet_reactive_flag\\s*<-\\s*function",
    info = "Rapid filter/replicate selection bursts should use a quiet-period stabilizer."
  )
  expect_match(
    server_txt,
    "begin_filter_selection_sync <- function\\(\\) \\{[\\s\\S]*?begin_quiet_reactive_flag\\([\\s\\S]*?quiet_ms = 1400L",
    perl = TRUE,
    info = "Filter selection changes should stay held until the user stops rapidly toggling."
  )
  expect_match(
    server_txt,
    "begin_replicate_selection_settle <- function\\(\\) \\{[\\s\\S]*?begin_quiet_reactive_flag\\([\\s\\S]*?quiet_ms = 1200L",
    perl = TRUE,
    info = "Biological/technical replicate selection maps should settle through a quiet-period hold."
  )
  expect_match(
    server_txt,
    "output\\$repSelCurvas <- renderUI\\(\\{\\s*guard_stable_output\\(c\\([\\s\\S]*?\"replicate_bulk_updating\"[\\s\\S]*?\\)\\)",
    perl = TRUE,
    info = "Curve replicate selectors should stay usable during passive rapid group/filter bursts."
  )
  expect_match(
    server_txt,
    "left_join\\([\\s\\S]*?meta_df %>% mutate\\(Well = as.character\\(Well\\)\\)[\\s\\S]*?by = \"Well\"[\\s\\S]*?relationship = \"many-to-many\"",
    perl = TRUE,
    info = "Curve long data should declare the expected many-to-many Well join to avoid warning-as-error crashes."
  )
  expect_match(
    server_txt,
    "guard_stable_output\\s*<-\\s*function",
    info = "Outputs should share the same stable-output guard."
  )
  expect_match(
    server_txt,
    "guard_reactive_loop\\s*<-\\s*function",
    info = "Repeated redraw loops should be detected by a threshold watchdog."
  )
  expect_match(
    server_txt,
    "plot_settle_tick <- reactiveVal\\(0L\\)",
    info = "The mobile plot refresh trigger should remain available."
  )
  expect_match(
    server_txt,
    "if \\(flag_key %in% plot_stability_keys\\) \\{\\s*plot_settle_tick\\(isolate\\(plot_settle_tick\\(\\)\\) \\+ 1L\\)",
    perl = TRUE,
    info = "Deferred plot gates should trigger one final render after the stabilized state is ready."
  )
  expect_match(
    server_txt,
    "output\\$plotInteractivo <- renderPlotly\\(\\{\\s*input\\$mobile_plot_refresh",
    perl = TRUE,
    info = "The interactive plot should keep the explicit mobile refresh dependency without subscribing to raw filter bursts."
  )
  expect_match(
    server_txt,
    "plot_base_interactive\\s*<-\\s*debounce\\([\\s\\S]*?millis = 900",
    perl = TRUE,
    info = "The plot output should debounce long enough to avoid visible intermediate plot states."
  )
  expect_match(
    server_txt,
    "plot_selection_settled_signal <- debounce\\([\\s\\S]*?show_groups = selected_show_groups\\(\\)[\\s\\S]*?reps_group = reactive_loop_signature_value\\(reps_group_selected\\(\\)\\)[\\s\\S]*?millis = 900",
    perl = TRUE,
    info = "Rapid group and replicate changes should coalesce into one explicit final plot refresh."
  )
  expect_match(
    server_txt,
    "observeEvent\\(plot_selection_settled_signal\\(\\), \\{[\\s\\S]*?plot_settle_tick\\(isolate\\(plot_settle_tick\\(\\)\\) \\+ 1L\\)",
    perl = TRUE,
    info = "The settled selection signal should remain available for non-plot outputs and future explicit refreshes."
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
    "plot_settle_tick\\(\\)",
    perl = TRUE,
    info = "If the first render is cancelled by a setup gate, the released settle tick must force the final plot render."
  )
  expect_false(grepl(
    "validate\\(need\\(FALSE, tr_text\\(\"loading_plot_data\"",
    plot_output,
    perl = TRUE
  ))
  expect_match(
    plot_output,
    "guard_stable_output\\(plot_cancel_keys\\)",
    perl = TRUE,
    info = "Plot rendering should only subscribe to hard cancel gates, not rapid filter/replicate settling pulses."
  )
  expect_false(
    grepl("input\\$showGroups|input\\$showMedios|reps_group_selected\\(\\)|reps_strain_selected\\(\\)|qc_tech_selected\\(\\)", plot_output, perl = TRUE),
    info = "renderPlotly must not subscribe directly to raw filter or replicate inputs; the debounced plot object owns those dependencies."
  )
  expect_match(
    server_txt,
    "reactive_loop_signature_value <- function",
    fixed = TRUE,
    info = "The loop watchdog should compare stable signatures before accumulating hits."
  )
  expect_match(
    server_txt,
    "state\\$hits > max_hits \\|\\| as.integer\\(state\\$signature_hits %\\|\\|% 0L\\) > max_hits",
    perl = TRUE,
    info = "The loop watchdog should catch repeated plot churn even when intermediate selection signatures differ."
  )
  expect_match(
    server_txt,
    "schedule_settle_refresh <- function\\(\\)[\\s\\S]*?plot_settle_tick\\(isolate\\(plot_settle_tick\\(\\)\\) \\+ 1L\\)",
    perl = TRUE,
    info = "Suppressed plot churn should schedule one final refresh after inputs quiet down."
  )
  expect_match(
    server_txt,
    "reactive_loop_watchdog_tokens <- new.env\\(parent = emptyenv\\(\\)\\)",
    perl = TRUE,
    info = "The final refresh scheduler should cancel stale settle timers."
  )
  expect_false(
    grepl("output\\$plotInteractivoUI <- renderUI\\(\\{\\s*guard_stable_output\\(plot_hold_keys\\)", server_txt, perl = TRUE),
    info = "The plot container itself should not be rebuilt or greyed out during passive filter settling."
  )
  expect_match(
    plot_output,
    "last_plotly_render\\$value <- plt",
    perl = TRUE,
    info = "Successful ggplotly renders should refresh the stable plot cache."
  )
  expect_match(
    server_txt,
    "plot_stability_keys <- c\\([\\s\\S]*?\"dataset_loading\"[\\s\\S]*?\"axis_sync_inflight\"[\\s\\S]*?\"plot_input_sync_inflight\"[\\s\\S]*?\"replicate_bulk_updating\"[\\s\\S]*?\\)",
    perl = TRUE,
    info = "The plot should wait for real loading/input/bulk phases, not passive filter-settle pulses."
  )
  plot_key_section <- sub(
    "^[\\s\\S]*?plot_stability_keys <- c\\(",
    "",
    server_txt,
    perl = TRUE
  )
  plot_key_section <- sub("\\)[\\s\\S]*$", "", plot_key_section, perl = TRUE)
  expect_false(
    grepl("\"filter_selection_sync_inflight\"", plot_key_section, perl = TRUE),
    info = "Passive filter settling must not grey out or directly gate the plot output."
  )
  expect_false(
    grepl("\"replicate_selection_settling\"", plot_key_section, perl = TRUE),
    info = "Passive replicate-map settling must not grey out or directly gate the plot output."
  )
  expect_match(
    server_txt,
    "plot_cancel_keys <- c\\([\\s\\S]*?\"dataset_loading\"[\\s\\S]*?\"axis_sync_inflight\"[\\s\\S]*?\"plot_input_sync_inflight\"[\\s\\S]*?\\)",
    perl = TRUE,
    info = "Only hard plot gates should directly invalidate the plot output."
  )
  expect_match(
    server_txt,
    "plot_hold_keys <- setdiff\\(plot_stability_keys, plot_cancel_keys\\)",
    perl = TRUE,
    info = "Filter and replicate settling should be hold-only gates for the cached plot."
  )
  expect_match(
    server_txt,
    "table_stability_keys <- c\\([\\s\\S]*?\"replicate_bulk_updating\"[\\s\\S]*?\"qc_tech_bulk_updating\"",
    perl = TRUE,
    info = "Tables under the plot should hold during real biological/technical bulk updates."
  )
  table_key_section <- sub(
    "^[\\s\\S]*?table_stability_keys <- c\\(",
    "",
    server_txt,
    perl = TRUE
  )
  table_key_section <- sub("\\)[\\s\\S]*$", "", table_key_section, perl = TRUE)
  expect_false(
    grepl(
      "\"replicate_selection_settling\"",
      table_key_section,
      perl = TRUE
    ),
    info = "Passive replicate-map synchronization should not cancel table redraws."
  )
  expect_match(
    server_txt,
    "begin_plot_input_sync\\(\\)[\\s\\S]*?update_selectize_adaptive\\(\\s*\"param\"",
    perl = TRUE,
    info = "Parameter selector refresh should hold the plot until the final selection reaches Shiny."
  )
  expect_match(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$scope, input\\$strain, selected_show_medios\\(\\), selected_show_groups\\(\\)\\)",
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

  data_loader_section <- sub(
    "^[\\s\\S]*?observeEvent\\(input\\$dataFile, \\{",
    "observeEvent(input$dataFile, {",
    server_txt,
    perl = TRUE
  )
  data_loader_section <- sub(
    "\\n\\s*observeEvent\\(input\\$mergePlatemaps,[\\s\\S]*$",
    "",
    data_loader_section,
    perl = TRUE
  )
  expect_match(
    data_loader_section,
    "refresh_static_choices\\(force_default_type = TRUE\\)",
    perl = TRUE,
    info = "Dataset loading should set the final plot type through one centralized path."
  )
  expect_false(grepl(
    "updateRadioButtons\\(\\s*session,\\s*\"tipo\"",
    data_loader_section,
    perl = TRUE
  ))

  merge_loader_section <- sub(
    "^[\\s\\S]*?observeEvent\\(input\\$mergePlatemaps, \\{",
    "observeEvent(input$mergePlatemaps, {",
    server_txt,
    perl = TRUE
  )
  merge_loader_section <- sub(
    "\\n\\s*merged_platemap_filename <- function[\\s\\S]*$",
    "",
    merge_loader_section,
    perl = TRUE
  )
  expect_match(
    merge_loader_section,
    "refresh_static_choices\\(force_default_type = TRUE\\)",
    perl = TRUE,
    info = "Merged platemap loading should also use the centralized plot-type update."
  )
  expect_false(grepl(
    "updateRadioButtons\\(\\s*session,\\s*\"tipo\"",
    merge_loader_section,
    perl = TRUE
  ))
  expect_false(grepl("cur_data_all\\(", server_txt, fixed = FALSE))
  expect_match(
    server_txt,
    "dplyr::pick\\(dplyr::everything\\(\\)\\)",
    perl = TRUE,
    info = "Technical replicate summaries should use dplyr::pick() instead of deprecated cur_data_all()."
  )
})

test_that("condition and group checkbox renderers do not replay stale selections", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  count_matches <- function(pattern, txt) {
    matches <- gregexpr(pattern, txt, perl = TRUE)[[1]]
    sum(matches > 0L)
  }
  extract_section <- function(txt, start_pattern, end_pattern) {
    start <- regexpr(start_pattern, txt, perl = TRUE)
    expect_true(start[[1]] > 0L, info = start_pattern)
    tail <- substring(txt, start[[1]])
    end <- regexpr(end_pattern, substring(tail, 2), perl = TRUE)
    if (end[[1]] > 0L) substring(tail, 1L, end[[1]]) else tail
  }

  expect_match(server_txt, "filter_medios_selected <- reactiveVal\\(NULL\\)", perl = TRUE)
  expect_match(server_txt, "filter_groups_selected <- reactiveVal\\(NULL\\)", perl = TRUE)
  expect_match(
    server_txt,
    "resolve_filter_selection\\s*<-\\s*function",
    perl = TRUE,
    info = "Filter checkbox groups should resolve selected values from the current browser input plus the cached last user state."
  )
  expect_match(
    server_txt,
    "has_current <- !is\\.null\\(current\\)",
    perl = TRUE,
    info = "An explicit empty checkbox selection must remain empty instead of falling back to cached/default choices."
  )
  expect_match(
    server_txt,
    "if \\(isTRUE\\(has_current\\)\\)",
    perl = TRUE,
    info = "Current checkbox input should be authoritative even when it is character(0)."
  )
  expect_equal(
    count_matches("output\\$showMediosUI\\s*<-\\s*renderUI", server_txt),
    1L,
    info = "The condition selector renderer should be defined once, outside upload observers."
  )
  expect_equal(
    count_matches("output\\$groupSel\\s*<-\\s*renderUI", server_txt),
    1L,
    info = "The combined-group selector renderer should be defined once, outside upload observers."
  )

  media_observer <- extract_section(
    server_txt,
    "observeEvent\\(list\\(datos_agrupados\\(\\), input\\$app_lang, strain_label_ui\\(\\), media_label_ui\\(\\)",
    "output\\$replicatesUI\\s*<-\\s*renderUI"
  )
  group_observer <- extract_section(
    server_txt,
    "observeEvent\\(datos_agrupados\\(\\)",
    "output\\$replicatesGroupUI\\s*<-\\s*renderUI"
  )

  expect_false(
    grepl("output\\$showMediosUI\\s*<-\\s*renderUI", media_observer, perl = TRUE),
    info = "The condition selector observer must not rebuild its own stale renderUI closure."
  )
  expect_false(
    grepl("output\\$groupSel\\s*<-\\s*renderUI", group_observer, perl = TRUE),
    info = "The combined group selector observer must not rebuild its own stale renderUI closure."
  )
  expect_match(media_observer, "resolve_filter_selection", fixed = TRUE)
  expect_match(group_observer, "resolve_filter_selection", fixed = TRUE)
})

test_that("browser refresh and replicate bursts are stabilized without stopping the app", {
  server_file <- app_test_path("server", "server_main.R")
  growth_file <- app_test_path("server", "growth_module.R")
  ui_file <- app_test_path("ui", "ui_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  growth_txt <- paste(readLines(growth_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(server_txt, "last_session_stop_token <- 0", fixed = TRUE)
  expect_match(server_txt, "schedule_stop_if_last_session <- function", fixed = TRUE)
  expect_match(server_txt, "cancel_last_session_stop <- function", fixed = TRUE)
  expect_match(server_txt, "cancel_last_session_stop\\(\\)", perl = TRUE)
  expect_match(server_txt, "later_fn\\(maybe_stop, delay = delay\\)", perl = TRUE)
  session_end_match <- regexpr(
    "session\\$onSessionEnded\\(function\\(\\) \\{[\\s\\S]*?\\n  \\}\\)",
    server_txt,
    perl = TRUE
  )
  expect_true(session_end_match[[1]] > 0L)
  session_end_section <- regmatches(server_txt, session_end_match)
  expect_match(
    session_end_section,
    "schedule_stop_if_last_session\\(\\)",
    perl = TRUE,
    info = "Browser refresh must get a reconnect grace window instead of immediate stopApp()."
  )
  expect_false(grepl("shiny::stopApp\\(\\)", session_end_section, perl = TRUE))
  expect_match(growth_txt, "schedule_stop_if_last_session", fixed = TRUE)

  expect_match(server_txt, "begin_replicate_selection_settle <- function", fixed = TRUE)
  expect_match(
    server_txt,
    "begin_replicate_selection_settle <- function\\(\\) \\{[\\s\\S]*?begin_quiet_reactive_flag\\([\\s\\S]*?flag_key = \"replicate_selection_settling\"[\\s\\S]*?quiet_ms = 1200L",
    perl = TRUE
  )
  settle_calls <- gregexpr("begin_replicate_selection_settle\\(\\)", server_txt, perl = TRUE)[[1]]
  expect_true(sum(settle_calls > 0) >= 3L)
  expect_match(
    server_txt,
    "output\\$statsTable <- renderDT\\(\\{\\s*guard_stable_output\\(table_stability_keys\\)",
    perl = TRUE
  )
  expect_match(
    server_txt,
    "output\\$qcSampleTable <- renderDT\\(\\{\\s*guard_stable_output\\(table_stability_keys\\)",
    perl = TRUE
  )

  expect_match(ui_txt, 'tags\\$meta\\(name = "viewport", content = "width=device-width, initial-scale=1"\\)', perl = TRUE)
  expect_match(ui_txt, "html \\{\\s*font-size: 16px;", perl = TRUE)
  expect_match(ui_txt, "text-size-adjust: 100%;", fixed = TRUE)
  expect_match(
    ui_txt,
    "#plotInteractivo\\.recalculating[\\s\\S]*?#showMediosUI\\.recalculating[\\s\\S]*?#groupSel\\.recalculating[\\s\\S]*?#repsStrainUI\\.recalculating[\\s\\S]*?opacity: 1 !important;",
    perl = TRUE,
    info = "Rapid filter changes should not visually fade the current plot or selector panels while Shiny recalculates."
  )
  expect_false(grepl("dispatchEvent\\(new Event\\('change'", ui_txt, perl = TRUE))
})

test_that("stacked and correlation value tables keep mean rows in the value column", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  extract_section <- function(txt, start_pattern, end_pattern) {
    start <- regexpr(start_pattern, txt, perl = TRUE)
    expect_true(start[[1]] > 0L, info = start_pattern)
    tail <- substring(txt, start[[1]])
    end <- regexpr(end_pattern, substring(tail, 2), perl = TRUE)
    expect_true(end[[1]] > 0L, info = end_pattern)
    substring(tail, 1L, end[[1]])
  }

  stats_section <- extract_section(
    server_txt,
    "output\\$statsTable <- renderDT\\(\\{",
    "\\}, server = FALSE\\)"
  )
  stacked_section <- extract_section(
    stats_section,
    "if \\(tipo == \"Apiladas\"\\)",
    "if \\(tipo == \"Correlacion\"\\)"
  )
  corr_section <- extract_section(
    stats_section,
    "if \\(tipo == \"Correlacion\"\\)",
    "datatable\\("
  )

  expect_false(
    grepl("summarise\\(\\s*Promedio\\s*=", stats_section, perl = TRUE),
    info = "Mean summary rows in the table must not create a separate Promedio column."
  )
  expect_match(
    stacked_section,
    "summarise\\(\\s*Valor\\s*=\\s*mean\\(Valor, na.rm = TRUE\\)",
    perl = TRUE
  )
  expect_match(
    corr_section,
    "summarise\\(\\s*Valor\\s*=\\s*mean\\(Valor, na.rm = TRUE\\)",
    perl = TRUE
  )
})

test_that("initial default plot titles wait for complete selector context", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  start <- regexpr(
    "observeEvent\\(\\s*list\\(input\\$scope, input\\$tipo, input\\$param",
    server_txt,
    perl = TRUE
  )
  expect_true(start[[1]] > 0L)
  tail <- substring(server_txt, start[[1]])
  end <- regexpr("# ---- Helpers para filtrar", tail, fixed = TRUE)
  expect_true(end[[1]] > 0L)
  title_section <- substring(tail, 1L, end[[1]])

  expect_match(
    title_section,
    "parameter_title_types <- c\\(\"Boxplot\", \"Barras\", \"Violin\", \"Curvas\", \"Apiladas\"\\)",
    perl = TRUE,
    info = "Curves need the same non-empty parameter guard as other parameter-based plot titles."
  )
  expect_match(
    title_section,
    "if \\(!nzchar\\(param_sel\\) && input\\$tipo %in% parameter_title_types\\) return\\(\\)",
    perl = TRUE
  )
  expect_match(
    title_section,
    "strain_sel <- trimws\\(as.character\\(input\\$strain %\\|\\|% \"\"\\)\\)",
    perl = TRUE
  )
  expect_match(
    title_section,
    "if \\(!identical\\(scope_sel, \"Combinado\"\\) && !nzchar\\(strain_sel\\) && !identical\\(input\\$tipo, \"Correlacion\"\\)\\)",
    perl = TRUE,
    info = "Per-strain default titles must not be written while the strain selector is still blank."
  )
  expect_match(
    title_section,
    "sprintf\\(tr_text\\(\"default_title_strain\", lang\\), type_label, param_sel, strain_sel\\)",
    perl = TRUE,
    info = "The default title must use the validated strain value after the blank-strain guard."
  )
  expect_false(
    grepl(
      "sprintf\\(tr_text\\(\"default_title_strain\", lang\\), type_label, param_sel, input\\$strain %\\|\\|% \"\"\\)",
      title_section,
      perl = TRUE
    ),
    info = "Default title text should not use an empty string fallback for strain."
  )
})

test_that("stacked parameter selector exposes available parameters without typing", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    server_txt,
    "selectizeInput\\(\\s*\"stackParams\"[\\s\\S]*?choices = params[\\s\\S]*?selected = default_stack_params\\(params\\)[\\s\\S]*?openOnFocus = TRUE",
    perl = TRUE,
    info = "Large stacked parameter sets should be loaded into the picker so users can browse them."
  )
  expect_match(
    server_txt,
    "update_selectize_adaptive\\(\\s*\"stackParams\"[\\s\\S]*?server = FALSE[\\s\\S]*?openOnFocus = TRUE[\\s\\S]*?maxOptions = min\\(1000L, length\\(params\\)\\)",
    perl = TRUE,
    info = "Stacked parameter updates should keep browseable client-side choices."
  )
})

test_that("stacked plots expose per-parameter statistics and labels", {
  server_file <- app_test_path("server", "server_main.R")
  ui_file <- app_test_path("ui", "ui_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui_txt <- paste(readLines(ui_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    ui_txt,
    "condition = \"\\['Boxplot','Barras','Violin','Apiladas'\\]\\.indexOf\\(input\\.tipo\\) >= 0\"[\\s\\S]*?DTOutput\\('normTable'\\)[\\s\\S]*?DTOutput\\('sigTable'\\)",
    perl = TRUE,
    info = "Stacked plots should have the same normality/significance panel as the other distribution plots."
  )
  expect_match(
    server_txt,
    "stacked_grouping_col\\s*<-\\s*function[\\s\\S]*?input\\$labelMode[\\s\\S]*?\"Strain\"[\\s\\S]*?\"Label\"",
    perl = TRUE,
    info = "Stacked statistics should compare the same visible groups used by stacked plots."
  )
  expect_match(
    server_txt,
    "make_stacked_test_dfs\\s*<-\\s*function",
    perl = TRUE,
    info = "Stacked statistics need a per-parameter data builder."
  )
  expect_match(
    server_txt,
    "identical\\(input\\$tipo, \"Apiladas\"\\)[\\s\\S]*?make_stacked_test_dfs\\([\\s\\S]*?dplyr::mutate\\(res_pm, Parameter = pm, \\.before = 1\\)",
    perl = TRUE,
    info = "Stacked significance should run once per parameter and retain the parameter name."
  )
  expect_match(
    server_txt,
    "sig_table_processed <- reactive\\([\\s\\S]*?\"Parameter\" %in% names\\(raw_tbl\\)[\\s\\S]*?prepare_sig_results_tbl\\(sub",
    perl = TRUE,
    info = "Stacked significance table processing should preserve single-parameter p-value behavior."
  )
  expect_match(
    server_txt,
    "sig_pair_key(auto_tbl$group1[i], auto_tbl$group2[i], row_params[i])",
    fixed = TRUE,
    info = "Auto-generated stacked labels should not collapse identical group pairs across different parameters."
  )
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
  expect_match(helper_section, "choices_arg <- choices", fixed = TRUE)
  expect_match(helper_section, "if \\(identical\\(normalized_choices, previous_signature\\$choices\\)\\)", perl = TRUE)
  expect_match(helper_section, "if \\(!is.null\\(choices_arg\\)\\) args\\$choices <- choices_arg", perl = TRUE)

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
  expect_match(
    server_txt,
    "media_choices_sync <- sort\\(unique\\(as.character\\(df\\$Media\\)\\)\\)[\\s\\S]*?update_checkbox_group_input_if_changed\\(\\s*input_id = \"showMedios\"[\\s\\S]*?selected = media_selected_sync",
    perl = TRUE,
    info = "The visible media checkboxes must be resynced from the settled filter state after rapid user changes."
  )
  expect_match(
    server_txt,
    "group_choices_sync <- unique\\(paste\\(df\\$Strain, df\\$Media, sep = \"-\"\\)\\)[\\s\\S]*?update_checkbox_group_input_if_changed\\(\\s*input_id = \"showGroups\"[\\s\\S]*?selected = group_selected_sync",
    perl = TRUE,
    info = "The visible combined-group checkboxes must be resynced from the settled filter state after rapid user changes."
  )
  expect_match(ui_txt, "target.id + '_user_change'", fixed = TRUE)
  expect_match(ui_txt, "target.id !== 'toggleMedios' && target.id !== 'toggleGroups'", fixed = TRUE)
  expect_match(ui_txt, "ev.isTrusted", fixed = TRUE)
  expect_match(ui_txt, "BIOSZEN_debouncedCheckboxGroups", fixed = TRUE)
  expect_match(ui_txt, "version: 4", fixed = TRUE)
  expect_match(ui_txt, "pendingSelections", fixed = TRUE)
  expect_match(ui_txt, "releasedSelections", fixed = TRUE)
  expect_match(ui_txt, "releasedKeys: activeReleasedKeys()", fixed = TRUE)
  expect_match(ui_txt, "var minHoldMs = 900", fixed = TRUE)
  expect_match(ui_txt, "firstReleaseCheckMs = Math.max(50, minHoldMs - ageMs)", fixed = TRUE)
  expect_match(ui_txt, "window.setTimeout(function(){ maybeRelease(key); }, 150)", fixed = TRUE)
  expect_match(ui_txt, "var maxHoldMs = 120000", fixed = TRUE)
  expect_match(ui_txt, "data-bioszen-selector-guard", fixed = TRUE)
  expect_match(ui_txt, "data-bioszen-selector-change-count", fixed = TRUE)
  expect_match(ui_txt, "normalizeProtectedValue", fixed = TRUE)
  expect_match(ui_txt, "directEl && directEl.type === 'file'", fixed = TRUE)
  expect_match(ui_txt, "protectedSelectorKeys", fixed = TRUE)
  expect_match(ui_txt, "isProtectedKey", fixed = TRUE)
  expect_match(ui_txt, "stackParams: true", fixed = TRUE)
  expect_match(ui_txt, "heat_params: true", fixed = TRUE)
  expect_match(ui_txt, "corrm_params: true", fixed = TRUE)
  expect_match(ui_txt, "normTests: true", fixed = TRUE)
  expect_match(ui_txt, "sigTest: true", fixed = TRUE)
  expect_match(ui_txt, "postHoc: true", fixed = TRUE)
  expect_match(ui_txt, "multitest_method: true", fixed = TRUE)
  expect_match(ui_txt, "errbar_stat: true", fixed = TRUE)
  expect_match(ui_txt, "adv_pal_filters: true", fixed = TRUE)
  expect_match(ui_txt, "elementIsVisible", fixed = TRUE)
  expect_match(ui_txt, "#plotInteractivo.recalculating", fixed = TRUE)
  expect_match(ui_txt, "pageIsBusy", fixed = TRUE)
  expect_match(ui_txt, "new MutationObserver", fixed = TRUE)
  expect_match(ui_txt, "window.addEventListener\\('change', handleProtectedControlChange, true\\)", perl = TRUE)
  expect_match(ui_txt, "ev.stopImmediatePropagation\\(\\)", perl = TRUE)
  expect_match(ui_txt, "bioszen_selector_pending", fixed = TRUE)
  expect_match(ui_txt, "bioszen_selector_commit", fixed = TRUE)
  expect_match(ui_txt, "bioszen_selector_release", fixed = TRUE)
  expect_match(ui_txt, "shinyInputMatches", fixed = TRUE)
  expect_match(ui_txt, "Shiny.setInputValue\\(key, selected, \\{priority: 'event'\\}\\)", perl = TRUE)
  expect_match(ui_txt, "controlKind(target)", fixed = TRUE)
  expect_match(ui_txt, "target.type === 'radio'", fixed = TRUE)
  expect_match(ui_txt, "String\\(target.tagName \\|\\| ''\\)\\.toLowerCase\\(\\) !== 'select'", perl = TRUE)
  expect_match(ui_txt, "name === 'showMedios'", fixed = TRUE)
  expect_match(ui_txt, "name === 'showGroups'", fixed = TRUE)
  expect_match(ui_txt, "name.indexOf\\('reps_'\\) === 0", perl = TRUE)
  expect_match(ui_txt, "name.indexOf\\('qc_tech_rep_'\\) === 0", perl = TRUE)
  expect_false(grepl("selectedCheckboxValues", ui_txt, fixed = TRUE))
  expect_false(grepl("showMedios_user_change", server_txt, fixed = TRUE))
  expect_false(grepl("showGroups_user_change", server_txt, fixed = TRUE))
  expect_match(server_txt, "selector_commit_store <- new.env", fixed = TRUE)
  expect_match(server_txt, "sync_checkbox_update_cache_from_client <- function", fixed = TRUE)
  expect_match(server_txt, "record_selector_commit <- function", fixed = TRUE)
  expect_match(server_txt, "sync_checkbox_update_cache_from_client(key[[1]], selected)", fixed = TRUE)
  expect_match(server_txt, "selector_commit_active <- function(info, active_window_sec = 8, release_grace_sec = 4)", fixed = TRUE)
  expect_match(server_txt, "selector_commit_should_win <- function(info, selected, cached = NULL)", fixed = TRUE)
  expect_match(server_txt, "selector_values_equal(cached, info$selected)", fixed = TRUE)
  expect_match(server_txt, "release_selector_commit <- function", fixed = TRUE)
  expect_match(server_txt, "selector_input_is_stale <- function(key, selected, cached = NULL)", fixed = TRUE)
  expect_match(server_txt, "selector_committed_or_input <- function(key, selected, cached = NULL)", fixed = TRUE)
  expect_match(server_txt, "observeEvent(input$bioszen_selector_pending", fixed = TRUE)
  expect_match(server_txt, "observeEvent(input$bioszen_selector_release", fixed = TRUE)
  expect_match(server_txt, "observeEvent(input$bioszen_selector_commit", fixed = TRUE)
  expect_match(server_txt, "selector_input_is_stale(\"showMedios\", selected, isolate(filter_medios_selected()))", fixed = TRUE)
  expect_match(server_txt, "selector_input_is_stale(\"showGroups\", selected, isolate(filter_groups_selected()))", fixed = TRUE)
  expect_match(server_txt, "filter_selector_retry_tick <- reactiveVal\\(0L\\)", perl = TRUE)
  expect_match(server_txt, "schedule_filter_selector_retry <- function", fixed = TRUE)
  expect_match(server_txt, "later::later", fixed = TRUE)
  expect_match(
    server_txt,
    "selector_input_is_stale\\(\"showMedios\", selected, isolate\\(filter_medios_selected\\(\\)\\)\\)\\) \\{\\s*schedule_filter_selector_retry\\(\\)",
    perl = TRUE,
    info = "A temporarily stale media input must be reconsidered after the selector guard expires."
  )
  expect_match(
    server_txt,
    "selector_input_is_stale\\(\"showGroups\", selected, isolate\\(filter_groups_selected\\(\\)\\)\\)\\) \\{\\s*schedule_filter_selector_retry\\(\\)",
    perl = TRUE,
    info = "A temporarily stale combined-group input must be reconsidered after the selector guard expires."
  )
  expect_match(server_txt, "input_map[[m]] <- selector_committed_or_input", fixed = TRUE)
  expect_match(server_txt, "input_map[[g]] <- selector_committed_or_input", fixed = TRUE)
  expect_match(server_txt, "cached = current_strain_map[[m]]", fixed = TRUE)
  expect_match(server_txt, "cached = current_group_map[[g]]", fixed = TRUE)
  expect_match(server_txt, "cached = current_map[[key]]", fixed = TRUE)
  expect_match(
    server_txt,
    "reps_strain_trigger <- debounce\\(reactive\\(\\{\\s*medias <- show_medios_for_reps\\(\\) %\\|\\|% character\\(0\\)",
    perl = TRUE,
    info = "Replicate selector observers should snapshot the visible media once before reading dynamic input ids."
  )
  expect_match(
    server_txt,
    "inputs = if \\(length\\(medias\\)\\) lapply\\(medias, function\\(m\\) input\\[\\[strain_rep_input_id\\(m\\)\\]\\]\\)",
    perl = TRUE
  )
  expect_match(
    server_txt,
    "reps_group_trigger <- debounce\\(reactive\\(\\{\\s*grps <- show_groups_for_reps\\(\\) %\\|\\|% character\\(0\\)",
    perl = TRUE,
    info = "Combined replicate selector observers should snapshot the visible groups once before reading dynamic input ids."
  )
  expect_match(
    server_txt,
    "inputs = if \\(length\\(grps\\)\\) lapply\\(grps, function\\(g\\) input\\[\\[group_rep_input_id\\(g\\)\\]\\]\\)",
    perl = TRUE
  )
  expect_false(grepl("if\\s*\\(\\s*length\\(show_medios_for_reps\\(\\)\\)", server_txt, perl = TRUE))
  expect_false(grepl("if\\s*\\(\\s*length\\(show_groups_for_reps\\(\\)\\)", server_txt, perl = TRUE))
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
    "observeEvent\\(\\s*list\\(selected_show_medios\\(\\), selected_show_groups\\(\\), datos_agrupados\\(\\)\\)"
  )
  expect_match(toggle_groups, "identical\\(current_sel, target_sel\\)", perl = TRUE)
  expect_match(toggle_groups, "update_checkbox_group_input_if_changed", fixed = TRUE)
  expect_match(toggle_groups, "freeze_input = TRUE", fixed = TRUE)
  expect_match(toggle_groups, "toggle_payload\\$value", perl = TRUE)
  expect_match(toggle_groups, "ignoreInit = TRUE", fixed = TRUE)
  expect_false(grepl("updateCheckboxGroupInput", toggle_groups, fixed = TRUE))

  filter_toggle_sync <- extract_section(
    server_txt,
    "observeEvent\\(\\s*list\\(selected_show_medios\\(\\), selected_show_groups\\(\\), datos_agrupados\\(\\)\\)",
    "observeEvent\\(\\s*list\\(input\\$scope"
  )
  expect_match(filter_toggle_sync, "sync_filter_toggle_input", fixed = TRUE)
  expect_match(filter_toggle_sync, "toggleMedios", fixed = TRUE)
  expect_match(filter_toggle_sync, "toggleGroups", fixed = TRUE)
  expect_match(filter_toggle_sync, "identical\\(scope_sel, \"Por Cepa\"\\)", perl = TRUE)
  expect_match(filter_toggle_sync, "identical\\(scope_sel, \"Combinado\"\\)", perl = TRUE)
  expect_match(filter_toggle_sync, "ignoreInit = TRUE", fixed = TRUE)

  sync_toggle_helper <- extract_section(
    server_txt,
    "sync_filter_toggle_input <- function",
    "update_checkbox_group_input_if_changed <- function"
  )
  expect_match(
    sync_toggle_helper,
    "current <- isTRUE\\(isolate\\(input\\[\\[input_id\\]\\]\\)\\)",
    perl = TRUE,
    info = "Master toggle synchronization must be idempotent and skip already-correct toggles."
  )
  expect_match(sync_toggle_helper, "if \\(identical\\(current, target\\)\\) return\\(FALSE\\)", perl = TRUE)
  expect_match(sync_toggle_helper, "if \\(!begin_bulk_update\\(", perl = TRUE)

  filter_guard <- extract_section(
    server_txt,
    "observeEvent\\(\\s*list\\(input\\$scope, input\\$strain, selected_show_medios\\(\\), selected_show_groups\\(\\)\\)",
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

  expect_match(server_txt, "stable_input_suffix <- function", fixed = TRUE)
  expect_match(server_txt, "strain_rep_input_id <- function", fixed = TRUE)
  expect_match(server_txt, "group_rep_input_id <- function", fixed = TRUE)
  expect_match(server_txt, "qc_tech_input_id <- function", fixed = TRUE)
  expect_match(server_txt, "prefix = \"reps_\"", fixed = TRUE)
  expect_match(server_txt, "prefix = \"reps_grp_\"", fixed = TRUE)
  expect_match(server_txt, "prefix = \"qc_tech_rep_\"", fixed = TRUE)
  expect_false(
    grepl("paste0\\(\"reps_(grp_)?\", make.names", server_txt, perl = TRUE),
    info = "Dynamic biological replicate inputs must use stable ASCII-safe IDs instead of label-derived make.names()."
  )
  expect_false(
    grepl("paste0\\(\"qc_tech_rep_\", make.names", server_txt, perl = TRUE),
    info = "Dynamic technical replicate inputs must use stable ASCII-safe IDs instead of label-derived make.names()."
  )
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

test_that("normalized parameter switching does not preserve stale unavailable parameters", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(server_txt, "normalize_param_selection\\(input\\$param, params_all\\)", perl = TRUE)
  expect_match(server_txt, "normalize_param_selection\\(isolate\\(input\\$param %\\|\\|% \"\"\\), params\\)", perl = TRUE)
  expect_match(server_txt, "param_sel <- normalize_param_selection\\(v, safe_plot_setting_params\\(\\)\\)", perl = TRUE)
  expect_match(server_txt, "metadata_param <- normalize_param_selection\\(input\\$param, safe_plot_setting_params\\(\\)\\)", perl = TRUE)
  expect_match(server_txt, "metadata_param <- normalize_param_selection\\(last_param_selection\\(\\), safe_plot_setting_params\\(\\)\\)", perl = TRUE)

  selector_start <- regexpr("param_choices <- params", server_txt, fixed = TRUE)
  expect_true(selector_start[[1]] > 0L)
  selector_tail <- substring(server_txt, selector_start[[1]])
  selector_end <- regexpr("selected_x <-", selector_tail, fixed = TRUE)
  expect_true(selector_end[[1]] > 0L)
  selector_section <- substring(selector_tail, 1L, selector_end[[1]])

  expect_match(selector_section, "param_choices <- normalized_ready_params\\(scope_df, params\\)", perl = TRUE)
  expect_false(
    grepl("param_choices <- unique\\(c\\(current_param", selector_section, perl = TRUE),
    info = "Strict normalization must not re-add the previous parameter when it lacks normalized data."
  )
  expect_false(
    grepl("param_choices <- unique\\(c\\(preferred_param", selector_section, perl = TRUE),
    info = "Strict normalization must not re-add the remembered parameter when it lacks normalized data."
  )
})

test_that("plots, replicate data, and growth imports normalize stale parameter selections", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  extract_section <- function(txt, start_pattern, end_pattern) {
    start <- regexpr(start_pattern, txt, perl = TRUE)
    expect_true(start[[1]] > 0L, info = start_pattern)
    tail <- substring(txt, start[[1]])
    end <- regexpr(end_pattern, substring(tail, 2), perl = TRUE)
    if (end[[1]] > 0L) substring(tail, 1L, end[[1]]) else tail
  }

  param_rep_section <- extract_section(
    server_txt,
    "param_rep_df <- function\\(\\)",
    "base_plot_df <- reactive"
  )
  expect_match(param_rep_section, "normalize_param_selection\\(input\\$param, params_all\\)", perl = TRUE)
  expect_match(param_rep_section, "normalize_param_selection\\(last_param_selection\\(\\), params_all\\)", perl = TRUE)
  expect_match(param_rep_section, "paste0\\(raw_param, \"_Norm\"\\) %in% names\\(df\\)", perl = TRUE)
  expect_false(grepl("param <- input\\$param", param_rep_section, fixed = FALSE))

  build_section <- extract_section(
    server_txt,
    "build_plot <- function",
    "downsample_points_by_group <- function"
  )
  expect_match(build_section, "normalize_param_selection\\(input\\$param, params_all\\)", perl = TRUE)
  expect_match(build_section, "normalize_param_selection\\(last_param_selection\\(\\), params_all\\)", perl = TRUE)
  expect_match(build_section, "last_param_selection\\(raw_param_input\\)", perl = TRUE)

  import_section <- extract_section(
    server_txt,
    "observeEvent\\(input\\$importToPlots",
    "updateTabsetPanel\\(session, \"mainTabs\""
  )
  expect_match(import_section, "current_import_param <- normalize_param_selection", fixed = TRUE)
  expect_match(import_section, "preferred_import_param <- normalize_param_selection", fixed = TRUE)
  expect_match(import_section, "selected_import_param", fixed = TRUE)
  expect_false(
    grepl("selected = if \\(length\\(plot_cfg_box\\(\\)\\$Parameter\\)\\) plot_cfg_box\\(\\)\\$Parameter\\[1\\]", import_section, perl = TRUE),
    info = "Importing growth parameters must preserve an equivalent selected parameter when possible."
  )
})

test_that("metadata restore validates enum-like design fields before updating inputs", {
  server_file <- app_test_path("server", "server_main.R")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(server_txt, "metadata_choice <- function", fixed = TRUE)
  expect_match(server_txt, "update_radio_metadata <- function", fixed = TRUE)
  expect_match(server_txt, "update_select_metadata <- function", fixed = TRUE)

  raw_updates <- c(
    'updateRadioButtons\\(session, "curve_geom", selected = v\\)',
    'updateRadioButtons\\(session, "corr_method", selected = v\\)',
    'updateRadioButtons\\(session, "corr_norm_target", selected = v\\)',
    'updateRadioButtons\\(session, "heat_orientation", selected = v\\)',
    'updateSelectInput\\(session, "corr_adv_direction", selected = v\\)'
  )
  for (pattern in raw_updates) {
    expect_false(grepl(pattern, server_txt, perl = TRUE), info = pattern)
  }

  expect_match(server_txt, 'update_radio_metadata\\("curve_geom", v, c\\("line_points", "line_only"\\)\\)', perl = TRUE)
  expect_match(server_txt, 'update_radio_metadata\\("corr_norm_target", v, c\\("both", "x_only", "y_only"\\)\\)', perl = TRUE)
  expect_match(server_txt, 'update_select_metadata\\(\\s*"heat_hclust_method"', perl = TRUE)
})
