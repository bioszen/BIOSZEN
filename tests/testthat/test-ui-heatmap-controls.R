test_that("heatmap controls are moved to dedicated UI fragment", {
  ui_main <- app_test_path( "ui", "ui_main.R")
  ui_fragment <- app_test_path( "ui", "ui_heatmap_controls.R")
  server_file <- app_test_path( "server", "server_main.R")

  expect_true(file.exists(ui_main))
  expect_true(file.exists(ui_fragment))
  expect_true(file.exists(server_file))

  main_txt <- paste(readLines(ui_main, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  frag_txt <- paste(readLines(ui_fragment, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  server_txt <- paste(readLines(server_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(main_txt, "heatmap_controls_ui\\(\\)", perl = TRUE)
  expect_match(frag_txt, "heatmap_controls_ui\\s*<-\\s*function", perl = TRUE)
  expect_match(
    frag_txt,
    "condition\\s*=\\s*\"input\\.tipo\\s*==\\s*'Heatmap'\"",
    perl = TRUE
  )
  main_lines <- readLines(ui_main, warn = FALSE, encoding = "UTF-8")
  chart_idx <- grep("id = \"section_chart_options\"", main_lines, fixed = TRUE)[1]
  heat_idx <- grep("heatmap_controls_ui\\(\\)", main_lines, perl = TRUE)[1]
  appearance_idx <- grep("id = \"section_appearance\"", main_lines, fixed = TRUE)[1]
  expect_true(is.finite(chart_idx) && is.finite(heat_idx) && is.finite(appearance_idx))
  expect_true(chart_idx < heat_idx && heat_idx < appearance_idx)

  expect_match(frag_txt, "\"heat_params\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_hclust_method\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_cluster_rows\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_cluster_cols\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_show_side_dend\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_show_top_dend\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_k_rows\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_k_cols\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_select_all_params\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_clear_all_params\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_show_param_labels\"", fixed = TRUE)
  expect_match(frag_txt, "\"heat_orientation\"", fixed = TRUE)
  expect_match(frag_txt, "\"heatmapLoadingUI\"", fixed = TRUE)
  expect_match(frag_txt, "\"downloadHeatClusters\"", fixed = TRUE)
  expect_match(server_txt, "observeEvent\\(input\\$heat_select_all_params", perl = TRUE)
  expect_match(server_txt, "observeEvent\\(input\\$heat_clear_all_params", perl = TRUE)
  expect_match(server_txt, "output\\$heatmapLoadingUI\\s*<-\\s*renderUI\\(", perl = TRUE)
  expect_match(
    frag_txt,
    "condition\\s*=\\s*\"input\\.heat_cluster_rows\"[\\s\\S]{0,800}?\"heat_k_rows\"",
    perl = TRUE
  )
  expect_match(
    frag_txt,
    "condition\\s*=\\s*\"input\\.heat_cluster_cols\"[\\s\\S]{0,800}?\"heat_k_cols\"",
    perl = TRUE
  )
  expect_match(
    frag_txt,
    "condition\\s*=\\s*\"input\\.heat_cluster_rows\"[\\s\\S]{0,1200}?\"downloadHeatClusters\"",
    perl = TRUE
  )
})
