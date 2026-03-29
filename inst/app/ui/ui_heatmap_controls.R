# Heatmap controls UI fragment -------------------------------------------------

heatmap_controls_ui <- function() {
  conditionalPanel(
    condition = "input.tipo == 'Heatmap'",
    h4(tr("heatmap_settings")),
    selectizeInput(
      "heat_params",
      tr("heatmap_params"),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    tags$div(
      style = "display: flex; gap: 8px; margin-top: 4px; margin-bottom: 8px;",
      actionButton(
        "heat_select_all_params",
        tr("heatmap_select_all_params"),
        class = "btn btn-outline-primary btn-sm",
        style = "flex: 1;"
      ),
      actionButton(
        "heat_clear_all_params",
        tr("heatmap_clear_all_params"),
        class = "btn btn-outline-secondary btn-sm",
        style = "flex: 1;"
      )
    ),
    uiOutput("heatmapLoadingUI"),
    checkboxInput("heat_show_param_labels", tr("heatmap_show_param_labels"), TRUE),
    radioButtons(
      "heat_orientation",
      tr("heatmap_orientation"),
      choices = named_choices(
        c("params_rows", "params_cols"),
        list(
          tr("heatmap_orientation_params_rows"),
          tr("heatmap_orientation_params_cols")
        )
      ),
      selected = "params_rows",
      inline = TRUE
    ),
    radioButtons(
      "heat_scale_mode",
      tr("heatmap_scale_mode"),
      choices = named_choices(
        c("none", "row", "column"),
        list(
          tr("heatmap_scale_none"),
          tr("heatmap_scale_row"),
          tr("heatmap_scale_col")
        )
      ),
      selected = "none",
      inline = TRUE
    ),
    selectInput(
      "heat_hclust_method",
      tr("heatmap_cluster_method"),
      choices = c("ward.D2", "ward.D", "complete", "average", "single", "mcquitty", "median", "centroid"),
      selected = "ward.D2"
    ),
    checkboxInput("heat_cluster_rows", tr("heatmap_cluster_rows"), FALSE),
    conditionalPanel(
      condition = "input.heat_cluster_rows",
      numericInput("heat_k_rows", tr("heatmap_row_clusters_n"), value = 2, min = 1, step = 1)
    ),
    checkboxInput("heat_show_side_dend", tr("heatmap_dendro_rows"), FALSE),
    checkboxInput("heat_cluster_cols", tr("heatmap_cluster_cols"), FALSE),
    conditionalPanel(
      condition = "input.heat_cluster_cols",
      numericInput("heat_k_cols", tr("heatmap_col_clusters_n"), value = 2, min = 1, step = 1)
    ),
    checkboxInput("heat_show_top_dend", tr("heatmap_dendro_cols"), FALSE),
    checkboxInput("heat_show_values", tr("heatmap_show_values"), FALSE),
    conditionalPanel(
      condition = "input.heat_cluster_rows",
      downloadButton(
        "downloadHeatClusters",
        tr("heatmap_download_clusters"),
        class = "btn btn-outline-secondary btn-sm"
      )
    )
  )
}
