# Internal dependency anchor for R CMD check.
#
# BIOSZEN's app code lives under inst/app and loads packages at runtime there.
# R CMD check only scans package R/ code when validating Imports usage, so we
# keep one inert reference per imported package here to make that usage explicit
# without changing runtime behavior.
.bioszen_imports_anchor <- function() {
  if (FALSE) {
    shiny::runApp
    shiny.i18n::Translator
    ggplot2::ggplot
    plotly::plot_ly
    dplyr::mutate
    tidyr::pivot_longer
    readxl::read_excel
    openxlsx::read.xlsx
    rstatix::anova_test
    PMCMRplus::kwAllPairsDunnTest
    DescTools::DunnettTest
    broom::tidy
    viridis::viridis
    RColorBrewer::brewer.pal
    gcplyr::calc_deriv
    patchwork::wrap_plots
    shinyjs::useShinyjs
    DT::datatable
    nortest::ad.test
    officer::read_docx
    rvg::dml
    stringr::str_detect
    forcats::fct_relevel
    tibble::tibble
    scales::comma
    bslib::bs_theme
    htmlwidgets::saveWidget
    webshot2::webshot
    shinyBS::bsTooltip
    multcomp::glht
    dunn.test::dunn.test
    ggrepel::geom_text_repel
    rlang::enquo
    writexl::write_xlsx
    readr::read_csv
    rmarkdown::render
    zip::zip
  }
  invisible(NULL)
}
