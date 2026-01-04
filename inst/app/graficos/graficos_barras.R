# Helpers para gráficos de barras ---------------------------------------------

get_palette <- function(n) {
  tone_down <- function(cols, amount = 0.35) {
    amt <- pmin(pmax(amount, 0), 1)
    m   <- grDevices::col2rgb(cols)
    m2  <- m + (255 - m) * amt
    grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)
  }
  if (n <= 0) return(character(0))
  okabe_cols <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )
  tableau_cols <- c(
    "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
    "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  )
  kelly_cols <- c(
    "#F3C300", "#875692", "#F38400", "#A1CAF1", "#BE0032", "#C2B280",
    "#848482", "#008856", "#E68FAC", "#0067A5", "#F99379", "#604E97",
    "#F6A600", "#B3446C", "#DCD300", "#882D17", "#8DB600", "#654522",
    "#E25822", "#2B3D26", "#F2F3F4", "#222222"
  )
  tol_bright <- c("#4477AA", "#EE6677", "#228833", "#CCBB44",
                  "#66CCEE", "#AA3377", "#BBBBBB")
  tol_muted  <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                  "#DDCC77", "#CC6677", "#882255", "#AA4499")
  tol_light  <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33", "#AAAA00",
                  "#EEDD88", "#EE8866", "#FFAABB", "#DDDDDD")
  d3_cat10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
  d3_cat20 <- c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
                "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
                "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
                "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5")
  take_n <- function(cols, n) {
    if (n <= 0) return(character(0))
    if (n > length(cols)) {
      return(grDevices::colorRampPalette(cols)(n))
    }
    cols[seq_len(n)]
  }
  okabe <- function(n) rep(okabe_cols, length.out = n)
  tableau <- function(n) rep(tableau_cols, length.out = n)
  brew <- function(n, name) {
    info <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
    pal  <- RColorBrewer::brewer.pal(info, name)
    rep(pal, length.out = n)
  }
  brew_adv <- function(n, name) {
    info <- RColorBrewer::brewer.pal.info
    if (is.null(name) || !nzchar(name) || !name %in% rownames(info)) {
      return(NULL)
    }
    max_col <- info[name, "maxcolors"]
    if (n < 3) {
      pal <- RColorBrewer::brewer.pal(3, name)
      return(pal[seq_len(n)])
    }
    if (n > max_col) {
      base <- RColorBrewer::brewer.pal(max_col, name)
      return(grDevices::colorRampPalette(base)(n))
    }
    RColorBrewer::brewer.pal(n, name)
  }
  adv_palette <- function(n, name) {
    if (is.null(name) || !nzchar(name)) return(NULL)
    if (name %in% rownames(RColorBrewer::brewer.pal.info)) {
      return(brew_adv(n, name))
    }
    switch(name,
           "Viridis"      = viridis::viridis(n),
           "Plasma"       = viridis::plasma(n),
           "Magma"        = viridis::magma(n),
           "Inferno"      = viridis::inferno(n),
           "Cividis"      = viridis::cividis(n),
           "Aqua"         = grDevices::colorRampPalette(
             c("#E8F6F8", "#7FC8D6", "#005B6A")
           )(n),
           "Rose"         = grDevices::colorRampPalette(
             c("#FFF0F3", "#FF8FA3", "#C9184A")
           )(n),
           "Amber"        = grDevices::colorRampPalette(
             c("#FFF3E0", "#FDBA74", "#B45309")
           )(n),
           "Slate"        = grDevices::colorRampPalette(
             c("#F1F5F9", "#94A3B8", "#1E293B")
           )(n),
           "Forest"       = grDevices::colorRampPalette(
             c("#E8F5E9", "#81C784", "#1B5E20")
           )(n),
           "Ocean"        = grDevices::colorRampPalette(
             c("#E0F2FE", "#38BDF8", "#0C4A6E")
           )(n),
           "BlueRed"      = grDevices::colorRampPalette(
             c("#2166AC", "#F7F7F7", "#B2182B")
           )(n),
           "PurpleOrange" = grDevices::colorRampPalette(
             c("#5E3C99", "#F7F7F7", "#E66101")
           )(n),
           "GreenBrown"   = grDevices::colorRampPalette(
             c("#1B7837", "#F7F7F7", "#8C510A")
           )(n),
           "BlueOrange"   = grDevices::colorRampPalette(
             c("#2166AC", "#F7F7F7", "#F4A582")
           )(n),
           "TealRed"      = grDevices::colorRampPalette(
             c("#008080", "#F7F7F7", "#D73027")
           )(n),
           "PurpleGreen"  = grDevices::colorRampPalette(
             c("#762A83", "#F7F7F7", "#1B7837")
           )(n),
           "CyanMagenta"  = grDevices::colorRampPalette(
             c("#00A6D6", "#F7F7F7", "#D81B60")
           )(n),
           "BrownTeal"    = grDevices::colorRampPalette(
             c("#8C510A", "#F7F7F7", "#01665E")
           )(n),
           "Hue"          = safe_hue(n),
           "OkabeIto"     = take_n(okabe_cols, n),
           "Tableau"      = take_n(tableau_cols, n),
           "Kelly"        = take_n(kelly_cols, n),
           "TolBright"    = take_n(tol_bright, n),
           "TolMuted"     = take_n(tol_muted, n),
           "TolLight"     = take_n(tol_light, n),
           "D3Category10" = take_n(d3_cat10, n),
           "D3Category20" = take_n(d3_cat20, n),
           NULL)
  }

  if (isTRUE(input$adv_pal_enable)) {
    pal_adv <- adv_palette(n, input$adv_pal_name)
    if (!is.null(pal_adv)) {
      if (isTRUE(input$adv_pal_reverse) && identical(input$adv_pal_type, "seq")) {
        pal_adv <- rev(pal_adv)
      }
      return(pal_adv)
    }
  }

  switch(input$colorMode,
         "Default"              = safe_hue(n),
         "Default Suave"        = tone_down(safe_hue(n)),
         "Blanco y Negro"       = rep("black", n),
         "Blanco y Negro Suave" = rep("#666666", n),
         "Viridis"              = viridis::viridis(n),
         "Viridis Suave"        = tone_down(viridis::viridis(n)),
         "Plasma"               = viridis::plasma(n),
         "Plasma Suave"         = tone_down(viridis::plasma(n)),
         "Magma"                = viridis::magma(n),
         "Magma Suave"          = tone_down(viridis::magma(n)),
         "Cividis"              = viridis::cividis(n),
         "Cividis Suave"        = tone_down(viridis::cividis(n)),
         "Set1"                 = brew(n, "Set1"),
         "Set1 Suave"           = tone_down(brew(n, "Set1")),
         "Set2"                 = brew(n, "Set2"),
         "Set2 Suave"           = tone_down(brew(n, "Set2")),
         "Set3"                 = brew(n, "Set3"),
         "Set3 Suave"           = tone_down(brew(n, "Set3")),
         "Dark2"                = brew(n, "Dark2"),
         "Dark2 Suave"          = tone_down(brew(n, "Dark2")),
         "Accent"               = brew(n, "Accent"),
         "Accent Suave"         = tone_down(brew(n, "Accent")),
         "Paired"               = brew(n, "Paired"),
         "Paired Suave"         = tone_down(brew(n, "Paired")),
         "Pastel1"              = brew(n, "Pastel1"),
         "Pastel1 Suave"        = tone_down(brew(n, "Pastel1"), amount = 0.2),
         "Pastel2"              = brew(n, "Pastel2"),
         "Pastel2 Suave"        = tone_down(brew(n, "Pastel2"), amount = 0.2),
         "OkabeIto"             = okabe(n),
         "OkabeIto Suave"       = tone_down(okabe(n)),
         "Tableau"              = tableau(n),
         "Tableau Suave"        = tone_down(tableau(n)),
         safe_hue(n))
}

plot_barras <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Barras")
}
