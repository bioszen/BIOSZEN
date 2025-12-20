# Helpers para gráficos de barras ---------------------------------------------

get_palette <- function(n) {
  tone_down <- function(cols, amount = 0.35) {
    amt <- pmin(pmax(amount, 0), 1)
    m   <- grDevices::col2rgb(cols)
    m2  <- m + (255 - m) * amt
    grDevices::rgb(m2[1, ]/255, m2[2, ]/255, m2[3, ]/255)
  }
  okabe <- function(n) rep(c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  ), length.out = n)
  tableau <- function(n) rep(c(
    "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
    "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  ), length.out = n)
  brew <- function(n, name) {
    info <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
    pal  <- RColorBrewer::brewer.pal(info, name)
    rep(pal, length.out = n)
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
