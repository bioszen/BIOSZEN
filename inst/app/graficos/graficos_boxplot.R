# Helpers para gráficos Boxplot ------------------------------------------------

add_sigline <- function(p, group1, group2, label = "*",
                        height   = .05,
                        vsize    = .02,
                        tpad     = .01,
                        linewidth = .8,
                        textsize  = 5) {
  build  <- ggplot_build(p)
  xbreaks <- build$layout$panel_params[[1]]$x$breaks
  get_x   <- function(g) if (is.numeric(g)) g else match(g, xbreaks)
  x1 <- get_x(group1); x2 <- get_x(group2)
  dat   <- build$data[[1]]
  ytop  <- if ("ymax" %in% names(dat)) max(dat$ymax, na.rm = TRUE)
            else max(dat$y, na.rm = TRUE)
  yrng  <- diff(range(build$layout$panel_params[[1]]$y.range))
  ybar  <- ytop + height * yrng
  ycap  <- ybar - vsize * yrng
  ytxt  <- ybar + tpad * yrng
  p +
    annotate("segment", x = x1, xend = x2, y = ybar, yend = ybar, linewidth = linewidth) +
    annotate("segment", x = x1, xend = x1, y = ybar, yend = ycap, linewidth = linewidth) +
    annotate("segment", x = x2, xend = x2, y = ybar, yend = ycap, linewidth = linewidth) +
    annotate("text",    x = mean(c(x1, x2)), y = ytxt,
             label = label, size = textsize, vjust = 0) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
}

stack_siglines <- function(p, sigs, sep = .05,
                           linewidth = .8, vsize = .02,
                           tpad = .01, tsize = 5) {
  if (length(sigs) == 0 || length(ggplot_build(p)$data) == 0)
    return(p)

  build  <- ggplot_build(p)
  yrng   <- build$layout$panel_params[[1]]$y.range
  xranks <- build$layout$panel_params[[1]]$x$breaks

  get_span <- function(cmp) {
    x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, xranks)
    x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, xranks)
    c(min(x1, x2), max(x1, x2))
  }

  levels <- list()
  bar_level <- integer(length(sigs))

  for (i in seq_along(sigs)) {
    span <- get_span(sigs[[i]])
    placed <- FALSE
    for (lvl in seq_along(levels)) {
      overlap <- vapply(levels[[lvl]], function(iv)
        !(span[2] < iv[1] || span[1] > iv[2]), logical(1))
      if (!any(overlap)) {
        levels[[lvl]] <- append(levels[[lvl]], list(span))
        bar_level[i]  <- lvl
        placed <- TRUE; break
      }
    }
    if (!placed) {
      levels[[length(levels) + 1]] <- list(span)
      bar_level[i] <- length(levels)
    }
  }

  extra <- (length(levels) + 1) * sep * diff(yrng)
  p <- p + expand_limits(y = max(yrng) + extra)

  for (i in seq_along(sigs)) {
    h   <- bar_level[i] * sep
    cmp <- sigs[[i]]
    p <- add_sigline(p, cmp$g1, cmp$g2, cmp$lab,
                     height = h, vsize = vsize,
                     tpad = tpad, linewidth = linewidth,
                     textsize = tsize)
  }
  p
}

plot_boxplot <- function(scope, strain = NULL) {
  build_plot(scope, strain, "Boxplot")
}
