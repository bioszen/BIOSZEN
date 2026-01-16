# Whisker cap helper ---------------------------------------------------------
# Draws horizontal whisker caps using precomputed box stats to keep them
# visible both in the interactive panel and in PNG/PDF exports.

draw_whisker_caps <- function(p, stats_df, x_levels, box_width, line_width) {
  if (is.null(stats_df) || !nrow(stats_df)) return(p)
  if (is.null(x_levels) || !length(x_levels)) {
    x_levels <- unique(stats_df$group)
  }

  bw <- as.numeric(box_width)
  if (!is.finite(bw) || bw <= 0) bw <- 0.8
  cap_half <- max((bw * 0.45) / 2, 0.05)

  lw <- as.numeric(line_width)
  if (!is.finite(lw) || lw <= 0) lw <- 0.6

  caps_df <- stats_df
  caps_df$group <- factor(caps_df$group, levels = x_levels)
  caps_df <- caps_df[!is.na(caps_df$group) &
                       is.finite(caps_df$lower) &
                       is.finite(caps_df$upper), , drop = FALSE]
  if (!nrow(caps_df)) return(p)

  caps_df$x_num   <- as.numeric(caps_df$group)
  caps_df$x_start <- caps_df$x_num - cap_half
  caps_df$x_end   <- caps_df$x_num + cap_half

  p_caps <- p + ggplot2::geom_segment(
    data = caps_df,
    ggplot2::aes(x = x_start, xend = x_end, y = lower, yend = lower),
    inherit.aes = FALSE,
    linewidth   = lw,
    colour      = "black",
    lineend     = "butt"
  )

  p_caps <- p_caps + ggplot2::geom_segment(
    data = caps_df,
    ggplot2::aes(x = x_start, xend = x_end, y = upper, yend = upper),
    inherit.aes = FALSE,
    linewidth   = lw,
    colour      = "black",
    lineend     = "butt"
  )

  p_caps
}
