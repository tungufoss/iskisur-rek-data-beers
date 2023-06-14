create_plots2 <- function(df,
                          label11 = NULL, label12 = '',
                          title11 = NULL, title12 = NULL,
                          output_filename = NULL,
                          scale_y_log10 = F,
                          p11 = NULL, p12 = NULL,
                          extra_layers_p11 = NULL) {

  if (is.null(p11)) {
    if (!"group" %in% colnames(df)) {
      df$group <- 1
    }

    p11 <- df %>%
      filter(!is.na(release) & !is.na(var11)) %>%
      ggplot(aes(x = release, y = var11)) +
      geom_hline(aes(yintercept = median(var11)), color = "gray", linetype = "dashed") +
      geom_hline(aes(yintercept = mean(var11)), color = "gray", linetype = "dotted") +
      geom_line(aes(group = group)) +
      labs(title = title11, x = NULL, y = label11)

    if (!"color" %in% colnames(df)) {
      p11 <- p11 + geom_point(size = 3)
    } else if (!"shape" %in% colnames(df)) {
      p11 <- p11 + geom_point(aes(color = color), size = 3)
    } else if (!"alpha" %in% colnames(df)) {
      p11 <- p11 + geom_point(aes(color = color, shape = shape), size = 3)
    } else {
      p11 <- p11 + geom_point(aes(color = color, shape = shape, alpha = alpha), size = 3)
    }
  }

  if (!is.null(extra_layers_p11)) {
    for (layer in extra_layers_p11) {
      p11 <- p11 + layer
    }
  }

  if (is.null(p12)) {
    df %>% ggplot(aes(x = '', y = var11)) +
      ggdist::stat_halfeye(justification = -.2, .width = 0, point_color = NA) +
      geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
      ggdist::stat_dots(side = 'left', justification = 1.2, layout = "hex", stackratio = 0.5) +
      tidyquant::scale_fill_tq() +
      tidyquant::theme_tq() +
      labs(title = title12, x = NULL, y = label12) -> p12
  }
  if (scale_y_log10) {
    p11 <- p11 + scale_y_log10()
    p12 <- p12 + scale_y_log10()
  }
  plot_grid(p11, p12, ncol = 2, rel_widths = c(1, 0.33)) -> p
  if (!is.null(output_filename)) {
    ggsave(paste0("figures/", output_filename, ".png"), p, width = 10, height = 4, dpi = 300)
  }

  return(list(p11 = p11, p12 = p12, plot = p))
}

create_plots <- function(
  df,
  label11 = NULL, label22 = NULL, label21 = NULL, label12 = NULL,
  title11 = NULL, title12 = '', title21 = NULL, title22 = NULL,
  output_filename = NULL,
  scale_y_log10 = F, scale_x_log10 = F,
  p11 = NULL, p12 = NULL, p21 = NULL, p22 = NULL,
  extra_layers_p11 = NULL
) {

  p <- create_plots2(
    df,
    label11 = label11, label12 = label12,
    title11 = title11, title12 = title12,
    output_filename = NULL,
    scale_y_log10 = scale_y_log10,
    p11 = p11, p12 = p12,
    extra_layers_p11 = extra_layers_p11
  )
  p11 <- p$p11
  p12 <- p$p12

  if (is.null(p22)) {
    maxcnt = df %>%
      group_by(var22) %>%
      tally() %>%
      summarise(n = max(n)) %>%
      pull(n)
    p22 <- df %>%
      filter(!is.na(var22)) %>%
      ggplot(aes(var22)) +
      geom_histogram(stat = 'count') +
      # write the percentage and count on top of the bars = cant use statbin because it doesnt work with factors
      geom_text(stat = 'count', aes(label = paste0(round(..count.. / sum(..count..) * 100, 1), "%\n#", ..count..)),
                vjust = +0.5) +
      scale_y_continuous(expand = c(0, maxcnt * .2)) +
      labs(title = title22, x = NULL, y = label22)
  }

  if (is.null(p21)) {
    p21 <- df %>% ggplot(aes(x = var21, y = '')) +
      ggdist::stat_halfeye(justification = -.2, .width = 0, point_color = NA) +
      geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
      ggdist::stat_dots(side = 'left', justification = 1.2, layout = "hex", stackratio = 0.5) +
      tidyquant::scale_fill_tq() +
      tidyquant::theme_tq() +
      labs(title = NULL, x = label21, y = title21)
  }
  if (scale_x_log10) {
    p21 <- p21 + scale_x_log10()
  }
  plot_grid(p11, p12, p21, p22, ncol = 2, rel_widths = c(1, 0.5), rel_heights = c(1, 0.5)) -> p
  if (!is.null(output_filename)) {
    ggsave(paste0("figures/", output_filename, ".png"), p, width = 10, height = 5, dpi = 300)
  }

  return(list(p11 = p11, p12 = p12, p21 = p21, p22 = p22, plot = p))
}
