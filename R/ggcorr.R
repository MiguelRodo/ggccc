#' @title Plot scatterplot with CCC and ab-line overlaid
#' @param data dataframe. Has columns group and y, which
#' specify the grouping variable and response variable, respectively.
#' @title character. Title, appended by the CCC estimate and 95% CI.
#' @param trans 'sqrt' or 'log'. Transformation to be applied before
#' both calculating CCC and displaying data.
#'
#' @export
ggcorr <- function(
  data,
  title,
  trans = NULL
){

  grp_vec <- unique(data$group)
  lim_vec <- range(data$y)
  if(!is.null(trans)){

  }
  plot_tbl <- data %>%
    tidyr::pivot_wider(
      names_from = 'group',
      values_from = 'y'
    )
  if(!is.null(trans)){
    trans_fn <- switch(trans,
                       "sqrt" = sqrt,
                       "log" = log,
                       stop("trans not recognised"))
    data %<>% dplyr::mutate(y = trans_fn(y))
  } else trans <- 'identity'
  corr <- suppressWarnings(cccrm::cccUst(
    dataset = data,
    ry = "y",
    rmet = "group",
    cl = 0.95))[1:3]
  corr %<>% round(2)
  cn_vec <- colnames(plot_tbl)
  cn_vec[cn_vec == grp_vec[1]] <- "g1"
  cn_vec[cn_vec == grp_vec[2]] <- "g2"
  colnames(plot_tbl) <- cn_vec
  ggplot(data = plot_tbl,
         aes(x = g1, y = g2)) +
    cowplot::theme_cowplot() +
    cowplot::background_grid(major = 'xy') +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(se = FALSE, method = 'loess', formula = y ~ x) +
    labs(x = grp_vec[1], y = grp_vec[2]) +
    coord_equal() +
    labs(title = paste0(title, " ", corr[1], " (", corr[2], "; ", corr[3], ")")) +
    scale_x_continuous(trans = trans,
                       limits = lim_vec) +
    scale_y_continuous(trans = trans,
                       limits = lim_vec)
}
