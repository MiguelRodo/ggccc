#' @title Plot scatterplot with CCC and ab-line overlaid
#'
#'
#' @param data dataframe. Has columns .
#' @param grp character. Name of column in \code{data} that specifies
#' the variables to compare between.
#' @param y character. Name of column in \code{data} that specifies
#' the values that should match.
#' @param id character. Name of column in \code{data} that specifies
#' the subject from which multiple measurements were taken.
#' @param
#' @title character. Title, appended by the CCC estimate and 95% CI.
#' @param trans object of class \code{trans} from package \code{scales}.
#' Specifies transformation and inverse transformation for plotting.
#' @param character. Name of column in \code{data}
#' that contains labels for points.
#' Uses \code{ggrepel::geom_text_repel}.
#' If \code{NULL}, then no labels are plotted.
#' Default is \code{NULL}.
#'
#' @export
ggcorr <- function(
  data,
  grp,
  y,
  id,
  label_id = TRUE,
  label_id_size = 3,
  title = NULL,
  trans = scales::identity_trans(),
  axis_lab = NULL,
  table_coord = c(0.1, 1),
  table_size_skip = 0.05,
  table_size_text = 10,
  range_extend = 0,
  limits_include = NULL,
  thm = cowplot::theme_cowplot(),
  grid = cowplot::background_grid(major = "xy"),
  grp_to_col = NULL,
  abline = TRUE,
  smooth = TRUE,
  smooth_method = "lm"
){

  cn_vec <- colnames(data)
  cn_vec[which(cn_vec == grp)] <- ".grp"
  cn_vec[which(cn_vec == y)] <- ".y"
  cn_vec[which(cn_vec == id)] <- ".id"
  colnames(data) <- cn_vec

  grp_vec <- unique(data$`.grp`)


  combn_mat <- combn(grp_vec, 2)
  results_tbl <- purrr::map_df(seq_len(ncol(combn_mat)), function(i) {
    grp_vec_curr <- combn_mat[,i]
    data_curr <- data %>%
      dplyr::filter(.grp %in% grp_vec_curr)
    corr <- suppressWarnings(cccrm::cccUst(
      dataset = data_curr,
      ry = ".y",
      rmet = ".grp",
      cl = 0.95))[1:3]
    corr <- corr %>% signif(2)
    tibble::tibble(g1 = grp_vec_curr[1],
                   g2 = grp_vec_curr[2],
                   est = corr[1],
                   lb = corr[2],
                   ub = corr[3]) %>%
      dplyr::mutate(
        txt = paste0(g1, " vs ", g2, ": ", corr[1], " (", corr[2], "; ", corr[3], ")")
      )
  })

  combn_tbl_for_plot_raw <- combn_mat %>%
    tibble::as_tibble(.name_repair = "minimal")
  match_elem <- combn_tbl_for_plot_raw[[1]][[1]]
  col_sel_vec <- purrr::map_lgl(seq_along(combn_tbl_for_plot_raw), function(i) {
    combn_tbl_for_plot_raw[[i]][[1]] == match_elem
  })

  col_sel_vec_ind <- which(col_sel_vec)
  plot_tbl_raw <- purrr::map_df(col_sel_vec_ind, function(i) {
    data %>%
      dplyr::filter(.grp == match_elem) %>%
      dplyr::select(.grp, .id, .y) %>%
      dplyr::rename(x = .y,
                    grp_x = .grp) %>%
      dplyr::full_join(
        data %>%
          dplyr::filter(.grp == combn_tbl_for_plot_raw[[i]][[2]]) %>%
          dplyr::select(.grp, .id, .y) %>%
          dplyr::rename(y = .y,
                        grp_y = .grp),
        by = ".id"
      ) %>%
      dplyr::select(.id, grp_x, grp_y, x, y)
  })

  p <- ggplot(
    plot_tbl_raw,
    aes(x = x, y = y, col = grp_y)
  ) +
    thm +
    grid

  p <- p +
    geom_point(
      data = plot_tbl_raw
    ) +
    theme(legend.position = "bottom",
          legend.title = element_blank())

  if (!is.null(grp_to_col)) {
    if (is.null(names(grp_to_col))) {
      grp_to_col <- setNames(
        rep(grp_to_col, length(unique(plot_tbl_raw$grp_y))),
        length(unique(plot_tbl_raw$grp_y))
      )
    }
    p <- p +
      scale_colour_manual(values = grp_to_col,
                          limits = unique(plot_tbl_raw$grp_y))
  } else {
    p <- p +
      switch(
        as.character(length(unique(plot_tbl_raw$grp_y))),
        "1" = scale_colour_manual(
          values = setNames(
            rep("black", length(unique(plot_tbl_raw$grp_y))),
            length(unique(plot_tbl_raw$grp_y))
          )),
        scale_colour_brewer(palette = "Set1")
      )
  }

  if (abline) {
    p <- p + geom_abline(intercept = 0, slope = 1, linetype = "solid",
                         colour = "gray85")
  }

  if (smooth) {
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
  }

  if (is.null(axis_lab)) {
    p <- p + labs(x = match_elem)
    if (ncol(combn_mat) == 1) {
      p <- p +
        labs(y = combn_mat[2, 1]) +
        theme(legend.position = "none")
    } else {
      p <- p + labs(y = "Comparison group")
    }
  } else {
    p <- p +
      labs(x = axis_lab[1], y = axis_lab[2])
  }

  if (!is.null(title)) {
    p <- p + labs(title = title)
  }

  lim_vec_orig <- range(data$`.y`)
  if (!is.null(limits_include)) {
    lim_vec_orig <- c(min(lim_vec_orig, limits_include),
                      max(lim_vec_orig, limits_include))
  }

  lim_vec_orig_trans <- trans$transform(lim_vec_orig)
  length_axis_orig_trans <- diff(lim_vec_orig_trans)
  range_upper_bound_trans_y <- lim_vec_orig_trans[2] * (1 + range_extend)

  p <- p +
    coord_equal() +
    expand_limits(x = c(limits_include, range_upper_bound_trans_y),
                  y = c(limits_include, range_upper_bound_trans_y)) +
    scale_x_continuous(trans = trans) +
    scale_y_continuous(trans = trans)

  for (i in seq_len(nrow(results_tbl))) {
    plot_tbl_txt <- tibble::tibble(
      x = lim_vec_orig_trans[1] + table_coord[1] * length_axis_orig_trans,
      y = lim_vec_orig_trans[1] + table_coord[2] * length_axis_orig_trans
      - table_size_skip * length_axis_orig_trans * (i - 1),
      txt = results_tbl$txt[[i]]
    ) %>%
      dplyr::mutate(x = trans$inverse(x),
                    y = trans$inverse(y))
    p <- p +
      geom_text(
        data = plot_tbl_txt,
        mapping = aes(x = x, y = y, label = txt),
        inherit.aes = FALSE,
        size = table_size_text,
        hjust = 0,
        vjust = 0
      )
  }

  if (label_id) {
    p <- p +
      ggrepel::geom_text_repel(aes(label = .id), size = label_id_size)
  }

  p
}
