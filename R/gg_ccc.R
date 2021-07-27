#' Plot of raw data with line of best fit and CCC
#'
#' @param data dataframe.
#' @param x,y numeric vectors or characters. If numeric vectors, then they specify the x- and y-
#' plotting variables, respectively. If characters, then they are taken to specify
#' the columns containing the x- and y-variables in \code{data}. If not provided,
#' then the x- and y-plotting variables are taken as the first two columsns of \code{data}.
#' \code{x} and \code{y} must have the same type, and anything else generates an error.
#' @param hor 'left' or 'right'. Specifies horizontal position of summary statistic table.
#' Default is \code{'left'}.
#' @param ver 'top' or 'bottom'. Specifies vertical position of summary statistic table.
#' Default is \code{'top'}.
#' @param shift_x numeric. Percentage of x-axis range to shift summary statistic table.
#' If positive, then the table is shifted to the right. For example, \code{shift_x=50}
#' means that the table will be shifted halfway across the width of the x-axis range.
#' Default is zero.
#' @param shift_y numeric. Analogous to \code{shift_x}, but for the y-axis. Positive
#' values move upwards. Default is 0.
#' @param table_font_size numeric. Size of font for summary statistic table.
#' @param table_font_gap numeric. Percentage of range of y-axis that rows in table are
#' separated by.
#' @param fixed_coord logical. If \code{TRUE}, then a one-unit increase in the y-axis is
#' visually represented the same length as one-unit increase on the x-axis.
#' Default is \code{TRUE}.
#' @param equal_axes_length logical. If \code{TRUE}, then x- and y-axes have the same
#' range. The range is set so that there all data points are displayed (with a slight buffer
#' around the upper and lower endpoints). Default is \code{TRUE}.
#' @param ggdraw_y_shift numeric. Provides the increase in the y-coordinate from
#' one table entry to the next. A reasonable value is 0.03. Default is \code{NULL},
#' in which case the table is placed according to the old method
#' (using shift_x, shift_y, table_font_gap).
#' @param ggdraw_x numeric. X-axis coordinate for ggdraw text. Default is 0.25.
#' @param ggdraw_text_size numeric. Font size for ggdraw text. Default is 14.
#' @param add_label logical. If \code{TRUE}, then each point is labelled using ggrepel::geom_text_repel. Default
#' is \code{FALSE}.
#' @param title character. Title to give plot. Default is \code{NULL}.
#'
#' @return A \code{ggplot2} plot with the following elements:
#' \itemize{
#'   \item raw data plotted
#'   \item black 45 degree line
#'   \item blue linear line of best fit (estimate + confidence bands)
#'   \item table of relevant summary statistics.
#' }
#'
#'
#' @details
#' The following summary statistics are
#' printed on the plot:
#' \itemize{
#'   \item Concordance CC: est (95\% CI)
#'   \item Pearson's CC: est (95\% CI)
#'   \item Intercept: est (95\% CI)
#'   \item Slope: est (95\% CI)
#' }
#' @examples
#' data( test_tbl )
#' gg_ccc( test_tbl )
#' @export
gg_ccc = function(data, x, y,
                  hor = 'left',
                  ver = 'top',
                  shift_x = 0,
                  shift_y = 0,
                  table_font_size = 5,
                  table_font_gap = 2.5,
                  fixed_coord = TRUE,
                  equal_axes_length = TRUE,
                  ggdraw_y_shift = NULL,
                  ggdraw_x = 0.25,
                  ggdraw_text_size = 14,
                  add_label = FALSE,
                  label_size = 2.5,
                  axis_lab_vec = NULL,
                  title = NULL){

  # get inputs
  if( missing( x ) | missing( y ) ){
    if( missing( data ) | ncol( data ) < 2 |
        !( is.data.frame( data ) |
           is.matrix( data ) ) ){
      stop( "If at least one of x and y are not supplied as vectors, then data must be a dataframe or matrix with at least two columns.")
    }
    x <- data[,1,drop=TRUE]
    y <- data[,2,drop=TRUE]
    lab_x <- "x"
    lab_y <- "y"
    message( "x and y not specified, and so taken as first and second column of data, respectively.")
  } else if( is.character( x ) & is.character( y ) ){
    if( x[1] %in% names( data ) ){
      lab_x <- x[1]
      x <- data[,x,drop=TRUE]
    } else{
      stop( "x not a column name in data, despite being a character vector of length 1.")
    }
    if( y[1] %in% names( data ) ){
      lab_y <- y[1]
      y <- data[,y,drop=TRUE]
    } else{
      stop( "y not a column name in data, despite being a character.")
    }
  } else{
    lab_x <- "x"
    lab_y <- "y"
  }

  if( !is.numeric( x ) | !is.numeric( y ) ){
    stop( "x and y were not ultimately set to numeric vectors, either through finding numeric columns in data or through both being provided as numeric vectors initially.")
  }

  if( length( x ) != length( y ) ) stop( "Lengths of x- and y-variables must match." )

  fit_tbl <- data.frame( ry = c( x, y ),
                         rmet = c( rep( lab_x, length(x) ),
                                   rep( lab_y, length( y ) ) ) )

  cccUst_obj = suppressWarnings( cccUst( dataset = fit_tbl,
                                         ry = "ry",
                                         rmet = "rmet",
                                         cl = 0.95 ) )
  specify_decimal <- function(x, k = 2) trimws(format(round(x, k), nsmall=k))
  ccc_vec = specify_decimal( cccUst_obj )

  raw_data_tbl <- data.frame( x = x, y = y )

  if(add_label) raw_data_tbl <- raw_data_tbl %>% mutate(label = data$label)

  mod1 <- lm( y ~ x )
  est_vec <- coef( mod1 )
  sd_vec <- coef( summary(mod1))[,2]
  bound <- qt( 0.025, nrow( data ) - 2, lower.tail = FALSE )
  int_vec <- c( est_vec[1], est_vec[1] - bound * sd_vec[1], est_vec[1] + bound * sd_vec[1] )
  int_vec <- specify_decimal( int_vec )
  slope_vec <- c( est_vec[2], est_vec[2] - bound * sd_vec[2], est_vec[2] + bound * sd_vec[2] )
  slope_vec <- specify_decimal( slope_vec )
  format(round(x, 2), nsmall = 2)
  ccc_vec <- signif( cccUst_obj, 2 )

  pcc_vec <- c( cor(x,y), cor.test(x,y,conf.level=0.95)$conf.int )
  pcc_vec <- specify_decimal( pcc_vec, 2 )
  summ_stat_vec = c( paste0( "Concordance CC:  ", ccc_vec[1], "  (", ccc_vec[2], ";", ccc_vec[3], ")" ),
                     paste0( "Pearson's CC: ", pcc_vec[1], "  (", pcc_vec[2], ";", pcc_vec[3], ")" ),
                     paste0( "Intercept:  ", int_vec[1], "  (", int_vec[2], ";", int_vec[3], ")" ),
                     paste0( "Slope:  ", slope_vec[1], "  (", slope_vec[2], ";", slope_vec[3], ")" ) )


  point_max <- max( x, y )
  range_x <- range( x )
  range_x_length <- range_x[2] - range_x[1]
  range_y <- range( y )
  range_y_length <- range_y[2] - range_y[1]
  if( fixed_coord & equal_axes_length ){
    xy <- c( x, y )
    range_x <- range( xy )
    range_x_length <- range_x_length <- range_x[2] - range_x[1]
    range_y <- range( x )
    range_y_length <- range_x_length
  }
  if( hor == "left" ){
    summ_stat_x <- range_x[1] + 0.1 * ( range_x[2] - range_x[1] )
    summ_stat_x_vec <- rep( summ_stat_x, length( summ_stat_vec ) ) +  shift_x / 100 * range_x_length
  } else if( hor == "right" ){
    summ_stat_x <- range_x[2] - 0.3 * ( range_x[2] - range_x[1] )
    summ_stat_x_vec <- rep( summ_stat_x, length( summ_stat_vec ) ) +  shift_x / 100 * range_x_length
  } else{ stop( "hor must be one of 'left' or 'right'.")}
  if( ver == "top" ){
    summ_stat_y_bottom_perc <- 100 - table_font_gap * ( length(summ_stat_vec ) - 1)
    summ_stat_y_vec <- range_y[2] * seq( summ_stat_y_bottom_perc, 100, by = table_font_gap ) / 100 + shift_y / 100 * range_y_length
    summ_stat_y_vec <- rev( summ_stat_y_vec )
  } else if( ver == "bot" ){
    summ_stat_y_top_perc <- 2.5 + 2.5 * ( length(summ_stat_vec ) - 1 )
    summ_stat_y_vec <- range_y[1] * seq( 5, summ_stat_y_top_perc, by = 2.5 ) / 100 + shift_y / 100 * range_y_length
  } else{ stop( "ver must be one of 'top' or 'bot'.")}

  # sum stat points
  if(is.null(ggdraw_y_shift)){
    p <- ggplot( raw_data_tbl, aes( x = x, y = y ) ) +
      geom_point() +
      geom_smooth( method = "lm" ) +
      geom_abline( intercept = 0, slope = 1 ) +
      labs( x = lab_x, y = lab_y ) +
      annotate( geom = 'text', x = summ_stat_x_vec,
                y = summ_stat_y_vec, label = summ_stat_vec,
                size = table_font_size )

    if( !fixed_coord ) return( p )
    if( !equal_axes_length ) p + coord_fixed()

    data_vec <- c( raw_data_tbl$x, raw_data_tbl$y )
    upper_lim = max( data_vec )
    lower_lim = min( data_vec )
    range <- upper_lim - lower_lim
    ext_value <- 0.1 * range
    lim_vec <- c( lower_lim - ext_value, upper_lim + ext_value )

    p <- p +
      coord_fixed(xlim = lim_vec, ylim = lim_vec, expand = FALSE)

  }

  p <- ggplot(raw_data_tbl,
              aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = lab_x, y = lab_y)

  if(fixed_coord){
    data_vec <- c(raw_data_tbl$x, raw_data_tbl$y)
    upper_lim = max(data_vec)
    lower_lim = min(data_vec)
    range <- upper_lim - lower_lim
    ext_value <- 0.1 * range
    lim_vec <- c(lower_lim - ext_value, upper_lim + ext_value)
    p <- p +
      coord_fixed(xlim = lim_vec,
                  ylim = lim_vec,
                  expand = FALSE)

  }
  if(equal_axes_length & !fixed_coord) p <- p + coord_fixed()

  summ_stat_y_vec <- seq(0.80, by = ggdraw_y_shift, length.out = 4) %>%
    rev()

  if(add_label) p <- p + ggrepel::geom_text_repel(aes(x = x, y = y, label = label),
                                                  size = label_size)

  if(!is.null(axis_lab_vec)) p <- p + labs(x = axis_lab_vec[1], y = axis_lab_vec[2])

  if(!is.null(title)) p <- p + labs(title = title)

  p <- ggdraw(p) +
    draw_text(text = summ_stat_vec,
              x = ggdraw_x,
              y = summ_stat_y_vec,
              size = ggdraw_text_size)

  p
}
