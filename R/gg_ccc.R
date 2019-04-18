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
#' @return A \code{ggplot2} plot with the following elements:
#' - raw data plotted
#' - y ~ x linear line of best fit (estimate + confidence bands)
#' - table of relevant summary statistics.
#' @details
#' The following summary statistics are
#' printed on the plot:
#' - Concordance CC: est (95% CI)
#' - Pearon's CC: est (95% CI)
#' - Intercept: est (95% CI)
#' - Slope: est (95% CI)
#' @examples
#' data( test_tbl )
#' gg_ccc( test_tbl )
#' @export
gg_ccc = function( data, x, y,
                   hor = 'left',
                   ver = 'top'){

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
  ccc_vec = round( cccUst_obj, 2 )

  raw_data_tbl <- data.frame( x = x, y = y )

  mod1 <- lm( y ~ x )
  est_vec <- coef( mod1 )
  sd_vec <- coef( summary(mod1))[,2]
  bound <- qt( 0.025, nrow( data ) - 2, lower.tail = FALSE )
  int_vec <- c( est_vec[1], est_vec[1] - bound * sd_vec[1], est_vec[1] + bound * sd_vec[1] )
  int_vec <- round( int_vec, 2 )
  slope_vec <- c( est_vec[2], est_vec[2] - bound * sd_vec[2], est_vec[2] + bound * sd_vec[2] )
  slope_vec <- round( slope_vec, 2 )

  ccc_vec <- round( cccUst_obj, 2 )

  pcc_vec <- c( cor(x,y), cor.test(x,y,conf.level=0.95)$conf.int )
  pcc_vec <- round( pcc_vec, 2 )
  summ_stat_vec = c( paste0( "Concordance CC:  ", ccc_vec[1], "  (", ccc_vec[2], ";", ccc_vec[3], ")" ),
                     paste0( "Pearson's CC: ", pcc_vec[1], "  (", pcc_vec[2], ";", pcc_vec[3], ")" ),
                     paste0( "Intercept:  ", int_vec[1], "  (", int_vec[2], ";", int_vec[3], ")" ),
                     paste0( "Slope:  ", slope_vec[1], "  (", slope_vec[2], ";", slope_vec[3], ")" ) )

  point_max = max( x, y )
  range_x <- range( x )
  range_y <- range( y )
  if( hor == "left" ){
    summ_stat_x <- range_x[1] + 0.1 * ( range_x[2] - range_x[1] )
    summ_stat_x_vec <- rep( summ_stat_x, length( summ_stat_vec ) )
  } else if( hor == "right" ){
    summ_stat_x <- range_x[2] - 0.3 * ( range_x[2] - range_x[1] )
    summ_stat_x_vec <- rep( summ_stat_x, length( summ_stat_vec ) )
  } else{ stop( "hor must be one of 'left' or 'right'.")}
  if( ver == "top" ){
    summ_stat_y_bottom_perc <- 100 - 2.5 * ( length(summ_stat_vec ) - 1)
    summ_stat_y_vec <- range_y[2] * seq( summ_stat_y_bottom_perc,
                                          100, by = 2.5 ) / 100
    summ_stat_y_vec <- rev( summ_stat_y_vec )
  } else if( ver == "bot" ){
    summ_stat_y_top_perc <- 2.5 + 2.5 * ( length(summ_stat_vec ) - 1 )
    summ_stat_y_vec <- range_y[1] * seq( 5, summ_stat_y_top_perc,
                                         by = 2.5 ) / 100
  } else{ stop( "ver must be one of 'top' or 'bot'.")}

  # sum stat points

  ggplot( raw_data_tbl, aes( x = x, y = y ) ) +
    geom_point() +
    geom_smooth( method = "lm" ) +
    geom_abline( intercept = 0, slope = 1 ) +
    labs( x = lab_x, y = lab_y ) +
    annotate( geom = 'text', x = summ_stat_x_vec,
              y = summ_stat_y_vec, label = summ_stat_vec,
              size = 5 )
}
