#' Plot of raw data with line of best fit and CCC
#'
#' \code{plotCCC} calculates the CCC for a
#' given combination of cd and cytokine combination,
#' and then plots over the
#' @param cyt character. If one of "G", "2" and "T". The
#' corresponding cytokine is summed over across all
#' cytokine combinations. If a full cytokine combination, then
#' the values of that cytokine combination are used.
#' @param cd 4 or 8. The cd of the data.
#' @param data dataframe. A dataframe with columns ptid, cd,
#' stim, type, proc, stain and resp.
#'
#' @section Assumptions:
#'
#' Note that this plot makes the following coding assumptions.
#' - SATVI is the site labelled "0" and SUN is the site labelled "1".
#'
#' @export
plotCCC = function( cyt, cd, data ){

  # force cyt to be a character vector
  cyt = as.character( cyt )

  # check if you need to sum over cyt
  if( str_length( cyt ) == 1 ){
    currCytPos = cytPosVec[ cyt ] + 1
    cytTbl = data %>%
      mutate( cytInd = Vectorize(ifelse)(
        str_sub( type, currCytPos, currCytPos ) == "+",
        TRUE, FALSE ) ) %>%
      group_by( ptid, stim, proc, cd ) %>%
      summarise( resp = sum( resp[ cytInd ] ) ) %>%
      ungroup()

    plotTitle = str_c("CCC: CD", cd, " ", cyt, "+", ", ", labLabVec[data$stain[1]], " Staining" )
  } else{ # if a cytcombo must be filtered for
    cytSubVec = data$type ==  cyt
    cytTbl = data[cytSubVec,]
    plotTitle = str_c("CCC: CD", cd, " ", cyt, ", ", labLabVec[data$stain[1]], " Staining"  )
  }

  # cd sub
  cdSubVec = cytTbl$cd == cd

  # cccTbl
  workingTbl = cytTbl %>%
    mutate( ptid.stim = str_c( ptid, ".", stim ) ) %>%
    filter( cdSubVec ) %>%
    arrange( ptid.stim, proc )

  # plotTbl
  plotTbl = workingTbl %>%
    spread( key = proc, value = resp ) %>%
    rename( sun = `0`, satvi = `1` ) %>%
    mutate( ptid.stim = str_c( ptid, ".", stim ) ) %>%
    arrange( ptid.stim )

  # ccc estimation
  cccEst = cccUst( workingTbl, "resp", rmet = "proc" )

  # line fit
  mod1 = lm( satvi ~ sun, data = plotTbl )
  estVec = coef( mod1 )
  sdVec = coef( summary(mod1))[,2]
  bound = qt( 0.025, nrow( data ) - 2, lower.tail = FALSE )
  intVec = c( estVec[1], estVec[1] - bound * sdVec[1], estVec[1] + bound * sdVec[1] ) %>% round(2)
  slopeVec = c( estVec[2], estVec[2] - bound * sdVec[2], estVec[2] + bound * sdVec[2] ) %>% round(2)
  pointMax = max( plotTbl$satvi, plotTbl$sun )
  xFitLine = c( 0, pointMax )
  yFitLine = c( 0, estVec[1] + estVec[2] * pointMax)


  # perfect line
  perfUpperPoint = max( plotTbl$satvi, plotTbl$sun )

  # sum stat points
  yVec = max( yFitLine[2], perfUpperPoint ) * rev( seq(8.5,9.5,by=0.5) ) / 10
  xVec = rep( 0.22 * xFitLine[2], 3 )
  cccVec = cccEst %>% round(2)
  labVec = c( str_c( "CCC:  ", cccVec[1], "  (", cccVec[2], ";", cccVec[3], ")" ),
    str_c( "Int:  ", intVec[1], "  (", intVec[2], ";", intVec[3], ")" ),
    str_c( "Slope:  ", slopeVec[1], "  (", slopeVec[2], ";", slopeVec[3], ")" ) )

  # plot
  ggplot( plotTbl, aes( x = sun, y = satvi ) ) +
    geom_point() +
    # geom_smooth( method = 'loess', e = FALSE ) +
    annotate( geom = 'line', x = c( 0, perfUpperPoint ),
      y = c( 0, perfUpperPoint ) ) +
    labs( x = str_c( labLabVec[1], " Processing"), y = str_c( labLabVec[2], " Processing"), title = plotTitle ) +
    # annotate( geom = 'line', x = xFitLine, y = yFitLine, linetype = 2 ) +
    annotate( geom = 'text', x = xVec, y = yVec, label = labVec ) +
    geom_smooth( method = "lm" )

}
