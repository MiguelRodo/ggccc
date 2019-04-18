context( "test plot_ccc")

data( test_tbl )

test_tbl <- cbind( test_tbl, char = "a" )

test_that( "default plot_ccc runs", {
  p <- try(  gg_ccc( data = test_tbl ) )
  expect_identical( class( p ), c( "gg", "ggplot" ) )
  expect_identical( p$labels$x, "x" )
  expect_identical( p$labels$y, "y" )
  expect_identical( class( p$layers[[4]]$geom ),
                    c( "GeomText", "Geom", "ggproto", "gg" ) )
} )

test_that( "axis re-labelling works", {
  p <- try(  gg_ccc( data = test_tbl, x = "CD4", y = "CD8" ) )
  expect_identical( p$labels$x, "CD4" )
  expect_identical( p$labels$y, "CD8" )
})

test_that( "incorrect inputs generate errors", {
  expect_message( gg_ccc( data = test_tbl ) )
  expect_error( gg_ccc( data = test_tbl, x = "gdTCR", y = "a" ) )
  expect_error( gg_ccc( data = test_tbl, x = "c", y = "CD8" ) )
  expect_error( gg_ccc( data = test_tbl, x = "char", y = "CD8" ) )
})



