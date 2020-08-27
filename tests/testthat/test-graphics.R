test_that("graphics works", {

  # Load file
  m <- drt_import(
    system.file("example_i1000.RData", package = "drtplanr")
  )

  # Test energy plot
  expect_is(drt_plot(m), "ggplot")

  # Test station map
  expect_is(drt_map(m), "mapview")

})
