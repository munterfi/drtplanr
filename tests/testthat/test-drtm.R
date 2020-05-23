test_that("drtm works", {
  # Example data
  aoi <-
    sf::st_read(system.file("example.gpkg", package="drtplanr"),
                layer = "aoi", quiet = TRUE)
  pop <-
    sf::st_read(system.file("example.gpkg", package="drtplanr"),
                layer = "pop", quiet = TRUE)

  # Create model
  m <- drt_drtm(
    model_name = "Jegenstorf",
    aoi = aoi, pop = pop,
    n_vir = 10, m_seg = 100
  )

  # Test
  expect_is(m, "drtm")
  expect_equal(length(m), 9)
})
