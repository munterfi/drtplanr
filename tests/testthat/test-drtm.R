test_that("drtm works", {
  # Example data
  aoi <-
    sf::st_read(system.file("example.gpkg", package = "drtplanr"),
                layer = "aoi", quiet = TRUE)
  pop <-
    sf::st_read(system.file("example.gpkg", package = "drtplanr"),
                layer = "pop", quiet = TRUE)

  # Create model
  m <- drt_drtm(
    model_name = "Jegenstorf",
    aoi = aoi, pop = pop,
    n_vir = 10, m_seg = 100
  )

  # Test drtm class
  expect_is(m, "drtm")
  expect_equal(length(m), 9)

  # Iterate model with different energy
  m$params$calc_energy <- e_walk_pop
  m <- drt_iterate(m, 10)

  # Test iteration
  expect_equal(m$i, 10)

})
