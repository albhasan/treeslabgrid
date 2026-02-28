test_that("get_geom_colname works", {
  g <- sf::st_sfc(sf::st_point(1:2))

  # sfc is not sf!
  expect_error(get_geom_colname(g))

  s <- sf::st_sf(a = 3, g)
  res <- get_geom_colname(s)
  expect_equal(length(res), expected = 1)
  expect_equal(res, expected = "g")

})

test_that("get_geom_colname works with 2 geom columns", {
  g1 <- sf::st_sfc(sf::st_point(1:2))
  g2 <- sf::st_sfc(sf::st_point(3:4))
  s  <- sf::st_sf(a = 3, g1, g2)
  res <- get_geom_colname(s)
  expect_equal(length(res), expected = 2)
  expect_equal(res, expected = c("g1", "g2"))
})

test_that("add_centroids_coords works", {

  coords <- cbind(c(0, 30, 30, 0, 0), c(0, 0, 3, 3, 0))
  pl <- sf::st_sfc(
    sf::st_polygon(list(coords)),
    sf::st_polygon(list(coords + 2))
  )

  # sfc is not sf!
  expect_error(add_centroids_coords(pl))

  data_df <- data.frame(id = 1:2, g = pl)
  data_sf <- sf::st_sf(data_df)
  cen_sf <- add_centroids_coords(data_sf)

  expect_true(inherits(cen_sf, what = "sf"))
  expect_identical(
    subset(sf::st_drop_geometry(cen_sf), select = -c(X, Y)),
    expected = subset(data_df, select = -geometry)
  )
  expect_true(all(c("X", "Y") %in% colnames(cen_sf)))
  expect_true(all(
    c("myX", "myY") %in%
      colnames(add_centroids_coords(data_sf, col_names = c("myX", "myY")))
  ))
  expect_true(all(
    cen_sf[["X"]] > cen_sf[["Y"]]
  ))

})

test_that("add_area works", {

  coords <- cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
  pl <- sf::st_sfc(
    sf::st_polygon(list(coords))
  )
  data_df <- data.frame(id = 1, g = pl)
  data_sf <- add_area(sf::st_sf(data_df))

  expect_true(inherits(data_sf, what = "sf"))
  expect_equal(data_sf[[".area"]], 1)
  expect_true(".area" %in% colnames(data_sf))
  expect_true("myarea" %in% colnames(add_area(sf::st_sf(data_df),
                                              col_name = "myarea")))
  expect_true(inherits(data_sf[[".area"]], what = "numeric"))

  pl <- sf::st_sfc(
    sf::st_polygon(list(coords)),
    crs = 4326
  )
  data_df <- data.frame(id = 1, g = pl)
  data_sf <- add_area(sf::st_sf(data_df))
  expect_true(inherits(data_sf[[".area"]], what = "units"))

  data_sf <- add_area(sf::st_sf(data_df), drop_units = TRUE)
  expect_true(inherits(data_sf[[".area"]], what = "numeric"))

})



test_that("add_length works", {

  n_lines <- 10
  n_vertex <- sample(2:20, size = n_lines, replace = TRUE)
  g <- sf::st_sfc(
    lapply(n_vertex, function(n) {
      lon <- sample(-18:18, size = n, replace = TRUE) + rnorm(n)
      lat <- sample(-9:9, size = n, replace = TRUE) + rnorm(n)
      line <- sf::st_linestring(cbind(lon, lat))
    }),
    crs = 4326
  )
  g_sf <- sf::st_sf(gid <- 1: n_lines, geomety = g)
  data_sf <- add_length(g_sf)

  expect_true(inherits(data_sf, what = "sf"))
  expect_equal(sum(data_sf[[".length"]]), sum(sf::st_length(data_sf)))
  expect_true(".length" %in% colnames(data_sf))
  expect_true("mylen" %in% colnames(add_length(sf::st_sf(data_sf),
                                               col_name = "mylen")))
  expect_true(inherits(data_sf[[".length"]], what = "units"))

  data_sf <- add_length(g_sf, drop_units = TRUE)
  expect_true(inherits(data_sf[[".length"]], what = "numeric"))

})
