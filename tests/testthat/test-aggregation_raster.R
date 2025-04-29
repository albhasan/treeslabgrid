test_that("aggregate_raster works with sf & sv", {
  x_min <- -10
  y_min <- -5
  x_max <- 10
  y_max <- 5

  # create a grid.
  grid_sf <- make_grid_min_max_cells(
    id_col = "grid_id",
    xy_min = c(x_min, y_min),
    xy_max = c(x_max, y_max),
    n = c(20, 10),
    crs = 4326
  )
  grid_sf["var1"] <- grid_sf[["grid_id"]]
  exp_val <- sum(grid_sf[["var1"]])
  grid_sv <- terra::vect(grid_sf)

  r <- terra::rasterize(
    x = grid_sv,
    y = terra::rast(),
    field = "var1"
  )

  # NOTE: Category does nothing durign the aggregation.

  # Test aggregate_raster using an sf grid.
  ragg_sf <- aggregate_raster(
    x = r,
    by = NA,
    grid = grid_sf,
    grid_id = "grid_id",
    funs = c("sum", "mean", "min", "max", "sd"),
    rm.na = TRUE
  )
  testthat::expect_true(inherits(ragg_sf, what = "sf"))
  c_sum <- colSums(sf::st_drop_geometry(ragg_sf))
  c_sum <- c_sum[!is.na(c_sum)]
  testthat::expect_true(all(c_sum == exp_val))

  # Test aggregate_raster using a terra grid.
  ragg_sv <- aggregate_raster(
    x = r,
    by = NA,
    grid = grid_sv,
    grid_id = "grid_id",
    funs = c("sum", "mean", "min", "max", "sd"),
    rm.na = TRUE
  )
  testthat::expect_true(inherits(ragg_sv, what = "SpatVector"))
  c_sum <- colSums(as.data.frame(ragg_sv))
  c_sum <- c_sum[!is.na(c_sum)]
  testthat::expect_true(all(c_sum == exp_val))
})
