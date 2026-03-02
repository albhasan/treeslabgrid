test_that("aggregate_raster works", {
  r <- terra::rast(
    nrows = 40,
    ncols = 20,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  terra::values(r) <- runif(terra::ncell(r))
  terra::crs(r) <- "EPSG:4326"
  # NOTE: Avoid warning regarding non-square cells in grid.
  suppressWarnings(
    grid <- make_grid(
      x = r,
      n = c(17, 11)
    )
  )

  # Aggregate.
  res <- aggregate_raster(
    x = r,
    by = NA_character_, # NOTE: Use one category for the whole dataset.
    grid = grid,
    grid_id = "id",
    funs = "sum",
    na.rm = TRUE
  )

  # The number of aggregated elements must match the number of raster cells.
  expect_equal(
    object = sum(res[["sum.n.1"]]),
    expected = prod(dim(r))
  )

  # The variable sum must match between the aggregation and the raster.
  expect_equal(
    object = sum(res[["sum.lyr.1.1"]]),
    expected = sum(r[])
  )
})

test_that("aggregate_raster works with sf & sv inputs", {
  # Extent.
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
  grid_sv <- terra::vect(grid_sf)

  r <- terra::rasterize(
    x = grid_sv,
    y = terra::rast(),
    field = "var1"
  )

  # Test aggregate_raster using an sf grid.
  ragg_sf <- aggregate_raster(
    x = r,
    by = NA_character_,
    grid = grid_sf,
    grid_id = "grid_id",
    funs = c("sum", "mean", "min", "max", "sd"),
    na.rm = TRUE
  )

  testthat::expect_true(inherits(ragg_sf, what = "sf"))
  testthat::expect_equal(
    object = sum(ragg_sf[["sum.var1.1"]]),
    expected = sum(grid_sf[["var1"]])
  )

  # Test aggregate_raster using a terra grid.
  ragg_sf <- aggregate_raster(
    x = r,
    by = NA_character_,
    grid = grid_sv,
    grid_id = "grid_id",
    funs = c("sum", "mean", "min", "max", "sd"),
    na.rm = TRUE
  )

  testthat::expect_true(inherits(ragg_sf, what = "sf"))
  testthat::expect_equal(
    object = sum(ragg_sf[["sum.var1.1"]]),
    expected = sum(grid_sf[["var1"]])
  )
})
