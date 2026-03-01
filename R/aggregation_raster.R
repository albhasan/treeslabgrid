#' Aggregate raster data
#'
#' Aggregate raster data using simple statistics and a grid (vector).
#'
#' @param x a terra object.
#' @param by a character(1). A column name in `x`.
#' @param grid A grid (polygons) used to aggregate `x`.
#' @param grid_id a character(1). A column name in `grid` with an unique
#'   identifier for each row.
#' @param funs a character. Functions names used to aggregate the data in `x`
#'  corresponding to each cell in `grid`.
#' @param ... additional parameters passed to funs.
#'
#' @return `grid` (sf) with additional columns.
#'
#' @export
#'
aggregate_raster <- function(x, by, grid, grid_id, funs, ...) {
  # Cast the raster to points.
  x_df <- as.data.frame(
    x = x,
    xy = TRUE
  )
  x_sf <- sf::st_as_sf(
    x = x_df,
    coords = c("x", "y"),
    crs = terra::crs(x)
  )
  # Aggregate the points.
  x_sf["dummy_raster_by"] <- 1
  res <- aggregate_vector(
    x = x_sf,
    by = "dummy_raster_by",
    grid = grid,
    grid_id = grid_id,
    funs = funs,
    ...
  )
  return(res)
}

# TODO: Add fast point aggregation from:
# /home/alber/Documents/github/seasonmetrics/R/fast_aggregate.R
