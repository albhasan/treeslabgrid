#' Aggregate raster data
#'
#' Aggregate raster data using simple statistics and a grid (vector).
#'
#' @param x a terra object.
#' @param by ignored parameter.
#' @param grid A grid (polygons) used to aggregate `x`.
#' @param grid_id a character(1). A column name in `grid` with an unique
#'   identifier for each row.
#' @param funs a character. Functions names used to aggregate the data in `x`
#'  corresponding to each cell in `grid`.
#' @param ... additional parameters passed to funs.
#'
#' @return `grid` with additional columns.
#'
#' @export
#'
aggregate_raster <- function(x, by, grid, grid_id, funs, ...) {
  # Check inputs.
  if (inherits(grid, what = "sf")) {
    grid_sv <- terra::vect(grid[grid_id])
  } else if (inherits(grid, what = "SpatVector")) {
    grid_sv <- grid[grid_id]
  } else {
    stop("The grid must be either an sf or SpatVector object.")
  }

  # Aggregate the given raster using the grid and aggregation functions.
  funs_ls <- lapply(funs,
    function(f, x, grid_sv) {
      terra::extract(
        x = x,
        y = grid_sv,
        fun = f,
        # NOTE: Comment when debugging.
        # ... = ...,
        ID = TRUE
      )
    },
    x = x, grid_sv = grid_sv
  )

  # Build new names for the columns
  new_names <- lapply(seq(funs_ls), function(i, funs_ls, funs) {
    colnames(funs_ls[[i]]) <-
      paste(colnames(funs_ls[[i]][-1]), funs[i], sep = "_")
  }, funs_ls = funs_ls, funs = funs)

  # Create a data frame for holding aggregation IDs.
  id_df <- funs_ls[[1]]["ID"]

  funs_ls <- lapply(seq(funs_ls), function(i, funs_ls, new_names) {
    # Remove the ID before binding columns.
    y <- funs_ls[[i]][-1]
    # Update the column names on each data frame.
    names(y) <- new_names[[i]]
    return(y)
  }, funs_ls = funs_ls, new_names = new_names)

  # Bind the aggregation data frames into one.
  res <- do.call(what = cbind, args = funs_ls)
  # Add the ID to the resulting data frame.
  res <- cbind(id_df, res)

  # Bind the grid to the results.
  if (inherits(grid, what = "sf")) {
    grid_res <- merge(
      x = grid,
      y = res,
      by.x = grid_id,
      by.y = "ID"
    )
  } else if (inherits(grid, what = "SpatVector")) {
    grid_res <- terra::merge(
      x = grid_sv,
      y = res,
      by.x = grid_id,
      by.y = "ID"
    )
  } else {
    stop("Unknown object type!")
  }

  return(grid_res)
}

# TODO: Add fast point aggregation from:
# /home/alber/Documents/github/seasonmetrics/R/fast_aggregate.R
