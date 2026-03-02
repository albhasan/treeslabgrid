# Aggregate geometries by category

Helper function called from `aggregate_geom`.

## Usage

``` r
aggregate_geom_categ(categ, data_sf_ls, grid, grid_id, funs, ...)
```

## Arguments

- categ:

  a character(1). Name of a category in the elements of `data_sf_ls`.

- data_sf_ls:

  a list of sf objects. The data in this list is meant to be aggregated
  using the given `grid`.

- grid:

  an sf object (polygons). Grid used to aggregate data.

- grid_id:

  a character(1). Name of a column in `grid` that identifies each cell
  in `grid`.

- funs:

  a character. Name of aggregation functions.

- ...:

  parameters passed to funs.

## Value

a list of data frames with the results of the aggrgation of the spatial
attributes of the data in `data_sf_ls` using `grid`.
