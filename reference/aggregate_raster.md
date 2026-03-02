# Aggregate raster data

Aggregate raster data using simple statistics and a grid (vector).

## Usage

``` r
aggregate_raster(x, by, grid, grid_id, funs, ...)
```

## Arguments

- x:

  a terra object.

- by:

  a character(1). A column name in `x`.

- grid:

  A grid (polygons) used to aggregate `x`.

- grid_id:

  a character(1). A column name in `grid` with an unique identifier for
  each row.

- funs:

  a character. Functions names used to aggregate the data in `x`
  corresponding to each cell in `grid`.

- ...:

  additional parameters passed to funs.

## Value

`grid` (sf) with additional columns.
