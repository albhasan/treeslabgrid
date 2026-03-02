# Aggregate vector data using a grid

This function estimates the geometric properties (number, length, and
area) resulting from the intersection of the given vector data and a
grid.

## Usage

``` r
aggregate_geom(x, by, grid, grid_id, funs, ...)
```

## Arguments

- x:

  an sf object.

- by:

  a character(1). A column name in `x`.

- grid:

  an sf object (polygon). A grid used for aggregating `x`.

- grid_id:

  a character(1). A column name in `grid` with identifiers for each cell
  in `grid`.

- funs:

  a character. Name of funcions for aggregating the data in `x` that
  intersects with each cell in `grid`.

- ...:

  additional parameters of `funs`.

## Value

`grid` with additional attributes.
