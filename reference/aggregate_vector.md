# Aggregate vector data

Aggregate vector data (points, lines, polygons) and their attributes
using simple statistics, a grid, and the data categories.

## Usage

``` r
aggregate_vector(x, by, grid, grid_id, funs, ...)
```

## Arguments

- x:

  an sf object.

- by:

  a character(1). A column name in `x` with a category for each row.

- grid:

  and sf object (polygon). A grid used to aggregate `x`.

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
