# Build a grid that can be used later for aggregating geographic data

Build a grid around spatial data (vector or raster) or from scratch.

`make_grid` returns a grid around spatial data (vector or raster).

`make_grid_min_max_cells` returns a grid using the minimum & maximum
coordinates, and the number of cells in the grid.

`make_grid_origin_dist` returns a grid using the coordinates of an
origin point (minimum XY), the number of cells, and the number of cells
in the grid.

`make_grid_origin_res` returns a grid using the coordinates of an origin
point inside an area and a cell size. The new grid will have a vertex on
the origin and will fit as many cells as possible in the area.

## Usage

``` r
make_grid(
  x,
  n,
  cellsize = c(diff(sf::st_bbox(x)[c(1, 3)]), diff(sf::st_bbox(x)[c(2, 4)]))/n,
  id_col = "id",
  add_row_col = FALSE,
  add_centroids = FALSE,
  add_area = FALSE,
  drop_area_units = FALSE
)

make_grid_min_max_cells(
  xy_min,
  xy_max,
  n,
  crs = 4326,
  id_col = "id",
  add_row_col = FALSE,
  add_centroids = FALSE,
  add_area = FALSE,
  drop_area_units = FALSE
)

make_grid_origin_dist(
  xy_origin,
  n,
  cell_size,
  crs,
  id_col = "id",
  add_row_col = FALSE,
  add_centroids = FALSE,
  add_area = FALSE,
  drop_area_units = FALSE
)

make_grid_origin_res(
  xy_origin,
  xy_min,
  xy_max,
  cell_size,
  crs = 4326,
  id_col = "id"
)
```

## Arguments

- x:

  An sf or terra object.

- n:

  integer. Number of cells (columns, rows).

- cellsize:

  numeric of length 1 or 2 (width, length). It defaults to the width and
  heigth of the bounding box of the given object divided by the number
  of cells.

- id_col:

  character of length 1. Name of the column for identifying grid cells.

- add_row_col:

  Either logical(1) or character(2). Should columns identifying the row
  and column of each cell be added? If so, provide their names;
  otherwise default names are used

- add_centroids:

  Either logical(1) or character(2). Should columns with the coordinates
  of the cells' centroids be added? If so, provide their names (X & Y);
  otherwise default names are used.

- add_area:

  Either logical(1) or character(1). Should a column with the cell's
  area be added? If so, provide its name; otherwise a default name is
  used.

- drop_area_units:

  A logical. Drop the area units?

- xy_min, xy_max:

  A pair of numeric vectors with the minimum and maximum coordiantes of
  the grid.

- crs:

  Numeric or character representing a Coordinate Reference System.

- xy_origin:

  A numeric(2) representing an XY point.

- cell_size:

  A numeric of length 1 or 2. The width and heigth o a cell. The units
  are those of the `crs`.

## Value

An `sf` object with polygons.

## See also

[`sf::st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.html)
which this function wraps.

## Examples

``` r
# Make a 5x5 grid covering a polygon.
pol <- sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 3, 3, 0, 0),
    c(0, 0, 3, 3, 0)
  ))),
  crs = 4326
)
make_grid(pol, cellsize = 1, n = 5)
#> Simple feature collection with 25 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 5 ymax: 5
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    id                       geometry
#> 1   1 POLYGON ((0 0, 1 0, 1 1, 0 ...
#> 2   2 POLYGON ((1 0, 2 0, 2 1, 1 ...
#> 3   3 POLYGON ((2 0, 3 0, 3 1, 2 ...
#> 4   4 POLYGON ((3 0, 4 0, 4 1, 3 ...
#> 5   5 POLYGON ((4 0, 5 0, 5 1, 4 ...
#> 6   6 POLYGON ((0 1, 1 1, 1 2, 0 ...
#> 7   7 POLYGON ((1 1, 2 1, 2 2, 1 ...
#> 8   8 POLYGON ((2 1, 3 1, 3 2, 2 ...
#> 9   9 POLYGON ((3 1, 4 1, 4 2, 3 ...
#> 10 10 POLYGON ((4 1, 5 1, 5 2, 4 ...


# Make a 5x10 grid covering a raster.
r <- terra::rast(matrix(1:25, nrow = 5, ncol = 5), crs = "EPSG:4326")
make_grid(r, cellsize = 1, n = c(10, 5))
#> Simple feature collection with 50 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10 ymax: 5
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    id                       geometry
#> 1   1 POLYGON ((0 0, 1 0, 1 1, 0 ...
#> 2   2 POLYGON ((1 0, 2 0, 2 1, 1 ...
#> 3   3 POLYGON ((2 0, 3 0, 3 1, 2 ...
#> 4   4 POLYGON ((3 0, 4 0, 4 1, 3 ...
#> 5   5 POLYGON ((4 0, 5 0, 5 1, 4 ...
#> 6   6 POLYGON ((5 0, 6 0, 6 1, 5 ...
#> 7   7 POLYGON ((6 0, 7 0, 7 1, 6 ...
#> 8   8 POLYGON ((7 0, 8 0, 8 1, 7 ...
#> 9   9 POLYGON ((8 0, 9 0, 9 1, 8 ...
#> 10 10 POLYGON ((9 0, 10 0, 10 1, ...


# Make a grid using mininum and maximum coordinates and the number of grid
# cells.
make_grid_min_max_cells(
  xy_min = c(-180, -90),
  xy_max = c(180, 90),
  n = c(14, 7),
  crs = 4326
)
#> Simple feature collection with 98 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -180 ymin: -90 xmax: 180 ymax: 90
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    id                       geometry
#> 1   1 POLYGON ((-180 -90, -154.28...
#> 2   2 POLYGON ((-154.2857 -90, -1...
#> 3   3 POLYGON ((-128.5714 -90, -1...
#> 4   4 POLYGON ((-102.8571 -90, -7...
#> 5   5 POLYGON ((-77.14286 -90, -5...
#> 6   6 POLYGON ((-51.42857 -90, -2...
#> 7   7 POLYGON ((-25.71429 -90, 0 ...
#> 8   8 POLYGON ((0 -90, 25.71429 -...
#> 9   9 POLYGON ((25.71429 -90, 51....
#> 10 10 POLYGON ((51.42857 -90, 77....


# Make a rectangular grid using square cells of 25 kilometers.
make_grid_origin_dist(
  xy_origin = c(-74, 4),
  n = c(14, 7),
  cell_size = 25000,
  crs = 3116
)
#> Simple feature collection with 98 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -74 ymin: 4 xmax: 349926 ymax: 175004
#> Projected CRS: MAGNA-SIRGAS / Colombia Bogota zone
#> First 10 features:
#>    id                       geometry
#> 1   1 POLYGON ((-74 4, 24926 4, 2...
#> 2   2 POLYGON ((24926 4, 49926 4,...
#> 3   3 POLYGON ((49926 4, 74926 4,...
#> 4   4 POLYGON ((74926 4, 99926 4,...
#> 5   5 POLYGON ((99926 4, 124926 4...
#> 6   6 POLYGON ((124926 4, 149926 ...
#> 7   7 POLYGON ((149926 4, 174926 ...
#> 8   8 POLYGON ((174926 4, 199926 ...
#> 9   9 POLYGON ((199926 4, 224926 ...
#> 10 10 POLYGON ((224926 4, 249926 ...


# Make a rectangular grid using square cells of 25 kilometers.
make_grid_origin_res(
  xy_origin = c(-74.2, 4.4),
  xy_min = c(-78, -3),
  xy_max = c(-70, 5),
  cell_size = 0.5
)
#> Simple feature collection with 225 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -77.7 ymin: -2.6 xmax: -70.2 ymax: 4.9
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                                 x id
#> 1  POLYGON ((-77.7 -2.6, -77.2...  1
#> 2  POLYGON ((-77.2 -2.6, -76.7...  2
#> 3  POLYGON ((-76.7 -2.6, -76.2...  3
#> 4  POLYGON ((-76.2 -2.6, -75.7...  4
#> 5  POLYGON ((-75.7 -2.6, -75.2...  5
#> 6  POLYGON ((-75.2 -2.6, -74.7...  6
#> 7  POLYGON ((-74.7 -2.6, -74.2...  7
#> 8  POLYGON ((-74.2 -2.6, -73.7...  8
#> 9  POLYGON ((-73.7 -2.6, -73.2...  9
#> 10 POLYGON ((-73.2 -2.6, -72.7... 10
```
