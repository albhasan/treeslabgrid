# Simplify geometries

Convert geometries to simple parts. The geometries which can't be
simplified are discarded. This is useful when dealing with data
resulting from spatial operations such as intersection.

## Usage

``` r
simplify_geoms(data_sf, geom_types = NA)
```

## Arguments

- data_sf:

  an sf object.

- geom_types:

  a character. Force specific geometry types. The first element must be
  the name of a simple geometry (e.g. c("POINT", "MULTIPOINT")).

## Value

an sf object with simple geometries (i.e. POINT, LINESTRING, POLYGON).
