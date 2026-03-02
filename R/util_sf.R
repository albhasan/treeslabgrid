#' Get the geometry column name
#'
#' @description
#' Get the names of the geometry columns in an sf object.
#'
#' @param x An sf object.
#'
#' @return  A character.
#'
#' @export
#'
get_geom_colname <- function(x) {
  stopifnot(
    "An sf object was expected!" =
      inherits(x, what = "sf")
  )
  return(names(which(sapply(x, inherits, what = "sfc"))))
}

#' Add centroid coordinates
#'
#' @description
#' Add columns with the coordinates of its centroids to the given object.
#'
#' @param x an sf object.
#' @param col_names a character (length 2). Names for the new XY columns.
#'
#' @return the given sf object with additional columns.
#'
add_centroids_coords <- function(x, col_names = c("X", "Y")) {
  stopifnot(
    "An sf object was expected!" =
      inherits(x, what = "sf")
  )
  stopifnot(all(
    is.character(col_names),
    length(col_names) == 2
  ))

  suppressWarnings(cen_sf <- sf::st_centroid(x))
  coor_df <- data.frame(sf::st_coordinates(cen_sf))
  colnames(coor_df) <- col_names
  return(cbind(x, coor_df))
}


#' Add area
#'
#' @description
#' Add a column with the area to the given object.
#'
#' @param x an sf object.
#' @param col_name a character (length 1). A name for the new column.
#' @param drop_units a logical. Should the area units be droppped?
#'
#' @return the given sf object with an additional column.
#'
add_area <- function(x, col_name = ".area", drop_units = FALSE) {
  stopifnot(
    "An sf object was expected!" =
      inherits(x, what = "sf")
  )
  stopifnot(all(
    is.character(col_name),
    length(col_name) == 1
  ))

  area_df <- sf::st_area(x)
  if (drop_units) {
    area_df <- units::drop_units(area_df)
  }
  area_df <- data.frame(area_df)
  colnames(area_df) <- col_name

  return(cbind(x, area_df))
}


#' Add length
#'
#' @description
#' Add a column with lengths to the given object.
#'
#' @param x an sf object.
#' @param col_name a character (length 1). A name for the new column.
#' @param drop_units a logical. Should the area units be droppped?
#'
#' @return the given sf object with an additional column.
#'
add_length <- function(x, col_name = ".length", drop_units = FALSE) {
  stopifnot(
    "An sf object was expected!" =
      inherits(x, what = "sf")
  )
  stopifnot(all(
    is.character(col_name),
    length(col_name) == 1
  ))

  len_df <- sf::st_length(x)
  if (drop_units) {
    len_df <- units::drop_units(len_df)
  }
  len_df <- data.frame(len_df)
  colnames(len_df) <- col_name

  return(cbind(x, len_df))
}


#' Simplify geometries
#'
#' @description
#' Convert geometries to simple parts. The geometries which can't be simplified
#' are discarded. This is useful when dealing with data resulting from spatial
#' operations such as intersection.
#'
#' @param data_sf an sf object.
#' @param geom_types a character. Force specific geometry types. The first
#'   element must be the name of a simple geometry (e.g. c("POINT",
#'  "MULTIPOINT")).
#'
#' @return an sf object with simple geometries (i.e. POINT, LINESTRING,
#'   POLYGON).
#'
simplify_geoms <- function(data_sf, geom_types = NA) {
  .Deprecated("sf::st_geometry_extract")

  if (all(is.na(geom_types))) {
    if (sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
      c("POINT", "MULTIPOINT")) {
      geom_types <- c("POINT", "MULTIPOINT")
    } else if (sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
      c("LINESTRING", "MULTILINESTRING")) {
      geom_types <- c("LINESTRING", "MULTILINESTRING")
    } else if (sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
      c("POLYGON", "MULTIPOLYGON")) {
      geom_types <- c("POLYGON", "MULTIPOLYGON")
    } else {
      stop("Unknown geometry type!")
    }
  }

  # Filter.
  gtype <- sf::st_geometry_type(data_sf, by_geometry = TRUE) %in%
    c(geom_types, "GEOMETRYCOLLECTION")
  dg_sf <- data_sf[gtype, ]

  # Split and filter multi-X geometries.
  for (g_type in rev(geom_types)) {
    dg_sf <- sf::st_cast(
      dg_sf,
      to = g_type,
      do_split = TRUE,
      warn = FALSE
    )
    gtype <- sf::st_geometry_type(dg_sf) == g_type
    dg_sf <- dg_sf[gtype, ]
  }

  return(dg_sf)
}


#' Get center coordinates
#'
#' @description
#' Get the coordinates of the center of the extent of the given  object (sf or
#' terra).
#'
#' @param x an sf or terra object.
#'
#' @return a named vector.
#'
#' @export
#'
get_center <- function(x) {
  # Helper. Estimate center coordinates of sf's extent object.
  get_bbox_center <- function(bb) {
    return(c(
      x = as.vector(bb["xmin"] + ((bb["xmax"] - bb["xmin"]) / 2)),
      y = as.vector(bb["ymin"] + ((bb["ymax"] - bb["ymin"]) / 2))
    ))
  }

  # Helper. Estimate center coordinates of terra's extent object.
  get_spat_extent_center <- function(se) {
    se <- as.vector(se)
    return(c(
      x = as.vector(se["xmin"] + ((se["xmax"] - se["xmin"]) / 2)),
      y = as.vector(se["ymin"] + ((se["ymax"] - se["ymin"]) / 2))
    ))
  }

  if (inherits(x = x, what = "sf")) {
    bb <- sf::st_bbox(x)
    return(get_bbox_center(bb))
  } else if (inherits(x = x, what = "bbox")) {
    return(get_bbox_center(x))
  } else if (inherits(x = x, what = "SpatRaster")) {
    bb <- terra::ext(x)
    return(get_spat_extent_center(bb))
  } else if (inherits(x = x, what = "SpatExtent")) {
    return(get_spat_extent_center(x))
  }
  stop("Unsupported data type!")
}


#' Get minimum coordinates
#'
#' @description
#' Get the mininum coordinates of the extent of the given  object (sf or
#' terra).
#'
#' @param x an sf or terra object.
#'
#' @return a named vector.
#'
#' @export
#'
get_min <- function(x) {
  # Helper. Estimate minimum coordinates of sf's extent object.
  get_bbox_min <- function(bb) {
    return(c(
      xmin = as.vector(bb["xmin"]),
      ymin = as.vector(bb["ymin"])
    ))
  }

  # Helper. Estimate minimum coordinates of terra's extent object.
  get_spat_extent_min <- function(se) {
    se <- as.vector(se)
    return(c(
      xmin = as.vector(se["xmin"]),
      ymin = as.vector(se["ymin"])
    ))
  }

  if (inherits(x = x, what = "sf")) {
    bb <- sf::st_bbox(x)
    return(get_bbox_min(bb))
  } else if (inherits(x = x, what = "bbox")) {
    return(get_bbox_min(x))
  } else if (inherits(x = x, what = "SpatRaster")) {
    bb <- terra::ext(x)
    return(get_spat_extent_min(bb))
  } else if (inherits(x = x, what = "SpatExtent")) {
    return(get_spat_extent_min(x))
  }
  stop("Unsupported data type!")
}


#' Get maximum coordinates
#'
#' @description
#' Get the maximum coordinates of the extent of the given  object (sf or
#' terra).
#'
#' @param x an sf or terra object.
#'
#' @return a named vector.
#'
#' @export
#'
get_max <- function(x) {
  # Helper. Estimate maximum coordinates of sf's extent object.
  get_bbox_max <- function(bb) {
    return(c(
      xmax = as.vector(bb["xmax"]),
      ymax = as.vector(bb["ymax"])
    ))
  }

  # Helper. Estimate maximum coordinates of terra's extent object.
  get_spat_extent_max <- function(se) {
    se <- as.vector(se)
    return(c(
      xmax = as.vector(se["xmax"]),
      ymax = as.vector(se["ymax"])
    ))
  }

  if (inherits(x = x, what = "sf")) {
    bb <- sf::st_bbox(x)
    return(get_bbox_max(bb))
  } else if (inherits(x = x, what = "bbox")) {
    return(get_bbox_max(x))
  } else if (inherits(x = x, what = "SpatRaster")) {
    bb <- terra::ext(x)
    return(get_spat_extent_max(bb))
  } else if (inherits(x = x, what = "SpatExtent")) {
    return(get_spat_extent_max(x))
  }
  stop("Unsupported data type!")
}
