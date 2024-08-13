#' Get the geometry column name
#'
#' @description
#' Get the names of the geometry columns in an sf object.
#'
#' @param x An sf object.
#'
#' @return  A character. 
#'
get_geom_colname <- function(x){
    stopifnot("An sf object was expected!" = 
        inherits(x, what = "sf"))
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
add_centroids <- function(x, col_names = c("X", "Y")) {

    stopifnot("An sf object was expected!" = 
        inherits(x, what = "sf"))
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

    stopifnot("An sf object was expected!" = 
        inherits(x, what = "sf"))
    stopifnot(all(
        is.character(col_name),
        length(col_name) == 1
    ))

    area_df <- sf::st_area(x)
    if (drop_units)
        area_df <- units::drop_units(area_df)
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

    stopifnot("An sf object was expected!" = 
        inherits(x, what = "sf"))
    stopifnot(all(
        is.character(col_name),
        length(col_name) == 1
    ))

    len_df <- sf::st_length(x)
    if (drop_units)
        len_df <- units::drop_units(len_df)
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

    if (all(is.na(geom_types))) {
        if(sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
           c("POINT", "MULTIPOINT")) {
            geom_types <- c("POINT", "MULTIPOINT")
        } else if(sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
                  c("LINESTRING", "MULTILINESTRING")) {
            geom_types <- c("LINESTRING", "MULTILINESTRING")
        } else if(sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
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
    #TODO: How do I open a geometry collection?
    for (geom_type in rev(geom_types)) {
        dg_sf <- sf::st_cast(
            dg_sf, 
            to = geom_type, 
            do_split = TRUE,
            warn = FALSE)
        gtype <- sf::st_geometry_type(dg_sf) == geom_type
        dg_sf <- dg_sf[gtype, ]
    }

    return(dg_sf)

}

