aggregate_geom_categ <- function(categ, data_sf_ls, grid, grid_id, funs) {

    stopifnot("`data_sf_ls` must have names!" = !is.null(names(data_sf_ls)))
    stopifnot("`categ` not found in `data_sf_ls`" = 
        categ %in% names(data_sf_ls)
    )

    data_sf <- data_sf_ls[[categ]]

    grid_agr <- sf::st_agr(grid)
    data_agr <- sf::st_agr(data_sf)
    sf::st_agr(grid) <- "constant"
    sf::st_agr(data_sf) <- "constant"
    dg_inter <- sf::st_intersection(data_sf, grid)
    sf::st_agr(grid) <- grid_agr
    sf::st_agr(data_sf) <- data_agr

    if(any(sf::st_geometry_type(data_sf, by_geometry = FALSE) %in%
        c("POINT", "MULTIPOINT"))) {

        gtype <- sf::st_geometry_type(dg_inter, by_geometry = TRUE) %in% 
            c("POINT", "MULTIPOINT", "GEOMETRYCOLLECTION")
        dg_inter <- dg_inter[gtype, ]
        cnames <- paste0(".n.", categ) 
        dg_inter[cnames] <- 1

    } else if(any(sf::st_geometry_type(data_sf, by_geometry = TRUE) %in% 
        c("LINESTRING", "MULTILINESTRING"))) {

        gtype <- sf::st_geometry_type(dg_inter, by_geometry = TRUE) %in% 
            c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")
        dg_inter <- dg_inter[gtype, ]
        cnames <- paste0(c(".n.", ".length."), categ)
        dg_inter[cnames[1]] <- 1
        dg_inter <- add_length(dg_inter, col_name = cnames[2])

    } else if(any(sf::st_geometry_type(data_sf, by_geometry = TRUE) %in% 
        c("POLYGON", "MULTIPOLYGON"))) {

        gtype <- sf::st_geometry_type(dg_inter, by_geometry = TRUE) %in% 
            c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION")
        dg_inter <- dg_inter[gtype, ]
        cnames <- paste0(c(".n.", ".area."), categ)
        dg_inter[cnames[1]] <- 1
        dg_inter <- add_area(dg_inter, col_name = cnames[2])

    } else {
        stop("Unknown geometry type!")
    }

    # Aggregate spatial attributes.
    dg_df <- sf::st_drop_geometry(dg_inter)

    funs_ls <- lapply(funs, function(f, dg_df, cnames, grid_id) {
        data_df <- stats::aggregate(
            x  = dg_df[cnames],
            by = dg_df[grid_id],
            FUN = f,
            na.rm = TRUE
        )
        names(data_df) <- vapply(names(data_df),
            FUN = function(n, f){
                if (n == grid_id) return(n)
                return(paste0(f, n))
            }, FUN.VALUE = character(1), f = f )
        return(data_df)
    }, dg_df = dg_df, cnames = cnames, grid_id = grid_id)
    names(funs_ls) <- funs

    return(funs_ls)

}

aggregate_geom <- function(x, by, grid, grid_id, funs) {

    # NOTE: This function tries to estimate geometric properties (length, area,
    #       etc.) when x is the result of an intersection and contains 
    #       multi-xx geometries or geometry collections. The idea is to ensure
    #       the properties are estimated including the simple objects inside
    #       the collections. I can't say for sure if st_length & st_area take
    #       into account simple geometries inside collections during 
    #       calculations. One problem with st_cast is that transforms
    #       linestrings into points instead of failing the cast.

    stopifnot("`grid_id` should be a character!" = 
            all(is.character(grid_id), length(grid_id) == 1))
    stopifnot("`by` should be a character!" = 
            all(is.character(by), length(by) ==1 ))
    stopifnot("`funs` must be a character!" = 
              is.character(funs))
    stopifnot("`x` is not an sf object!" = inherits(x, what = "sf"))
    stopifnot("!Invalid sf object!" = sf::st_is_valid(x))
    stopifnot("`grid` is not an sf object!" = inherits(grid, what = "sf"))
    stopifnot("!Invalid grid (sf object)!" = sf::st_is_valid(grid))
    stopifnot("Empty sf object!" = nrow(x) > 0)
    stopifnot("Empty grid!" = nrow(grid) > 0)
    stopifnot("`grid_id` must be a column in grid!" = 
            grid_id %in% colnames(grid))

    # Split data into data frames by `by` attribute.
    data_sf_ls  <- split(x = x[!colnames(x) %in% by], f = x[[by]], drop = TRUE)
    cnames <- character(0)

    categ_ls <- lapply(
        X = names(data_sf_ls),
        FUN = aggregate_geom_categ,
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = grid_id,
        funs = funs
    )

    # Merge inner and then then outer lists.
    m_categ_ls <- lapply(categ_ls, function(categ_df, grid_id){
        return(merge_data_frames(
            data_frames = categ_df,
            by = grid_id
        ))
    }, grid_id = grid_id)
    data_df <- merge_data_frames( data_frames = m_categ_ls, by = grid_id)

    return(
        merge(grid, data_df, by = grid_id, all = TRUE)
    )

}



aggregate_vector <- function(x, by, grid, grid_id, funs, ...) {

    stopifnot("`funs` should be a character!" = all(is.character(funs)))
    stopifnot("`grid_id` should be a character!" = 
            all(is.character(grid_id), length(grid_id) == 1))
    stopifnot("`by` should be a character!" = 
            all(is.character(by), length(by) ==1 ))
    stopifnot("!Invalid sf object!" = sf::st_is_valid(x))
    stopifnot("!Invalid grid (sf object)!" = sf::st_is_valid(grid))
    stopifnot("Empty sf object!" = nrow(x) > 0)
    stopifnot("Empty grid!" = nrow(grid) > 0)
    x_df <- sf::st_drop_geometry(x)
    x_df <- x_df[colnames(x_df) != by]
    stopifnot("Only numeric columns allowed in `x`! (besides `by`)" =
        all( vapply(x_df, is.numeric, logical(1))))
    rm(x_df)

    # Split data into data frames by `by` attribute.
    data_sf_ls  <- split(x = x[!colnames(x) %in% by], f = x[[by]], drop = TRUE)

    # Apply list of functions to list of data frames.
    categ_ls <- lapply(data_sf_ls, function(data_sf, funs, grid) {
        # Spatially aggregate each data frame using function f.
        data_f_ls <- lapply(funs, function(f, data_sf, grid) {
            agg_df <- stats::aggregate(
                x = data_sf,
                by = grid,
                FUN = f,
                # TODO: Remove this comment!
                #... = ...,
                do_union = TRUE,
                simplify = TRUE,
                join = sf::st_intersects
            )
        }, data_sf = data_sf, grid = grid)
        names(data_f_ls) <- funs
        return(data_f_ls)
    }, funs = funs, grid = grid)

    # Remove geometry column.
    # Format column names with function name and category.
    data_ls <- lapply(names(categ_ls), function(categ, categ_ls) {
        fun_ls <- categ_ls[[categ]]
        lapply(names(fun_ls), function(fun, fun_ls, categ) {
            data_df <- sf::st_drop_geometry(fun_ls[[fun]])
            colnames(data_df) <- paste(fun, 
                colnames(sf::st_drop_geometry(data_df)), categ, sep = ".")
            return(data_df)
        }, fun_ls = fun_ls, categ = categ)
    }, categ_ls = categ_ls)

    # Unnest data frames.
    data_ls <- lapply(data_ls, function(data_df) {
        do.call("cbind", data_df)
    })
    data_df <- do.call("cbind", data_ls)
    data_df <- data_df[sort(colnames(data_df))]

    # Aggregate spatial properties.
    geom_sf <- aggregate_geom(
        x = x,
        by = by,
        grid = grid,
        grid_id = grid_id,
        funs = funs
    )

    # Column bind spatial grid to aggregated data.
    grid <- cbind(geom_sf, data_df)

    return(grid)

}

