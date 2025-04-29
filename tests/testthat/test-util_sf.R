test_that("get_geom_colname works", {
    g <- sf::st_sfc(sf::st_point(1:2))

    # sfc is not sf!
    expect_error(get_geom_colname(g))

    s <- sf::st_sf(a = 3, g)
    res <- get_geom_colname(s)
    expect_equal(length(res), expected = 1)
    expect_equal(res, expected = "g")

})

test_that("get_geom_colname works with 2 geom columns", {
    g1 <- sf::st_sfc(sf::st_point(1:2))
    g2 <- sf::st_sfc(sf::st_point(3:4))
    s  <- sf::st_sf(a = 3, g1, g2)
    res <- get_geom_colname(s)
    expect_equal(length(res), expected = 2)
    expect_equal(res, expected = c("g1", "g2"))
})

test_that("add_centroids works", {

    coords <- cbind(c(0,30,30,0,0),c(0,0,3,3, 0))
    pl <- sf::st_sfc(
        sf::st_polygon(list(coords)),
        sf::st_polygon(list(coords + 2))
    )

    # sfc is not sf!
    expect_error(add_centroids(pl))

    data_df <- data.frame(id = 1:2, g = pl)
    data_sf <- sf::st_sf(data_df)
    cen_sf <- add_centroids(data_sf)

    expect_true(inherits(cen_sf, what = "sf"))
    expect_identical(
        subset(sf::st_drop_geometry(cen_sf), select = -c(X, Y)),
        expected = subset(data_df, select = -geometry)
    )
    expect_true(all(c("X", "Y") %in% colnames(cen_sf)))
    expect_true(all(
        c("myX", "myY") %in%
            colnames(add_centroids(data_sf, col_names = c("myX", "myY")))
    ))
    expect_true(all(
        cen_sf[["X"]] > cen_sf[["Y"]]
    ))

})

test_that("add_area works", {

    coords <- cbind(c(0,1,1,0,0),c(0,0,1,1, 0))
    pl <- sf::st_sfc(
        sf::st_polygon(list(coords))
    )
    data_df <- data.frame(id = 1, g = pl)
    data_sf <- add_area(sf::st_sf(data_df))

    expect_true(inherits(data_sf, what = "sf"))
    expect_equal(data_sf[[".area"]], 1)
    expect_true(".area" %in% colnames(data_sf))
    expect_true("myarea" %in% colnames(add_area(sf::st_sf(data_df),
                                                col_name= "myarea")))
    expect_true(inherits(data_sf[[".area"]], what = "numeric"))

    pl <- sf::st_sfc(
        sf::st_polygon(list(coords)), 
        crs = 4326
    )
    data_df <- data.frame(id = 1, g = pl)
    data_sf <- add_area(sf::st_sf(data_df))
    expect_true(inherits(data_sf[[".area"]], what = "units"))

    data_sf <- add_area(sf::st_sf(data_df), drop_units = TRUE)
    expect_true(inherits(data_sf[[".area"]], what = "numeric"))

})



test_that("add_length works", {

    n_lines <- 10
    n_vertex <- sample(2:20, size = n_lines, replace = TRUE)
    g <- sf::st_sfc(
        lapply(n_vertex, function(n) {
            lon <- sample(-18:18, size = n, replace = TRUE) + rnorm(n)
            lat <- sample(-9:9, size = n, replace = TRUE) + rnorm(n)
            line <- sf::st_linestring(cbind(lon, lat))
        }),
        crs = 4326
    )
    g_sf <- sf::st_sf(gid <- 1: n_lines, geomety = g)
    data_sf <- add_length(g_sf)

    expect_true(inherits(data_sf, what = "sf"))
    expect_equal(sum(data_sf[[".length"]]), sum(sf::st_length(data_sf)))
    expect_true(".length" %in% colnames(data_sf))
    expect_true("mylen" %in% colnames(add_length(sf::st_sf(data_sf),
                                                 col_name = "mylen")))
    expect_true(inherits(data_sf[[".length"]], what = "units"))

    data_sf <- add_length(g_sf, drop_units = TRUE)
    expect_true(inherits(data_sf[[".length"]], what = "numeric"))

})



test_that("simplify_geoms works", {

    n_points <- 10
    lon <- sample(-18:18, size = n_points, replace = TRUE) + rnorm(n_points)
    lat <- sample(-9:9, size = n_points, replace = TRUE) + rnorm(n_points)
    points_ls <- lapply(seq(n_points), function(n) {
        sf::st_point(cbind(lon[n], lat[n]), dim = "XY")
    })

    n_lines <- 10
    n_vertex <- sample(2:20, size = n_lines, replace = TRUE)
    lines_ls <- lapply(n_vertex, function(n) {
        lon <- sample(-18:18, size = n, replace = TRUE) + rnorm(n)
        lat <- sample(-9:9, size = n, replace = TRUE) + rnorm(n)
        return(sf::st_linestring(cbind(lon, lat)))
    })

    n_polygons <- 10
    n_vertex <- sample(3:30, size = n_polygons, replace = TRUE)
    polygons_ls <- lapply(n_vertex, function(n) {
        lon <- sample(-18:18, size = n, replace = TRUE) + rnorm(n)
        lat <- sample(-9:9, size = n, replace = TRUE) + rnorm(n)
        lon <- append(lon, lon[1])
        lat <- append(lat, lat[1])
        sf::st_convex_hull(sf::st_polygon(list(cbind(lon, lat))))
    })

    simple_g <- sf::st_as_sf(
        simplify_geoms(sf::st_combine(sf::st_sfc(points_ls, crs = 4326)))
        )
    expt <- sf::st_as_sf(sf::st_sfc(points_ls, crs = 4326))
    expect_equal(dim(simple_g), expected = dim(expt))
    expect_true(all(diag(sf::st_equals(simple_g, expt, sparse = FALSE))))

    simple_g <- sf::st_as_sf(
        simplify_geoms(sf::st_combine(sf::st_sfc(lines_ls, crs = 4326)))
        )
    expt <- sf::st_as_sf(sf::st_sfc(lines_ls, crs = 4326))
    expect_equal(dim(simple_g), expected = dim(expt))
    expect_true(all(diag(sf::st_equals(simple_g, expt, sparse = FALSE))))

    simple_g <- sf::st_as_sf(
        simplify_geoms(sf::st_combine(sf::st_sfc(polygons_ls, crs = 4326)))
        )
    expt <- sf::st_as_sf(sf::st_sfc(polygons_ls, crs = 4326))
    expect_equal(dim(simple_g), expected = dim(expt))
    expect_true(all(diag(sf::st_equals(simple_g, expt, sparse = FALSE))))

    pl   <- sf::st_sf(sf::st_combine(sf::st_sfc(points_ls[[1]], 
        lines_ls[[1]])))
    ppol <- sf::st_sf(sf::st_combine(sf::st_sfc(points_ls[[1]], 
        polygons_ls[[1]])))
    lpol <- sf::st_sf(sf::st_combine(sf::st_sfc(lines_ls[[1]], 
        polygons_ls[[1]])))

     #TODO: 1 point, 1 lines becones 13 points
    simplify_geoms(pl, geom_types = c("POINT", "MULTIPOINT"))

})

