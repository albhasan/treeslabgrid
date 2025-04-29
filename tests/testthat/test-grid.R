test_that("make_grid works with sf objects", {

     pl <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 3, 3, 0, 0), 
                                                c(0, 0, 3, 3, 0)))))

    # Expect an error when the CRS is missing.
    expect_error(make_grid(pl, n = 5))

    sf::st_crs(pl) <- 4326

    # Expect a warning when the grid's cells aren't squares.
    expect_warning(make_grid(pl, n = c(5, 21)))

    # Test invalid n.
    expect_error(make_grid(pl, n = integer()))
    expect_error(make_grid(pl, n = c(5, 5, 5)))
    expect_error(make_grid(pl, n = c(5, 5, 5, 5)))

    # Test a squre grid of squares.
    n_cells <- rep(5, times = 2)
    grid <- make_grid(pl, n = n_cells)
    expect_true(inherits(grid, what = "sf"))
    expect_equal(nrow(grid), expected = prod(n_cells))

    # Test grid's columns.
    expect_true("id" %in% colnames(make_grid(pl, n = 2)))
    expect_true("my_id" %in% colnames(make_grid(pl, n = 2, id_col = "my_id")))
    expect_true(all(
        c("grid_row", "grid_col") %in% 
            colnames(make_grid(pl, n = 2, add_row_col = TRUE))
    ))
    expect_true(all(
        c("myid1", "myid2") %in% 
            colnames(make_grid(pl, n = 2, 
                     add_row_col = c("myid1", "myid2")))
    ))
    expect_true(all(
        c("myid1", "myid2", "myid3") %in% 
            colnames(make_grid(pl, n = 2, id_col = "myid1",
                     add_row_col = c("myid2", "myid3")))
    ))
    expect_true(all(
        c("X", "Y") %in% 
            colnames(make_grid(pl, n = 2, 
                     add_centroids = TRUE))
    ))
    expect_true(all(
        c("myx", "myy") %in% 
            colnames(make_grid(pl, n = 2, 
                     add_centroids = c("myx", "myy")))
    ))
    expect_true(all(
        c("myid1", "myid2", "myid3", "myid4", "myid5") %in% 
            colnames(make_grid(pl, n = 2, id_col = "myid1",
                     add_row_col = c("myid2", "myid3"),
                     add_centroids = c("myid4", "myid5")))
    ))
    expect_true(all(
        ".area" %in% colnames(make_grid(pl, n = 2, add_area = TRUE))
    ))
    expect_true(all(
        "myarea" %in% colnames(make_grid(pl, n = 2, add_area = "myarea"))
    ))


})

test_simple_grid <- function(grid) {

    expect_true(inherits(grid, what = "sf"))
    expect_true("id" %in% colnames(grid))
    expect_true(inherits(sf::st_crs(grid), what = "crs"))

}

test_that("make_grid works with terra::SpatRaster", {

    m <- matrix(1:25, nrow = 5, ncol = 5)
    r <- terra::rast(m)
    terra::crs(r) <- "EPSG:4326"
    test_simple_grid(make_grid(r, n = 10))

})

test_that("make_grid works with terra::SpatVector", {

     x1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
     x2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
     x3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))
     hole <- rbind(c(80,0), c(105,13), c(120,2), c(105,-13))
     z <- rbind(
        cbind(object=1, part=1, x1, hole=0), 
        cbind(object=2, part=1, x3, hole=0), 
        cbind(object=3, part=1, x2, hole=0), 
        cbind(object=3, part=1, hole, hole=1)
    )
     colnames(z)[3:4] <- c('x', 'y')
     v <- terra::vect(z, "polygons")

    terra::crs(v) <- "EPSG:4326"
    suppressWarnings(
        test_simple_grid(make_grid(v, n = 7))
    )

})

test_that("make_grid_min_max_cells works", {

    grid <- make_grid_min_max_cells(
        xy_min = c(-180, -90),
        xy_max = c(180, 90),
        n = c(14, 7),
        crs = 4326
    )
    test_simple_grid(grid)

})


test_that("make_grid_origin_dist works", {

    grid <- make_grid_origin_dist(
        xy_min = c(0, 0),
        n = c(7, 14),
        cell_size = 0.25,
        crs = 4326
    )
    test_simple_grid(grid)

})



test_that("make_grid_origin_res works", {

    # TOOD: test the default name of the id column.
    xy_origin <- c(-74.2, 4.4)
    xy_min <- c(-78, -3)
    xy_max <- c(-70, 5)
    cell_size <- 0.5
    crs <- 4326

    grid <- make_grid_origin_res(xy_origin = xy_origin, xy_min = xy_min,
                                 xy_max = xy_max, cell_size = cell_size,
                                 crs = crs)
    test_simple_grid(grid)

    expect_true(all(
        sf::st_bbox(grid)["xmin"] >= xy_min[1],
        sf::st_bbox(grid)["ymin"] >= xy_min[2],
        sf::st_bbox(grid)["xmax"] <= xy_max[1],
        sf::st_bbox(grid)["ymax"] <= xy_max[2]
    ))

    # Expect an error wheh the resolution is too large to fit cells in the 
    # grid.
    expect_error(
        make_grid_origin_res(
            xy_origin = c(-45.886944, -23.178889),
            xy_min = c(-46, -24),
            xy_max = c(-45, -22),
            cell_size = 1,
            crs = 4326,
            id_col = "id"
        )
    )

})

test_that("is_grid_na works", {

    grid <- make_grid_min_max_cells(
        xy_min = c(-180, -90),
        xy_max = c(180, 90),
        n = c(14, 7),
        crs = 4326
    )

    grid["att1"] <- NA
    expect_true(is_grid_na(grid["att1"]))
    grid["att1"] <- 1
    expect_false(is_grid_na(grid))

})

