test_that("aggregate_geom_categ works with points", {

    x_min <- -10
    y_min <- -5
    x_max <- 10
    y_max <- 5

    # create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = c(20, 10),
        crs = 4326
    )

    # Create random points.
    n_points <- 5
    categs <- "categ_1"
    points_sf <- sf::st_sf(data.frame(
        category = sample(categs, size = n_points, replace = TRUE),
        geom = list(sf::st_sample(grid, size = n_points))
    ))

    data_sf_ls <- list(points_sf)
    names(data_sf_ls) <- categs
    res <- aggregate_geom_categ(
        categ = categs[1],
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    expect_equal(length(res), expected = length(categs))
    expect_equal(
        sum(res[[1]][["sum.n.categ_1"]]),
        expected = nrow(points_sf)
    )

    points_sf["var1"] <- rnorm(n_points)

    data_sf_ls <- list(points_sf)
    names(data_sf_ls) <- categs
    res <- aggregate_geom_categ(
        categ = categs[1],
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = "grid_id",
        funs = "mean"
    )
    expect_true(all(res[["mean.n.categ_1"]] == 1))

    sf::st_agr(grid) <- "constant"
    points_sf <- sf::st_centroid(grid)
    data_sf_ls <- list(points_sf)
    names(data_sf_ls) <- categs
    funs <- c("mean", "sum")
    res <- aggregate_geom_categ(
        categ = categs[1],
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = "grid_id",
        funs = funs
    )
    expect_true(all(res[["mean.n.categ_1"]] == 1))
    expect_equal(length(res), expected = length(funs))
    expect_equal(
        sum(res[[2]][["sum.n.categ_1"]]),
        expected = nrow(points_sf)
    )

})



test_that("aggregate_geom_categ works with lines", {

    x_min <- 412090
    y_min <- 7433005
    x_max <- x_min + 130
    y_max <- y_min + 130

    # Create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = 5,
        crs = 32723
    )

    # Create lines.
    ls_1 <- sf::st_linestring(
        matrix(c(412092.92, 7433082.79, 412143.87, 7433133.493), byrow = TRUE,
            ncol = 2), dim = "XY"
    )
    ls_2 <- sf::st_linestring(
        matrix(c(412143.87, 7433133.493, 412218.449, 7433059.477),
            byrow = TRUE, ncol = 2), dim = "XY"
    )
    ls_3 <- sf::st_linestring(
        matrix(c(412218.449, 7433059.477, 412167.573, 7433008.878),
            byrow = TRUE, ncol = 2), dim = "XY"
    )
    ls_4 <- sf::st_linestring(
        matrix(c(412167.573, 7433008.878, 412092.92, 7433082.79),
            byrow = TRUE, ncol = 2), dim = "XY"
    )
    lines_sfc <- sf::st_sfc(
        list(ls_1, ls_2, ls_3, ls_4),
        crs = 32723,
        dim = "XY"
    )
    lines_sf <- sf::st_sf(data.frame(
        category = rep("A", times = 4),
        geom = lines_sfc
    ))

    data_sf_ls <- list(lines_sf)
    names(data_sf_ls) <- lines_sf[["category"]][1]
    res <- aggregate_geom_categ(
        categ = lines_sf[["category"]][1],
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    expect_equal(
        sum(res[[1]][["sum.length.A"]]),
        expected = sum(sf::st_length(lines_sf))
    )
    expect_equal(
        res[[1]][["sum.n.A"]],
        expected = c(2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1)
    )

})



test_that("aggregate_geom_categ works with polygons", {

    x_min <- 412090
    y_min <- 7433005
    x_max <- x_min + 130
    y_max <- y_min + 130

    # Create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = 5,
        crs = 32723
    )

    # Create lines.
    pol_1 <- sf::st_polygon(
        list(matrix(
            c(412092.92,  7433082.79,
              412143.87,  7433133.493,
              412218.449, 7433059.477,
              412167.573, 7433008.878,
              412092.92,  7433082.79
        ), byrow = TRUE,
            ncol = 2)), dim = "XY"
    )
    pol_sfc <- sf::st_sfc(
        list(pol_1),
        crs = 32723,
        dim = "XY"
    )
    pol_sf <- sf::st_sf(data.frame(
        category = rep("A", times = 1),
        geom = pol_sfc
    ))

    data_sf_ls <- list(pol_sf)
    names(data_sf_ls) <- pol_sf[["category"]][1]
    res <- aggregate_geom_categ(
        categ = pol_sf[["category"]][1],
        data_sf_ls = data_sf_ls,
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    expect_equal(
        sum(res[[1]][["sum.area.A"]]),
        expected = sf::st_area(pol_sf)
    )
    expect_true(all(res[[1]][["sum.n.A"]] == 1))

})



test_that("aggregate_vector works with points", {

    # Create a new grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(-180, -90), 
        xy_max = c(180, 90),
        n = c(14, 7),
        crs = 4326
    )

    vars <- c(trees = "integer", temperature = "double")
    categs <- LETTERS[1:3]
    funs <- c("sum", "mean", "sd", "min", "max")

    # Create random points.
    n_points <- 100
    points_sf <- sf::st_sf(data.frame(
        category = sample(categs, size = n_points, replace = TRUE),
        geom = list(sf::st_sample(grid, size = n_points))
    ))
    for (v in names(vars)) {
        if (vars[v] == "integer")
            points_sf[v] <- sample(1:10, size = n_points, replace = TRUE)
        if (vars[v] == "double")
            points_sf[v] <- 20 * rnorm(n = n_points)
    }

    # Test error when non-numeric columns are given (besides `by`).
    points_e <- points_sf
    points_e["char_col"] <- rep("e", times = nrow(points_e))
    expect_error(agg_sf <- aggregate_vector(x = points_e, by = "category", 
                                            grid = grid, grid_id = "grid_id",
                                            funs = funs, na.rm = TRUE))

    # Do aggregate.
    agg_sf <- aggregate_vector(
        x = points_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = funs,
        na.rm = TRUE
    )

    # The resulting sf object must have the same number of rows as the grid.
    expect_equal(nrow(agg_sf), expected = nrow(grid))

    # The resulting sf object must have a column for each combination of 
    # categories, variables, and aggregation functions.
    n_col_att  <- length(funs) * length(vars) * length(categs)
    n_col_geom <- length(funs) * length(categs)
    expect_equal(
        ncol(sf::st_drop_geometry(agg_sf)),
        expected = n_col_att + n_col_geom + 1 # grid_id
    )

    # The sum of categories per variable must match the total.
    expt <- aggregate(
        sf::st_drop_geometry(points_sf)[names(vars)],
        by = sf::st_drop_geometry(points_sf)["category"],
        FUN = "sum"
    )
    cnames <- paste(
        "sum.",
        do.call(paste, c(expand.grid(names(vars), categs), sep=".")),
        sep = ""
    )
    res <- colSums(sf::st_drop_geometry(agg_sf)[cnames], na.rm = TRUE)
    expect_true(res["sum.trees.A"] == expt[expt$category == "A", "trees"])
    expect_true(res["sum.trees.B"] == expt[expt$category == "B", "trees"])
    expect_true(res["sum.trees.C"] == expt[expt$category == "C", "trees"])

    sum.temperature.A <- expt[expt["category"] == "A", "temperature"]
    names(sum.temperature.A) <- "sum.temperature.A" 
    expect_equal(
        res["sum.temperature.A"],
        expected = sum.temperature.A
    )

    ex_vec <- expt[expt["category"] == "B", "temperature"]
    names(ex_vec) <- "sum.temperature.B"
    expect_equal(
        res["sum.temperature.B"],
        expected = ex_vec
    )

    ex_vec <- expt[expt["category"] == "C", "temperature"]
    names(ex_vec) <- "sum.temperature.C"
    expect_equal(
        res["sum.temperature.C"],
        expected = ex_vec
    )

    # The total number of points must match.
    points_sf["counter"]  = 1
    agg_sf <- aggregate_vector(
        x = points_sf[c("category", "counter")],
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = "sum",
        na.rm = TRUE
    )
    cnames <- paste(
        "sum.",
        do.call(paste, c(expand.grid("counter", categs), sep=".")),
        sep = ""
    )
    expect_equal(
        sum(colSums(sf::st_drop_geometry(agg_sf)[cnames], na.rm = TRUE)),
        expected = nrow(points_sf)
    )

})



test_that("aggregate_vector works with lines", {

    categs <- tolower(LETTERS[13:15])
    funs <- sort(c("sum", "mean", "sd", "min", "max"))
    n_lines <- 100
    line_vertex <- 1 + sample.int(n = 10, size = n_lines, replace = TRUE)
    vars <- c(trees = "integer", temperature = "double", albedo = "double")
    x_min <- -80
    x_max <- -70
    y_min <- -2.5
    y_max <- 2.5
    neg_buffer <- 0.1

    # Create a new grid.
    grid <- make_grid_min_max_cells( id_col = "grid_id",
        xy_min = c(x_min, y_min), xy_max = c(x_max, y_max), n = c(12, 6),
        crs = 4326)

    # Create lines.
    lin_ls <- lapply(line_vertex , function(x) {
        lon <- sample(seq(x_min + neg_buffer, x_max - neg_buffer, 0.1),
                      size = x)
        lat <- sample(seq(y_min + neg_buffer, y_max - neg_buffer, 0.1),
                      size = x)
        sf::st_linestring(matrix(c(lon, lat), byrow = FALSE, ncol = 2), 
                          dim = "XY")
    })
    lines_sf <- sf::st_sf(data.frame(
        category = sample(categs, size = n_lines, replace = TRUE),
        geom = sf::st_sfc(lin_ls, crs = 4326)
    ))
    for (v in names(vars)) {
        if (vars[v] == "integer")
            lines_sf[v] <- sample(1:11, size = n_lines, replace = TRUE)
        if (vars[v] == "double")
            lines_sf[v] <- 7 * rnorm(n = n_lines)
    }

    # Aggregate lines.
    agg_sf <- aggregate_vector(
        x = lines_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = funs
    )

    agg_df <- sf::st_drop_geometry(agg_sf)

    # Test number of rows.
    expect_equal(nrow(agg_df), expected = nrow(grid))

    # Test number of columns.
    ncol_att  <- length(categs) * length(vars) * length(funs)
    ncol_geom <- length(categs) * length(funs) * 2 # Lines: n & length.
    expect_equal(
        ncol(sf::st_drop_geometry(agg_sf)),
        expected = ncol_att + ncol_geom + 1
    )

    # TODO: lenght of lines aggregated by category should match with the 
    # aggregation results.
    lines_sf["length"] <- sf::st_length(lines_sf)
    # Aggregate lines.
    agg_sf <- aggregate_vector(
        x = lines_sf[c("category", "length")],
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    agg_len <- sf::st_drop_geometry(agg_sf)
    agg_len <- agg_len[c("sum.length.m", "sum.length.n", "sum.length.o")]
    agg_len <- colSums(agg_len, na.rm = TRUE)
    agg_len <- sum(agg_len, na.rm = TRUE)
    expect_equal(
        agg_len,
        expected = units::drop_units(sum(sf::st_length(lines_sf)))
    )

})



test_that("aggregate_vector works with polygons", {

    x_min <- -50
    y_min <- -25
    x_max <- 40
    y_max <- 20

    # create a new grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = c(12, 6),
        crs = 4326
    )

    suppressWarnings(
        polygons_sf <- make_grid_min_max_cells(
            id_col = "pol_id",
            xy_min = c(x_min, y_min),
            xy_max = c(x_max, y_max),
            n = c(12, 6),
            crs = 4326
        )
    )
    polygons_sf[["pol_id"]] <- NULL
    polygons_sf <- polygons_sf[sample(nrow(polygons_sf),
                               size = floor(nrow(polygons_sf)/2)), ]

    categs <- tolower(letters[24:26])
    funs <- sort(c("sum", "mean", "sd", "min", "max"))

    polygons_sf <- cbind(polygons_sf, 
        data.frame(
            category = sample(categs, size = nrow(polygons_sf),
                              replace = TRUE),
            trees = sample(7, size = nrow(polygons_sf), replace = TRUE),
            temperature = 20 * rnorm(n = nrow(polygons_sf))
        ))

    agg_sf <- aggregate_vector(
        x = polygons_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = c("sum", "mean", "sd", "min", "max")
    )

    # Test number of columns.
    vars <- c("trees", "temperature")
    ncol_att  <- length(funs) * length(vars) * length(categs)
    ncol_geom <- length(categs) * length(funs) * 2 # Lines: n & area
    expect_equal(
        ncol(sf::st_drop_geometry(agg_sf)),
        expected = ncol_att + ncol_geom + 1
    )

})



test_that("aggregate_geom works with points", {

    x_min <- -10
    y_min <- -5
    x_max <- 10
    y_max <- 5

    # create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = c(20, 10),
        crs = 4326
    )

    # Create random points.
    n_points <- 5
    categs <- LETTERS[1:n_points]
    points_sf <- sf::st_sf(data.frame(
        category = sample(categs, size = n_points, replace = FALSE),
        geom = list(sf::st_sample(grid, size = n_points))
    ))

    agg_points <- aggregate_geom(
        x = points_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    expect_equal(nrow(agg_points), expected = nrow(grid))
    expect_equal(
        sum(sf::st_drop_geometry(agg_points)[-1], na.rm = TRUE),
        expected = nrow(points_sf)
    )
    expect_equal(
        ncol(sf::st_drop_geometry(agg_points)[-1]),
        expected = length(categs)
    )
    expect_true(
        all(vapply(
            sf::st_drop_geometry(agg_points)[-1],
            sum,
            numeric(1),
            na.rm = TRUE
        ) == 1)
    )

})



test_that("aggregate_geom works with lines", {

    x_min <- -10
    y_min <- -5
    x_max <- 10
    y_max <- 5

    # create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = c(20, 10),
        crs = 4326
    )

    categs <- LETTERS[1:3]

    # Create lines.
    n_lines <- 3
    line_vertex <- 1 + sample.int(n = 10, size = n_lines, replace = TRUE)
    neg_buffer <- 0.1
    lin_ls <- lapply(line_vertex , function(x) {
        lon <- sample(seq(x_min + neg_buffer, x_max - neg_buffer, 0.1),
                      size = x)
        lat <- sample(seq(y_min + neg_buffer, y_max - neg_buffer, 0.1),
                      size = x)
        sf::st_linestring(matrix(c(lon, lat), byrow = FALSE, ncol = 2), 
                          dim = "XY")
    })
    lines_sf <- sf::st_sf(data.frame(
        category = sample(categs, size = n_lines, replace = TRUE),
        geom = sf::st_sfc(lin_ls, crs = 4326)
    ))

    agg_lines <- aggregate_geom(
        x = lines_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    # Test against the number of elements in the grid.
    expect_equal(nrow(agg_lines), expected = nrow(grid))

    # Test the total length of the lines.
    res <- sum(sf::st_drop_geometry(agg_lines)[-1], na.rm = TRUE) 
    expt <- sum(units::drop_units(sf::st_length(lines_sf)))
    tol <- .Machine$double.eps^0.3
    expect_true(abs(res - expt) / expt < tol)

})



test_that("aggregate_geom works with polygons", {

    x_min <- 412090
    y_min <- 7433005
    x_max <- x_min + 130
    y_max <- y_min + 130

    # Create a grid.
    grid <- make_grid_min_max_cells(
        id_col = "grid_id",
        xy_min = c(x_min, y_min), 
        xy_max = c(x_max, y_max),
        n = 5,
        crs = 32723
    )

    # Create lines.
    pol_1 <- sf::st_polygon(
        list(matrix(
            c(412092.92,  7433082.79,
                412143.87,  7433133.493,
                412218.449, 7433059.477,
                412167.573, 7433008.878,
                412092.92,  7433082.79
            ), byrow = TRUE,
            ncol = 2)), dim = "XY"
    )
    pol_sfc <- sf::st_sfc(
        list(pol_1),
        crs = 32723,
        dim = "XY"
    )
    pol_sf <- sf::st_sf(data.frame(
        category = rep("A", times = 1),
        geom = pol_sfc
    ))

    data_sf_ls <- list(pol_sf)
    names(data_sf_ls) <- pol_sf[["category"]][1]

    res <- aggregate_geom(
        x = pol_sf,
        by = "category",
        grid = grid,
        grid_id = "grid_id",
        funs = "sum"
    )

    expect_equal(
        sum(res[["sum.area.A"]], na.rm = TRUE),
        expected = sf::st_area(pol_sf)
    )

    expect_equal(
        res[["sum.n.A"]],
        expected = c(NA, NA, 1, 1, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, NA, 1, 1, 1, NA)
    )

})

