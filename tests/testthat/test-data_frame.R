# Create random names
# @param n an integer. Number of names to create.
# @param len an integer. Number of characters in each name.
# @return a character.
#
random_names <- function(n, len = 3) {
    vapply(
        X = 1:n,
        FUN = function(x) { paste(sample(LETTERS, len), collapse = "") },
        FUN.VALUE = character(1)
    )
}

test_that("listnames2dataframes works", {

    df_ls <- lapply(
        X = 1: sample(1:10, 1),
        FUN = function(x) {
            nr_nc <- sample(1:20, 2)
            data_mt <- matrix(data = rnorm(n = prod(nr_nc)), nrow = nr_nc[1])
            data_mt <- cbind(1:nr_nc[1], data_mt)
            colnames(data_mt) <- c(LETTERS[1], random_names(nr_nc[2]))
            return(as.data.frame(data_mt))
        }
    )
    names(df_ls) <- random_names(n = length(df_ls), len = 5)

    # "new_col" must be a column in the resultng data frames.
    res <- listnames2dataframes(df_ls = df_ls, cname = "new_col")
    expect_true(
        all(vapply(res, function(x) {"new_col" %in% colnames(x)}, logical(1)))
    )

    expect_equal(
        names(df_ls),
        vapply(res, function(x){unique(x[["new_col"]])}, character(1))
    )

})


test_that("merge_data_frames works", {

    # Create a list of data frames.
    df_ls <- lapply(
        X = 1: sample(1:10, 1),
        FUN = function(x) {
            nr_nc <- sample(1:20, 2)
            data_mt <- matrix(data = rnorm(n = prod(nr_nc)), nrow = nr_nc[1])
            data_mt <- cbind(1:nr_nc[1], data_mt)
            colnames(data_mt) <- c(LETTERS[1], random_names(nr_nc[2]))
            return(as.data.frame(data_mt))
        }
    )

    res <- merge_data_frames(df_ls, by = "A")

    expect_true("A" %in% colnames(res))
    expect_equal(nrow(res), expected = max(sapply(df_ls, nrow)))
    expect_equal(
        ncol(res),
        expected = sum(sapply(df_ls, ncol)) - length(df_ls) + 1
    )

    # Remove the ID col from one data frame in the list.
    data_df <- df_ls[[1]]
    data_df <- data_df[2:ncol(data_df)]
    df_ls[[1]] <- data_df
    # Throw an error when the ID column is missing from some data frames.
    expect_error(
        res <- merge_data_frames(df_ls, by = "A")
    )

})


