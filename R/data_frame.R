#' Add the list's names to the data frames in the list
#'
#' @description
#' Given a list of data frames, add each list's name to its corresponding data
#' frame.
#'
#' @param df_ls a named list of data frames.
#' @param cname a character(1). The name for the new column.
#'
#' @return         a list of data frames.
#' 
listnames2dataframes <- function(df_ls, cname){
    stopifnot("Expected a list of data frames!" = 
              all(vapply(df_ls, is.data.frame, logical(1))))
    stopifnot("Expected a named list!" = 
              length(names(df_ls)) == length(df_ls))
    lapply(seq(df_ls), function(x, df_ls){
            df_ls[[x]][cname] <- names(df_ls)[x]
            return(df_ls[[x]])
        },
        df_ls = df_ls
    )
}



#' Merge a list of data frames
#'
#' @description
#' Merge a list of data.frame by a given column name.
#'
#' @param data_frames a list of data.frame.
#' @param by a character of column names.
#'
#' @return a data.frame.
#'
merge_data_frames <- function(data_frames, by) {

    stopifnot("`by` must be a character!" = inherits(by, what = "character"))
    stopifnot("The columns in `by` must be present in all the data frames" = 
        all(vapply(
            data_frames, 
            FUN = function(x) {all(by %in% colnames(x))}, 
            logical(1)
        ))
    )

    Reduce(f = function(x, y) { merge(x, y, all = TRUE, by = by)}, 
           x = data_frames)

}

