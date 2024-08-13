#' Clean the names in a data.frame
#'
#' @description
#' Parse a character or a data.frame's names 
#'
#' @param .data a data.frame or a character.
#' @param unique a logical indicating is the produced names should be unique.
#' @param to_lower,to_upper Should the results be converted to a single case?
#'
#' @return a data.frame or a character.
#'
#' @seealso This function was adapted from the blog post called "Clean, 
#' Consistent Column Names" by William Doane publishe in R-bloggers July 7, 
#' 2019.
#'
clean_names <- function(.data, unique = FALSE, 
                        to_lower = FALSE, to_upper = FALSE) {

    n <- if (is.data.frame(.data)) colnames(.data) else .data
    n <- gsub("%+", "_pct_", n)
    n <- gsub("\\$+", "_dollars_", n)
    n <- gsub("\\++", "_plus_", n)
    n <- gsub("-+", "_minus_", n)
    n <- gsub("\\*+", "_star_", n)
    n <- gsub("#+", "_cnt_", n)
    n <- gsub("&+", "_and_", n)
    n <- gsub("@+", "_at_", n)
    n <- gsub("[^a-zA-Z0-9_]+", "_", n)
    n <- gsub("([A-Z][a-z])", "_\\1", n)
    n <- trimws(n)
    if (to_lower) n <- tolower(n)
    if (to_upper) n <- toupper(n)
    n <- gsub("(^_+|_+$)", "", n)
    n <- gsub("_+", "_", n)
    if (unique) n <- make.unique(n, sep = "_")
    if (is.data.frame(.data)) {
        colnames(.data) <- n
        .data
    } else {
        n
    }

}


