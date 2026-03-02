# Clean the names in a data.frame

Parse a character or a data.frame's names

## Usage

``` r
clean_names(.data, unique = FALSE, to_lower = FALSE, to_upper = FALSE)
```

## Arguments

- .data:

  a data.frame or a character.

- unique:

  a logical indicating is the produced names should be unique.

- to_lower, to_upper:

  Should the results be converted to a single case?

## Value

a data.frame or a character.

## See also

This function was adapted from the blog post called "Clean, Consistent
Column Names" by William Doane publishe in R-bloggers July 7, 2019.
