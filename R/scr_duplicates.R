#' Fix any duplicates by dropping them.
#'
#' @param .data Data with duplicates.
#' @param .variables Variables (columns) to use as the basis for determining duplicates.
#' @param action The action to take for duplicates:
#'
#'     - "keeplast": Keep the last duplicate value.
#'     - "keepfirst": Keep the first duplicate value.
#'
#' @return Data without duplicates from specific columns.
#' @export
#'
#' @examples
#'
#' ds <- data.frame(
#' a = c(rep(1, 3), rep(2, 3)),
#' b = c(1:3, 1, 1, 2),
#' v = rnorm(6)
#' )
#' ds %>%
#' scr_duplicates(c("a", "b"), "keepfirst")
scr_duplicates <- function(.data,
                           .variables,
                           action = c('keeplast', 'keepfirst')) {

    switch(
        action,
        keeplast = {
            dups <- !duplicated(.data[.variables], fromLast = TRUE)
            output <- dplyr::filter(.data, dups)
            message('... Removed first duplicate.')
        },
        keepfirst = {
            dups <- !duplicated(.data[.variables])
            output <- dplyr::filter(.data, dups)
            message('... Removed last duplicate.')
        }
    )

    return(output)
}
