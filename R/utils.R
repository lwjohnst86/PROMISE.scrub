#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

#' Convert a date variable into another format (e.g. ISO standard).
#'
#' @param variable The date variable.
#' @param from Original date format.
#' @export
convert_to_date <- function(variable, from = "%m/%d/%y") {
    converted <- lubridate::ymd(as.Date(variable, from))
    yr <- lubridate::year(converted) %% 100
    # need to fix years that are less than 1950.
    lubridate::year(converted) <-
        ifelse(yr > lubridate::year(Sys.Date()) %% 100, 1900 + yr, 2000 + yr)
    converted
}

#' Get a vector of files with the full path for a given regular expression.
#'
#' @param pattern Regular expression pattern to search for files.
#' @param path Where to search for the files.
#'
#' @export
multiple_files <- function(pattern, path) {
    files <-
        list.files(path,
                   pattern = pattern,
                   full.names = TRUE)
    all_files_exist(files)
    files
}

all_files_exist <- function(files) {
    assertive::assert_all_are_existing_files(files, 'stop')
}
