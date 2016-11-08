#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

confirm_columns_in_data <- function(data, name, columns, error = stop) {
    if (!all(columns %in% names(data))) {
        stop(paste0(
            name,
            ' dataset does not have the columns ',
            paste(columns, collapse = ', ')
        ))
    }
}

convert_to_date <- function(variable, from = "%m/%d/%y") {
    converted <- lubridate::ymd(as.Date(variable, from))
    yr <- lubridate::year(converted) %% 100
    # need to fix years that are less than 1950.
    lubridate::year(converted) <-
        ifelse(yr > lubridate::year(Sys.Date()) %% 100, 1900 + yr, 2000 + yr)
    converted
}

#' Path to the PROMISE package.
#'
#' @param ... folder names.
#' @export
pkg_path <- function(..., package = 'PROMISE') {
    path <- file.path(...)
    normalizePath(system.file(path, package = package))
}

multiple_files <- function(pattern, path) {
    files <-
        list.files(path,
                   pattern = pattern,
                   full.names = TRUE)
    assertive::assert_is_non_empty(files)
    all_files_exist(files)
    files
}

join_data_list <- function(data_list, by = 'SID') {
    assertive::assert_is_list(data_list)
    # Use this as the base index for the for loop full_join.
    output <- data.frame(SID = data_list[[1]]$SID)
    for (index in 1:length(data_list)) {
        output <-
            dplyr::full_join(output,
                             data_list[[index]],
                             by = by)
    }
    output
}

all_files_exist <- function(files) {
    assertive::assert_all_are_existing_files(files, 'stop')
}
