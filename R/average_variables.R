#' Average variables that have two or more measurements.
#'
#' @param .data Dataframe to scrub.
#' @param .pattern Regular expression pattern to search for variable names.
#' @export
average_variables <- function(.data, .pattern) {
    .data %>%
        dplyr::select_at(dplyr::vars("SID", "VN", dplyr::matches(.pattern))) %>%
        tidyr::gather("Measure", "Value", -dplyr::matches("^SID$"), -dplyr::matches("^VN$")) %>%
        dplyr::mutate_at("Measure", ~ gsub('\\d$', '', .)) %>%
        stats::na.omit() %>%
        dplyr::group_by_at(c("SID", "VN", "Measure")) %>%
        dplyr::summarise_at("Value", mean) %>%
        tidyr::spread("Measure", "Value") %>%
        dplyr::full_join(dplyr::select(.data, -dplyr::matches(.pattern)),
                         by = c('SID', 'VN')) %>%
        dplyr::ungroup()
}
