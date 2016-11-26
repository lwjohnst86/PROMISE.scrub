#' Average variables that have two or more measurements.
#'
#' @param data The scrubbing dataset.
#' @param var_pattern Regular expression pattern to search for variable names.
#' @export
average_variables <- function(data, var_pattern) {
    data %>%
        dplyr::select(SID, VN, dplyr::matches(var_pattern)) %>%
        tidyr::gather(Measure, Value, -SID, -VN) %>%
        dplyr::mutate(Measure = gsub('\\d$', '', Measure)) %>%
        stats::na.omit() %>%
        dplyr::group_by(SID, VN, Measure) %>%
        dplyr::summarise(Mean = mean(Value)) %>%
        tidyr::spread(Measure, Mean) %>%
        dplyr::full_join(dplyr::select(data, -dplyr::matches(var_pattern)),
                         by = c('SID', 'VN')) %>%
        dplyr::ungroup()
}
