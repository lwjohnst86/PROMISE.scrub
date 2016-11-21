#' Combine multiple datasets together into one, merged dataset. Merges based on
#' SID and VN columns.
#'
#' @param ... Any number of datasets that need to be merged.
#' @param by_cols Combine datasets based on these variables.
#'
#' @export
combine_datasets <- function(..., by_cols = c('SID', 'VN')) {
    data_list <- suppressMessages(list(...))
    ds_num <- length(data_list)

    if (ds_num == 1) {
        message('No other dataset to add. Outputting a single dataset.')
        combined <- dplyr::tbl_df(data_list[[1]])
    } else if (ds_num > 1) {
        message('There are ',
                ds_num,
                ' datasets to combine. Combining them together.')
        combined <- data_list[[1]]
        confirm_columns_in_data(combined, by_cols)
        for (index in 2:ds_num) {
            confirm_columns_in_data(data_list[[index]], by_cols)
            combined <- dplyr::full_join(
                combined, data_list[[index]],
                by = by_cols
                )
            combined <- remove_duplicate_columns(combined)
        }
    }

    if (length(combined) == 0)
        stop('There was a problem with the merging.. check into it.')

    return(dplyr::tbl_df(combined))
}

remove_duplicate_columns <- function(data) {
    data <- data[, !duplicated(colnames(data))]
    data[, !grepl('\\.[xy]$', colnames(data))]
}

confirm_columns_in_data <- function(data, columns) {
    if (!all(columns %in% names(data))) {
        stop('Dataset does not have the columns ',
             paste(columns, collapse = ', '))
    }
}
