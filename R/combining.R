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
        # TODO: use purrr::reduce instead?
        for (index in 2:ds_num) {
            confirm_columns_in_data(data_list[[index]], by_cols)
            combined <- dplyr::full_join(
                combined, data_list[[index]],
                by = by_cols
                )
        }

    warn_if_duplicate_columns(combined)
    combined <- remove_duplicate_columns(combined)
    }

    if (length(combined) == 0)
        stop('There was a problem with the merging.. check into it.')

    return(dplyr::tbl_df(combined))
}

warn_if_duplicate_columns <- function(.data) {
    dup_cols <- duplicated(colnames(.data)) |
        grepl('\\.[xy]$', colnames(.data))
    dup_column_names <- names(.data[dup_cols])
    number_dups <- sum(as.numeric(dup_cols))
    if (any(dup_cols)) {
        warning("There are ", number_dups, " duplicate columns in the dataset.\n",
                "These columns are: ", paste(dup_column_names, collapse = "; "), ".\n",
                "(columns ending in '.x' or '.y' are from the datasets to be combined).\n",
                "The duplicate columns will be dropped, please make ",
                "sure that you want these columns dropped in the ",
                "combined dataset.", call. = FALSE)
    }
}

remove_duplicate_columns <- function(.data, drop.cols = TRUE) {
    .data <- .data[, !duplicated(colnames(.data))]
    .data[, !grepl('\\.[xy]$', colnames(.data))]
}

confirm_columns_in_data <- function(data, columns) {
    if (!all(columns %in% names(data))) {
        stop('Dataset does not have the columns ',
             paste(columns, collapse = ', '))
    }
}

#' Merge together a list of dataframes by SID into a single dataframe.
#'
#' Simpler than the \code{\link{combine_datasets}} function. Only allows for
#' merging by the variable SID (subject identification number).
#'
#' @param data_list List of data.frames
#' @export
join_data_list <- function(data_list) {
    assertive::assert_is_list(data_list)
    # Use this as the base index for the for loop full_join.
    output <- data.frame(SID = data_list[[1]]$SID)
    # TODO: use purrr::reduce instead?
    for (index in 1:length(data_list)) {
        output <-
            dplyr::full_join(output,
                             data_list[[index]])
    }
    output
}
