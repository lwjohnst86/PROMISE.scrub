#' Extract the VN information from the Visit column (e.g. that has the data as
#' 'yr3', or 'YR6', or 'y1', etc.).
#'
#' @param .data Processing data.
#' @param original_visit Visit date variable to extract the VN from.
#'
#' @export
extract_vn <- function(.data, original_visit = 'Visit') {
    .data[['VN']] <- as.numeric(gsub('\\D', '', .data[[original_visit]]))
    .data
}

#' Add a visit counter to the raw data file.
#'
#' @param .data Processing data.
#' @param vars Variable to add visit information by (ie. SID with
#'   earlier VisitDate would be 1, increasing up as VisitDates increase).
#' @param start_count When to start the counting (ie. if for first visit, it
#'   would be 1)
#'
#' @export
add_visit_count <- function(.data,
                            vars = c('SID', 'VisitDate'),
                            start_count = 1) {

    # Add visit count
    .data <- .data %>%
        dplyr::arrange_(vars) %>%
        dplyr::group_by_(vars) %>%
        # Substract one to balance out the row_number and start_count
        dplyr::mutate(VisitCount = row_number() - 1 + start_count) %>%
        dplyr::ungroup()

    if (!'VN' %in% names(.data)) {
        v_num <- .data$VisitCount
        old_nums <- min(v_num):max(v_num)
        new_nums <- getOption('PROMISE.visit.numbers')[old_nums]

        .data["VN"] <-
            plyr::mapvalues(v_num, from = old_nums,
                            to = new_nums)
    }

    .data
}

#' Rename the raw dataset variable names based on the yaml files.
#'
#' @param .data The processing dataset.
#' @param yaml_file The yaml renaming files in the \code{inst/rename/} folder.
#' @param path Path to the yaml file. Defaults to the rename folder.
#'
#' @export
rename_variables <-
    function(.data, yaml_file, path = NULL) {
        if (is.null(path))
            path <- getOption('PROMISE.rename.path')
        yaml_file <- file.path(path, yaml_file)
        all_files_exist(yaml_file)

        # Extract the renaming part from the yaml file
        new_names <- unlist(yaml::yaml.load_file(yaml_file))
        new_names <- new_names[grep('.renamed', names(new_names))]

        # This drops the '.renamed' part of the new variable names
        new_names <- stats::setNames(new_names, gsub('\\.renamed', '', names(new_names)))

        .data <- remove_spaces_in_colnames(.data)
        names(.data) <- new_names[names(.data)]
        .data
    }

remove_spaces_in_colnames <- function(.data) {
    stats::setNames(.data, gsub(" +", "", names(.data)))
}

#' Drop the variables named 'NA' from the raw dataset.
#'
#' @param .data The processing dataset.
#'
#' @export
drop_na_variables <- function(.data) {
    .data[!grepl('NA', names(.data))]
}

#' Drops rows that are completely empty (excluding SID, etc).
#'
#' @param .data The processing dataset.
#' @param col_nums Numeric value for the columns to exclude or include (e.g.
#'   -1:-2 to exclude the first two columns, or 3:10 to include columns 3 to
#'   10).
#'
#' @export
drop_empty_rows <- function(.data, col_nums) {
    stopifnot(is.numeric(col_nums))
    .data[rowSums(is.na(.data[col_nums])) != ncol(.data[col_nums]), ]
}

#' Spread a variable values from one visit date to the next.
#'
#' @param .data The processing dataset.
#' @param column Variables to spread values across over the visits.
#'
#' @export
spread_over_visits <- function(.data, column) {
    .data %>%
        dplyr::arrange_('SID', column) %>%
        tidyr::fill_(column)
}

#' Fix with with any date variable.
#'
#' @param .data The processing dataset.
#' @param date_var The date variable found in the dataset.
#' @param original_format The original format of the date (e.g. MM/DD/YY is
#'   \%m/\%d/\%y or YYYY-MM-DD is \%Y-\%m-\%d).
#' @seealso To see the different options for date formats, see
#'   \code{\link[base]{strptime}}.
#'
#' @export
fix_date <- function(.data, date_var, original_format = '%m/%d/%y') {
    .data[date_var] <- convert_to_date(.data[[date_var]], from = original_format)
    .data
}
