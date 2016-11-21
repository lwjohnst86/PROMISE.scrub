#' General function; import raw csv datafile into R.
#'
#' @param file The name of the raw csv datafile that is found in the
#'   \code{data-raw/} folder.
#' @param path Path to file.
#' @param skip_lines Whether to skip n lines when importing the csv file.
#' @export
import_csv <-
    function(file,
             path = NULL,
             skip_lines = 0) {
        if (is.null(path))
            path <- getOption('PROMISE.data.raw.path')
        data_file <- file.path(path, file)
        all_files_exist(data_file)

        readr::read_csv(data_file,
                        na = c('NA', '', '.'),
                        skip = skip_lines)
    }

#' General function; imports multiple raw data
#' files based on a pattern, only used for data of the same type (e.g. 'ogtt' or
#' 'fattyacids').
#'
#' @param pattern The file pattern to search for in the
#'   \code{data-raw/} folder.
#' @param merge_direction Whether to combine datasets by rows or by columns.
#'   This is highly situational... Mostly will need by rows.
#' @param path Path to directory where the files are stored.
#' @export
import_csv_multiple <- function(pattern, merge_direction = c('rows', 'columns'),
                                path = getOption('PROMISE.data.raw.path')) {
    merge_direction <- match.arg(merge_direction)
    files <- multiple_files(pattern, path)
    files <- basename(files)

    switch(merge_direction,
           rows = {
               dplyr::bind_rows(lapply(files, import_csv, path = path))
           },
           columns = {
               data <- lapply(files, import_csv, path = path)
               join_data_list(data)
           })
}

