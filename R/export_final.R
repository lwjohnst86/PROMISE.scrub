#' Export a dataset into another format (Rds, csv, or as a package dataset).
#'
#' @param data The dataset.
#' @param name The name of the output dataset filename (without filename
#'   extensions).
#' @param type The format to output/save.
#' @param path The path to save the dataset to.
#'
#' @export
export_data <-
    function(data, name = deparse(substitute(data)),
             type = c('Rds', 'csv', 'pkg'),
             path = getOption('PROMISE.extdata.path')) {
        type <- match.arg(type)
        if (type != 'pkg' & path == system.file('extdata', package = 'PROMISE.data'))
            stop('Please provide a file path to export the dataset, not the default. ',
                 'I recommend the "." path (working directory).')

        if (name == '.' & type != 'pkg')
            stop("Please don't use this in a %>% pipe chain. ",
                 "For instance, use as export_data(PROMISE).")

        switch(
            type,
            Rds = export_to_rds_file(data, name, path),
            csv = export_to_csv_file(data, name, path),
            pkg = export_as_pkg_data(data)
        )
    }

# TODO: Remove this.
export_to_csv_file <- function(data, name, path) {
    utils::write.csv(data,
                     file.path(path, paste0(name, '.csv')),
                     na = '',
                     row.names = FALSE)
}

export_to_rds_file <- function(data, name, path) {
    saveRDS(data, file = file.path(path, paste0(name, '.Rds')))
}

export_as_pkg_data <- function(data) {
    PROMISE <- data
    if (getwd() != system.file(package = 'PROMISE.data'))
        stop("Please only use this within the PROMISE.data package.")
    usethis::use_data(PROMISE, overwrite = TRUE)
}
