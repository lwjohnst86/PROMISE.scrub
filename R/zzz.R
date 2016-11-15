.onLoad <- function(libname, pkgname) {
    op <- options()
    op.PROMISE <- list(
        PROMISE.rename.path = system.file('rename', package = "PROMISE"),
        PROMISE.data.raw.path = system.file('data-raw', package = "PROMISE")
    )
    toset <- !(names(op.PROMISE) %in% names(op))
    if (any(toset))
        options(op.PROMISE[toset])

    invisible()
}
