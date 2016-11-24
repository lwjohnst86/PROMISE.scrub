.onLoad <- function(libname, pkgname) {
    op <- options()
    op.PROMISE <- list(
        PROMISE.rename.path = tempdir(),
        PROMISE.data.raw.path = tempdir(),
        PROMISE.extdata.path = tempdir(),
        PROMISE.visit.numbers = 0:3
    )
    toset <- !(names(op.PROMISE) %in% names(op))
    if (any(toset))
        options(op.PROMISE[toset])

    invisible()
}
