context("Utility functions")

test_that("Vector of filenames is given", {
    path <- tempdir()
    filesR <-
        sort(tempfile(
            c("test1", "test2", "hi1", "hi2"),
            tmpdir = path,
            fileext = c(".R")
        ))
    filesmd <-
        sort(tempfile(
            c("test1", "test2", "hi1", "hi2"),
            tmpdir = path,
            fileext = c(".md")
        ))
    file.create(filesmd, filesR)
    expect_equal(multiple_files("md$", path), filesmd)
    expect_equal(multiple_files("\\.R$", path), filesR)
    expect_equal(multiple_files("^hi", path), sort(c(filesR[1:2], filesmd[1:2])))
    expect_equal(multiple_files("^test", path), sort(c(filesR[3:4], filesmd[3:4])))
})
