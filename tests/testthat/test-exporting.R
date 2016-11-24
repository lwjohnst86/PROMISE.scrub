context("Exporting dataset")

path <- tempdir()

test_that("exporting creates files", {
    export_data(swiss, type = 'csv', path = path)
    expect_true(any(list.files(path, '*.csv') %in% 'swiss.csv'))
    file.remove(file.path(path, 'swiss.csv'))

    export_data(swiss, type = 'Rds', path = path)
    expect_true(any(list.files(path, '*.Rds') %in% 'swiss.Rds'))
    file.remove(file.path(path, 'swiss.Rds'))
})

test_that("exporting throws errors properly", {
    expect_error(swiss %>% export_data(type = 'csv'))
    expect_error(export_data(swiss, type = 'pkg'))
})
