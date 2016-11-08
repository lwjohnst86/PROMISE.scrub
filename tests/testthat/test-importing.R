context("CSV imported correctly")

path <- tempdir()
write.csv(head(swiss), file = file.path(path, 'data.csv'), row.names = FALSE)
write.csv(tail(swiss), file = file.path(path, 'data_tail.csv'), row.names = FALSE)

test_that("csv gets imports correctly", {
    data <- import_csv('data.csv', path = path)
    expect_is(data, 'tbl_df')
    expect_length(data, 6)
})

test_that("multiple csv imports correctly and is merged", {
    data <- import_csv_multiple('*csv', path = path)
    expect_is(data, 'tbl_df')
    expect_equal(nrow(data), 12)
})
