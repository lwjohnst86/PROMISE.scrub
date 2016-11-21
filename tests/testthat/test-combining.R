context("Combining datasets")

test_that("combining datasets returns proper number of columns and rows", {
    d1 <- data.frame(
        id = rep(1:4, each = 4),
        vn = rep(1:4, times = 4),
        wgt = sample(50:100, 16, replace = TRUE)
    )
    d1[c(3, 10, 15), ] <- NA
    d1 <- stats::na.omit(d1)
    d2 <- data.frame(
        id = rep(1:4, each = 4),
        vn = rep(1:4, times = 4),
        hgt = sample(150:200, 16, replace = TRUE)
    )
    d2[c(4, 11, 16), ] <- NA
    d2 <- stats::na.omit(d2)
    d3 <- data.frame(
        id = 1:4,
        sex = c('F', 'M', 'F', 'F')
    )

    data <- combine_datasets(d1, d2, by_cols = c('id', 'vn'))
    expect_identical(names(data), c('id', 'vn', 'wgt', 'hgt'))
    expect_is(data, 'data.frame')
    expect_equal(nrow(data), 16)

    data <- combine_datasets(data, d3, by_cols = 'id')
    expect_identical(names(data), c('id', 'vn', 'wgt', 'hgt', 'sex'))
})
