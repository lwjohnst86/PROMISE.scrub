context("Conversion of date formats")

orig_dates <- c('12/12/12', '01/23/15', '05/01/99', '10/14/41')
real_dates <- as.character(convert_to_date(orig_dates))
expected_dates <- c('2012-12-12', '2015-01-23', '1999-05-01', '1941-10-14')

test_that("vector of dates are converted", {
    expect_identical(real_dates, expected_dates)
})

test_that("data frame with dates are converted", {
    data <- data.frame(Dates = orig_dates)
    real_dates <- fix_date(data, 'Dates')
    expect_is(real_dates, 'data.frame')
    expect_identical(real_dates[['Dates']], as.Date(expected_dates))
})
