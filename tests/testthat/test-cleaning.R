context("Cleaning functions")

path <- tempdir()
write.csv(head(swiss), file = file.path(path, 'data.csv'), row.names = FALSE)
data <- suppressMessages(import_csv('data.csv', path = path))
writeLines(
    "
    Fertility:
    - renamed: Fertile
    Agriculture:
    - renamed: NA
    Examination:
    - renamed: Exam
    Education:
    - renamed: NA
    Catholic:
    - renamed: Catholic
    Infant.Mortality:
    - renamed: NA
    ",
    con = file.path(path, 'renaming.yaml')
)
data <- rename_variables(data, 'renaming.yaml', path = path)
data <- drop_na_variables(data)

test_that("renaming and dropping of NA variables works", {
    expect_length(data, 3)
    expect_identical(names(data), c('Fertile', 'Exam', 'Catholic'))
})

test_that("visit number is extracted from a visit string", {
    visit <- data.frame(Visit = c('Yr3', 'YR5', '4', 'year1', 'yr2'))
    expect_equal(extract_vn(visit)[['VN']], c(3, 5, 4, 1, 2))
})

test_that("completely empty rows are dropped", {
    data <- rbind(data, c(2, NA, NA))
    expect_equal(nrow(data), 7)
    expect_equal(nrow(drop_empty_rows(data, -1)), 6)
    expect_error(drop_empty_rows(data, 'VN'))
})

test_that("visit count is added", {


})

test_that("dates are fixed", {
    real <- data.frame(date = c("06/12/01", "03/21/12", "12/23/34"))
    expected <- c("2001-06-12", "2012-03-21", "1934-12-23")
    expect_identical(as.character(fix_date(real, 'date')[[1]]), expected)
})
