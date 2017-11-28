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
    data <- data.frame(
        id = rep(1:4, each = 3),
        date = as.Date(rep(c('2010-10-01', '2011-10-01', '2012-10-01'), times = 4)),
        VN = rep(1:3, times = 4)
    )

    # First three numbers
    real_visits <- getOption('PROMISE.visit.numbers')[1:3]

    expect_equal(add_visit_count(data[1:2], c('id', 'date'))$VisitCount, rep(1:3, times = 4))
    expect_equal(add_visit_count(data[1:2], c('id', 'date'))$VN, rep(real_visits, times = 4))
    expect_equal(add_visit_count(data, c('id', 'date'))$VisitCount, rep(1:3, times = 4))
})

test_that("dates are fixed", {
    real <- data.frame(date = c("06/12/01", "03/21/12", "12/23/34"))
    expected <- c("2001-06-12", "2012-03-21", "1934-12-23")
    expect_identical(as.character(fix_date(real, 'date')[[1]]), expected)
})

test_that("duplicates removed", {
    dups <- data.frame(a = c(rep(1, 3), rep(2, 3), rep(3, 3)),
                       b = c(1:3, 1, 1, 2, 1, 2, 2),
                       id = 1:9)
    no_dups_1 <- scr_duplicates(dups, c("a", "b"), "keepfirst")
    expect_equal(nrow(dups) - 2, nrow(no_dups_1))
    expect_equal(no_dups_1$id, c(1:4, 6:8))

    no_dups_2 <- scr_duplicates(dups, c("a", "b"), "keeplast")
    expect_equal(nrow(dups) - 2, nrow(no_dups_2))
    expect_equal(no_dups_2$id, c(1:3, 5:7, 9))
})
