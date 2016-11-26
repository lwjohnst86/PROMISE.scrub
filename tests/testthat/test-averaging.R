context("Averaging variables")

test_that("averaging goes from two or more to one variable", {
    data <- data.frame(
        SID = 1:10,
        VN = 1,
        Wt1 = sample(50:100, 10),
        Wt2 = sample(50:100, 10)
    )
    exp_ave <- rowMeans(data[c('Wt1', 'Wt2')])
    real_ave <- average_variables(data, 'Wt[12]')
    expect_length(real_ave, 3)
    expect_identical(names(real_ave), c("SID", "VN", "Wt"))
    expect_equal(real_ave[['Wt']], exp_ave)

})
