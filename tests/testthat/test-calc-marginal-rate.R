test_that("calc_marginal_rate works", {


  v1 <- calc_marginal_rate(total_income = 50000,
                     brackets = c(50198, 100393, 155626, 221708),
                     rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- 50000 * 0.15
  expect_equal(v1, v2)


  v1 <- calc_marginal_rate(total_income = 60000,
                           brackets = c(50198, 100393, 155626, 221708),
                           rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- (50198 * 0.15) + ((60000 - 50198) * 0.205)
  expect_equal(v1, v2)


  v1 <- calc_marginal_rate(total_income = 120000,
                           brackets = c(50198, 100393, 155626, 221708),
                           rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- (50198 * 0.15) + ((100393 - 50198) * 0.205) + ((120000 - 100393) * 0.26)
  expect_equal(v1, v2)

  v1 <- calc_marginal_rate(total_income = 160000,
                           brackets = c(50198, 100393, 155626, 221708),
                           rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- (50198 * 0.15) + ((100393 - 50198) * 0.205) + ((155626 - 100393) * 0.26) + ((160000 - 155626) * 0.29)
  expect_equal(v1, v2)


  v1 <- calc_marginal_rate(total_income = 300000,
                           brackets = c(50198, 100393, 155626, 221708),
                           rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- (50198 * 0.15) + ((100393 - 50198) * 0.205) + ((155626 - 100393) * 0.26) + ((221708 - 155626) * 0.29) + ((300000 - 221708) * 0.33)
  expect_equal(v1, v2)


  v1 <- calc_marginal_rate(total_income = 0,
                           brackets = c(50198, 100393, 155626, 221708),
                           rates = c(0.15, 0.205, 0.26, 0.29, 0.33))
  v2 <- 0
  expect_equal(v1, v2)









})
