
context("Checking population statistics")



test_that("Population statistics are correct", {
  myTB2  <- c(65, 70, 70,70, 75)
  res <- pop_var(myTB2)

  expect_equal(sd(myTB2)*sqrt(4/5), sqrt(50/5))
  expect_equal(var(myTB2)*4/5, 50/5)

  myTB3 <- c(65, 70, 70,70, 75,NA,NA)
  res3 <- pop_var(myTB3)
  expect_equal(res3$popvar, 50/5)
  expect_equal(res3$popsd, sqrt(50/5))
})


