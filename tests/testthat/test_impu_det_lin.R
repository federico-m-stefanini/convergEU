
context("Imputation of missing values")




# debug(impu_det_lin)


test_that("Simplest Imputation of one missing between two observed values", {

  res <- impu_det_lin(timeIni= 88,
                      timeEnd = 90,
                      timeDelta = 89,
                      indicIni = 120,
                      indicFin = 100);

  expect_equal(as.numeric(res[1,2]), 110,tolerance=0.0001)
  expect_true(as.logical(res[1,3]))
})






test_that("Multiple Imputation of  missing", {
  res <- impu_det_lin(timeIni= 90,
                      timeEnd = 93,
                      timeDelta=c(91,92),
                      indicIni = 100,
                      indicFin = 108);

  expect_equal(as.numeric(res[1,1]), 91)
  expect_equal(as.numeric(res[2,1]), 92)
  #expect_equal(as.numeric(res[1,2]), 100)
  #expect_equal(as.numeric(res[4,2]), 108)
  expect_equal(as.numeric(res[1,2]), -140.000000 + 2.666667 * 91, tolerance = .0001)
  expect_equal(as.numeric(res[2,2]), -140.000000 + 2.666667* 92, tolerance = .0001)

  expect_true(as.logical(res[1,3]))
  expect_true(as.logical(res[2,3]))

})




test_that("Multiple Imputation of  missing with delta > 1", {
  res <- impu_det_lin(timeIni= 2000,
                      timeEnd = 2015,
                      timeDelta=seq(2005,2010,5),
                      indicIni = 100,
                      indicFin = 108);

  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 3)
  expect_equal(as.numeric(res[1,2]), -966.6666667 + 0.5333333  * 2005, tolerance = .0001)
  expect_equal(as.numeric(res[2,2]), -966.6666667 + 0.5333333 * 2010, tolerance = .0001)

  expect_true(as.logical(res[1,3]))
  expect_true(as.logical(res[2,3]))

})

