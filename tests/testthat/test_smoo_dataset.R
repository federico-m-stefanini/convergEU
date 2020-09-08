
context("Smoother of raw data")


# debug(smoo_dataset)

# undebug(smoo_dataset)

test_that("Smoothing: special case", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
    )
  # oriDat
  res <- smoo_dataset(oriDat[,-1], leadW=1)
  expect_equal(res[,1], as.numeric(unlist(oriDat[,2])))
  expect_equal(res[,2], as.numeric(unlist(oriDat[,3])))


})

test_that("Smoothing: weight   0.5", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  #oriDat
  res <- smoo_dataset(oriDat[,-1], leadW=0.5)
  expect_equal(res[1,1], 10*0.5+14*0.5  )
  expect_equal(res[2,1], 10*0.25+14*0.5+0.25*13  )
  expect_equal(res[5,1], 12*0.25+9*0.5+0.25*11  )
  expect_equal(res[10,1], 0.5*15 + 0.5*25  )
  #
  expect_equal(res[1, 2], 10*0.5 + 11*0.5  )
  expect_equal(res[2, 2], 10*0.25+ 11*0.5+0.25*12  )
  expect_equal(res[5, 2], 9*0.25+ 14*0.5+0.25*17  )
  expect_equal(res[10,2], 26*0.5 + 0.5*23  )


})


test_that("Smoothing: exceptions", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  res <- smoo_dataset(oriDat[,-1], leadW=1.5)
  expect_true(is.na(res))
  res <- smoo_dataset(oriDat[,-1], leadW=0)
  expect_true(is.na(res))
  res <- smoo_dataset(oriDat[,-1], leadW=-0.5)
  expect_true(is.na(res))
  # missing
  oriDat[3,3] <- NA
  res <- smoo_dataset(oriDat[,-1], leadW=-0.5)
  expect_true(is.na(res))

})


test_that("Smoothing: tibble in output?", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  res <- smoo_dataset(oriDat[,-1], leadW=.5,timeTB= dplyr::select(oriDat,time))
  expect_true(tibble::is_tibble(res))
  res <- smoo_dataset(oriDat[,-1], leadW=.5)
  expect_true(is.matrix(res))
})


