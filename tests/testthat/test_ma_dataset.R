
context("Smoother of raw data based on moving average")




# debug(ma_dataset)

# undebug(ma_dataset)

test_that("Smoothing MA: basic case kappa == 1", {
  oriDat  <- tibble::tibble(
    time = 2010:2001,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
    )
  # oriDat
  res <- ma_dataset(oriDat, kappa=1)
  expect_equal(c(25, 15, 17, 13, 11, 9, 12, 13, 14, 10),
               as.numeric(unlist(res$res[,2])))
  expect_equal(c(23, 26, 29, 23, 17, 14, 9, 12, 11, 10),
               as.numeric(unlist(res$res[,3])))


})

test_that("Smoothing MA: kappa = 3", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  # oriDat[10:1,]


  res <- ma_dataset(oriDat, kappa=3)

  expect_equal(as.numeric(res$res[1,2]), 10   )
  expect_equal(as.numeric(res$res[1,3]),  10  )

  expect_equal(as.numeric(res$res[2,2]), (10+14+13)/3  )
  expect_equal(as.numeric(res$res[2,3]), (10+11+12)/3   )

  expect_equal(as.numeric(res$res[9,2]),(17+15+25)/3  )
  expect_equal(as.numeric(res$res[9,3]),  (29+26+23)/3   )

  expect_equal(as.numeric(res$res[10,2]), 25)
  expect_equal(as.numeric(res$res[10,3]), 23)

})


test_that("Smoothing: exceptions", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  out <- ma_dataset(oriDat, kappa=3,timeName = "pippo")
  expect_null(out$res)
  expect_equal(out$err, "Error: wrong timeName")

  # missing
  oriDat[5,3] <- NA
  out <- ma_dataset(oriDat, kappa=3,timeName = "time")
  expect_null(out$res)
  expect_equal(out$err, "Error: there are missing values.")



})


test_that("Smoothing MA: wrong kappa", {
  oriDat  <- tibble::tibble(
    time = 2001:2010,
    IT = c(10,14,13,12,9,11,13,17,15,25),
    DE = c(10,11,12,9,14,17,23,29,26,23)
  )
  out <- ma_dataset(oriDat, kappa=33,timeName = "time")
  expect_null(out$res)
  expect_equal(out$err, "Error: wrong kappa")

  out <- ma_dataset(oriDat, kappa=-1,timeName = "time")
  expect_null(out$res)
  expect_equal(out$err, "Error: wrong kappa")

})


