
context("From time series to parameters of straight lines")




#  debug(ts_parlin)


test_that("Basic calulations", {

  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )

  curcountry <- 2
  res <- ts_parlin(testTB[,c(1,curcountry)])
  alternative <- points2par(c(2000,0.8)  , c(2001,1.2 ))
  expect_equal(as.numeric(res[1,2]), alternative[1])
  expect_equal(as.numeric(res[1,3]), alternative[2])

  alternative <- points2par(c(2001,1.2)  , c(2002,0.9 ))
  expect_equal(as.numeric(res[2,2]), alternative[1])
  expect_equal(as.numeric(res[2,3]), alternative[2])

  alternative <- points2par(c(2004,1.2)  , c(2005,1.2 ))
  expect_equal(as.numeric(res[5,2]), alternative[1])
  expect_equal(as.numeric(res[5,3]), alternative[2])


  beta1s <-  (1.3-0.9)/(1)
  beta0s <-  0.9-beta1s*2002
  curcountry <- 2
  res <- ts_parlin(testTB[,c(1,curcountry)])
  expect_equal(as.numeric(res[3,2]), beta0s )
  expect_equal(as.numeric(res[3,3]), beta1s )


  curcountry <- 4
  res <- ts_parlin(testTB[,c(1,curcountry)])
  alternative <- points2par(c(2000,3.9)  , c(2001,4.2 ))
  expect_equal(as.numeric(res[1,2]), alternative[1])
  expect_equal(as.numeric(res[1,3]), alternative[2])

  alternative <- points2par(c(2001,4.2)  , c(2002,4.1 ))
  expect_equal(as.numeric(res[2,2]), alternative[1])
  expect_equal(as.numeric(res[2,3]), alternative[2])

  alternative <- points2par(c(2004,4.1)  , c(2005,4.0 ))
  expect_equal(as.numeric(res[5,2]), alternative[1])
  expect_equal(as.numeric(res[5,3]), alternative[2])

})




