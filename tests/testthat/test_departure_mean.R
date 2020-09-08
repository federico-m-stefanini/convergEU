
context("Departure of an  indicator from a given average")



# debug(departure_mean)

test_that("Simplest test on results", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
   )
  mySTB <- sigma_conv(testTB,timeName="time")
  res <-  departure_mean(oriTB=testTB, sigmaTB=mySTB$res,timeName="time")
  expect_equal(sum(res$res$departures$countryA == -1),6)
  expect_equal(sum(res$res$departures$countryC == 1),6)
  expect_equal(sum(res$res$departures$countryB == 0),6)
  # component calculations
  testvec <- c(0.8,   2.7,    3.9)
  sigma <- pop_var(testvec)
  media <-  mean(testvec)
  expect_equal(mySTB$res$stdDev[1],sigma$popsd)
  expect_equal(mySTB$res$mean[1],media)
})

# debug(departure_mean)




test_that("Change time name", {

    testTB <- dplyr::tribble(
          ~TTtime, ~countryA ,  ~countryB,  ~countryC,
          2000,     0.8,   2.7,    3.9,
          2001,     1.2,   3.2,    4.2,
          2002,     0.9,   2.9,    4.1,
          2003,     1.3,   2.9,    4.0,
          2004,     1.2,   3.1,    4.1,
          2005,     1.2,   3.0,    4.0
   )
  mySTB <- sigma_conv(testTB,timeName="TTtime")
  res <-  departure_mean(oriTB=testTB,
                         sigmaTB=mySTB$res,timeName="TTtime")
  expect_equal(sum(res$res$departures$countryA == -1),6)
  expect_equal(sum(res$res$departures$countryC == 1),6)
  expect_equal(sum(res$res$departures$countryB == 0),6)
})






test_that("Test of eurofound dataset, scrambled, further statistics.", {

  data(emp_20_64_MS)
  testTB <-  emp_20_64_MS[c(15:10,1:7,8:9),]
  mySC <- sigma_conv(testTB)
  res <- departure_mean(oriTB = testTB, sigmaTB = mySC$res)
  test1 <- sqrt(apply(res$res$squaredContrib,1,sum)/28)
  test2 <- apply(res$res$devianceContrib,1,sum)
  # mySTB
  expect_equal(test2,rep(100,15))
  expect_equal(test1,mySC$res$stdDev)
})





