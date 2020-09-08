
context("Absolute change each year")
# library(convergEU)
# require(tibble)
# require(devtools)



# debug(departure_mean)

test_that("Basic tests on absolute change.", {
  testTB <- dplyr::tribble(
    ~years, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
   )
  mySTB <- abso_change(testTB,2000,2005, TRUE,"years")
  for(auxR in 2:6){
    for(auxC in 2:4){
      diffe <- as.numeric(testTB[auxR,auxC] - testTB[auxR-1,auxC])
      expect_equal(diffe,as.numeric(mySTB$res$abso_change[auxR-1,auxC]))
    }
  }
  # diffe abs totali
  expect_equal(diff(testTB$countryA),mySTB$res$abso_change[,2] )
  expect_equal(diff(testTB$countryB),mySTB$res$abso_change[,3] )
  expect_equal(diff(testTB$countryC),mySTB$res$abso_change[,4] )
  ##
  expect_equal(sum(abs(diff(testTB$countryA))), as.numeric(mySTB$res$sum_abs_change[1]))
  expect_equal(sum(abs(diff(testTB$countryB))), as.numeric(mySTB$res$sum_abs_change[2]))
  expect_equal(sum(abs(diff(testTB$countryC))), as.numeric(mySTB$res$sum_abs_change[3]))
  ##
  expect_equal(sum(abs(diff(testTB$countryA)))/5, as.numeric(mySTB$res$average_abs_change[1]))
  expect_equal(sum(abs(diff(testTB$countryB)))/5, as.numeric(mySTB$res$average_abs_change[2]))
  expect_equal(sum(abs(diff(testTB$countryC)))/5, as.numeric(mySTB$res$average_abs_change[3]))

})




test_that("Tests for failures in absolute change.", {
  testTB <- dplyr::tribble(
    ~years, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )
  mySTB <- abso_change(testTB,
                       time_0 = 200,
                       time_t = 2005,
                       all_within=TRUE,
                       timeName = "years")
  expect_null(mySTB$res)
  #
  mySTB <- abso_change(testTB,
                       time_0 = 2001,
                       time_t = 200,
                       all_within=TRUE,
                       timeName = "years")
  expect_null(mySTB$res)
  #
  mySTB2 <- abso_change(testTB,2000,2005, F,"ttiimmee")
  expect_null(mySTB2$res)
  mySTB2 <- abso_change(testTB,2004,2001, F,"years")
  expect_equal(mySTB2$err, "Error: wrong selected time window.")

})


