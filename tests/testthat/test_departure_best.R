
context("Departure of an  indicator from the best performer")


# debug( )

test_that("Indicator highBest", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    0.1,
    2003,     1.3,   2.9,    1.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
   )
  mySTB <- departure_best(testTB,timeName="time",indiType = "highBest")
  bestVals <- c(3.9,4.2,2.9,2.9,4.1,4.0)
  differaw <- testTB[-1] - cbind(bestVals,bestVals,bestVals)
  for(auxR in 1:6){
    for(auxC in 1:3){
      expect_equal(as.numeric(mySTB$res$raw_departures[auxR,auxC+1]),
                   differaw[auxR,auxC])
    }
  }



  # cumulated values
  for(aux in 1:3){
    expect_equal(
      as.numeric(mySTB$res$cumulated_dif[aux]) ,  sum(differaw[,aux])
    )
  }

})

# debug( )



test_that("Indicator lowBest", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    0.1,
    2003,     1.3,   2.9,    1.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )
  mySTB <- departure_best(testTB,timeName="time",indiType = "lowBest")
  bestVals <- c(0.8,1.2,0.1,1.0,1.2,1.2)
  differaw <- cbind(bestVals,bestVals,bestVals) - testTB[-1]
  for(auxR in 1:6){
    for(auxC in 1:3){
      expect_equal(as.numeric(mySTB$res$raw_departures[auxR,auxC+1]),
                   differaw[auxR,auxC])
    }
  }
  # cumulated values
  for(aux in 1:3){
     expect_equal(
       as.numeric(mySTB$res$cumulated_dif[aux]) ,  sum(differaw[,aux])
     )
   }
})



