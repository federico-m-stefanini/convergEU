
context("Country ranking based on levels")



#  debug(country_ranking)


test_that("Ranking highBest", {

  myTB  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )


  res <- country_ranking(myTB,timeName="time", time_0=NA,time_t=NA,
                         typeInd="highBest" )
  expect_equal(res$err, "Error: declared time variable absent.")

  res <- country_ranking(myTB,timeName="years", time_0=NA,time_t=NA,
                         typeInd="highBest" )


  expect_equal(as.numeric(unlist(res$res[,2])),c(1,1,2,1))
  expect_equal(as.numeric(unlist(res$res[,3])),c(2,2,1,2))
  expect_equal(as.numeric(unlist(res$res[,4])),c(3,3,3,3))

})


test_that("Ranking lowBest", {

  myTB  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,    12, 8,  3,
    88,    12,24, 36,
    89,    11,11, 22,
    91,    111,111,111
  )


  res <- country_ranking(myTB,timeName="time", time_0=NA,time_t=NA,
                         typeInd="lowBest" )
  expect_equal(res$err, "Error: declared time variable absent.")

  res <- country_ranking(myTB,timeName="years", time_0=NA,time_t=NA,
                         typeInd="lowBest" )


  expect_equal(as.numeric(unlist(res$res[,2])),c(1,1,3,1 ))
  expect_equal(as.numeric(unlist(res$res[,3])),c(2,1,2,1 ))
  expect_equal(as.numeric(unlist(res$res[,4])),c(3,3,1,1 ))

})


