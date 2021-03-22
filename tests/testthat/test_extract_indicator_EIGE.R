
context("Extraction of a indicator dataset from EIGE database.")



# debug(extract_indicator_EIGE)

test_that("Simple  extraction of METADATA", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "METADATA", #Code_in_database
    fromTime=2003,
    toTime=2015)
  # myTB
  expect_equal(myTB$res$`Worksheet name`[1], "INDEX" )
  expect_equal(ncol(myTB$res),7 )
})


test_that("Simple  extraction of types", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "METADATA", #Code_in_database
    fromTime=2003,
    toTime=2015,
    type_flag=TRUE)
  # myTB
  expect_equal(sum(is.na(myTB$res)),2 )
  expect_equal(length(myTB$res),2 )
})


test_that("Simple  extraction of types", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "MINISTER", #Code_in_database
    fromTime=2003,
    toTime=2015,
    type_flag=TRUE)
  # myTB
  expect_equal(myTB$res[1], "highBest")
  expect_equal(myTB$res[2],  "maximise")
})




test_that("Simple  extraction  indicator  WORK", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "WORK", #Code_in_database
    fromTime=2000,
    toTime=2025)
  expect_equal(ncol(myTB$res),28 )
})


test_that("Simple  extraction  indicator  AZIMUTH", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "AZIMUTH", #Code_in_database
    fromTime=2000,
    toTime=2025)
  expect_equal(myTB$err, "Error: data not available." )
})


test_that("Simple  extraction  indicator  WORK subset of years", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "WORK", #Code_in_database
    fromTime=2012,
    toTime=2015)
  # myTB
  expect_equal(dim(myTB$res)[2], 28 )
})



test_that("Simple  extraction  indicator  WORK unavailable subset of years", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "WORK", #Code_in_database
    fromTime=1988,
    toTime=1998)
  # myTB
  expect_equal(dim(myTB$res)[1], 0 )
})


test_that("Simple  extraction  indicator  MINISTER less countries", {
  myTB <- extract_indicator_EIGE (
    indicator_code = "MINISTER", #Code_in_database
    fromTime=2012,
    toTime=2016,
    c("IT","AT"))
  # myTB
  expect_equal(dim(myTB$res)[1], 5 )
})

 