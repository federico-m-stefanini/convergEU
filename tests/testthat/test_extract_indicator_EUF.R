
context("Extraction of a indicator dataset from Eurofound database.")



# debug(extract_indicator_EUF)

test_that("Simple  extraction first indicator", {
  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=2003,
    toTime=2015,
    gender= c("Total","Females","Males")[1])
  # myTB
  expect_equal(nrow(myTB$res),3 )
  expect_equal(ncol(myTB$res),29 )
})



test_that("Simple  extraction last indicator", {
  myTB <- extract_indicator_EUF(
    indicator_code = "exposdiscr_p", #Code_in_database
    fromTime=2003,
    toTime=2016,
    gender= c("Total","Females","Males")[1])
  # myTB
  expect_equal(nrow(myTB$res),3 )
  expect_equal(ncol(myTB$res),29 )
})



test_that("Extraction Females, first indicator", {
  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime = 1998,
    toTime = 2016,
    gender = c("Total","Females","Males")[2])
  # myTB
  expect_equal(nrow(myTB$res),4 )
  expect_equal(ncol(myTB$res),29 )

  expect_equal(sum(myTB$res$sex == "Females"),4 )
  expect_equal(sum(myTB$res$time == c(2003,2007,2011,2016)), 4 )

  # countries in output
  nomiOut <- names(myTB$res)
  for (aux in  convergEU_glb()$EU27$memberStates$codeMS) {
     expect_true(aux %in% nomiOut)
  }

})



test_that("Extraction Males of  EU12, first indicator", {
  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=1960,
    toTime=2016,
    gender= c("Total","Females","Males")[3],
    countries= convergEU_glb()$EU12$memberStates$codeMS)
  # myTB
  expect_equal(nrow(myTB$res),4 )
  expect_equal(ncol(myTB$res),14 )

  expect_equal(sum(myTB$res$sex == "Males"),4 )
  expect_equal(sum(myTB$res$time == c(2003,2007,2011,2016)), 4 )

  # countries in output
  nomiOut <- names(myTB$res)
  for (aux in  convergEU_glb()$EU12$memberStates$codeMS) {
    expect_true(aux %in% nomiOut)
  }

})




test_that("Extraction with bad input", {
  myTB <- extract_indicator_EUF(
    indicator_code = "GuglielmoTell", #Code_in_database
    fromTime=1960,
    toTime=2016,
    gender= c("Total","Females","Males")[3],
    countries= convergEU_glb()$EU12$memberStates$codeMS)
  # myTB
  expect_true(myTB$err == "Error: indicator not included into the Eurofound database.")

  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=1960,
    toTime=2016,
    gender= "drag",
    countries= convergEU_glb()$EU12$memberStates$codeMS)
  # myTB
  expect_true(myTB$err == "Error: Unknown gender selection.")


  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=160,
    toTime=2016,
    gender= "Total",
    countries= convergEU_glb()$EU12$memberStates$codeMS)
  # myTB
  expect_true(myTB$err == "Error: wrong time window.")

  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=2000,
    toTime=216,
    gender= "Total",
    countries= convergEU_glb()$EU12$memberStates$codeMS)
  # myTB
  expect_true(myTB$err == "Error: wrong time window.")


  myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=2000,
    toTime=2016,
    gender= "Total",
    countries= c("IT","HH"))
  # myTB
  expect_true(myTB$err == "Error: at least one country not available.")
})


