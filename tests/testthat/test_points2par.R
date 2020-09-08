
context("From two points to line straight line parameters")





test_that("Convert", {
  myTB <- dplyr::tribble(
    ~time , ~indic,
    1    ,   25,
    10   ,   5,
    1,       10,
    10,       3
    )

  resparamIT <- points2par(as.numeric(myTB[1,]),as.numeric(myTB[2,]))
  #resparamIT
  model <- lm(indic~time,data=myTB[1:2,])
  expect_equal(as.numeric(model$coefficients) , resparamIT)

})




