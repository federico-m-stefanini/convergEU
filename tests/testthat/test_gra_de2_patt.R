
context("Basic computation gradient for patterns")



#  debug(gra_de2_patt)


test_that("Basic checks on all patterns", {

  # parallel lines
  vaEU <- c(5,7)
  vaMS <- c(6,8)
  vaTime <- c(1999,2000)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,13)

  vaEU <- c(7,2)
  vaMS <- c(9,4)
  vaTime <- c(2009,2010)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,15)

  vaEU <- c(9,9)
  vaMS <- c(12,12)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,14)


  # parallel lines
  vaEU <- c(5,7)
  vaMS <- c(6,8)-2
  vaTime <- c(1999,2000)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,18)

  vaEU <- c(7,2)
  vaMS <- c(9,4)-3
  vaTime <- c(2009,2010)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,16)

  vaEU <- c(9,9)
  vaMS <- c(12,12)-6
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,17)

  # crossing
  vaEU <- c(123,80)
  vaMS <- c(75,95)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,20)

  # Eurofound published patterns
  vaTime <- c(2009,2010)

  vaEU <- c(100 , 120)
  vaMS <- c( 50, 90)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 1)

  vaEU <- c( 50, 90)
  vaMS <- c( 100, 120)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 2)

  vaEU <- c( 50, 90)
  vaMS <- c( 120, 100)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 3)

  vaEU <- c( 30, 40)
  vaMS <- c( 50, 120)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 4)

  vaEU <- c( 90, 130)
  vaMS <- c( 50, 70)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 5)

  vaEU <- c( 90, 160)
  vaMS <- c( 80, 55)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 6)

  vaEU <- c( 100, 50)
  vaMS <- c( 120, 100)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 7)

  vaEU <- c( 70, 30)
  vaMS <- c( 80, 120)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 8)

  vaEU <- c( 100, 90)
  vaMS <- c( 80, 20)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 9)

  vaEU <- c( 100, 90)
  vaMS <- c( 170, 120)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 10)

  vaEU <- c( 170, 120)
  vaMS <- c( 40, 110)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res,11 )

  vaEU <- c( 160, 100)
  vaMS <- c( 120, 90)
  res <- gra_de2_patt(vaEU,vaMS,vaTime)
  expect_equal(res, 12)


})




