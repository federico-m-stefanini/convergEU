
context("Checking function for dataframes and tibbles")

# debug(check_data)

test_that("Missing values present", {
  myTB2  <- tibble::tribble(
    ~time, ~veval,
    88,   1201,
    89,    NA,
    90,   998,
    91,    NA
  )
  expect_false(is.null(check_data(myTB2)$err))
  #
  myDF <- data.frame(
    time= c(99,97,98),
    veval=c(1201,NA,998));
  expect_false(is.null(check_data(myDF)$err))

  myTB  <- tibble::tribble(
    ~time, ~veval,
    88,   1201,
    89,    666,
    90,   998,
    91,    333
  )
  expect_true(is.null(check_data(myTB)$err))
})



test_that("Qualitative variables present", {
  myTB2  <- tibble::tribble(
    ~time, ~veval,
    88,   1201,
    89,    NA,
    90,   998,
    91,    NA
  )
  myTB2$veval <- factor(myTB2$veval)
  expect_false(is.null(check_data(myTB2)$err))
  #
  myDF <- data.frame(
    time= c(99,97,98),
    veval=factor(c(1201,NA,998)));
  expect_false(is.null(check_data(myDF)$err))
})




test_that("String variables present", {
  myTB2  <- tibble::tribble(
    ~time, ~veval,
    88,   "1201",
    89,    NA,
    90,   "998",
    91,    NA
  )
  expect_false(is.null(check_data(myTB2)$err))
  #
  myDF <- data.frame(
    time= c(99,97,98),
    veval=c("1201",NA,"998"),
    stringsAsFactors = FALSE
   )
  expect_false(is.null(check_data(myDF)$err))
})





test_that("Time out of order", {
  myTB2  <- tibble::tribble(
    ~ttyr, ~IT,
    90,   1201,
    89,   22,
    88,   998,
    91,   32
  )
  expect_true(
    check_data(myTB2,"ttyr")$err == "Error: time variable is not ordered."
    )

  myTB2  <- tibble::tribble(
    ~ttyr, ~IT,
    88,   1201,
    89,   22,
    90,   998,
    91,   32
  )
  expect_null(check_data(myTB2,"ttyr")$err)

})



