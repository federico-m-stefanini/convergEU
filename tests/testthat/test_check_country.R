
context("Test the presence of countries in a dataset")





# debug(check_country)
test_that("Check presence EU28", {

  res <- check_country(emp_20_64_MS, clusterCode="EU28")
  expect_true(res$res)
})

test_that("Check presence EU12", {

  res <- check_country(emp_20_64_MS, clusterCode="EU12")
  expect_true(res$res)
})


test_that("Check absence for EU28", {

  res <- check_country(emp_20_64_MS[,-(6:8)], clusterCode="EU28")
  expect_false(res$res)
})


test_that("Check wrong label", {

  res <- check_country(emp_20_64_MS, clusterCode="EUEU28")
  expect_null(res$res)
})

