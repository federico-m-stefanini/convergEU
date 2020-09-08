
context("Unweighted averaging over a cluster of countries")


# debug(average_clust)

test_that("Check the average value", {
  testTB <- tibble::tibble(TrueTime =2002:2011)
  valo <- 1
  for(aux in convergEU_glb()$EUcodes$paeS)testTB[,aux] <-  1
  for(aux in 1:10)testTB[aux,-1] <-  aux

  res <- average_clust(testTB,timeName = "TrueTime",cluster="EU28")$res$EU28
  for(aux in 1:10) expect_equal(res[aux], aux);
  })




test_that("Test if checking works", {
  testTB<- tibble::tibble(TrueTime =2002:2011)
  valo <- 1
  for(aux in convergEU_glb()$EUcodes$paeS)testTB[,aux] <-  1
  for(aux in 1:10)testTB[aux,-1] <-  aux
  testTB2 <- testTB
  # first
  testTB <-  testTB[,-7]
  res <- average_clust(testTB,timeName = "TrueTime",cluster="EU28")
  expect_null(res$res)
  # second
  testTB2 <- testTB2[,-1]
  res <- average_clust(testTB2,timeName = "TrueTime",cluster="EU28")
  expect_null(res$res)
  #
  res <- average_clust(testTB,timeName = "TrueTime",cluster="EU")
  expect_null(res$res)

})



test_that("Test on EU12", {
  testTB <-  emp_20_64_MS#[,-7]
  MS12 <- unlist(convergEU_glb()[["EU12"]]$memberStates[,2])
  res1 <-  mean(unlist(emp_20_64_MS[1,MS12]))
  res15 <-  mean(unlist(emp_20_64_MS[15,MS12]))
  # debug(average_clust)
  res <- average_clust(testTB,timeName = "time",cluster="EU12")
  expect_equal(res$res$EU12[1], res1)
  expect_equal(res$res$EU12[15], res15)
})


test_that("Test on EU27", {
  testTB <-  emp_20_64_MS#[,-7]
  MS <- unlist(convergEU_glb()[["EU27"]]$memberStates[,2])
  res1 <-  mean(unlist(emp_20_64_MS[1,MS]))
  res15 <-  mean(unlist(emp_20_64_MS[15,MS]))
  # debug(average_clust)
  res <- average_clust(testTB,timeName = "time",cluster="EU27")
  expect_equal(res$res$EU27[1], res1)
  expect_equal(res$res$EU27[15], res15)
})





test_that("Test on custom-all countries", {
  testTB <-  emp_20_64_MS#[,-7]
  media1 <-  mean(unlist(testTB[1,-1]))
  media10 <-  mean(unlist(testTB[10,-1]))
  names(testTB)<- c("time",paste("GG-",1:28,sep=""))
  # debug(average_clust)
  res <- average_clust(testTB,timeName = "time",cluster="all")
  expect_equal(res$res$all[1], media1)
  expect_equal(res$res$all[10], media10)
})
