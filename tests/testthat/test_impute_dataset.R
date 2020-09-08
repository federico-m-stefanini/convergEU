
context("Imputation of a dataset.")


# debug(impute_dataset )

startDF <- structure(list(time = c(1975, 1980, 1985, 1990, 1995, 2000, 2005,
                                   2010, 2015),
sex = c("Total", "Total", "Total", "Total", "Total",
"Total", "Total", "Total", "Total"), BE = c(NA, NA, 7.67059697183018,
8.39902672836614, 11.6420901497116, 11.4898630114755, 6.14325603623,
11.0486600722873, 9.6458024531154), DE = c(2.91385788275978,
5.45361566316032, 8.26163094605195, 6.87366978303647, 7.42214488173531,
   7.31331872868227, 4.53331166450983, NA, NA), DK = c(4.13276534633507,
5.55418437990934, 5.26311623211179, 5.73733111716602, 3.52058698718078,
NA, 5.66327519820799, 4.9343679607065, 5.3998680859727), EL = c(7.41448630473462,
7.04744927241176, 6.6723829986381, 5.79629936266576, 9.98384123673372,
10.1620218962284, 7.88730971293409, 8.31873540306173, 7.88033601141932
), ES = c(2.27702751875014, 4.91765932318728, 3.45557478141297,
3.38821267246359, 6.78610949682977, 3.55640506605875, 2.13295508178973,
4.47450625034023, 4.8824931855898), FR = c(7.15674676237259,
9.59107308837682, 13.1154576798744, 4.17465292619173, 11.1802572077429,
13.1813377007219, 5.07570212943111, 10.6354658045164, 11.1552791151061
), IE = c(4.31193855109117, 7.37638841869126, 7.10741252564876,
4.62162991503677, 3.31582364646534, 7.50192394523001, 6.75402735227212,
5.32184638801779, 6.83387096664919), IT = c(2.99301634591272,
5.00683742480599, NA, NA, 1.16985248656258, NA, NA, NA, 6.74960202015997
), LU = c(10.1760469285058, 11.0310724978212, 13.3452410149571,
8.57330890616724, 13.2306666632525, 11.7410701062714, 8.85614483163083,
10.8837482270195, 13.5722455816527), NL = c(10.5126025991194,
4.84773525059953, 10.3134784463925, 6.15035762872133, 5.56554242991641,
11.8891036965085, 8.5080577826738, 6.02355967648126, 12.2432573687417
), PT = c(5.93857463643058, 4.44493228178682, 2.76293541048128,
6.61852053909666, 4.4942823059878, 2.83084942935441, 4.97292686098358,
3.875542471234, 3.6127015317272), UK = c(4.24065997485763, 3.53455021438049,
7.58245378013761, 3.1000339101231, 4.2440206026272, 8.55947250130647,
5.25858312157674, 5.22847788800346, 6.94056358633624)),
class = "data.frame", row.names = c(NA, -9L))


# debug(impute_dataset )


test_that("Simplest Imputation using option cut", {
  myTB2  <- tibble::as_tibble(startDF)
  # myTB2[["IT"]]
  toBeProcessed <- c( "IT","BE","DK")
  resImpu <- impute_dataset(myTB2, countries=toBeProcessed,
                            #deltaTime=5,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[1],
                            headMiss = c("cut", "constant")[1])

  expect_null(resImpu$err)
  expect_equal(dim(resImpu$res),c(7,14) )
  # the only missing in DK
  imputed <- sum(myTB2[["DK"]][c(5,7)])/2
  expect_equal( resImpu$res[["DK"]][4], imputed )
  })


test_that("Imputation using option constant", {
  myTB2  <- tibble::as_tibble(startDF)
  toBeProcessed <- c( "IT","BE", "DE", "DK","UK")
  resImpu <- impute_dataset(myTB2, countries=toBeProcessed,
                            #deltaTime=5,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[2],
                            headMiss = c("cut", "constant")[2])

  expect_null(resImpu$err)
  expect_equal(dim(resImpu$res),c(9,14) )
  # the only missing in DK
  #imputed <- (as.numeric(myTB2[5,"DK"])+as.numeric(myTB2[7,"DK"]))/2
  imputed <- sum(myTB2[["DK"]][c(5,7)])/2
  expect_equal( resImpu$res[["DK"]][6], imputed )

  expect_equal(resImpu$res[["DK"]][6], imputed )
  # tail BE
  expect_equal(resImpu$res[["BE"]][1:2], myTB2[["BE"]][c(3,3)] )
  # head DE
  expect_equal(unlist(resImpu$res[8:9,"DE"]), unlist(myTB2[c(7,7),"DE"]) )
  expect_equal( resImpu$res[["DE"]][8:9] , myTB2[["DE"]][c(7,7)]  )

})


test_that("Imputation into a chunk of missing", {
  myTB2  <- tibble::as_tibble(startDF)
  toBeProcessed <- c( "IT","BE", "DE", "DK","UK")
  resImpu <- impute_dataset(myTB2, countries=toBeProcessed,
                            #deltaTime=5,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[2],
                            headMiss = c("cut", "constant")[2])

  expect_null(resImpu$err)
  expect_equal(dim(resImpu$res),c(9,14) )

  # several missing
  y1 <- myTB2[["IT"]][c(2)]
  y2 <- myTB2[["IT"]][c(5)]
  years <- c(1980,1995)
  beta1 <- (y2-y1)/diff(years)
  beta0 <- as.numeric(y2-beta1*years[2])
  imputedOBS <- beta0+beta1*years
  imputedHand <- beta0+beta1*c(1985,1990)
  expect_equal(resImpu$res[["IT"]][3:4], imputedHand)
  # other chunk
  y1 <-  myTB2[["IT"]][c(5)]
  y2 <-  myTB2[["IT"]][c(9)]
  years <- c(1995,2015)
  beta1 <- (y2-y1)/diff(years)
  beta0 <- as.numeric(y2-beta1*years[2])
  imputedOBS <- beta0+beta1*years
  imputedHand <- beta0+beta1*c(2000,2005,2010)
  expect_equal(resImpu$res[["IT"]][6:8], imputedHand)
  # what about a country without missing?
  expect_equal( myTB2[["IT"]][c(1,2,5,9)],
               resImpu$res[["IT"]][c(1,2,5,9)])
  # what about original values near chunks of missing?
  expect_equal(myTB2[["UK"]], resImpu$res[["UK"]])

})




test_that("Imputation into a chunk with delta time  equal to 1", {
  myTB2  <- tibble::as_tibble(startDF)[,c("time","IT","DK")]
  myTB2$time <- 2000:2008
  resImpu <- impute_dataset(myTB2, countries=c("IT","DK"),
                 #deltaTime=1,
                 timeName = "time",
                 tailMiss = c("cut", "constant")[2],
                 headMiss = c("cut", "constant")[2])

  expect_null(resImpu$err)
  expect_equal(dim(resImpu$res),c(9,3) )

  # several missing
  y1 <-  myTB2[["IT"]][c(2)]
  y2 <-  myTB2[["IT"]][c(5)]
  years <- c(2001,2004)
  beta1 <- (y2-y1)/diff(years)
  beta0 <- as.numeric(y2-beta1*years[2])
  imputedOBS <- beta0+beta1*years
  imputedHand <- beta0+beta1*c(2002,2003)
  expect_equal(resImpu$res[["IT"]][3:4], imputedHand)
  # other chunk
  y1 <-  myTB2[["IT"]][c(5)]
  y2 <-  myTB2[["IT"]][c(9)]
  years <- c(2004,2008)
  beta1 <- (y2-y1)/diff(years)
  beta0 <- as.numeric(y2-beta1*years[2])
  imputedOBS <- beta0+beta1*years
  imputedHand <- beta0+beta1*c(2005:2007)
  expect_equal(resImpu$res[["IT"]][6:8], imputedHand)

})

##  debug(impute_dataset)


test_that("Imputation into chunk  missing, timeName not standard", {
  myTB2  <- tibble::as_tibble(startDF)
  toBeProcessed <- names(myTB2)[-c(1:2)]#c( "IT","BE", "DE", "DK","UK")
  resImpu1 <- impute_dataset(myTB2, countries=toBeProcessed,
                            #deltaTime=5,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[2],
                            headMiss = c("cut", "constant")[2])
  myTB2 <- dplyr::rename(myTB2, SANtime = time)
  myTB2 <- dplyr::arrange(myTB2, -SANtime )

  #toBeProcessed <- c( "IT","BE", "DE", "DK","UK")
  resImpu <- impute_dataset(myTB2, countries=toBeProcessed,
                            #deltaTime=5,
                            timeName = "SANtime",
                            tailMiss = c("cut", "constant")[2],
                            headMiss = c("cut", "constant")[2])

  expect_null(resImpu$err)
  expect_equal(dim(resImpu$res),c(9,14) )


  myres1 <- dplyr::select(resImpu1$res,-time,-sex)
  myres2 <- dplyr::select(resImpu$res,-SANtime,-sex)
  dimensioni <-  dim(myres1)
  for(auxR in 1:dimensioni[1]){
    for(auxC in 1:dimensioni[2]){
      expect_equal(myres1[[auxC]][auxR],  myres2[[auxC]][auxR] )
    }
  }


})
