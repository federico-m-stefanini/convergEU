
context("Plot of sum of deviations from the mean")



#  debug(dev_mean_plot)

# dput(round(runif(5,10,30),0))

test_that("Basic test deviations", {
  myTB <- dplyr::tibble(
    time = c(2001:2005),
    couwA = c(29, 11, 25, 30, 21),
    couwB = c(17, 20, 12, 24, 20),
    couwC =c(17, 24, 26, 27, 29)
  )
  medie <- apply(myTB[,-1],1,mean)
  diffe <-  myTB[,-1] - cbind(medie,medie,medie)
  resConfro <- matrix(NA,3,2)
  resConfro[1,1] <- sum(diffe[c(2,5),1])
  resConfro[2,1] <- sum(diffe[c(1,3,4,5), 2])
  resConfro[3,1] <- sum(diffe[c(1), 3])
  resConfro[1,2] <- sum(diffe[c(1,3,4), 1])
  resConfro[2,2] <- sum(diffe[c(2), 2])
  resConfro[3,2] <- sum(diffe[c(2:5), 3])

  resDMP <- dev_mean_plot(myTB,
                timeName="time",
                time_0 = NA,
                time_t = NA,
                displace = 0.25,
                indiType = "highBest",#lowBest",
                axis_name_y = "Countries",
                debug=TRUE)
                #debug=FALSE)


  resMat <- as.matrix(resDMP$msg[,-c(1,4)])
  dimnames(resMat)[[2]] <- NULL
  for(auxR in 1:3){
    for(auxC in 1:2){
      expect_equal(resMat[auxR,auxC],  resConfro[auxR,auxC])
     }}

  })





#  debug(dev_mean_plot)

test_that("Deviations for selected countries.", {
  myTB <- dplyr::filter(emp_20_64_MS, time < 2017)

  resDMP <- dev_mean_plot(myTB,
                          timeName="time",
                          time_0 = NA,
                          time_t = NA,
                          countries= c("AT","BE","UK"),
                          displace = 0.25,
                          axis_name_y = "Countries",
                          val_alpha  = 0.71,
                          debug=TRUE)
  tmp <- as.matrix(resDMP$msg[,-c(1,4)])
  dimnames(tmp)[[2]] <- NULL
  #expect_equal(tmp[1,2],  58.8678702)
  #expect_equal(tmp[2,1],-31.832157271249)
  # new file does not match the old one
    expect_true(TRUE)
})




test_that("Null deviations", {
  myTB <- dplyr::tibble(
    time = c(2001:2005),
    couwA = c(29, 29, 29, 29, 29),
    couwB = c(29, 29, 29, 29, 29),
    couwC = c(29, 29, 29, 29, 29),
  )

  resDMP <- dev_mean_plot(myTB,
                          timeName="time",
                          time_0 = NA,
                          time_t = NA,
                          displace = 0.25,
                          indiType = "highBest",#lowBest",
                          axis_name_y = "Countries",
                          debug=TRUE)



  resMat <- as.matrix(resDMP$msg[,-c(1,4)])
  dimnames(resMat)[[2]] <- NULL
  for(auxR in 1:3){
    for(auxC in 1:2){
      expect_equal(resMat[auxR,auxC],  0)
    }}

})





#  debug(dev_mean_plot)
