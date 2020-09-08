
context("Basic computation")



#  debug(demea_change)


test_that("Scrambled data", {
  myTB <- emp_20_64_MS
  myTBscrambles<- myTB[c(15,14,13,7,6,5,1,2,3,4,12,11,8,10,9),]
  res <- demea_change(myTB,
                           timeName="time",
                           time_0 = 2005,
                           time_t = 2015,
                      sele_countries=NA)

  resrev <- demea_change(myTBscrambles,
                      timeName="time",
                      time_0 = 2005,
                      time_t = 2015,
                      sele_countries=NA)

  expect_equal(res$res$stats, resrev$res$stats)

})



test_that("With without selecountries ", {
  # Results provided by Eleonora Peruffo,
  # excel file 20 nov 2019.
  myTB <- emp_20_64_MS
  res <- demea_change(myTB,
                      timeName="time",
                      time_0 = 2002,
                      time_t = 2016,
                      sele_countries= c("IT","DE","FR"),
                      doplot=TRUE)

  expect_true("ggplot" %in% class(res$res$res_graph))

  res <- demea_change(myTB,
                      timeName="time",
                      time_0 = 2002,
                      time_t = 2016,
                      sele_countries=c("IT","DE","FR"),
                      doplot=TRUE)

  for(aux in c("gg", "ggplot")){
    tmp<- expect_true(aux %in% class(res$res$res_graph))
  }
})

