
context("Download data from Eurostat.")



## Tests below are performed by hand, because demanding and also they require
## an internet connection

hideTests <- function(){

  debug(download_indicator_EUS)

  eleDBvars <-  list()
  for(indicaName in convergEU_glb()$metaEUStat$selectorUser[2]){

    scaricati <- download_indicator_EUS(
      indicator_code=indicaName, # metaEUStat$selectorUser
      fromTime = 2005,
      toTime = 2015,
      gender= c(NA,"T","F","M")[3],#c("Total","Females","Males")
      ageInterv = NA,
      countries =  convergEU_glb()$EU28$memberStates$codeMS,
      rawDump=F)

    eleDBvars[[indicaName]]<- names(scaricati)
  }

  tmpS <- scaricati
  tmpS[c("unit")[1] ] <- as.character((tmpS %>% select(  matches(c("unit")[1])))[[1]])
  length(eleDBvars)

   unique(as.character(tmpS[["isced11"]]))

  # filtrati senza variabili note
  leftOver <-  lapply(eleDBvars,function(vx){
    setdiff(vx,c("sex","age","time","geo","values"))
  })
  leftOver
  knownVars <-  lapply(eleDBvars,function(vx){
    setdiff(c("sex","age","time","geo","values"),vx)
  })
  knownVars

  ### new test

  compo_cond_EUS(scaricati) # a new unique tag is produced
  unique(compo_cond_EUS(scaricati))



  ### new test
  # debug(download_indicator_EUS)
  # undebug(download_indicator_EUS)
  ttmp2 <-  download_indicator_EUS(
    indicator_code="labourcost_i", # metaEUStat$selectorUser
    fromTime = 2005,
    toTime = 2015,
    gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
    ageInterv = NA,
    countries =  convergEU_glb()$EU28$memberStates$codeMS,
    rawDump=F,
    uniqueIdentif=1)
 # ttmp2

  ttmp2 <- download_indicator_EUS(
    indicator_code="labourcost_i", # metaEUStat$selectorUser
    fromTime = 2005,
    toTime = 2015,
    gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
    ageInterv = NA,
    #countries =  convergEU_glb()$EU28$memberStates$codeMS,
    rawDump=F,
    uniqueIdentif=26)
 # ttmp2

  expect_equal(dim(ttmp2$res), c(286,29))


   ttmp <- download_indicator_EUS(
    indicator_code="hlth_dm060", # metaEUStat$selectorUser
    fromTime = 2005,
    toTime = 2015,
    gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
    ageInterv = "Y16-64",
    countries =  convergEU_glb()$EU28$memberStates$codeMS)
  #

  debug(down_lo_EUS)




  # context("Download bulk data  from Eurostat.")



  ## what happens if I want all available countries??

    ttmp <- down_lo_EUS(
      indicator_code="hlth_dm060", # metaEUStat$selectorUser
      fromTime = 2005,
      toTime = 2015,
      gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
      ageInterv = "Y16-64",
      countries =  NA
    )

    debug(down_lo_EUS)


    ttmp <- down_lo_EUS(
      indicator_code="t2020_rk310", # metaEUStat$selectorUser
      fromTime = 2005,
      toTime = 2015,
      gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
      ageInterv = "Y16-64",
      countries =  NA,
      rawDump=TRUE
    )

    unique(as.character(ttmp$geo))

    debug(down_lo_EUS)


    ttmp <- down_lo_EUS(
      indicator_code="t2020_rk310", # metaEUStat$selectorUser
      fromTime = 2000,
      toTime = 2015,
      gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
      ageInterv = "Y16-64",
      countries =  NA,
      rawDump=F
    )

ttmp <- down_lo_EUS(
      indicator_code="t2020_rk310", # metaEUStat$selectorUser
      fromTime = 2000,
      toTime = 2015,
      gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
      ageInterv = "Y16-64",
      countries =  NA,
      rawDump=F,
      uniqueIdentif = 3
    )



    ## download of all indicators belonging to the social scoreboard

     tmpDS <- dow_soc_scor_boa (fromTime = 1999 ,
                                toTime = 2018)




}# end of hides










