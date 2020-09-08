#' Imputation to make a dataset complete
#'
#' For initial and final missing values there are two options: they could be completely
#' cancelled or, otherwise
#' propagated. For all other missing values within the dataset,
#' deterministic linear imputation is applied in order to obtain
#' complete data.
#'
#' @param  myTB  a dataset (tibble) time by countries for a given indicator,
#'              sorted by time. Note that times corresponding to missing data
#'              must be contained in the dataset.
#' @param  countries the collection of labels representing countries to process.
#' @param  timeName the string that represent the name of the time variable.
#' @param  tailMiss what should be done with subsequent missing values
#'                  starting at  the oldest year: cut those years, or input constant
#'                  values equal to the first observed year.
#' @param  headMiss  what should be done with subsequent missing values
#'                  ending at  the last year: cut those years, or input constant
#'                  values equal to the first observed year.
#' @return  a list with three components: "res": the dataset (tibble) without
#'          missing values; "msg" and "err"
#'
#'
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#'
#' # Example 1
#' # Dataset in the format time by countries with missing values:
#' myTB2  <- tibble::tribble(
#'     ~time, ~UK, ~DE, ~IT,
#'     1988,   998,  1250, 332,
#'     1989,   NA, 868, NA,
#'     1990,   1150, 978, NA,
#'     1991,  1600,  NA, 802
#'     )
#' toBeProcessed <- c( "UK","DE","IT")
#' # Simplest Imputation using option "cut":
#' resImpu <- impute_dataset(myTB2, countries=toBeProcessed,
#'                          timeName = "time",
#'                          tailMiss = c("cut", "constant")[1],
#'                          headMiss = c("cut", "constant")[1])
#'
#'
#' # Imputation using option "constant":
#' resImpu1 <- impute_dataset(myTB2, countries=toBeProcessed,
#'     timeName = "time",
#'     tailMiss = c("cut", "constant")[2],
#'     headMiss = c("cut", "constant")[2])
#'
#' # Imputation using both options "cut" and "constant":
#' resImput <- impute_dataset(myTB2, countries=toBeProcessed,
#'     timeName = "time",
#'     tailMiss = c("cut", "constant")[2],
#'     headMiss = c("cut", "constant")[1])
#'
#' # Example 2
#' # dataset time by countries for the indicator "JQIintensity_i":
#' myTB <- extract_indicator_EUF(
#'     indicator_code = "JQIintensity_i", #Code_in_database
#'     fromTime= 1965,
#'     toTime=2016,
#'     gender= c("Total","Females","Males")[1],
#'     countries= convergEU_glb()$EU27$memberStates$codeMS)
#'
#' # Imputation of missing values, option "cut":
#' myTBinp <- impute_dataset(myTB$res, timeName = "time",
#'     countries=convergEU_glb()$EU27$memberStates$codeMS,
#'     tailMiss = c("cut", "constant")[1],
#'     headMiss = c("cut", "constant")[1])
#'
#' # Imputation of missing values, option "constant":
#' myTBinp1 <- impute_dataset(myTB$res, timeName = "time",
#'     countries=convergEU_glb()$EU27$memberStates$codeMS,
#'     tailMiss = c("cut", "constant")[2],
#'     headMiss = c("cut", "constant")[2])
#'
#' @export
#'
#'
impute_dataset <- function(myTB,
                           countries,
                           timeName = "time",
                           tailMiss = c("cut", "constant")[2],
                           headMiss = c("cut", "constant")[1]){

  out_obj <- convergEU_glb()$tmpl_out
  #checking step: time
  if(!(timeName %in% names(myTB))){
    out_obj$err <- "Error: wrong time variable."
    return(out_obj)
  }else{};
  # sort by time
  myTB <- dplyr::arrange(myTB, .data[[timeName]])
  tmpTB <- myTB # destination
  deltaTime <- diff(myTB[[timeName]])
  #checking step: tailMiss
  if(!(tailMiss %in% c("cut", "constant"))){
    out_obj$err <- "Error: wrong tailMiss selection."
    return(out_obj)
  }
  #checking step: headMis
  if(!(headMiss %in% c("cut", "constant"))){
    out_obj$err <- "Error: wrong headMiss selection."
    return(out_obj)
  }
  #checking step: countries
  nomiTB <- names(myTB)
  if(length(setdiff(countries,names(myTB)))>0){
    out_obj$err <- "Error: wrong country label."
    return(out_obj)
    }
  ## Calculations
  ## first with inner chunks of missing
  for (aux in countries) {
    sottoTB1 <- myTB[c(timeName,aux)]
    estrattore <- stats::complete.cases(sottoTB1)
    ## we can move this outside this cycle
    sottoTB1 <- dplyr::mutate(sottoTB1,DeltaT= c(diff(sottoTB1[[timeName]]),NA))
    startV <- min(which(estrattore))# posizione
    endV <- max(which(estrattore))# posizione
    # here imputation takes place "within", head and tail cut
    if (any(is.na(sottoTB1[startV:endV,aux]))) {
      sottoTB2 <- sottoTB1[startV:endV,]
      #allTimes <- unlist(sottoTB2[,timeName]) ## all considered times
      allTimes <-  sottoTB2[[timeName]] ## all considered times
      #
      sottoTB4 <- sottoTB2[!is.na(sottoTB2[[aux]]),]# reduced to full data
      lastRow <- nrow(sottoTB4)
      clusterMiss <- list()
      for(auxSM in 1:(lastRow-1)){
        timeCur <-  sottoTB4[[timeName]][auxSM]  #tempo completo corrente
        #timeCur <- unlist(sottoTB4[auxSM,timeName]);
        #timeCurP1 <- unlist(sottoTB4[auxSM + 1,timeName]); #tempo successivo completo
        timeCurP1 <- sottoTB4[[timeName]][auxSM + 1]; #tempo successivo completo

        estrattoMis <- (sottoTB2[[timeName]] > timeCur) &
                       (sottoTB2[[timeName]] < timeCurP1);
        if(sum(estrattoMis) > 0){
          #valTimMis <- unlist(sottoTB2[estrattoMis,timeName])
          valTimMis <- sottoTB2[[timeName]][estrattoMis]
          clusterMiss[[paste(timeCur)]] <- list(
                    t1 = timeCur,
                    t2 = timeCurP1,
                    timeMiss = valTimMis,
                    #indicaT1 = unlist(sottoTB4[auxSM,aux]),
                    #indicaT2 = unlist(sottoTB4[auxSM+1,aux]))
          indicaT1 =  sottoTB4[[aux]][auxSM],
          indicaT2 =  sottoTB4[[aux]][auxSM+1])
# imputa
          resImpu <- impu_det_lin(clusterMiss[[paste(timeCur)]]$t1,
                       clusterMiss[[paste(timeCur)]]$t2,
                       valTimMis,
                       clusterMiss[[paste(timeCur)]]$indicaT1,
                       clusterMiss[[paste(timeCur)]]$indicaT2)
          for(auxI in 1:length(valTimMis)) {
            #tmpTB[(tmpTB$time == valTimMis[auxI]),aux] <- resImpu$indicator[auxI]
            #tmpTB[(tmpTB[[timeName]] == valTimMis[auxI]),aux] <- resImpu$indicator[auxI]
            tmpTB[[aux]][(tmpTB[[timeName]] == valTimMis[auxI])] <- resImpu$indicator[auxI]
            }
          }else{};
      }
    # imputa il dato mancante
     for(auxI in 1:length(tmpTB[[timeName]])) {
       #myTB[(myTB[[timeName]] == as.numeric(tmpTB[auxI,timeName])),  aux] <- tmpTB[auxI,aux]
       myTB[[aux]][ (myTB[[timeName]] ==  tmpTB[[timeName]][auxI])] <- tmpTB[[aux]][auxI]
     }
    }}
  ## then handle tail
  if (tailMiss == "cut"){
    myTBred <- myTB[,countries]
    mayNA <- is.na(myTBred)
    rowsToCut <- apply(mayNA,1,any)
    # check if deleting first rows at list two time points remain
    firstObserved <- min(which(!rowsToCut))
    if(firstObserved >1){ # let's chop the first rows
      if(nrow(myTB)-firstObserved+1 < 2){
        out_obj$err <- "Error:two few remaining time points."
        return(out_obj)
      }
      myTB <- myTB[-(1:(firstObserved-1)),]
    }
  }else{};#end of tailMiss == cut
  #
  # head miss == cut
  if (headMiss == "cut"){
    myTBred <- myTB[countries]
    mayNA <- is.na(myTBred)
    rowsToCut <- apply(mayNA,1,any)
    # check if deleting first rows at list two time points remain
    lastObserved <- max(which(!rowsToCut))
    if(lastObserved < nrow(myTB)){ # let's chop the last rows
       if(nrow(myTB)-lastObserved+1 < 2){
         out_obj$err <- "Error:two few remaining time points."
         return(out_obj)
       }else{}
      myTB <- myTB[1:lastObserved,]
      }else{}
  }else{};#end of headlMiss == cut
  #
  # tail and head  missing imputed by a constant needs of a cycle
  for(aux in countries){
      if (tailMiss == "constant"){
        firstObserved <- min(which(!is.na(myTB[[aux]])))
        if(firstObserved>1){
          #myTB[1:(firstObserved-1),aux] <- myTB[firstObserved, aux]
          myTB[[aux]][1:(firstObserved-1)] <- myTB[[aux]][firstObserved]
        }else{};
       }else{};#end of tailMiss == constant
       #
       ##  then handle head
      if (headMiss == "constant"){
        lastObserved <- max(which(!is.na(myTB[[aux]])))
        if(lastObserved < nrow(myTB)){
          #myTB[(lastObserved+1):nrow(myTB),aux] <- myTB[lastObserved, aux]
          myTB[[aux]][(lastObserved+1):nrow(myTB)] <- myTB[[aux]][lastObserved]
        }else{};
      }else{};#end of tailMiss == constant
  }# end for all countries

  out_obj$res <- myTB
  return(out_obj)
}
