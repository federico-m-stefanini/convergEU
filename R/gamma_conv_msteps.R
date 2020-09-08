#' Gamma convergence iterated on several years in pairs
#'
#' Given a dataframe (tibble) of sorted times by countries indicator,
#' the gamma convergence is calculated between pairs of subsequent years.
#' A time index is required. Missing values are not allowed.
#'
#'
#' @param rawDat  the tibble made by  times and countries.
#' @param startTime the first year  to  consider, included.
#' @param endTime  the last year to consider, included.
#' @param timeName the name of the variable that contains time information.
#' @return  dataset of gamma values  (indicated as KIt in Eurofound 2018 paper).
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Example 1
#' # Dataframe in the format time by countries:
#' require(tibble)
#' myTB  <- tibble::tribble(
#' ~time, ~UK, ~DE, ~IT,
#'  1988,   1201, 868, 578,
#'  1989,   1150, 978, 682,
#'  1990,   998,  1250, 332,
#'  1991,  1600,  1350, 802
#'  )
#'  resGammaST <- gamma_conv_msteps(myTB,startTime = 1988,endTime=1991, timeName = "time")
#'
#' # Example 2
#' # Gamma convergence iterated for several pairs of years for the emp_20_64_MS Eurofound dataset
#' data("emp_20_64_MS")
#' # check name of the time variable
#' names(emp_20_64_MS)
#' resGammaST2<-gamma_conv_msteps(emp_20_64_MS,startTime=2002,endTime=2006, timeName = "time")
#' resGammaST3<-gamma_conv_msteps(emp_20_64_MS,startTime=2002,endTime=2018, timeName = "time")
#' resGammaST4<-gamma_conv_msteps(emp_20_64_MS,startTime=2007,endTime=2012, timeName = "time")
#'
#' @export
#'
#'
gamma_conv_msteps <-  function(rawDat,
                                 startTime,
                                 endTime,
                                 timeName = "time"){

    obj_out <- check_data(rawDat,timeName)
    if(!is.null(obj_out$err)){
      return(obj_out);
    }else{ };
    # check if timeName is present
    if(not_in(timeName, names(rawDat))) {
      tmp <- convergEU_glb()$tmpl_out
      tmp$err <- "Error: Time variable not in the dataframe."
      return(tmp)
    }else{ };
    # check time window
    if(# at least one feature wrong
      !((endTime %in% rawDat[[timeName]]) &
         (startTime %in% rawDat[[timeName]])  &
         (nrow(rawDat) >= 2))
    ){
      tmp <- convergEU_glb()$tmpl_out
      tmp$err <- "Error: Time references wrong and/or not enough points."
      return(tmp)
    };
    # sequence of times
    estrattore <- (rawDat[[timeName]] >= startTime)  &
                  (rawDat[[timeName]] <= endTime);
      timeSteps <- dplyr::filter(rawDat, estrattore)
    outRes <- timeSteps[,timeName]
    outRes$gammaConv <- NA
    for(auxY in 2:nrow(timeSteps)) {
      outRes$gammaConv[auxY] <- gamma_conv(timeSteps,
                                           last = timeSteps[[timeName]][auxY],
                                           ref =  timeSteps[[timeName]][auxY-1],
                                           timeName = timeName,
                                           printRanks = FALSE)$res;
    }# end of for
    obj_out$res <-outRes
    return(obj_out)
}

