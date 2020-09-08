#' Gamma convergence
#'
#' Given a dataframe (tibble) of times by countries indicator,
#' the gamma convergence is calculated. A time index is required.
#' Missing values are not allowed.
#'
#'
#' @param rawDat  the tibble made by  times and countries.
#' @param last the last time  to be considered.
#' @param ref  the reference time, typically zero.
#' @param timeName the name of the variable that contains time information.
#' @param printRanks logical flag for printing ranks based on data.
#' @return  gamma convergence  (indicated as KIt in Eurofound 2018 paper).
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Dataframe in the format time by countries:
#' require(tibble)
#' myTB  <- tibble::tribble(
#'     ~years, ~UK, ~DE, ~IT,
#'     1990,   998,  1250, 332,
#'     1988,   1201, 868, 578,
#'     1989,   1150, 978, 682,
#'     1991,  1600,  1350, 802
#'     )
#'
#' # Gamma convergence,  scrambled time and different time name:
#' resGamma <- gamma_conv(myTB,ref=1988, last=1991, timeName="years")
#'
#' # Example 2
#' myTB1  <- tibble::tribble(
#'     ~time, ~UK, ~DE, ~IT,
#'     1990,   998,  1250, 332,
#'     1988,   1201, 868, 578,
#'     1989,   1150, 978, 682,
#'     1991,  1600,  1350, 802
#'     )
#' resGamma1 <- gamma_conv(myTB1, ref=1989,last=1990)
#'
#' # Example 3
#' # Gamma convergence for the emp_20_64_MS Eurofound dataset:
#' data("emp_20_64_MS")
#'
#' # check name of the time variable
#' names(emp_20_64_MS)
#' resGamma2<-gamma_conv(emp_20_64_MS,ref=2002,last=2005)
#' resGamma3<-gamma_conv(emp_20_64_MS,ref=2002,last=2018)
#' # Print also ranks based on data:
#' resGamma4<-gamma_conv(emp_20_64_MS,ref=2002,last=2018,printRanks=TRUE)
#'
#' @export
#'
#'
gamma_conv <- function(rawDat, ref=NA, last=NA, timeName = "time",printRanks=F){
  # Make standard cheks on the dataframe
  obj_out <- check_data(rawDat)
  if(!is.null(obj_out$err)){
    return(obj_out);
    }else{ };
  # check if timeName is present
  if(!(timeName %in% names(rawDat))){
    tmp <- convergEU_glb()$tmpl_out
    tmp$err <- "Error: Time variable not in the dataframe."
    return(tmp)
    }else{ };
  # check time window
  if(# at least one feature wrong
    !((last %in% unlist(rawDat[,timeName])) &
       (ref %in% unlist(rawDat[,timeName]))  &
       (nrow(rawDat) >= 2))
  ){
    tmp <- convergEU_glb()$tmpl_out
    tmp$err <- "Error: Time references wrong and/or not enough points."
    return(tmp)
  }else{  };
  gamcon <- NA
  # select rows
  estraT <- which(rawDat[,timeName] <= last)
  numTime <- length(estraT)
  # select ref
  posizRef <- which(rawDat[,timeName] == ref)
  # go with the index
  myDat <- dplyr::select(rawDat,-timeName)
  myMat <- t(as.matrix(myDat))# countries by times
  myRanghi <- apply(myMat,2, function(vetto){rank(vetto)})
  if(printRanks){
     cat("Ranks:\n")
     print(t(myRanghi))
     cat("\n")
  }
  # calculate  statistics
  numCountries <- nrow(myRanghi)
  myRk <- apply(myRanghi[,estraT], 1, function(vetto){sum(vetto)})
  variaNume <- stats::var(myRk)*(length(myRk)-1)/length(myRk)
  variaDenom <- stats::var((numTime+1)*myRanghi[,posizRef])*(numCountries-1)/numCountries
  gamcon <- variaNume/variaDenom
  obj_out <- convergEU_glb()$tmpl_out
  obj_out$res <-  gamcon
  return(obj_out)
}
