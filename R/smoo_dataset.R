#' Smoother based on weighting
#'
#' The smoother substitutes an original raw value $y_{m,i,t}$ of country $m$
#' indicator $i$ at time $t$ with the weighted average
#' $$\\check{y_{m,i,t}} = y_{m,i,t-1} ~ (1-w)/2   +w ~y_{m,i,t} +y_{m,i,t+1} ~(1-w)/2$$,
#' where $0< w \\leq 1$. The special case $w=1$ corresponds to no smoothing.
#' In case of missing values an NA is returned. If the weight is outside
#' the interval $(0,1]$ then a NA is returned.
#' The first and last values are smoothed using weights $w$ and $1-w$.
#'
#' @param  myTB  a complete dataset time by countries, with just country columns.
#' @param  leadW leading positive weight less or equal to 1.
#' @param timeTB a dataset with the time variable, if a dataset is desired as output
#' @return  a matrix of dataset  of smoothed values
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Dataset in the format time by countries:
#' myTB  <- tibble::tibble(
#'     time = 2001:2010,
#'     IT = c(10,14,13,12,9,11,13,17,15,25),
#'     DE = c(10,11,12,9,14,17,23,29,26,23)
#'     )
#'
#' # Remove the time variable in order to obtain just country columns and compute smoothed values:
#' reSMO <- smoo_dataset(myTB[,-1], leadW=1)
#' reSMO1 <- smoo_dataset(myTB[,-1], leadW=0.5)
#'
#' # Add the time variable for tibble in output:
#' reSMO2 <- smoo_dataset(myTB[,-1], leadW=.5,timeTB= dplyr::select(myTB,time))
#'
#' # Example 2
#' # Smoother based on weighting for the emp_20_64_MS Eurofound dataset:
#' data(emp_20_64_MS)
#' # Select countries:
#' myTB <- dplyr::select(emp_20_64_MS, time, IT,DE,FR)
#' # Compute smoothed values by also adding the time variable to the output:
#' resSM <- smoo_dataset(dplyr::select(myTB,-time), leadW = 0.2, timeTB= dplyr::select(myTB,time))
#'
#' @export
#'
#'
smoo_dataset <- function(myTB, leadW=1, timeTB=NULL){
  # check weight
  if(!(leadW >0 & leadW <=1)) {
    return(NA)
  }
  if(sum(is.na(myTB)) != 0) {
    return(NA)
  }
  enne <- nrow(myTB)
  mainW <- c( (1-leadW)/2, leadW,(1-leadW)/2)
  res <- matrix(c(rep(0,enne),
                (rep(c(mainW,rep(0,enne-2)),enne-2) )[1:((enne-2)*enne)],
                rep(0,enne))
                ,nrow=enne,ncol=enne,byrow=T)
  res[1,1:2] <- c(leadW,(1-leadW))
  res[enne,c(enne-1,enne)] <- c((1-leadW),leadW)
  outRes <- res %*% matrix(unlist(myTB),nrow=enne,ncol=ncol(myTB))
  if(tibble::is_tibble(timeTB)) {
    dimnames(outRes)[[2]] <- names(myTB)
    outTB <- dplyr::bind_cols(timeTB,dplyr::as_tibble(outRes))
  }else{# no time provided
    return(outRes)
  }
  return(outTB)
}


