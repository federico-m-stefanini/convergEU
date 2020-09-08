#' Time-indicator serie to straight lines parameters
#'
#' Given a dataset with first column times and second column the indicator values
#' parameters of time-spliced straight lines are calculated. No checking is
#' performed in input.
#' Time values must differ by a positive  constant.
#'
#'
#' @param  dataMat  two columns (times, indicator) dataset
#' @return  dataset(tibble) where each row is  (times, intercept, slope)
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' require(tibble)
#' testTB <- dplyr::tribble(
#'     ~time, ~countryA ,  ~countryB,  ~countryC,
#'     2000,     0.8,   2.7,    3.9,
#'     2001,     1.2,   3.2,    4.2,
#'     2002,     0.9,   2.9,    4.1,
#'     2003,     1.3,   2.9,    4.0,
#'     2004,     1.2,   3.1,    4.1,
#'     2005,     1.2,   3.0,    4.0
#'     )
#'
#' curcountry <- 2
#' resPAR <- ts_parlin(testTB[,c(1,curcountry)])
#'
#' curcountry <- 4
#' resPAR1 <- ts_parlin(testTB[,c(1,curcountry)])
#'
#' @export
#'
#'
ts_parlin <- function(dataMat){
  dataMat<- as.matrix(dataMat)
  dimnames(dataMat) <- list(NULL,NULL)
  estrattore2 <- 2:nrow(dataMat)
  estrattore1 <- 1:(nrow(dataMat)-1)
  label_time <- paste(dataMat[estrattore1,1], "/", dataMat[estrattore2,1],sep="")

  # Delta ordinates / delta abscissas
   beta1 <- (dataMat[estrattore2,2]-dataMat[estrattore1,2])/(dataMat[estrattore2,1]-dataMat[estrattore1,1])
   beta0 <-    dataMat[estrattore1, 2] - beta1* dataMat[estrattore1, 1]
  return( dplyr::tibble(time=label_time,beta0=beta0,beta1=unlist(beta1)))
}
