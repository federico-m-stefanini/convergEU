#' Smoother based on moving average
#'
#' The smoother change each value into the average of values around it spanning
#' a window of size kappa.
#' Missing values are not allowed.
#'
#'
#' @param  myTB  a complete dataset (tibble) time by countries, with just
#'               time column and    country columns.
#' @param  kappa  integer greater than 1 as smoothed value, to set the time window of
#'                the moving average.
#' @param  timeName name of the time variable.
#' @return  a dataset  of smoothed values.
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Smoother based on moving average with k=1.5:
#' require(tibble)
#'
#' # Dataset in the format time by countries
#' myTB  <- tibble::tibble(
#'     time = 2010:2001,
#'     IT = c(10,14,13,12,9,11,13,17,15,25),
#'     DE = c(10,11,12,9,14,17,23,29,26,23)
#'    )
#' resMA1 <- ma_dataset(myTB, kappa=1.5)
#'
#' # Smoother based on moving average with k=3:
#' resMA2<-ma_dataset(myTB, kappa=3)
#'
#' # Example 2
#' # Smoother based on moving average for the emp_20_64_MS Eurofound dataset:
#'
#' myTB1 <-  emp_20_64_MS[,c("time","IT","DE", "FR")]
#' # Smoother based on moving average with k=2:
#' resMAeu<-ma_dataset(myTB1, kappa=2, timeName= "time")
#'
#' # Smoother based on moving average with k=3:
#' resMAeu1<-ma_dataset(myTB1, kappa=3, timeName= "time")
#'
#' @export
#'
#'
ma_dataset <- function(myTB, kappa=2, timeName= "time"){
  out_obj <- convergEU_glb()$tmpl_out
  ## time
  if( !(timeName %in% names(myTB))) {
    out_obj$err <-  "Error: wrong timeName";
    return(out_obj)
  }
  kk <- round(kappa,0)
  ## choice of kappa
  if(kk < 1 | kk >= nrow(myTB)) {
    out_obj$err <-  "Error: wrong kappa";
    return(out_obj)
  }
  # missing
  if(sum(is.na(myTB)) != 0) {
    out_obj$err <- "Error: there are missing values."
    return(out_obj)
  }
  # sorted??
  resTB <- dplyr::arrange_all(myTB, .vars= timeName)# because of unquoting
  for(aux in setdiff(names(resTB),timeName)) {
    resTB[,aux] <- caTools::runmean(unlist(resTB[,aux]),
          k = kk,
          alg=c("C", "R", "fast", "exact")[4],
          endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
          align = c("center", "left", "right")[1])
      }
  out_obj$res <- resTB
  return(out_obj)
}


