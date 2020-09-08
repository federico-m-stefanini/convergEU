#' Population variance and standard deviation
#'
#' The denominator in n instead of n-1, like in the R base function.
#' Note that missing values are deleted by default.
#'
#' Note that the second argument, if assigned,  causes only one summary
#' of object returned.
#'
#' @param  veval  vector of data.
#' @return  the variance and standard deviation
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#'
#' @examples
#'
#'
#' myvec<-c(5,2,3,NA,4)
#' pop_var(myvec)
#'
#' vec1<-c(10, 20, 15,60,32)
#' pop_var(vec1)
#'
#' vec2<-c(NA,NA, 13, 19, 20)
#' pop_var(vec2)
#'
#' vec4<-c(seq(from = 5, to = 100, by = 5))
#' pop_var(vec4)
#'
#' @export
#'
#'
pop_var <- function(veval){
  popovar <- list()
  enne <- sum(stats::xtabs(~veval))
  popovar$popvar <- stats::var(veval, na.rm=TRUE)*(enne-1)/enne
  popovar$popsd <- sqrt(popovar$popvar)
  return( popovar)
}
