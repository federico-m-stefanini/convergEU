#' Auxiliary function for membership
#'
#' A fast check if one or more values are outside a set.
#'
#' @param values one or more values
#' @param set_collection a collection of values
#' @return TRUE if not within or FALSE otherwise
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' val<-c(1,2,3,5)
#' mycol<-c(7,8)
#' not_in(val,mycol)
#'
#' val1<-c(1,2,3,5)
#' mycol1<-c(3,5)
#' not_in(val1,mycol1)
#'
#' val2<-c("FR", "IT", "LU")
#' mycol2<-c("FR", "ES")
#' not_in(val2,mycol2)
#'
#' @export
#'
#'
not_in <-  function(values,set_collection){
!(values %in% set_collection)
}
