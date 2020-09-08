#' Check a dataset (tibble) for the presence of countries
#'
#' A given list of countries is contained into a dataset (tibble).
#' If not, an object  signaling this error is returned.
#'
#'
#' @param  myTB  dataset (tibble) to be checked
#' @param  clusterCode string to denote  which countries should be in the dataset
#' @return  TRUE if they are inside, FALSE otherwise
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Check the dataset "emp_20_64_MS" for the presence of countries in cluster EU27:
#' check_country(emp_20_64_MS, clusterCode="EU27")
#'
#' # Check absence for EU27:
#' check_country(emp_20_64_MS[,-(6:8)], clusterCode="EU27")
#'
#' # Check the dataset "emp_20_64_MS" for the presence of countries in cluster EU25:
#' check_country(emp_20_64_MS, clusterCode="EU25")
#'
#' # Check the dataset "emp_20_64_MS" for the presence of countries in cluster EU12:
#' check_country(emp_20_64_MS, clusterCode="EU12")
#'
#' @export
#'
check_country <- function(myTB, clusterCode="EU27"){
  out_obj <- convergEU_glb()$tmpl_out
  # get EU labels
  if(!(clusterCode %in% names(convergEU_glb()))){
    out_obj$err <- "Error: clusterCode not recognized."
    return(out_obj)
   }
  labelMS <- unlist(convergEU_glb()[[clusterCode]][["memberStates"]][,2])
  # All MS are present? Name on columns?
  nomiTB <- names(myTB)
  out_obj$res <- TRUE
  for(aux in labelMS){
    if( !(aux %in% nomiTB)){
      # lacking country
      out_obj$res <- FALSE
      return(out_obj);
    }else{};
  }# end for

  return(out_obj)
}

