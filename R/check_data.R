#' Make tests on a dataset (dataframe and tibbles)
#'
#' A dataset can't have qualitative variables, neither vector of strings
#' nor missing values for computing convergence measures.
#' A time variable should also be present, and if the name is passed
#' then a check on the time order is performed.
#' The object returned states if the dataset is ready for calculations, and
#' if it is not, the error component states why checking failed.
#'
#' @param  tavDes  the dataframe under examination
#' @param  timeName  a string with the name of the time  variable, optional
#' @return  an object stating if errors are present
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Example 1
#' # Tibble dataset with missing values:
#' require(tibble)
#' myTB1  <- tibble::tribble(
#' ~time, ~veval,
#' 1988,   1201,
#' 1989,    NA,
#' 1990,   998,
#' 1991,    NA
#' )
#' # Check dataset:
#' check_data(myTB1)
#'
#' # Example 2
#' # Dataset with no missing values, no qualitative variables, and variable time present:
#' myTB2  <- tibble::tribble(
#' ~time, ~veval,
#' 1988,   1201,
#' 1989,    450,
#' 1990,   998,
#' 1991,   675
#' )
#' check_data(myTB2)
#'
#' # Check the "emp_20_64_MS" Eurofound dataset:
#' data(emp_20_64_MS)
#' check_data(emp_20_64_MS, timeName="time")
#'
#' @export
#'
#'
check_data <- function(tavDes,timeName=NA){
  obj_out <- convergEU_glb()$tmpl_out
  # Missing values present?
  if( any(!stats::complete.cases(tavDes))){
    obj_out$err <- "Error: one or more missing values in the dataframe."
    return(obj_out)
  }
  # Any factor?
  if( any(sapply(tavDes,is.factor) )   ){
    obj_out$err <- "Error: qualitative variables  in the dataframe."
    return(obj_out)
  }
  # Any variable with strings?
  if( any(sapply(tavDes,function(vx)is.character(vx[1]))) ){
    obj_out$err <- "Error: string  variables in the dataframe."
    return(obj_out)
  }
  # check time order, if timeName is not NA
  if(!is.na(timeName)){
      if( !(timeName  %in% names(tavDes))){
        obj_out$err <- "Error: timeName variable absent."
        return(obj_out)
      }
    # the time variable is present
    # check if ordered
    testTO <- unlist(tavDes[,timeName]) != sort(unlist(tavDes[,timeName]))
    if(sum(testTO) != 0){
      obj_out$err <- "Error: time variable is not ordered."
      return(obj_out)
      }
    }
  #obj_out$msg <- "Test passed"
  obj_out$res <- TRUE
  return(obj_out)
}
