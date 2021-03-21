#' Create a dataset (tibble) for an EIGE indicator.
#'
#' From the EIGE database, a dataset is created whose structure is
#' years by countries.
#'
#' If the indicator_code is equal to "METADATA" then information on
#' available indicators is provided as a dataframe (tibble):
#' names of indicators are contained in the variable "Worksheet name".
#'
#' @param indicator_code one of the following strings:"INDEX",  "WORK",
#'                       "MONEY", "KNOWLEDGE",  "TIME", "POWER",
#'                       "HEALTH",  "FTE", "DWL", "SEGRE", "INCOME",
#'                        "TERTIARY", "CARE", "HOUSE",  "MINISTER",
#'                         "PARLIAMENT", "BOARD", "HLY","METADATA"
#' @param fromTime first year to be considered
#' @param toTime last year to be considered
#' @param countries a collection of strings representing countries
#'                   in the standard two letters format
#' @return  a dataset (tibble) years by countries
#'
#'
#'
#' @examples
#'
#'
#' # Extract metadata:
#' myTB1 <- extract_indicator_EIGE(
#'     indicator_code = "METADATA" #Code_in_database
#'     )
#'
#' # Extract indicator "HOUSE" from 2010 to 2015:
#' myTB2 <- extract_indicator_EIGE(
#'     indicator_code = "HOUSE", #Code_in_database
#'     fromTime=2010,
#'     toTime=2015)
#'
#'
#'
#' @export
#'
#'
extract_indicator_EIGE <- function(
                      indicator_code, #Code_in_database
                      fromTime,
                      toTime,
                      countries =  convergEU_glb()$EU27$memberStates$codeMS){
  #
  # available indicators on March 2021
  indi_all_EIGE <- c("INDEX",  "WORK",  "MONEY", "KNOWLEDGE",  "TIME", "POWER",
                     "HEALTH",  "FTE", "DWL", "SEGRE", "INCOME", "TERTIARY", "CARE", 
                     "HOUSE",  "MINISTER", "PARLIAMENT", "BOARD", "HLY",
                     "METADATA")
  out_obj <- convergEU_glb()$tmpl_out
  myTB <- NULL
  # data available?
  if(!(indicator_code %in% indi_all_EIGE)){
    out_obj$err <- "Error: data not available."
    return(out_obj)
  }
  tmpenv <- new.env()
  sourceFile1 <- system.file("EIGE", "EIGEdata.RData", package = "convergEU")
  tmpdataenv <- load(sourceFile1, envir=tmpenv)
  myTB <- get(indicator_code,envir = tmpenv)
  if(is.null(myTB)){
    out_obj$err <- "Error: data not included into the EIGE database."
    return(out_obj)
  }
  # if metadata then exit
  if(indicator_code == "METADATA"){# nothing else to do
    out_obj$res <-  myTB
    return(out_obj)
  }
  # else it is an actual indicator
  # check time windows
  test1 <-  fromTime >= 1960
  test2 <-  toTime >  fromTime
  if ((!test1) | (!test2)) {
    out_obj$err <- "Error: wrong time window."
    return(out_obj)
  }
  # check selected countries
  namesDB <- names(myTB[-1])# which is always "time" variable
  test4 <-  sapply(countries,function(vx){vx %in% namesDB})
  if (any(!test4)) {
    out_obj$err <- "Error: at least one country not available."
    return(out_obj)
  }
  ## create final database by selecting columns
  myTB <-  myTB[c("time",countries)]
  #estrattore <- estrattore & ((myTB$time >= fromTime) & (myTB$time <= toTime))
  # do extract
  resTB <- dplyr::filter(myTB, (myTB$time >= fromTime) & (myTB$time <= toTime))
  out_obj$res <-  resTB
  return(out_obj)
}

