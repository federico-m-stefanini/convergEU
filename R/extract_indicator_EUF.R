#' Create a dataset (tibble) for an indicator.
#'
#' From the Eurofound database, a dataset is created whose structure is
#' years by countries, possibly conditioned to  gender.
#'
#' @param indicator_code the variable describing countries
#' @param fromTime first year to be considered
#' @param toTime last year to be considered
#' @param gender  which gender, one of  c("Total","Females","Males")
#' @param countries a collection of strings representing countries
#'                   in the standard two letters format
#' @return  a dataset (tibble) years by countries, possibly conditioned to gender
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#'
#' # Extract indicator labelled "lifesatisf" and accessible from "dbEUF2018meta" data:
#' print(dbEUF2018meta, n=20, width=100)
#' dbEUF2018meta$Code_in_database
#' myTB1 <- extract_indicator_EUF(
#'     indicator_code = "lifesatisf", #Code_in_database
#'     fromTime=2003,
#'     toTime=2015,
#'     gender= c("Total","Females","Males")[1])
#'
#' # Extract indicator "exposdiscr_p" (Code_in_database) from 2003 to 2016:
#' myTB2 <- extract_indicator_EUF(
#'     indicator_code = "exposdiscr_p", #Code_in_database
#'     fromTime=2003,
#'     toTime=2016,
#'     gender= c("Total","Females","Males")[1])
#'
#' # Extract indicator "lifesatisf" from 1998 to 2016 for females:
#' myTB3 <- extract_indicator_EUF(
#'     indicator_code = "lifesatisf", #Code_in_database
#'     fromTime = 1998,
#'     toTime = 2016,
#'     gender = c("Total","Females","Males")[2])
#'
#' # Extract indicator "lifesatisf" from 1960 to 2016 for males of EU12:
#' myTB4 <- extract_indicator_EUF(
#'     indicator_code = "lifesatisf", #Code_in_database
#'     fromTime=1960,
#'     toTime=2016,
#'    gender= c("Total","Females","Males")[3],
#'    countries= convergEU_glb()$EU12$memberStates$codeMS)
#'
#'
#' @export
#'
#'
extract_indicator_EUF <- function(
                      indicator_code, #Code_in_database
                      fromTime,
                      toTime,
                      gender= c("Total","Females","Males")[1],
                      countries =  convergEU_glb()$EU27$memberStates$codeMS){
  #
  out_obj <- convergEU_glb()$tmpl_out
  myTB <- NULL
  # data available?
  try(myTB <- convergEU::dbEurofound);
  if(is.null(myTB)){
    out_obj$err <- "Error: data not available."
    return(out_obj)
  }
  # check time windows
  test1 <-  fromTime >= 1960
  test2 <-  toTime >  fromTime
  if ((!test1) | (!test2)) {
    out_obj$err <- "Error: wrong time window."
    return(out_obj)
  }
  # check indicator code
  test3 <- indicator_code %in% convergEU::dbEUF2018meta$Code_in_database
  if (!test3) {
    out_obj$err <- "Error: indicator not included into the Eurofound database."
    return(out_obj)
  }
  # check gender
  if (!(gender %in% c("Total","Females","Males"))) {
    out_obj$err <- "Error: Unknown gender selection."
    return(out_obj)
  }
  # check selected countries
  namesDB <- unique(myTB$geo)
  test4 <-  sapply(countries,function(vx){vx %in% namesDB})
  if (any(!test4)) {
    out_obj$err <- "Error: at least one country not available."
    return(out_obj)
  }
  ## create a time by country database
  estrattore <- rep(FALSE,nrow(myTB))
  for(aux in countries){
    estrattore <- estrattore | (myTB$geo == aux)
  }
  # time
  estrattore <- estrattore & ((myTB$time >= fromTime) & (myTB$time <= toTime))
  # gender
  estrattore <- estrattore & (myTB$sex == gender)
  # do extract
  ttmp <- myTB[estrattore, c("time","geo","sex",indicator_code)]
  ttmp2 <-  ttmp[stats::complete.cases(ttmp),]
  resTB <- tidyr::spread(ttmp2,key="geo", value = indicator_code)
  out_obj$res <-  resTB
  return(out_obj)
}

