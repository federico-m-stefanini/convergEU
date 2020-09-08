#' Download  a dataset (tibble) from Eurostat.
#'
#' From the Eurostat web site, a dataset is created whose structure is
#' time by countries, possibly conditioned to  gender, age class and other
#' variables. All indicators are supported and, after  downloading, data are not
#' filtered by country members (geo) and/or EU clusters.
#'
#' It is up to the user to proceed with further filtering/selection
#' so that the desired
#' collection of member states is obtained.
#'
#'
#' @param indicator_code  defined by Eurostat as id.
#' @param fromTime first year to be considered.
#' @param toTime last year to be considered.
#' @param gender  if available, the gender of interest
#'                 c("T","F","M") for Total, Females, Males.
#' @param ageInterv if available,
#'                  a string of character representing the age class to
#'                  be considered as coded by Eurostat, for example 'Y15-74'.
#' @param rawDump if TRUE raw downloaded data are returned, otherwise
#'         filtered values are provided.
#' @param uniqueIdentif  identifiers of further conditional variables (1,2,...).
#' @return  a dataset (tibble) years by countries, possibly conditioned
#'          to gender, within the  list  as  component named res.
#'          If rawDump is TRUE then bulk data are provided. The list component
#'          msg may contain auxiliary information on conditioning variables.
#' @importFrom rlang :=
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'@examples
#'\dontrun{
#' myDF1 <- down_lo_EUS(indicator_code = "lfsa_ergaed",
#'                     fromTime = 2005,
#'                     toTime = 2015,
#'                     gender=  "F",
#'                     ageInterv = NA,
#'                     rawDump=FALSE,
#'                     uniqueIdentif = 1)
#'
#' myDF2 <- down_lo_EUS(indicator_code = "lfsa_ergaed",
#'                     fromTime = 2005,
#'                     toTime = 2015,
#'                     gender=  "M",
#'                     ageInterv = "Y15-64",
#'                     rawDump=FALSE,
#'                     uniqueIdentif = 3)
#'
#'
#' myDF3 <- down_lo_EUS(indicator_code = "t2020_rk310",
#'                      fromTime = 2005,
#'                      toTime = 2015,
#'                      gender=  "F",
#'                      ageInterv = NA,
#'                      rawDump=FALSE,
#'                      uniqueIdentif = 1)
#'
#' myDF4 <- down_lo_EUS(indicator_code = "t2020_rk310",
#'                      fromTime = 2005,
#'                      toTime = 2015,
#'                      gender=  "F",
#'                      ageInterv = "Y15-39",
#'                      rawDump=FALSE,
#'                      uniqueIdentif = 1)
#'
#' }
#' @export
#'
#'
down_lo_EUS <- function(
                  indicator_code,
                  fromTime,
                  toTime,
                  gender= c(NA,"T","F","M")[1],
                  ageInterv = NA,
                  rawDump=FALSE,
                  uniqueIdentif = 1){
  #
  message_out<-list()
  out_obj <- convergEU_glb()$tmpl_out
  downTB <- NULL
  varFinali <- NULL
  #
  # check time windows
  test1 <-  fromTime >= 1960
  test2 <-  toTime >  fromTime
  if ((!test1) || (!test2)) {
    out_obj$err <- "Error: wrong time window."
    return(out_obj)
  }
  # check gender
  if ((!(gender %in% c("T","F","M"))) & !is.na(gender)) {
    out_obj$err <- "Error: Impossible gender selection."
    return(out_obj)
  }
  ## try to download data
  mytmp <- utils::capture.output(
        downTB <- purrr::possibly(eurostat::get_eurostat,
                  NULL,
                  quiet = FALSE)(indicator_code,
                                 time_format = "num"),
                                 type = "message");
  #
  # test downloaded
  if(is.null(downTB)){
    out_obj$err <- list("Error: data not available, check connection and indicator.",
                        mytmp)
    return(out_obj)
  }
  ## OK data downloaded
  # early return for bulk data
  if(rawDump) return(downTB)
  ## check if time present
  isTime <- "time" %in% names(downTB)
  if(isTime){
    test91 <- fromTime <= downTB$time
    test92 <- toTime >= downTB$time
    if(sum(test91 & test92) < 1){
      out_obj$err <- "Error: no observations in the selected time interval."
      return(out_obj)
    }else{};
    #
    namesDB <- unique(as.character(downTB$geo))
    varFinali <- c("time",namesDB)
    estrattore <-  ((downTB$time >= fromTime) & (downTB$time <= toTime))
  }else{};# fine time
  ## check if sex present
  isGender <-   "sex" %in% names(downTB)
  if(isGender ){
      if(is.na(gender)){## because unassigned
          gender <- "T"
          message_out[["gender"]] <- "Gender automatically set to 'T'."
          }else{};
      test8 <- gender %in% unique(downTB$sex)
      if(!test8){
         out_obj$err <- "Error: wrong gender selection."
         return(out_obj)
      }else{};
    varFinali <- c("sex",varFinali)
    estrattore <- estrattore & (downTB$sex == gender)
  }else{};# fine sex gender
  #
  ## check if age class present
  isAgeClass <- "age" %in% names(downTB)
  if(isAgeClass){
    if(is.na(ageInterv)){
      ageInterv <- unique(downTB$age)[1]
      message_out[["Age"]] <- "Age automatically set."
    }else{};
    test7 <- ageInterv %in% unique(downTB$age)
    if(!test7){
      out_obj$err <- "Error: wrong age class."
      return(out_obj)
      }else{};
    varFinali <- c("age",varFinali)
    estrattore <- estrattore & (downTB$age == ageInterv)
   }else{};# fine age
  #
  ## create a time by country database
  ttmp <- downTB[estrattore,]
  ttmp2 <-  ttmp # no filtering
  #
  auxTag <- compo_cond_EUS(ttmp2)
  if(!is.null(auxTag)){
      seleTagLs <- unique(auxTag) # unique idenifiers of further conditioning variables
      ttmp3 <- dplyr::mutate(ttmp2,auxTag = auxTag)
      ttmp3 <- dplyr::filter(ttmp3,auxTag == seleTagLs[uniqueIdentif])
      resTB <- tidyr::spread(ttmp3, key = "geo", value = "values")
      message_out[["Further_Conditioning"]] <- list(
        current=paste0("Selected uniqueIdentif = ",
               uniqueIdentif," -> ",seleTagLs[uniqueIdentif]),
        available_seleTagLs= data.frame(uniqueIdentif = 1:length(seleTagLs),
                                           tags=seleTagLs)
      )
  }else{
    resTB <- tidyr::spread(ttmp2, key = "geo", value = "values")
  }
  #
  # Exit message
  message_out[["Conditioning"]] <- list(
    indicator_code = indicator_code,
    ageInterv = ageInterv,
    gender = gender
  )
  if(isAgeClass && !("age" %in% varFinali)) varFinali <- c("age",varFinali);
  if(isGender && !("sex" %in% varFinali)) varFinali <- c("sex",varFinali);
  #
  for(aux in varFinali){
    if(!(aux %in% names(resTB) )){
      resTB <- dplyr::mutate(resTB, !!aux := rep(NA,nrow(resTB)))
    }else{};
  }
  out_obj$res <-  resTB[,varFinali]
  if(length(message_out) > 0) out_obj$msg <- message_out
  return(out_obj)
}

