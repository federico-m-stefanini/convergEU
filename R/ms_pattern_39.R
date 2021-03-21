#' Find patterns for all countries
#'
#' The input is a time by countries dataset where all countries contributing
#' to the average must be present. The expanded set of qualitative equivalence
#' classes is made by 39 patterns.
#'
#' This is an expanded  implementation recently submitted for publications.
#'
#' @param  myTB a dataset (tibble) for an indicator, time by countries.
#'               The first and last time are respectively the first
#'               and last rows of the dataset,
#'               which must be  time sorted.
#' @param  timeName  a string with name of the time variable
#' @return  the type of pattern
#'
#'
#'
#'
#' @export
#'
#' 
#' 
ms_pattern_39 <- function( myTB,
                           timeName="time"
                           ){
  #
  out_obj <- convergEU_glb()$tmpl_out
  res_chk <- check_data(myTB, timeName)
  if(!is.null(res_chk$err)){
    out_obj$err <- paste("Error: the dataset failed to pass preliminary checks. ",
                         res_chk$err);
    return(out_obj)
  }else{};
  countries <- setdiff(names(myTB),timeName)
  # EU averages
  matri_indi <- myTB[,countries]
  EU_ave  <- apply(matri_indi,1,mean)
  #
  res_MS <- list()# parameters for each member state
  num_rows <- nrow(myTB)
  temp_num_pat <- matrix(NA,nrow=length(countries),  ncol=num_rows)
  temp_str_pat <- matrix(NA,nrow=length(countries),  ncol=num_rows)
  temp_str_pat_num <- matrix(NA,nrow=length(countries),  ncol=num_rows)
  #-----------------
  #mappa_str_patt <- NA
  #----------------------------
  puntaCountry<- 0
  for( auxC in countries){# auxC <- "AT"
    puntaCountry <- puntaCountry + 1
    # type of indicator
    for(auxY in 2:num_rows){# auxY <- 4
      mMS <- unlist(myTB[(auxY-1):auxY, auxC])
      mEU <- unlist(EU_ave[(auxY-1):auxY])
      delta_E <- mEU[2] -mEU[1]
      timeW <-  myTB[[timeName]][(auxY-1):auxY]
      #
      temp_num_pat[puntaCountry,auxY] <-  map_2_patt_39(mMS,mEU,timeW,remap=TRUE)
      temp_str_pat[puntaCountry,auxY] <-  as.character(temp_num_pat[puntaCountry,auxY])
      temp_str_pat_num[puntaCountry,auxY] <- temp_num_pat[puntaCountry,auxY]
      }
    }
  # matrix of numerical tags produced as output
  mat_num_tags <- dplyr::tibble(Country=countries)# map
  mat_label_tags <- dplyr::tibble(Country=countries)# map
  #
  # column labels
  eticheTT <- paste(unlist(myTB[-1*num_rows,timeName]),"/",
                    unlist(myTB[-1,timeName]),sep="")
  # insert results
  punta <- 0
  for(auxTT in eticheTT){
    punta <- punta + 1
    mat_num_tags<- dplyr::mutate(mat_num_tags,  !!auxTT := temp_str_pat_num[,punta+1])
    mat_label_tags <- dplyr::mutate(mat_label_tags, !!auxTT := temp_str_pat[,punta+1])
  }
  ## Summaries: 1, 9 ,6
   workTag <- mat_num_tags
   cumulaOne <- apply(workTag[,-1] == 1, 1,sum, na.rm=T)
   cumulaNine <- apply(workTag[,-1] == 9, 1,sum, na.rm=T)
   cumulaSix <- apply(workTag[,-1] == 6, 1,sum, na.rm=T)
   mat_num_tags[,"Catching_up"] <- cumulaOne
   mat_num_tags[,"Falling_away"] <- cumulaNine
   mat_num_tags[,"Diving"] <- cumulaSix
  # # end of summaries
  out_obj$res <-  list(
    mat_num_tags   = mat_num_tags,
    mat_label_tags = mat_label_tags,
    mat_without_summaries =  workTag
    )
  return(out_obj);
}
