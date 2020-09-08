#' Auxiliary function compo_cond_EUS
#'
#' Not exported
#'
#'
#' @param myScarica a bulk downloaded tibble from Eurostat
#' @return a tag based on indicator-specific  conditioning variables
#'
#'
compo_cond_EUS <- function(myScarica){
  all_vars <- names(myScarica)
  left_over <- setdiff(all_vars,c("sex","age","time","geo","values"))
  if(length(left_over) <1) return(NULL)
  # known_vars <- setdiff(c("sex","age","time","geo","values"),all_vars)
  tmpTB <- dplyr::select(myScarica,left_over)
  for(aux in left_over){
    tmpTB[aux] <- paste(aux,": ",as.character(tmpTB[[aux]],"; "),sep="")
  }

  nuovoTag <- apply(tmpTB,1,paste,collapse="; ")
  nuovoTag
}



