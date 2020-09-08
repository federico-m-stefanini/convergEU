#' Find patterns for all countries
#'
#' The input is a time by countries dataset where all countries contributing
#' to the average must be present.
#' Indicators of type 'low is better' are transformed
#' (highestRef - Y), thus the distance from the maximum value
#' for each original observation is calculated.
#'
#' This is the reference implementation as described  by the Eurofound  report
#' "Monitoring convergence in the European Union
#' Upward convergence in the EU: Concepts, measurements and indicators", 2018.
#'
#' @param  myTB a dataset (tibble) for an indicator, time by countries.
#'               The first and last time are respectively the first
#'               and last rows of the dataset,
#'               which must be  time sorted.
#' @param  timeName  a string with name of the time variable
#' @param  typeIn  the type of indicator considered 'highBest' (default)
#'                 or 'lowBest'
#' @return  the type of pattern
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#'
#' @export
#'
#'
ms_pattern_ori <- function(myTB,
                           timeName="time",
                           typeIn=c('highBest','lowBest')[1]){
  #
  out_obj <- convergEU_glb()$tmpl_out
  epsilonV <- 0.001;
  res_chk <- check_data(myTB, timeName)
  if(!is.null(res_chk$err)){
    out_obj$err <- paste("Error: the dataset failed to pass preliminary checks. ",
                         res_chk$err);
    return(out_obj)
  }else{};
  #
  countries <- setdiff(names(myTB),timeName)
  # type of indicator
  if(typeIn == "lowBest") {
    tmpM <- as.matrix(myTB[, -which(timeName == names(myTB))])
    observed_max <- max(unlist(tmpM))
    highestRef <- observed_max + epsilonV;
    # transform them into highBest equivalent indicator
    for(auxLI in countries){
      myTB <- dplyr::mutate(myTB,  !! auxLI := highestRef - myTB[[auxLI]])
      };
  }else{};
  # do calulations
  # EU averages
  matri_indi <- myTB[,countries]
  EU_ave  <- apply(matri_indi,1,mean)
  #
  res_MS <- list()# parameters for each member state
  num_rows <- nrow(myTB)
  temp_num_pat <- matrix(NA,nrow=length(countries),  ncol=num_rows)
  temp_str_pat <- matrix(NA,nrow=length(countries),  ncol=num_rows)
  temp_str_pat_num <- matrix(NA,nrow=length(countries),  ncol=num_rows)

  mappa_str_patt <- dplyr::tibble(
      tag.HB = c("Catching up", "Flattening", "Inversion", "Outperforming",
       "Slower pace", "Diving", "Defending better", "Escaping",
       "Falling away", "Underperforming", "Recovering",
       "Reacting better", "Parallel-better-over", "Parallel-equal-over",
       "Parallel-worse-over", "Parallel-worse-under", "Parallel-equal-under",
       "Parallel-better-under", "Crossing", "Crossing reversed",
       "Other (Inspection)"
     ),
     num.tag=1:21,
     num.tag.HB=1:21,
     num.tag.LB=c(10,12,11,9,7,8,5,6,4,1,3,2,16,17,18,13,14,15,20,19,21),
     tag.LB =
     c("Underperforming" ,
     "Reacting better" ,
     "Recovering" ,
     "Falling away",
     "Defending better",
     "Escaping",
     "Slower pace",
     "Diving",
     "Outperforming",
     "Catching up",
     "Inversion",
     "Flattening",
     "Parallel-worse-under" ,
     "Parallel-equal-under" ,
     "Parallel-better-under" ,
     "Parallel-better-over" ,
     "Parallel-equal-over" ,
     "Parallel-worse-over" ,
     "Crossing reversed",
     "Crossing",
     "Other (Inspection)")
     )
  puntaCountry<- 0
  for( auxC in countries){
    puntaCountry <- puntaCountry + 1
    # type of indicator
    for(auxY in 2:num_rows){# auxY <- 2
      mMS <- unlist(myTB[(auxY-1):auxY, auxC])
      mEU <- unlist(EU_ave[(auxY-1):auxY])
      timeW <- unlist(myTB[(auxY-1):auxY, timeName])
      temp_num_pat[puntaCountry,auxY]  <- gra_de2_patt(mEU,mMS,timeW)
      if(typeIn == 'highBest'){
         record_sel <- dplyr::filter(mappa_str_patt,
                        .data$num.tag.HB == temp_num_pat[puntaCountry,auxY])
         etiche_patt <- record_sel$tag.HB
         nume_patte <- record_sel$num.tag.HB
      }else if(typeIn == 'lowBest'){
        record_sel  <- dplyr::filter(mappa_str_patt,
                    .data$num.tag.LB == temp_num_pat[puntaCountry,auxY])
        etiche_patt <- record_sel$tag.LB
        nume_patte <- record_sel$num.tag.LB
      }else{
        stop("Error: indicator type unknown.")
      }
      temp_str_pat[puntaCountry,auxY] <-  etiche_patt
      temp_str_pat_num[puntaCountry,auxY] <- nume_patte
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
    mat_num_tags[,auxTT] <- temp_str_pat_num[,punta+1]
    mat_label_tags[,auxTT]  <- temp_str_pat[,punta+1]
  }
  #
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
