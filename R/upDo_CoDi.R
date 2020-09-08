#' Upward-downward convergence declaration
#'
#'
#' Convergence and divergence may be strict or weak, upward or
#' downward. The interpretation depends on the type of indicator,
#' that is "highBest" or "lowBest".
#'
#'
#' Note that if the argument heter_fun is set to sd or var,
#' then those statistics use a denominator which is n-1, i.e.
#' the number of observations decreased by 1.
#' This is not typically what one wants here, thus
#' the function pop_var may be used instead, because it adopts n as denominator.
#' It is also possible to map a summary of dispersion with a
#' monotonic function, like sqrt (see examples).
#'
#' All the Member states contributing to the mean must be columns
#' of the dataset given as input.
#'
#'
#' @param myTB  time by member states dataset. No other variables can
#'              be in the dataset.
#' @param timeName name of the variable that contains time.
#' @param indiType   a string, "lowBest" or "highBest".
#' @param time_0 reference time.
#' @param time_t target time strictly larger than time_0.
#' @param heter_fun function to summarize dispersion, like var(),
#'                    sd(); user-developed function
#'                    are allowed; pop_var is the variance with denominator n.
#' @return  list of declarations.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#'
#' @examples
#'
#' # using the standard deviation
#' upDo_CoDi(emp_20_64_MS,
#'          timeName = "time",
#'          indiType = "highBest",
#'          time_0 = 2010,
#'          time_t = 2015,
#'          heter_fun = "var" # watchout the denominator here is n-1
#'          )
#'
#'
#' # using the standard pop_var function
#' upDo_CoDi(emp_20_64_MS,
#'          timeName = "time",
#'          indiType = "highBest",
#'          time_0 = 2010,
#'          time_t = 2015,
#'          heter_fun = "pop_var" # the denominator here is n
#'          )
#'
#'
#'
#' # using personalized summary of dispersion
#' diffQQmu <-  function(vettore){
#'    (quantile(vettore,0.75)-quantile(vettore,0.25))/mean(vettore)
#'    }
#'
#' upDo_CoDi(emp_20_64_MS,
#'          timeName = "time",
#'          indiType = "highBest",
#'          time_0 = 2010,
#'          time_t = 2015,
#'          heter_fun = "diffQQmu"
#'          )
#'
#' @export
#'
#'
upDo_CoDi <- function(myTB,
                      timeName = "time",
                      indiType = "highBest",
                      time_0 = NA,
                      time_t = NA,
                      heter_fun = "pop_var"
                      ){
  # results
  outRes <- convergEU_glb()$tmpl_out
  # Checked data?
  res <-  check_data(myTB)
  if(!is.null(res$err)){
    return(res);
    };
  # check if timeName is present
  if(!(timeName %in% names(myTB))){
    outRes$err <- "Error: Time variable not in the dataframe."
    return(outRes)
  }else{ };
  # check time
  if(not_in(time_0 ,unlist(myTB[,timeName]))) {
    outRes$err <- "Error: wrong selected time window."
    return(outRes);
  }
  if(not_in(time_t, unlist(myTB[,timeName]))) {
    outRes$err <- "Error: wrong selected time window."
    return(outRes);
  };
  delta_time <- time_t - time_0
  if(delta_time <= 0){
    outRes$err <- "Error: wrong  time window selected."
    return(outRes)
  }
  workTB <- dplyr::filter(myTB, .data[[timeName]] == time_t |
                            .data[[timeName]] == time_0 )
 if(workTB[[timeName]][1] > workTB[[timeName]][2]){
   workTB <- workTB[c(2,1),]
 }
 # now they are properly sorted anyway
 # remove time and calculate
  wTB <- dplyr::select(workTB, - .data[[timeName]])
  averages <- apply(wTB,1,mean)
  aver_diffe <- averages[2] - averages[1]
  ## summarize dispersion
  sum_hete <- get(heter_fun)
  dispersion <- apply(wTB,1,sum_hete)
  #special case due to the structure of the returned object
  if(heter_fun == "pop_var"){
    dispersion <- unlist(lapply(dispersion,function(vx)vx$popvar))
  }
  ## differences by each country
  outRes$res<- list(declaration_type   = NA,
                    declaration_strict = "none",
                    declaration_weak   = "none",
                    declaration_split  = NA);
  ms_diffe <- wTB[2,] - wTB[1,]
  dispe_rid <- dispersion[2] < dispersion[1]
  # save numbers
  outRes$res$diffe_MS <- ms_diffe
  outRes$res$diffe_averages <- aver_diffe
  outRes$res$dispersions  <- dispersion
  names(outRes$res$dispersions) <- c(paste0("Time: ",time_0),
                                     paste0("Time: ",time_t));
  # the declaration of convergence/divergence does not depend of indictar type
  if(dispe_rid){#
    outRes$res$declaration_type <- "Convergence"
  }else{# case of equality aggregate to divergence
    outRes$res$declaration_type <- "Divergence"
  };
  # now the direction of changes
  # depends on indicator type
  # first highBest
  if( all(ms_diffe>=0) & (indiType == "highBest")){
    outRes$res$declaration_strict <- "Strict upward"
  }else if( all(ms_diffe < 0)& (indiType == "highBest")){
    outRes$res$declaration_strict <- "Strict downward"
  };
  # now lowBest
  if( all(ms_diffe>=0) & (indiType == "lowBest")){
    outRes$res$declaration_strict <- "Strict downward"
  }else if( all(ms_diffe < 0)& (indiType == "lowBest")){
    outRes$res$declaration_strict <- "Strict upward"
  };

  # Is there a weak behaviour?
  if( aver_diffe >= 0 & (indiType == "highBest")){
      outRes$res$declaration_weak <- "Weak upward"
  }else if( aver_diffe < 0& (indiType == "highBest")){
      outRes$res$declaration_weak <- "Weak downward"
  }
  if( aver_diffe >= 0 & (indiType == "lowBest")){
    outRes$res$declaration_weak <- "Weak downward"
  }else if( aver_diffe < 0& (indiType == "lowBest")){
    outRes$res$declaration_weak <- "Weak upward"
  }
  # split MS into two groups
  names_incre <- names(wTB)[ms_diffe >=0]
  names_decrea <- names(wTB)[ms_diffe <0]
  if(length(names_incre)==0) names_incre<-"none"
  if(length(names_decrea)==0) names_decrea<-"none"
  outRes$res$declaration_split <- list(
           names_incre = names_incre,
           names_decre = names_decrea
           );

  return(outRes)
}


utils::globalVariables(c(".data"))


