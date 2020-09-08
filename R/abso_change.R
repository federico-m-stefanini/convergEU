#' Absolute change
#'
#' Given a dataframe of quantitative indicators along time,
#' the absolute change is calculated.
#' A time variable must be present and sorted.
#' Missing values are not allowed. All other columns are indicator values
#' in each considered country.
#'
#' @param  tavDes the sorted dataframe time by countries.
#'                No other variables besides time and countries' indicator
#'                must be present.
#' @param  time_0 reference time
#' @param  time_t focus time strictly larger than time_0
#' @param  all_within  is TRUE is several times are considered within the
#'         specified interval (default), otherwise FALSE; the reference time remains
#'         time_0.
#' @param  timeName the name of the variable that contains time information
#' @return a list of absolute changes for each country, the sum of absolute values and the average per pairs
#'         of years.
#' @examples
#'
#' # Example 1
#' # Sorted dataframe in the format years by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#' ~years, ~countryA ,  ~countryB,  ~countryC,
#' 2000,     0.8,   2.7,    3.9,
#' 2001,     1.2,   3.2,    4.2,
#' 2002,     0.9,   2.9,    4.1,
#' 2003,     1.3,   2.9,    4.0,
#' 2004,     1.2,   3.1,    4.1,
#' 2005,     1.2,   3.0,    4.0)
#'
#' # Absolute change for each country with time_0=2000 and time_t=2005:
#' mySTB<-abso_change(tavDes=testTB,time_0=2000, time_t=2005, timeName ="years")
#'
#' # The component "res" is a list of absolute changes for each country,
#' # the sum of absolute values and the average per pairs of years:
#' names(mySTB$res)
#'
#' # Absolute change for each country with time_0=2002 and time_t=2005:
#' mySTB1<-abso_change(tavDes=testTB,time_0=2002, time_t=2005, timeName="years")
#'
#' # If all_within is FALSE, only times 2002 and 2005 are considered:
#' mySTB2<-abso_change(tavDes=testTB,time_0=2002, time_t=2005, all_within =FALSE, timeName="years")
#'
#' # Example 2
#' # Absolute changes of Member States for the emp_20_64_MS Eurofound dataset:
#' data(emp_20_64_MS)
#' mySTB3 <- abso_change(emp_20_64_MS,time_0 = 2005,time_t = 2010,timeName = "time")
#' mySTB4 <- abso_change(emp_20_64_MS,time_0 = 2007,time_t = 2012,timeName = "time")
#'
#'
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @export
#'
abso_change <- function(tavDes,time_0, time_t,
                        all_within=TRUE,
                        timeName = "time"){
  obj_out <- convergEU_glb()$tmpl_out
  # check if timeName is present
  if(not_in(timeName,names(tavDes))) {
    obj_out$err <- "Error: declared time variable absent."
    return(obj_out)
  }else{};
  # check time
  if(time_0 >= time_t){
    obj_out$err <- "Error: wrong selected time window."
    return(obj_out);
  }
  if(not_in(time_0 ,unlist(tavDes[,timeName]))) {
    obj_out$err <- "Error: wrong selected time window."
    return(obj_out);
  }
  if(not_in(time_t, unlist(tavDes[,timeName]))) {
    obj_out$err <- "Error: wrong selected time window."
    return(obj_out);
  }
  # Make standard cheks on the dataframe
  ck_out <- check_data(tavDes, timeName = timeName)
  if(!is.null(ck_out$err)){
    return(ck_out);
  }
  # At least three columns and 2 rows needed due to differencing
  if(nrow(tavDes) < 2 ){
    obj_out$err <- "Error: at least 2 rows needed in the dataframe."
    return(obj_out)
    }
  # extract and sort
  timeRaw <- unlist(tavDes[,timeName])
  pos_t0 <- which(timeRaw == time_0)
  pos_tt <- which(timeRaw == time_t)
  pos_all <- which( (timeRaw >= time_0) & (timeRaw <= time_t) )
  #
  if(all_within) {# all times within the interval
      resTB <- tavDes[pos_all,]
      # sorted by time
      resTB <- dplyr::arrange_at(resTB,timeName)
   }else{
      #just the two considered rows
      resTB <- tavDes[c(pos_t0,pos_tt),]
      };
  # now it is sure the reference is in the first row
  ## differencing
  posiTime <-  which(timeName == names(tavDes))
  workTB <- tavDes[2,-posiTime] - tavDes[1,-posiTime]
  workTB <- cbind(tavDes[2,posiTime], workTB)
  if(nrow(resTB) > 2){# several years besides time_t and time_0
    for(auxR in 3:nrow(resTB)){
      TTB <- tavDes[auxR,-posiTime] - tavDes[auxR-1,-posiTime]
      TTB2 <- cbind(tavDes[auxR,posiTime], TTB)
      workTB <- rbind(workTB,TTB2)
    };
  }# end several years
  # sum of absolute deviations
  TTB3 <- workTB[,-1]
  sumAbCh <- apply(TTB3,2,function(vx){sum(abs(vx))})
  average_over_transitions <-  sumAbCh /(time_t - time_0 )
  obj_out$res <- list(
    abso_change = workTB,
    sum_abs_change =  sumAbCh,
    average_abs_change = average_over_transitions
    )
  return(obj_out)
}
