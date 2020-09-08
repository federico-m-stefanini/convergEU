#' Delta-convergence statistic
#'
#' Given a dataframe of quantitative indicators along time,
#' the delta convergence is a statistic describing departures from best performer.
#' A time variable may be present or not, but if it is not present
#' then rows must be already sorted. Missing values are not allowed.
#' If the time variable is omitted, subsequent rows are separated
#' by one time unit.
#'
#' @param  tavDes the dataframe time by countries.
#' @param  timeName the name of the variable that contains time information;
#'                  if it is set to NA then no time information is exploited.
#' @param  indiType the indicator type; the default is "highBest", otherwise
#'                   it is equal to "lowBest".
#' @param  time_0 starting time to consider; if NA all times considered.
#' @param  time_t last time to consider; if NA all times considered.
#' @param  extended if FALSE only  measures of convergence are produced, otherwise
#'                 the declaration of convergence is also provided.
#' @return  a tibble with the value of delta-conv (called delta) along time,
#'          which is called 'time'.
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Delta convergence with time present
#' # Dataframe in the format time by countries:
#' myTB  <- tibble::tribble(
#' ~time, ~UK, ~DE, ~IT,
#' 1988,   1201, 868, 578,
#' 1989,   1150, 978, 682,
#' 1990,   998,  1250, 332
#' )
#' resDelta <- delta_conv(myTB)
#'
#' # Example 2
#' # Delta convergence with scrambled time order (time present):
#' myTB2  <- tibble::tribble(
#' ~time, ~UK, ~DE, ~IT,
#' 1990,   998,  1250, 332,
#' 1988,   1201, 868, 578,
#' 1989,   1150, 978, 682
#' )
#' resDelta1<-delta_conv(myTB2)
#'
#' # Example 3
#' # Delta convergence, scrambled time and different name for the time variable:
#' myTB2  <- tibble::tribble(
#' ~years, ~UK, ~DE, ~IT,
#' 90,   998,  1250, 332,
#' 88,   1201, 868, 578,
#' 89,   1150, 978, 682
#' )
#' resDelta2 <- delta_conv(myTB2,timeName="years")
#'
#' # Example 4
#' # Delta convergence for the emp_20_64_MS Eurofound dataset:
#' data("emp_20_64_MS")
#' # check name of the time variable:
#' names(emp_20_64_MS)
#'
#' # Calculate delta convergence:
#' resDelta3<-delta_conv(emp_20_64_MS)
#'
#' # Obtain measures of delta-convergence and the declaration of convergence:
#' resDelta4<-delta_conv(emp_20_64_MS, extended = TRUE)
#'
#' @export
#'
#'
delta_conv <- function(tavDes,
                       timeName = "time",
                       indiType="highBest",
                       time_0 = NA,
                       time_t = NA,
                       extended = FALSE){

  obj_out <- check_data(tavDes)# Make standard cheks on the dataset
  if(!is.null(obj_out$err)){
    return(obj_out);
  }
  #
  obj_out <- convergEU_glb()$tmpl_out
  # check if timeName is present
  if(timeName %in% names(tavDes)){
        # sort by timeName
        myDes1 <- dplyr::arrange_at(tavDes,timeName)# ordered by time
        # select time window
        if(is.na(time_0)) {
          time_0 <- myDes1[[timeName]][1]
        }
        if(is.na(time_t)) {
          time_t <- myDes1[[timeName]][nrow(myDes1)]
        }
        # check
        tempiCur <- unlist(myDes1[[timeName]])
        if( (time_0 < time_t) &&
            (time_0 %in% tempiCur) &&
            (time_t %in% tempiCur)){
                   myDes1 <- dplyr::filter(myDes1,
                            (tempiCur >= time_0 ) & (tempiCur <= time_t));
        }else{# condizione di errore
            obj_out$err <- "Error: wrong time window."
            return(obj_out)
          };
        # eliminate time variables
        myDes2 <- dplyr::select(myDes1, -timeName)
    }else{
        obj_out$err <- "Error: declared time variable absent."
        return(obj_out)
  }
  #
  # make calculations
  if(indiType == "highBest"){
      delta_co_val0 <- t(apply(myDes2,1,function(vetto){
             massimo <- max(vetto)
             (massimo - vetto)
         }))
      delta_co_val <- apply(delta_co_val0,1,sum)
  }else if(indiType == "lowBest"){
      delta_co_val0 <- t(apply(myDes2,1,function(vetto){
         minimo<- min(vetto)
         (vetto-minimo)
         }))
      delta_co_val <- apply(delta_co_val0,1,sum)
  }else{
     # error
    obj_out$err <- "Error: wrong indicator type."
    return(obj_out)
  }
  #
  ## packaging the output
  res <- dplyr::tibble(time = myDes1[[timeName]],
                       delta = delta_co_val)
  if(!extended){
      obj_out$res <- res
  }else{
    # further calculations to declare type of convergence
     diffeFL <- delta_co_val0[nrow(delta_co_val0),]-delta_co_val0[1,]
     # to be strict: all but one negative  or all but one positive
     strict_il <- ((all(diffeFL <= 0) && (sum(diffeFL == 0) == 1)) |
                  (all(diffeFL >= 0) && (sum(diffeFL == 0) == 1)));
     #
     diffe_delta <- delta_co_val[length(delta_co_val)] - delta_co_val[1]
     conv_il <-  diffe_delta < 0;
     #
    if(diffe_delta  == 0){
      label_type <- "unchanged"
    }else if(diffe_delta < 0){
      label_type <- "convergence"
    }else{
      label_type <- "divergence"
    }
     #
     if(strict_il){
       label_strict <- "strict"
     }else{
       label_strict <- " "
     }
    obj_out$res <- list(
        delta_conv = res,
        differences = delta_co_val0,
        difference_last_first = diffeFL,
        strict_conv_ini_last = strict_il,
        label_strict = label_strict,
        converg_ini_last = conv_il,
        label_conver = label_type,
        diffe_delta = diffe_delta
       )
  }
  return(obj_out)
}
