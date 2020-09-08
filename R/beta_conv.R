#' Beta-convergence statistic
#'
#' Given a dataframe of quantitative indicators along time,
#' the unconditional beta convergence is a statistic
#' capturing some important features.
#' A time variable must be present and sorted.
#' Missing values are not allowed. All other columns are indicator values
#' in each considered country.
#'
#'
#'
#' @param  tavDes the sorted dataframe time by countries on the original scale.
#'                No other variable besides time and countries' indicator
#'                must be present.
#' @param  time_0 reference time.
#' @param  time_t target time strictly larger than time_0.
#' @param  all_within  is FALSE if just two different years are considered (default);
#'         if  more than two years  are desired within the
#'         specified interval then it must be  TRUE ; the reference time remains
#'         time_0.
#' @param  timeName the name of the variable that contains time information.
#' @param  useTau  if TRUE the log ratio of indicator values is divided for
#'         the elapsed time (years).
#' @return a list with the value of beta-conv, by OLS (least-squares), the
#'         transformed data and standard statistical tests.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @importFrom  rlang  .data
#' @examples
#'
#' # Example 1:
#' # Dataframe in the format years by countries:
#' require(tibble)
#' myTB1  <- tibble::tribble(
#'   ~years, ~UK, ~DE, ~IT,
#'   1990,   998,  1250, 332,
#'   1988,   1201, 868, 578,
#'   1989,   1150, 978, 682,
#'   1991,  1600,  1350, 802
#' )
#'
#' # Sort the time variable:
#' newdata <- myTB1[order(myTB1$years),]
#'
#' # Beta convergence statistic by considering just two times, e.g. 1989 and 1991:
#' myBC1 <- beta_conv(newdata,1989,1991,timeName="years")
#'
#' # Visualize the summary of the results (estimated coefficients, standard errors, p-values):
#' myBC1$res$summary
#'
#' # Visualize the adjusted R-squared:
#' myBC1$res$adj.r.squared
#'
#' # Beta convergence statistic by considering more than two times:
#' myBC2 <- beta_conv(newdata,1988,1991,all_within=TRUE,timeName="years")
#'
#' # Example 2:
#' # Dataframe in the format years by countries, time variable already sorted:
#' testTB <- tribble(
#'     ~time, ~countryA ,  ~countryB,  ~countryC,
#'     2000,     0.8,   2.7,    3.9,
#'     2001,     1.2,   3.2,    4.2,
#'     2002,     0.9,   2.9,    4.1,
#'     2003,     1.3,   2.9,    4.0,
#'     2004,     1.2,   3.1,    4.1,
#'     2005,     1.2,   3.0,    4.0
#'     )
#' myBC3 <- beta_conv(testTB, time_0 = 2000, time_t = 2005, timeName = "time")
#' myBC4 <- beta_conv(testTB, time_0 = 2000, time_t = 2005, all_within = TRUE, timeName = "time")
#'
#' # Example 3
#' # Beta convergence for the emp_20_64_MS Eurofound dataset:
#' data(emp_20_64_MS)
#' empBC <- beta_conv(emp_20_64_MS, time_0 = 2002, time_t = 2006, timeName = "time")
#'
#' # Summary of the model results:
#' empBC$res$summary
#'
#' # Adjusted R-squared:
#' empBC$res$adj.r.squared
#'
#'
#' @export
#'
beta_conv <- function(tavDes,time_0, time_t, all_within=FALSE,
                      timeName = "time",  useTau = TRUE){
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
  # Make standard checks on the dataframe
  tt_out <- check_data(tavDes, timeName = timeName)
  if(!is.null(tt_out$err)){
    return(tt_out);
    }
  # At least three columns and 2 rows needed due to differencing
  if(nrow(tavDes) < 2 && ncol(tavDes) < 4){
    obj_out$err <- "Error: at least 2 rows and 4 columns needed in the dataframe."
    return(obj_out)
    }
  # extract and sort
  tavDes <- dplyr::arrange_at(tavDes,timeName)
  #timeRaw <- unlist(tavDes[,timeName])
  timeRaw <- tavDes[[timeName]]
  pos_t0 <- which(timeRaw == time_0)
  pos_tt <- which(timeRaw == time_t)
  pos_all <- which( (timeRaw >= time_0) & (timeRaw <= time_t) )
  delta_time <- time_t - time_0
  if(delta_time <= 0){
    obj_out$err  <- "Error: null or negative elapsed years."
    return(obj_out)
  }
  #
  if(all_within) {# all times within the interval
      resTB <- tavDes[pos_all,]
      # sorted by time
      resTB <- dplyr::arrange_at(resTB,timeName)
   }else{
      #just the two considered rows
      resTB <- tavDes[c(pos_t0,pos_tt),]
      };
  # now it it sure the reference is in the first row
  # Non negative values? Needed because log(indicator) taken
  if( sum(dplyr::select(resTB, -timeName)  <= 0) > 0){
    obj_out$err  <- "Error: negative values in the indicator."
    return(obj_out)
  }
  # log-transform, already checked positivity
  reslogTB <- log(dplyr::select(resTB, -timeName))
  ## wTB <- cbind(dplyr::select(resTB, timeName),reslogTB)
  ## Should we divide the ordinates by number of elapsed years? YES, possibly
  workTB <- dplyr::tibble(
        deltaIndic = as.numeric(reslogTB[2,] - reslogTB[1,]),
        indic = as.numeric(reslogTB[1,]),
        countries = names(reslogTB))
  if(useTau){
    # divide by elapsed time
    workTB <- dplyr::mutate(workTB, deltaIndic = .data$deltaIndic / delta_time);
    }
  #
  if(nrow(reslogTB) > 2){# several years besides time_t and time_0
    for(auxR in 3:nrow(reslogTB)){
       tmpTB <- dplyr::tibble(
         deltaIndic = as.numeric(reslogTB[auxR,] - reslogTB[1,]),
         indic = as.numeric(reslogTB[1,]),# maybe reslogTB[auxR-1, ]
         countries = names(reslogTB))
       # tau?
       if(useTau){
         # divide by elapsed time
         tmpTB <- dplyr::mutate(tmpTB, deltaIndic = .data$deltaIndic / delta_time);
         }
        workTB <- rbind(workTB,tmpTB)# end add_row
    };
  }# end several years
  #
  # make calculations
  workMOD <- stats::lm(deltaIndic  ~ indic, data=workTB)
  obj_out$res <- list(
    workTB = workTB,
    model = workMOD,
    summary = broom::tidy(workMOD),
    beta1 = NA,
    adj.r.squared = summary(workMOD)$adj.r.squared
  )
  obj_out$res$beta1 <- as.numeric(obj_out$res$summary[2,2])
  return(obj_out)
}

utils::globalVariables(c(".data"))
