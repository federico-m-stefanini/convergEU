#' Sigma-convergence statistic
#'
#' Given a dataframe of quantitative indicators along time,
#' the sigma convergence is a statistic capturing some convergence features.
#' A time variable must be present whether sorted or not.
#' Missing values are not allowed.
#' Here it is calculated at each observed time.
#' All countries belonging to the reference mean must be included into the
#' dataset.
#'
#' @param  tavDes the dataframe time by countries.
#' @param  timeName the name of the variable that contains time information.
#' @param  time_0 starting time to consider; if NA all times considered.
#' @param  time_t last time to consider; if NA all times considered.
#' @return  a tibble with the value of sigma convergence (called stdDev or CV)
#'           along time, where the original *timeName* is preserved.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Example 1
#' # Dataframe in the format time by countries:
#' require(tibble)
#' myTB  <- tibble::tribble(
#'     ~years, ~UK, ~DE, ~IT,
#'     1990,   998,  1250, 332,
#'     1988,   1201, 868, 578,
#'     1989,   1150, 978, 682
#'     )
#' reSigConv <- sigma_conv(myTB,timeName="years")
#'
#' # Results for the sigma convergence:
#' reSigConv$res
#'
#' # Example 2
#' # Sigma convergence, scrambled time, different name, subset of times:
#' myTB1  <- tibble::tribble(
#'     ~years, ~UK, ~DE, ~IT,
#'     1990,   998,  1250, 332,
#'     1988,   1201, 868, 578,
#'     1989,   1150, 978, 682,
#'     1991,   232, 225, 227,
#'     1987,   122, 212, 154
#'     )
#' reSigConv1 <- sigma_conv(myTB1,timeName="years", time_0 = 1988,time_t = 1990)
#'
#' # Example 3
#' # Sigma convergence for the emp_20_64_MS Eurofound dataset:
#' data("emp_20_64_MS")
#' reSigConv2 <- sigma_conv(emp_20_64_MS)
#' reSigConv3 <- sigma_conv(emp_20_64_MS, timeName = "time", time_0 = 2002,time_t = 2004)
#' reSigConv4 <- sigma_conv(emp_20_64_MS, timeName = "time", time_0 = 2002,time_t = 2016)
#'
#' @export
#'
#'
sigma_conv <- function(tavDes, timeName = "time", time_0 = NA, time_t=NA){
  # Make standard cheks on the dataset
  obj_out <- check_data(tavDes)
  if(!is.null(obj_out$err)){
    return(obj_out);
  }
  # check if timeName is present
  if(timeName %in% names(tavDes)){
    # sort by timeName
    myDes1 <- dplyr::arrange_at(tavDes,timeName)# ordered by time
    # select time window
    if(!is.na(time_0) & !is.na(time_t)) {
        # check
        tempiCur <- unlist(myDes1[,timeName])
        if( (time_0 < time_t) && (time_0 %in% tempiCur)   && (time_t %in% tempiCur) ){
            myDes1 <- myDes1[(tempiCur >= time_0 ) & (tempiCur <= time_t),];
        }else{# condizione di errore
          obj_out <-  convergEU_glb()$tmpl_out
          obj_out$err <- "Error: wrong time window."
          return(obj_out)
          }
        }else{};
    # eliminate time variables
    myDes2 <- dplyr::select(myDes1, -timeName)
  }else{
    tmp <- convergEU_glb()$tmpl_out
    tmp$err <- "Error: declared time variable absent."
    return(tmp)
    }
  # make calculations
  res_med <- apply(myDes2,1,mean)
  res_sd <- apply(myDes2,1,function(vetDat){
     tmp <- sum((vetDat- mean(vetDat))^2)
     tmp
     })
  denominat <- ncol(myDes2)
  res <- dplyr::tibble(stdDev= sqrt(res_sd/denominat),
                       CV = sqrt(res_sd/denominat) / res_med,# compatibility with CRAN
                       mean = res_med,
                       devianceT= res_sd
  )
  res2 <- dplyr::mutate(res , !!timeName := as.numeric(unlist(myDes1[,timeName])))
  obj_out$res <- res2[,c(5,1,2,3,4)]
  return(obj_out)
}
