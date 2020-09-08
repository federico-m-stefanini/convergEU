#' Departures from the best country
#'
#' For each country the departure from the best performing Member State  is calculated.
#' Then, differences are cumulated over years.
#'
#' @param  oriTB  original dataset (tibble) with time by country values.
#' @param  timeName string with the name of the time variable in oriTB.
#' @param  indiType  the type of indicator 'highBest' (default) or 'lowBest'.
#' @return a list with component res which contains the departures from the best performer (for each country and for each year)
#'        and the cumulated differences over years.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Example 1
#' # Sorted dataframe in the format years by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#' ~time, ~countryA ,  ~countryB,  ~countryC,
#' 2000,     0.8,   2.7,    3.9,
#' 2001,     1.2,   3.2,    4.2,
#' 2002,     0.9,   2.9,    0.1,
#' 2003,     1.3,   2.9,    1.0,
#' 2004,     1.2,   3.1,    4.1,
#' 2005,     1.2,   3.0,    4.0
#' )
#'
#' # Departures from the best country according to the indicator higher is the best:
#' mySTB <- departure_best(testTB,timeName="time",indiType = "highBest")
#' # Differences from the best country for each year:
#' mySTB$res$raw_departures
#' # Sum of the cumulated differences for each country:
#' mySTB$res$cumulated_dif
#'
#' # Departures from the best country according to the indicator lower is the best:
#' mySTB1 <- departure_best(testTB,timeName="time",indiType = "lowBest")
#'
#' # Example 2
#' # Departures from the best country for the emp_20_64_MS Eurofound dataset:
#' mySTB2 <- departure_best(emp_20_64_MS,timeName="time",indiType = "highBest")
#' mySTB3 <- departure_best(emp_20_64_MS,timeName="time",indiType = "lowBest")
#'
#'
#' @export
#'
#'
departure_best <- function(oriTB,
                           timeName = "time",
                           indiType = 'highBest'){
  obj_out <- check_data(oriTB)
  if(!is.null(obj_out$err)){
    return(obj_out);
  }
  # check if time   matches after sorting
  outRes <- convergEU_glb()$tmpl_out
  oriTB <- dplyr::arrange_at(oriTB,timeName)
  posizTime <- which(names(oriTB) == timeName)
  dif_best <-  oriTB  # output
  if(indiType == 'highBest'){
    for( aux in 1:nrow(oriTB)) {
      tmpYear <-  oriTB[aux, -posizTime]
      dif_best[aux,-posizTime] <- tmpYear - max(tmpYear)
    }

  }else if(indiType == 'lowBest'){
    for( aux in 1:nrow(oriTB)) {
      tmpYear <-  oriTB[aux, -posizTime]
      dif_best[aux,-posizTime] <- min(tmpYear) - tmpYear
    }
  }else{
    outRes$err <- "Error: wrong selection"
    return(outRes)
  }
  cumul_diffe <- apply(dif_best[,-posizTime],2,sum)


  outRes$res <- list(
      raw_departures=  dif_best,
      cumulated_dif=   cumul_diffe
      )
  return(outRes)
}

