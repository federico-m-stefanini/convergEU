#' Departures from an average
#'
#' For each country the departure from the average is calculated
#' and a numerical label is created: -1 if smaller than one standard deviation
#' from the mean, +1 if above one standard deviation from the mean, 0 otherwise.
#'
#'
#' @param  oriTB  original dataset (tibble) with time by country values.
#' @param  sigmaTB result from sigma_convergence called on oriTB.
#' @param  timeName string with the name of the time variable in oriTB.
#' @return  list of tibbles containing  labelled departures from the mean, square difference from the mean,
#'          and percentage of deviance.
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # The original dataset in the format time by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#' ~time, ~countryA ,  ~countryB,  ~countryC,
#' 2000,     0.8,   2.7,    3.9,
#' 2001,     1.2,   3.2,    4.2,
#' 2002,     0.9,   2.9,    4.1,
#' 2003,     1.3,   2.9,    4.0,
#' 2004,     1.2,   3.1,    4.1,
#' 2005,     1.2,   3.0,    4.0
#' )
#'
#' # Calculate sigma_convergence on the original dataset:
#' mySTB <- sigma_conv(testTB)
#'
#' # Calculate departures from the average for each country:
#' resDM <-  departure_mean(oriTB=testTB, sigmaTB=mySTB$res)
#' names(resDM$res)
#'
#' # Example 2: Departures from the average for the Eurofound dataset "emp_20_64_MS"
#' data(emp_20_64_MS)
#' # Sigma convergence on the original dataset:
#' mySC <- sigma_conv(emp_20_64_MS)
#'
#' # Calculate departures from the mean for each country:
#' resDMeur <- departure_mean(oriTB = emp_20_64_MS, sigmaTB = mySC$res)
#'
#' # Results for labelled departures from the mean:
#' resDMeur$res$departures
#'
#' # Results for square difference from the mean:
#' resDMeur$res$squaredContrib
#'
#' # Results for the percentage of deviance:
#' resDMeur$res$devianceContrib
#'
#' @export
#'
#'
departure_mean <- function(oriTB, sigmaTB, timeName = "time"){
  obj_out <- check_data(oriTB)
  if(!is.null(obj_out$err)){
    return(obj_out);
  }
  # check if time   matches after sorting
  outRes <- convergEU_glb()$tmpl_out
  oriTB <- dplyr::arrange_at(oriTB,timeName)
  sigmaTB <- dplyr::arrange_at(sigmaTB,timeName)
  if(any(oriTB[[timeName]] !=  sigmaTB[[timeName]])){
      outRes$err <- "Error: wrong time scale for data in imput."
    return(outRes);
  }
  squaDif <-  dplyr::select(oriTB,-!!timeName)
  perceVaT <-  squaDif
  recodedTB <- dplyr::select(oriTB,-!!timeName)
  resTB <- dplyr::mutate(sigmaTB,
                  elle1 = sigmaTB$mean -1* sigmaTB$stdDev,
                  elle2 = sigmaTB$mean +1* sigmaTB$stdDev
  )
  # for each country
  for( aux in names(recodedTB)){
    # do calculations tag departures
    infEstra <- recodedTB[[aux]] < resTB$elle1
    supEstra <- recodedTB[[aux]] > resTB$elle2
    recodedTB[[aux]] <- 0
    recodedTB[infEstra,aux] <- -1
    recodedTB[supEstra,aux] <- +1
    squaDif[[aux]] <- (squaDif[[aux]] - sigmaTB$mean)^2
    perceVaT[[aux]] <- 100* squaDif[[aux]]/ sigmaTB$devianceT
  }
  outRes$res <- list(
    departures= dplyr::bind_cols(sigmaTB,recodedTB),
    squaredContrib= squaDif,
    devianceContrib = perceVaT
  )
  return(outRes)
}
