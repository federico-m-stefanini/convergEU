#' Plot of deviations from the best performer
#'
#' Deviations from the best performer  are added over years
#' and plotted by country.
#'
#' @param cumulaDifVector a vector of cumulated differences, say from
#'         a call to departure_best()$res$cumulated_dif, with named elements.
#' @param mainCountry the main country of interest.
#' @param countries selection of countries to display; NA means all countries
#' @param displace graphical displacement
#' @param axis_name_y  name of the axis
#' @param val_alpha  transparency value in (0,1].
#' @param debug  a flag to get debug information as msg component
#' @return a list with ggplot2 graphical object within res component
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#' \donttest{
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
#' # Departures from the best country according to the "highBest" indicator:
#' mySTB <- departure_best(testTB,timeName="time",indiType = "highBest")
#'
#' # Plot of deviations from the best performer:
#' departure_best_plot(cumulaDifVector = mySTB$res$cumulated_dif, mainCountry = "countryC",
#' countries = c("countryA","countryB"),displace = 0.25,
#' axis_name_y = "Countries",val_alpha  = 0.95,debug=FALSE)
#'
#' # Departures from the best country according to the "lowBest" indicator:
#' mySTB1 <- departure_best(testTB,timeName="time",indiType = "lowBest")
#' departure_best_plot(cumulaDifVector = mySTB1$res$cumulated_dif, mainCountry = "countryC",
#' countries = c("countryA","countryB"),displace = 0.25,
#' axis_name_y = "Countries",val_alpha  = 0.95,debug=FALSE)
#'
#' # Example 2
#' # Departures from the best country for the emp_20_64_MS Eurofound dataset:
#' mySTB2 <- departure_best(emp_20_64_MS,timeName="time",indiType = "highBest")
#' # Plot of deviations from the best performer with Italy as the country of interest:
#' departure_best_plot(mySTB2$res$cumulated_dif,
#'   mainCountry = "IT",
#'   countries=c("AT", "DE", "FR","SE","SK"),
#'   displace = 0.25,
#'   axis_name_y = "Countries",
#'   val_alpha  = 0.95,
#'   debug=FALSE)
#'
#'  mySTB3 <- departure_best(emp_20_64_MS,timeName="time",indiType = "lowBest")
#'  # Plot of deviations from the best performer with Germany as the country of interest:
#'  departure_best_plot(mySTB3$res$cumulated_dif,
#'  mainCountry = "DE",
#'  countries=c("AT", "SE", "FR","IT","SK"),
#'  displace = 0.25,
#'  axis_name_y = "Countries",
#'  val_alpha  = 0.95,
#'  debug=FALSE)
#'  }
#' @export
#'
#'
departure_best_plot <- function(cumulaDifVector,
                          mainCountry = NA,
                          countries=c(NA,NA),
                          displace = 0.25,
                          axis_name_y = "Countries",
                          val_alpha  = 0.95,
                          debug=FALSE){
allCountries <- names(cumulaDifVector)
if(length(setdiff(countries,allCountries)) > 0  & !is.na(countries[1])){
    out_obj <- convergEU_glb()$tmpl_out
    out_obj$err <- "Error: wrong selection of countries."
    return(out_obj)
  }
 if(!(mainCountry %in% allCountries)){
   out_obj <- convergEU_glb()$tmpl_out
   out_obj$err <- "Error: wrong selection of the main country."
   return(out_obj)
 }
  # put countries of interest top
  nameMS <- mainCountry
  if(!is.na(countries[1])){
    nameMS <- c(nameMS,setdiff(countries,mainCountry));
  }
  resDiffe <- dplyr::tibble(MS = nameMS,
                            negaSum = cumulaDifVector[nameMS],
                            posi = 1:length(nameMS));

  miniX <- min(resDiffe[["negaSum"]])
  miniX <-  miniX + miniX/100
  maxiX <-  0+ abs(min(resDiffe[["negaSum"]]))/100
  etichY  <-  resDiffe$MS
  names(etichY) <-  etichY

  myTBr  <- dplyr::tibble(
                   MS = c(resDiffe$MS ),
                   posi = c(resDiffe$posi ),
                   xmin = c(resDiffe$negaSum),
                   xmax = c(0*resDiffe$negaSum),
                   ymin = c(resDiffe$posi - displace),
                   ymax = c(resDiffe$posi + displace),
                   fillCol= factor(
                     rep(c("neg."), each=length(resDiffe$posi))
                   ));
  ## myTBr already filtered
  myGG <- ggplot2::ggplot(myTBr,
                  ggplot2::aes(x = xmin, y = posi)) +
    ggplot2::scale_y_discrete(
      axis_name_y,
      labels = etichY,
      limits = etichY
    ) + ggplot2::xlim(c(miniX,maxiX)) +
    ggplot2::geom_rect(data = myTBr,
                       mapping = ggplot2::aes(
                         xmin = xmin,
                         xmax = xmax,
                         ymin = ymin,
                         ymax = ymax,
                         fill = fillCol),
                       color = "grey3", alpha = val_alpha
    ) +
    ggplot2::scale_fill_manual(values = c('#ff0000','#0033cc')) +
    ggplot2::geom_vline(xintercept=0,colour="gray40") +
    ggplot2::xlab("Sum of deviations from best performer") +
    ggplot2::theme(legend.position = "none")

out_obj <- convergEU_glb()$tmpl_out
out_obj$res <- myGG
if(debug){
  out_obj$msg <- resDiffe
  }
return(out_obj)
}

