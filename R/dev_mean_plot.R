#' Plot of deviations from the mean
#'
#' Negative deviations and positive deviations are added over years
#' and plotted by country.
#'
#' @param myTB a dataset time by countries
#' @param timeName name of the variable representing time
#' @param time_0 starting time
#' @param time_t ending time
#' @param countries selection of countries to display; NA means all countries
#' @param indiType  the type of indicator  "highBest" or "lowBest"
#' @param displace graphical displacement
#' @param axis_name_y  name of the axis
#' @param val_alpha  transparency value in (0,1].
#' @param debug  a flag to get debug information as msg component
#' @return a list with ggplot2 graphical object within res component
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' \dontrun{
#' # Example 1
#' # A dataset in the format time by countries:
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
#' # Plot the deviations from the mean for all countries:
#' resDMP <- dev_mean_plot(testTB,
#'                         timeName="time",
#'                         displace = 0.25,
#'                         axis_name_y = "Countries")
#' resDMP
#'
#' # Plot by considering only some of the years:
#' resDMP1 <- dev_mean_plot(testTB,
#'                        timeName="time",
#'                        time_0 = 2002,
#'                        time_t = 2004,
#'                        displace = 0.25,
#'                        axis_name_y = "Countries")
#' resDMP1
#'
#' # Example 2
#' # The Eurofound dataset "emp_20_64_MS":
#' myTB1 <- emp_20_64_MS
#'
#' # Plot the deviations from the mean only for some of the member states:
#' resDMP2 <- dev_mean_plot(myTB1,
#'                        timeName="time",
#'                        time_0 = 2005,
#'                        time_t = 2010,
#'                        countries= c("AT","BE","IT"),
#'                        displace = 0.25,
#'                        axis_name_y = "Countries")
#' resDMP2
#' }
#'
#' @export
#'
#'
dev_mean_plot <- function(myTB,
                          timeName="time",
                          time_0 = NA,
                          time_t = NA,
                          countries=c(NA,NA),
                          indiType="highBest",
                          displace = 0.25,
                          axis_name_y = "Countries",
                          val_alpha  = 0.95,
                          debug=FALSE){
  #
  out_obj <- convergEU_glb()$tmpl_out
  if(length(setdiff(countries,names(myTB))) > 0  & !is.na(countries[1])){
    out_obj$err <- "Error: wrong selection of countries."
    return(out_obj)
  }
  if(!(timeName %in% names(myTB)) ){
    out_obj$err <- "Error: wrong timeName."
    return(out_obj)
  }
  if(is.na(time_0)) {
    time_0 <- min(myTB[[timeName]])
  }
  if(is.na(time_t)) {
    time_t <- max(myTB[[timeName]])
  }
  sigmaRes <- sigma_conv(myTB,
                         timeName = timeName,
                         time_0 = time_0,
                         time_t = time_t)$res;
  # put countries of interest top
  all_indi_names_myTB <- base::setdiff(names(myTB),timeName)
  if(!is.na(countries[1])){
    nameMS <- c(countries,base::setdiff(all_indi_names_myTB , countries));
  }else{
    nameMS <- all_indi_names_myTB
  }
  resDiffe <- dplyr::tibble(MS=nameMS,
                            negaSum=NA,
                            posiSum=NA,
                            posi= 1:length(nameMS));

  myTB <- dplyr::filter(myTB, (.data[[timeName]] >= time_0)  & (.data[[timeName]] <= time_t))
  for(auxN in nameMS){
    intermedioA <- myTB[[auxN]] - sigmaRes$mean
    estrattoreNega <- intermedioA <= 0;
    posiz <- which(resDiffe[['MS']] == auxN)
    resDiffe$negaSum[posiz ] <- sum(intermedioA[estrattoreNega])
    resDiffe$posiSum[posiz] <- sum(intermedioA[!estrattoreNega])
  }
  miniX <- min(resDiffe[,"negaSum"])
  miniX <-  miniX + miniX/1000
  maxiX <- max(resDiffe[,"posiSum"])
  maxiX <-  maxiX+ maxiX/1000
  etichY  <-  resDiffe$MS
  names(etichY) <-  etichY
  # switch of colors according to indicator type
  color_assign <- c('#ff0000','#0033cc')
  if(indiType == "lowBest"){
    color_assign <- color_assign[c(2,1)]
  }
  #
  myTBr0  <- dplyr::tibble( MS = c(resDiffe$MS,resDiffe$MS),
                   posi = c(resDiffe$posi,resDiffe$posi),
                   xmin = c(0*resDiffe$negaSum,resDiffe$negaSum),
                   xmax = c(resDiffe$posiSum, 0*resDiffe$posiSum),
                   ymin = c(resDiffe$posi - displace,resDiffe$posi - displace),
                   ymax = c(resDiffe$posi + displace,resDiffe$posi + displace),
                   fillCol= factor(
                     rep(c("pos.","neg."), each=length(resDiffe$posi))
                   ));
    myTBr <-   dplyr::filter(myTBr0, myTBr0$posi <= length(nameMS))

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
    ggplot2::scale_fill_manual(values =color_assign ) +
    ggplot2::geom_vline(xintercept=0,colour="gray40") +
    ggplot2::xlab("Sum of deviations from average.") +
    ggplot2::guides(fill = ggplot2::guide_legend(title=" "))

out_obj <- convergEU_glb()$tmpl_out
out_obj$res <- myGG
if(debug){
  out_obj$msg <- resDiffe
  }
return(out_obj)
}

utils::globalVariables(c("xmin","xmax","ymin","ymax","fillCol","posi"))
