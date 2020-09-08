#' Calculate changes of deviations from the mean
#'
#' Deviations from the mean of a collection of countries is calculated for
#' each year. Then differences at subsequent times are calculated within
#' each member state. Finally negative differences are added over years within
#' member state, and similarly  positive differences are added over years
#' within member state.
#' The output is made by  datasets with intermediate calculations,
#' and by the component statistics which is  member state by statistics.
#'
#' Let \deqn{Y_{i,t,m}} be the indicator value i at time t for
#' country m.
#' Let \deqn{D_{i,t,m} = Y_{i,t,m} - M_{i,t,m}} be the departure from the mean
#'         at time t.
#' Let \deqn{d_{i,t,m} = | D_{i,t,m}| - | D_{i,t,m}|} be the difference of
#'         absolute values within country m at time t.
#' Then the overall negative and positive changes   are
#'    \deqn{Cn(i,t,m) = \sum_{t} d_{i,t,m} I_{d<=0}(d)}
#'  and
#'        \deqn{Cp(i,t,m) = \sum_{t} d_{i,t,m} I_{d>0}(d)}
#'
#' @param myTB a dataset time by countries
#' @param timeName name of the variable representing time
#' @param time_0 starting time
#' @param time_t ending time
#' @param sele_countries selection of countries to display;
#'                         NA means all countries
#' @param doplot if a ggplot2 graphical object desired then TRUE, otherwise
#'               it is FALSE
#' @return A list with intermediate and final statistics; list component res_graph
#'         is a ggplot2 object if the argument doplot = TRUE; to plot the object
#'         use function plot().
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#'
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
#'res <- demea_change(testTB,
#'              timeName="time",
#'              time_0 = 2000,
#'              time_t = 2005,
#'              sele_countries= NA,
#'              doplot=TRUE)
#'
#' \donttest{
#' plot(res$res$res_graph)
#' }
#'
#' # Example 2
#' # Deviations from the mean for the emp_20_64_MS Eurofound dataset
#' data(emp_20_64_MS)
#'
#' # Calculate deviations from the mean from 2013 to 2016 for Italy, France and Germany
#' res1<-demea_change(emp_20_64_MS,
#'       timeName="time",
#'       time_0 = 2013,
#'       time_t = 2016,
#'       sele_countries= c('IT','FR','DE'),
#'       doplot=TRUE)
#' \donttest{
#' plot(res1$res$res_graph)
#' }
#'
#'
#' @export
#'
#'
demea_change <- function(myTB,
                          timeName="time",
                          time_0 = NA,
                          time_t = NA,
                          sele_countries= NA,
                          doplot=FALSE){
  out_obj <- convergEU_glb()$tmpl_out
  if((length(setdiff(sele_countries,names(myTB))) > 0)  &
      (!is.na(sele_countries[1]))){
    out_obj$err <- "Error: wrong selection of countries."
    return(out_obj)
  }
  if(!(timeName %in% names(myTB)) ){
    out_obj$err <- "Error: wrong timeName."
    return(out_obj)
  }
  if(time_t <= time_0 ){
    out_obj$err <- "Error: wrong time values."
    return(out_obj)
  }
  myTB <- dplyr::filter(myTB,unlist(.data[[timeName]]) >= time_0 &
                                      unlist(.data[[timeName]]) <= time_t)
  myTB <- dplyr::arrange(myTB, .data[[timeName]])
  #
  sigmaRes <- sigma_conv(myTB,
                         timeName = timeName,
                         time_0 = time_0,
                         time_t = time_t)$res;
  # put countries of interest top
  nameMS <- all_indi_names_myTB <- base::setdiff(names(myTB),timeName)
  resDiffe <- dplyr::tibble(!!timeName := myTB[[timeName]])
  # differences
  for(auxN in nameMS){
     intermedioA <- unlist(myTB[,auxN] - sigmaRes$mean)
     resDiffe <- dplyr::mutate(resDiffe, !!auxN := intermedioA)
  }
  # difference of absolute values between times
  diffe_abs_diff <- dplyr::tibble(!!timeName :=
                                    unlist(myTB[[timeName]])[-1]);
  for(auxN in nameMS){
    intermedioD <- diff(abs(unlist(resDiffe[,auxN])))
    diffe_abs_diff <- dplyr::mutate(diffe_abs_diff,
                                    !!auxN := intermedioD)
  }
  # output object
   out_stats <- dplyr::tibble(MS=nameMS,
                             negaSum=NA,
                             posiSum=NA,
                             posi= 1:length(nameMS));

  # final statistics
  for(auxN in nameMS){
    intermedioDA <- unlist(diffe_abs_diff[[auxN]])
    estrattoreNega <- intermedioDA <= 0;
    posiz <- which(out_stats$MS == auxN)
    out_stats$negaSum[posiz] <- sum(intermedioDA[estrattoreNega])
    out_stats$posiSum[posiz] <- sum(intermedioDA[!estrattoreNega])
  }
  miniX <- min(out_stats[ ,"negaSum"])
  maxiX <- max(out_stats[ ,"posiSum"])
  #
  # selection what to display
  if(!is.na(sele_countries[1])){
     estrattoreC <- rep(F,nrow(out_stats))

    for(auxN in 1:length(out_stats$MS)){
      estrattoreC <- estrattoreC  | (out_stats$MS == sele_countries[auxN])
      }
     out_stats <- dplyr::filter(out_stats, estrattoreC)
  }
  #
  res_graph <- plot_all <- NULL
  if(doplot == TRUE){
    ## new function
    plot_all <- ggplot2::ggplot(out_stats,
                                ggplot2::aes(x = MS)) +
      ggplot2::geom_col(ggplot2::aes(y = posiSum), fill = "darkorange2") +
      ggplot2::geom_col(ggplot2::aes(y = negaSum), fill = "dodgerblue3") +
      ggplot2::ylab("Sums")+
      ggplot2::theme(plot.title =
                       ggplot2::element_text(hjust = 0.5),
            plot.subtitle =
              ggplot2::element_text(hjust = 0.5))+
      ggplot2::ggtitle(label="Absolute gap from EU mean",
        subtitle="Total reduction   \t\t\t\t\t\t Total increase   " )+
      ggplot2::ylim(-1*max(out_stats[['posiSum']],
                             abs(out_stats[['negaSum']])),
                      max(out_stats[['posiSum']],
                          abs(out_stats[['negaSum']]))
                     ) +
      ggplot2::coord_flip()
  }
  out_obj$res <- list(
                   resDiffe=resDiffe,
                   diffe_abs_diff=diffe_abs_diff,
                   stats = out_stats,
                   miniX = miniX,
                   maxiX = maxiX,
                   #res_graph = res_graph
                   res_graph =plot_all
                   )

  return(out_obj)
}

utils::globalVariables(c("MS","negaSum","posiSum"))
