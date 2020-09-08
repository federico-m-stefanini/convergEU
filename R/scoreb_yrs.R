#' Scoreboard of countries
#'
#' A scoreboard of countries shows the departure of an indicator level
#' from the average, for each year in the dataset.
#' It also considers one-year changes and the inherent average (and departure)
#' for  each year.
#'
#'
#' @param  myTB  original complete dataset (tibble) time by country,
#'         ordered by time; only time and countries variables must be present,
#'         no average or auxiliary variables at all.
#'         Only years of interest must be present and only countries
#'         contributing to the average of each year.
#' @param  timeName string with the name of the time variable in myTB
#' @return  list of tibbles containing   departures and integer labels. Integer
#'          values in the result refers to the partition
#'          (-Inf, m-1 s, m-0.5 s, m+0.5 s, m+1 s, Inf) where m is the average
#'          and s the standard deviation at a given time t; in particular
#'          the ordinal is 1 if the interval (-Inf, m -1 s) contains the indicator,
#'          it is 2 if the interval ( m-1 s, m-0.5 s) contains the indicator, and so on
#'          up to the value 5 that means an indicator value above m + 1 s.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' # Example 1
#' # Dataset in the format years by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#'     ~time, ~countryA ,  ~countryB,  ~countryC,
#'     2000,     0.8,   2.7,    3.9,
#'     2001,     1.2,   3.2,    4.2,
#'     2002,     0.9,   2.9,    4.1,
#'     2003,     1.3,   2.9,    4.0,
#'     2004,     1.2,   3.1,    4.1,
#'     2005,     1.2,   3.0,    4.0
#'     )
#' resTB1<-scoreb_yrs(testTB, timeName = "time")
#'
#' # Example 2
#' # Scoreboard of countries for the emp_20_64_MS Eurofound dataset:
#' data("emp_20_64_MS")
#' resTB2 <- scoreb_yrs(emp_20_64_MS,timeName = "time")
#'
#' @export
#'
#'
scoreb_yrs <- function(myTB, timeName = "time"){
  obj_out <- check_data(myTB,timeName)
  if(!is.null(obj_out$err)) {
    return(obj_out);
  }# ok, complete and sorted dataset
  # check if subsequent times differ  by one unit
  veTime <- unlist(myTB[,timeName])
  increTime <- diff(veTime)
  if(any(increTime > 1)) {
    obj_out$res <- NULL
    obj_out$msg <- "Warning: different length of time intervals among subsequent years."
  }
  posiz <- which(names(myTB) == timeName)
  esse_conv <- sigma_conv(myTB, timeName = timeName, time_0 = NA, time_t = NA)
  esse_conv$res <- dplyr::mutate(esse_conv$res,
                         elle1in = esse_conv$res$mean -0.5 * esse_conv$res$stdDev,
                         elle1su = esse_conv$res$mean +0.5 * esse_conv$res$stdDev,
                         elle2in = esse_conv$res$mean -1 * esse_conv$res$stdDev,
                         elle2su = esse_conv$res$mean +1 * esse_conv$res$stdDev
  )
  # for each country on levels
  ## object in output
  sco_lev <- myTB
  sco_lev_num <- myTB
  nomiOut <- names(myTB[,-posiz]) # only columns for countries
  estrattore <- matrix(F, ncol = 4, nrow=nrow(myTB))
  for(auxN in nomiOut) {
    estrattore[,] <-  NA
    # do calculations tag departures
    estrattore[,1] <- myTB[,auxN] <= esse_conv$res$elle2in
    estrattore[,2] <- myTB[,auxN] <= esse_conv$res$elle1in
    #estrattore[,3] <- myTB[,auxN] <= esse_conv$res$mean
    estrattore[,3] <- myTB[,auxN] <= esse_conv$res$elle1su
    estrattore[,4] <- myTB[,auxN] <= esse_conv$res$elle2su
    sco_lev[,auxN] <- apply(estrattore,1,function(vx){
        vx2 <- c(vx, !any(vx))
        min( c(1:5)[vx2])
        })
    ## obtain convenient-standard numerical tagging
    sco_lev_num[,auxN] <- apply(estrattore,1,function(vx){
      vx2 <- c(vx, !any(vx))
      min( c(-1, -0.5, 0, 0.5, 1)[vx2])
    })
  }
  #
  # for each country on change
  sco_chan <-  myTB
  estrattore2 <- matrix(F, ncol=4, nrow=nrow(myTB)-1)
  # change in one year
  for(auxN in nomiOut) {
    sco_chan[,auxN] <- c(NA,diff(unlist(sco_chan[,auxN])))
   }
  # calculate sigma on change
  esse_conv_chan <- sigma_conv(sco_chan[-1,], timeName = timeName, time_0 = NA, time_t = NA)
  esse_conv_chan$res <- dplyr::mutate(esse_conv_chan$res,
        elle1in = esse_conv_chan$res$mean -0.5 * esse_conv_chan$res$stdDev,
        elle1su = esse_conv_chan$res$mean +0.5 * esse_conv_chan$res$stdDev,
        elle2in = esse_conv_chan$res$mean -1 * esse_conv_chan$res$stdDev,
        elle2su = esse_conv_chan$res$mean +1 * esse_conv_chan$res$stdDev
        )
  # assign numeric tag
  for(auxN in nomiOut) {
    estrattore2[,] <-  NA
    # do calculations tag departures
    estrattore2[,1] <- sco_chan[-1,auxN] <= esse_conv_chan$res$elle2in
    estrattore2[,2] <- sco_chan[-1,auxN] <= esse_conv_chan$res$elle1in
    #estrattore2[,3] <- sco_chan[-1,auxN] <= esse_conv_chan$res$mean
    estrattore2[,3] <- sco_chan[-1,auxN] <= esse_conv_chan$res$elle1su
    estrattore2[,4] <- sco_chan[-1,auxN] <= esse_conv_chan$res$elle2su
    sco_chan[-1,auxN] <- apply(estrattore2,1,function(vx){
      vx2 <- c(vx, !any(vx))
      min(c(1:5)[vx2])
    })
    }#  scores for change are ready
  # note that there is one row less due to differencing (first omitted)
  obj_out$res <- list(
    sigma_conv= esse_conv$res,
    sco_level = sco_lev,
    sco_change = sco_chan,
    sco_level_num = sco_lev_num # numerical tags from boundaries of the partition
  )
  return(obj_out)
}
