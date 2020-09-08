#' Graphical representation based on beta convergence
#'
#' A ggplot of transformed data and a straight line for the results obtained for beta-convergence
#'
#' @param  betaRes  the output obtained from beta_conv function.
#' @param time_0 starting time.
#' @param time_t ending time.
#' @param indiName name of the  considered indicator as a string.
#' @return  a ggplot object to be displayed of saved using ggsave.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#'
#' # Example 1
#' # Beta convergence for the emp_20_64_MS Eurofound dataset in the period 2002-2006:
#' data(emp_20_64_MS)
#' empBC <- beta_conv(emp_20_64_MS, time_0 = 2002, time_t = 2006, timeName = "time")
#'
#' # Graphical plot based on the results for beta-convergence
#' empBCgraph <- beta_conv_graph(empBC,2002,2006,indiName = 'Employment rate')
#' empBCgraph
#'
#' # Example 2
#' # Beta convergence for the emp_20_64_MS Eurofound dataset in the period 2008-2016:
#' empBC1 <- beta_conv(emp_20_64_MS, time_0 = 2008, time_t = 2016, timeName = "time")
#'
#' # Graphical plot based on the results for beta-convergence
#' empBCgraph1 <- beta_conv_graph(empBC1,2008,2016,indiName = 'Employment rate')
#' empBCgraph1
#'
#'
#' @export
#'
#'
beta_conv_graph <- function(betaRes,
                      indiName=NA,
                      time_0 = NA,
                      time_t = NA
)
  {
  ptime_0 <-  as.numeric(time_0)
  ptime_t <-  as.numeric(time_t)
  beta_conv_gr1 <- ggplot2::qplot(betaRes$res$workTB$indic,
                         betaRes$res$workTB$deltaIndic,
                         xlab= paste("log ",indiName),
                         ylab= paste("Log rate of growth:",ptime_0,"-",ptime_t)) +
    ggplot2::geom_abline(intercept = as.numeric(betaRes$res$summary[1,2]),
                slope = as.numeric(betaRes$res$summary[2,2]),
                colour = "red") +
    ggplot2::geom_text(
             ggplot2::aes(label=betaRes$res$workTB$countries),
              hjust=0, vjust=0,colour="blue")
  return(beta_conv_gr1)
}

