#' Imputation of  missing values
#'
#' Imputation is deterministic and based on a straight line between two
#' points.
#'
#' @param  timeIni starting time
#' @param timeEnd  ending time
#' @param timeDelta collection of times where missing values are located
#' @param indicIni   observed value at timeIni
#' @param indicFin  observed value at timeEnd
#' @return  imputed tibble with   an indicator of missingness (wasMissing).
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Simplest Imputation of one missing value between two observed values:
#' res1 <- impu_det_lin(timeIni= 88,
#'     timeEnd = 90,
#'     timeDelta = 89,
#'     indicIni = 120,
#'     indicFin = 100)
#'
#' # Example 2
#' # Multiple Imputation of  missing values:
#'     res2 <-impu_det_lin(timeIni= 90,
#'     timeEnd = 93,
#'     timeDelta=c(91,92),
#'     indicIni = 100,
#'     indicFin = 108)
#'
#' # Multiple Imputation of  missing values with delta > 1:
#' res3 <- impu_det_lin(timeIni= 2000,
#'     timeEnd = 2015,
#'     timeDelta=seq(2005,2010,5),
#'     indicIni = 100,
#'     indicFin = 108)
#'
#'
#'
#' @export
#'
#'
impu_det_lin <- function(timeIni,timeEnd, timeDelta,
                         indicIni,indicFin){
  # build dataframe
   workDF <- data.frame(time=c(timeIni,timeEnd),
                       indic =c(indicIni,indicFin))
  # calculate
  molin <- stats::lm(indic ~ time,data = workDF)
  afterImput <- stats::predict(molin,
                               data.frame(
                                time = timeDelta))
  res <-  tibble::tibble(
            time =   timeDelta,
            indicator =   afterImput,
            wasMissing = TRUE
          )
  return(res)
}


