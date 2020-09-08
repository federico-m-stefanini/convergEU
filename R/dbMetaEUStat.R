#' Eurostat metainformation
#'
#' Metainformation about data from Eurostat processed at Eurofound. More precisely,
#' metainformation is provided for three dimensions: employment, socio economic and quality of life.
#' For each dimension, metainformation for several indicators is reported, e.g. coding in database, official code,
#' measurement unit, source organization, disaggregation and bookmark URL.
#' Variable names often end with characters denoting scales.
#' The following convention holds for names of variables:
#' "_p"	percentage, "_i" index, "_pop" persons, "_h" hours,
#' "_eur" euros, "_pps"	purchasing power standards,
#' "_y"	years.
#'
#'
#' @source \url{https://ec.europa.eu/eurostat/data/database}
#' @docType data
#' @keywords datasets
#' @name dbMetaEUStat
#' @usage data(dbMetaEUStat)
#' @format A tibble dataset with 56 rows and 10 columns
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' data(dbMetaEUStat)
#' names(dbMetaEUStat)
#'
#'
#' # Visualize indicators' information:
#' dbMetaEUStat$INDICATOR
#'
#' # Visualize the indicators' coding in database:
#' dbMetaEUStat$Code_in_database
#'
#' # Visualize the indicators' official coding:
#' dbMetaEUStat$Official_code
#'
#'
NULL


