#' From points to parameters of a straight line
#'
#' Given two points on a plane, parameters of a straight line are calculated.
#'
#' @param  point1  collection abscissa ,ordinate.
#' @param  point2 collection abscissa ,ordinate.
#' @return  collection made by (intercept, slope)
#' @examples
#'
#' # Example 1
#' require(tibble)
#' myTB <- tribble(
#'     ~time , ~indic,
#'     1    ,   25,
#'     10   ,   5,
#'     1,       10,
#'     10,       3
#'     )
#' resparamIT1 <- points2par(as.numeric(myTB[1,]),as.numeric(myTB[2,]))
#'
#' # Example 2
#' myTB1 <- tribble(
#'     ~time , ~indic,
#'     2    ,   25,
#'     16   ,   5,
#'     1,       9,
#'     10,       3,
#'     34,       4
#'     )
#' resparamIT2 <- points2par(as.numeric(myTB1[1,]),as.numeric(myTB1[2,]))
#'
#' # Example 3
#' myTB2 <- tribble(
#'     ~time , ~indic,
#'     5    ,   2,
#'     1   ,   15,
#'     11,       19,
#'     20,       33,
#'     25,       14
#'     )
#' resparamIT3 <- points2par(as.numeric(myTB2[1,]),as.numeric(myTB2[2,]))
#'
#' @export
#'
#'
points2par <- function(point1, point2){
  beta1 <-  (point2[2]-point1[2])/(point2[1]-point1[1])
  beta0 <-  point1[2] - beta1 * point1[1]
  return( c(beta0,beta1))
}

