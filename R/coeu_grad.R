#' Auxiliary function for gradients  and delta2
#'
#' Intermediate calculation to define patterns.
#'
#' @param mEU2 average at time 2, EU
#' @param mEU1 average at time 1, EU
#' @param mMS2 average at time 2, Member State
#' @param mMS1 average at time 1, Member State
#' @param time2  time 2
#' @param time1  time 1
#' @return  a list with components time length, grad of member state,
#'          grad of EU average and the delta squared difference at a pair
#'          of times.
#'
#'
coeu_grad <- function(mEU2,mEU1,mMS2,mMS1,time2,time1){
  deltaTime <- time2 - time1
  Delta2 <- (mMS2-mEU2)^2 - (mMS1 -mEU1)^2
  GraMS <- (mMS2-mMS1)/deltaTime
  GraEU <- (mEU2-mEU1)/deltaTime
  list(
    time_len = deltaTime,
    GraMS = GraMS,
    GraEU = GraEU,
    Delta2 = Delta2)
}
#'
#'
#' Auxiliary function to provide a different object as input
#'
#' See  function coeu_grad for details.
#'
#' @param mEU averages at time1 and time 2
#' @param mMS  indicator for a member country at time1 and time2
#' @param time the two times considered, sorted in ascending order
#' @return a list with components time length, grad of member state,
#'          grad of EU average and the delta squared difference at a pair
#'          of times.
#'
#'
coeu_gradV <- function(mEU,mMS,time){# ordering time, time-1
  coeu_grad(mEU[1],mEU[2],mMS[1],mMS[2],time[1],time[2])
}
