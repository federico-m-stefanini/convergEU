#'  Values to patterns
#'
#'
#'  Gradients values and Delta2 are mapped to one pattern (string and number).
#'  See Eurofound 2018 report.
#'  In the mapping table within this function
#'  +1 means greater than zero, 0 means equal to zero,
#'  -1 means smaller than 0.
#'  For column EU_vs_MS,
#'  if  graEU > graMS then  EU_vs_MS = +1;
#'  if  graEU < graMS then  EU_vs_MS = -1;
#'  if  graEU == graMS then  EU_vs_MS = 0.
#'  Code NA is left to indicate not relevant features.
#'  Further codes are added here from 13 to 18 for parallelism;
#'  codes 19 and 20 are for crossed lines joining the EU pair and the MS pair.
#'  Code 21 stands for "to be visually inspected".
#'
#' @param vaEU  EU values sorted in ascending order by time.
#' @param vaMS  member state values  sorted in ascending order by time.
#' @param vaTime  sorted pair of times.
#' @return  a number referring to  pattern whose label depends on the
#'          indicator type
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' vaEU <- c(5,7)
#' vaMS <- c(6,8)
#' vaTime <- c(1999,2000)
#' resG1 <- gra_de2_patt(vaEU,vaMS,vaTime)
#'
#' # Example 2:
#' vaEU <- c(7,2)
#' vaMS <- c(9,4)
#' vaTime <- c(2009,2010)
#' resG2 <- gra_de2_patt(vaEU,vaMS,vaTime)
#'
#' # Example 3:
#' vaTime <- c(2009,2010)
#' vaEU <- c(100 , 120)
#' vaMS <- c( 50, 90)
#' resG3 <- gra_de2_patt(vaEU,vaMS,vaTime)
#'
#' @export
#'
gra_de2_patt <- function(vaEU,vaMS,vaTime){
    resGraDe <- coeu_grad(mEU2=vaEU[2],
                          mEU1=vaEU[1],
                          mMS2=vaMS[2],
                          mMS1=vaMS[1],
                          time2=vaTime[2],
                          time1=vaTime[1])
    # Parallels lines
    if( (resGraDe$GraEU == resGraDe$GraMS) & (vaMS[1] >= vaEU[1]) ) {
      if(resGraDe$GraEU >  0) return(13);
      if(resGraDe$GraEU == 0) return(14);
      if(resGraDe$GraEU <  0) return(15);
    }else if((resGraDe$GraEU == resGraDe$GraMS) & (vaMS[1] < vaEU[1]) ){
      if(resGraDe$GraEU <  0) return(16);
      if(resGraDe$GraEU == 0) return(17);
      if(resGraDe$GraEU >  0) return(18);
    };
    # now check for crossing
    if( (vaMS[1] > vaEU[1])  & (vaMS[2] < vaEU[2])){
       return(19)
    }else if((vaMS[1] < vaEU[1])  & (vaMS[2] > vaEU[2])){
       return(20)
    }
    # build map table
    db_patt <- dplyr::tribble(
    ~stateEU, ~stateMS, ~stateD2, ~EU_vs_MS, ~code,
       1,   1,  -1,  -1,   1,
       1,   1,  -1,  +1,   2,
       1,  -1,  -1,  +1,   3,
       1,   1,   1,  -1,   4,
       1,   1,   1,   1,   5,
       1,  -1,   1,   1,   6,
      -1,   -1,  1,  -1,   7,
      -1,   1,   1,  -1,   8,
      -1,   -1,  1,  1,    9,
      -1,   -1,  -1,  1,   10,
      -1,   1,   -1, -1,   11,
      -1,   -1,  -1, -1,   12)
    # other patterns intercepted before this line
   sele_c1 <- c(-1,0,1)[c(resGraDe$GraEU < 0,
                           resGraDe$GraEU == 0,
                           resGraDe$GraEU >0)]
    sele_c2 <- c(-1,0,1)[c(resGraDe$GraMS < 0,
                           resGraDe$GraMS == 0,
                           resGraDe$GraMS >0)]
    sele_c3 <- c(-1,0,1)[c(resGraDe$Delta2 < 0,
                           resGraDe$Delta2 == 0,
                           resGraDe$Delta2 >0)]
    sele_c4 <- c(-1,0,1)[c(resGraDe$GraEU < resGraDe$GraMS,
                           resGraDe$GraEU == resGraDe$GraMS,
                           resGraDe$GraEU > resGraDe$GraMS)]
    re_fi <- dplyr::filter(db_patt,
                           .data$stateEU == sele_c1,
                           .data$stateMS == sele_c2,
                           .data$stateD2 == sele_c3,
                           .data$EU_vs_MS == sele_c4)
    if(nrow(re_fi) != 1){
      return(21);##   missing  patterns
    }else{
      return(as.numeric(unlist(re_fi[1,5])))
    }
stop("Error in function gra_de2_patt: pattern not found")
}

