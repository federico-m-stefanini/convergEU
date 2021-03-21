#'  Values to patterns
#'
#'
#'  Gradients values and Delta2 are mapped to one pattern (string and number).
#'  See Eurofound 2018 report.
#'  
#'  
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
#' @param vaMS  member state values  sorted in ascending order by time.
#' @param vaEU  EU values sorted in ascending order by time.
#' @param vaT  sorted pair of times.
#' @param remap  is FALSE for the original numerical labelling of patterns
#'              otherwise TRUE to map to old numerical correspondence.  
#' @return  a number referring to  pattern whose label depends on the
#'          indicator type as originally produced in the technical report.
#'
#'
#'
map_2_patt_39 <- function(vaMS,vaEU,vaT,remap=FALSE){
 res <- NA   
 delta_E <- vaEU[2]-vaEU[1]
 if(delta_E >  0) res <- map_up_patt_39(vaMS,vaEU,delta_E,vaT);
 if(delta_E <  0) res <- map_down_patt_39(vaMS,vaEU,delta_E,vaT);
 if(delta_E == 0) res <- map_const_patt_39(vaMS,vaEU,delta_E,vaT);
 if(remap){ # remapping
     # 40 is the "inspect the plot" special label
     current_map <- c(c(16,13,11,15,18,20,3,1,10,5,6,7,14,23,4,9,27,17,33,32),
                     c(2,8,12,19,21,22,24,25,26,28,29,30,31,34,35:40))
     res_remap <- current_map[res]
     return(res_remap)
 }else{
     return(res)
   }
 stop("Fake gradient: fix it please!")
 };
#'
map_up_patt_39 <- function(vaMS,vaEU,delta_E,vaT){
  if(delta_E <= 0)stop("Wrong function called!")
  delta_M <- vaMS[2]-vaMS[1]
  if(delta_M < 0 && vaMS[2] >= vaEU[2]){
    return(11)
  }else if(delta_M == 0 && vaMS[2] >= vaEU[2]){
    return(12)  
  }else if(delta_M > 0 && delta_M < delta_E && vaMS[2] >= vaEU[2]){
    return(13)  
  }else if(delta_M == delta_E && vaMS[1] > vaEU[1] && vaMS[2] > vaEU[2]){
    return(14)  
  }else if(delta_M  > delta_E && vaMS[1] > vaEU[1]){
    return(15)  
  }else if(delta_M  > delta_E && vaMS[2] <= vaEU[2]){
    return(16)  
  }else if(delta_M == delta_E && vaMS[1] < vaEU[1] && vaMS[2] < vaEU[2]){
    return(17)  
  }else if(delta_M < delta_E && delta_M > 0 && vaMS[1] < vaEU[1]){
    return(18)  
  }else if(delta_M == 0 && vaMS[1] <= vaEU[1] ){
    return(19)  
  }else if(delta_M < 0 && vaMS[1] <= vaEU[1]){
    return(20)  
  }else if(delta_M < 0 && vaMS[1] >= vaEU[1] && vaMS[2] <= vaEU[2]){
    return(33)  
  }else if(delta_M == 0 && vaMS[1] >= vaEU[1] && vaMS[2] <= vaEU[2]){
    return(34)  
  }else if(delta_M > 0 && delta_M < delta_E && vaEU[1] >= vaEU[1] && vaMS[2] <= vaEU[2]){
    return(35)  
  }else if(delta_M > 0 && delta_M > delta_E && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
    return(36)  
  }else if(vaMS[1] == vaEU[1]  &&  vaMS[2] == vaEU[2]){
    return(37)  
  }else{# require inspection
     return(40)  
  }
  return(NA) 
};
#` 
map_down_patt_39 <- function(vaMS,vaEU,delta_E,vaT){
  if(delta_E >= 0)stop("Wrong function called!")
  delta_M <- vaMS[2]-vaMS[1]
  
  if(delta_M > 0 && vaMS[1] >= vaEU[1]){
    return(1)
  }else if(delta_M == 0 && vaMS[1] >= vaEU[1]){
    return(2)  
  }else if(delta_M < 0 && delta_M > delta_E && vaMS[1] >= vaEU[1]){
    return(3)  
  }else if(delta_M == delta_E && vaMS[1] > vaEU[1] && vaMS[2] > vaEU[2]){
    return(4)  
  }else if(delta_M < delta_E  && vaMS[2] >= vaEU[2]){
    return(5)  
  }else if(delta_M > 0 &&  vaMS[2] <= vaEU[2]){
    return(6)  
  }else if(delta_M == 0 &&  vaMS[2] <= vaEU[2]){
    return(7)  
  }else if(delta_M <0 && delta_M > delta_E &&  vaMS[2] <= vaEU[2]){
    return(8)  
  }else if(delta_M == delta_E &&  vaMS[2] < vaEU[2]){
    return(9)  
  }else if(delta_M < delta_E && vaMS[1] <= vaEU[1] ){
    return(10)  
  }else if(delta_M < delta_E && vaMS[1] > vaEU[1] && vaMS[2] <= vaEU[2]){
    return(29)  
  }else if(delta_M < 0 && delta_M > delta_E  && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
    return(30)  
  }else if(delta_M == 0 && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
    return(31)  
  }else if(delta_M > 0 && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
    return(32)  
  }else if( vaMS[1] == vaEU[1] && vaMS[2] == vaEU[2]){
    return(39)
  }else{ # visual inspection required
    return(40)  
  }
  return(NA) 
};
#'
map_const_patt_39 <- function(vaMS,vaEU,delta_E,vaT){
  if(delta_E != 0)stop("Wrong function called!")
  delta_M <- vaMS[2]-vaMS[1]
  if(vaMS[1] <= vaEU[1] && vaMS[2] > vaEU[2]){
    return(21)
  }else if(delta_M > 0 && vaMS[1] > vaEU[1]){
    return(22)  
  }else if(delta_M == 0  && vaMS[1] > vaEU[1]){
    return(23)  
  }else if(delta_M < 0 && vaMS[2] > vaEU[2]){
    return(24)  
  }else if(delta_M < 0 && vaMS[1] > vaEU[1] && vaMS[2] <= vaEU[2]){
    return(25)  
  }else if(delta_M < 0  && vaMS[1] <= vaEU[1] ){
    return(26)  
  }else if(delta_M == 0 && vaMS[1] < vaEU[1] ){
    return(27)  
  }else if(delta_M > 0 && vaMS[2] <= vaEU[2]){
    return(28)  
  }else if(vaMS[1] == vaEU[1] && vaMS[2] == vaEU[2]){
    return(38)  
  }else{ # visual inspection required
    return(40)  
  }
 return(NA) 
};
#'


 












