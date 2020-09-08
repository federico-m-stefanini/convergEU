#' Ranking of EU countries by time
#'
#' Countries are ranked for each time according to two types of indicators: higher is the best (highBest)
#'  or lower is the best (lowBest).
#'
#' @param  myTB the dataframe time by countries (complete and sorted by
#'               increasing time).
#' @param  timeName the name of the variable that contains time information.
#' @param  time_0 starting time to consider; if NA all times considered.
#' @param  time_t last time to consider; if NA all times considered.
#' @param  typeInd  "highBest" is the default, "lowBest" is the alternative
#' @return  a list  with component res which contains ranking by each considered year
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Sorted dataframe in the format years by countries:
#' require(tibble)
#' myTB  <- tibble::tribble(
#' ~years, ~UK, ~DE, ~IT,
#' 1988,   1201, 868, 578,
#' 1989,   1150, 978, 682,
#' 1990,   998,  1250, 332,
#' 1991,  1600,  1350, 802
#' )
#'
#' # Country ranking according to the indicator higher is the best:
#' res <- country_ranking(myTB,timeName="years")
#'
#' # Country ranking according to the indicator lower is the best:
#' res1 <- country_ranking(myTB,timeName="years", typeInd="lowBest")
#'
#' # Country ranking for some years only:
#' myres <- country_ranking(myTB,timeName="years", time_0=1989,time_t=1990,typeInd="lowBest" )
#'
#' # Example 2
#' # Ranking of the Member States for the "emp_20_64_MS" dataset
#' data(emp_20_64_MS)
#' myCR<-country_ranking(emp_20_64_MS,timeName = "time", time_0 = 2007, time_t = 2010)
#'
#' # Visualize the results for the first five countries:
#' myCR$res[1:6]
#'
#'
#' @export
#'
#'
country_ranking <- function(myTB,
                            timeName="time",
                            time_0=NA,
                            time_t=NA,
                            typeInd="highBest" ){
  # Make standard cheks on the dataset
  obj_out <- check_data(myTB)
  if(!is.null(obj_out$err)){
    return(obj_out);
  }
  # typeInd
  if(not_in(typeInd, c("highBest", "lowBest"))){
    obj_out$err <- "Error: unknown type of index.";
    return(obj_out);
  }
  # check if timeName is present
  if(timeName %in% names(myTB)){
    # sort by timeName
    myDes1 <- dplyr::arrange_at(myTB,timeName)# ordered by time
    # select time window
    if(!is.na(time_0) & !is.na(time_t)) {
      # check
      tempiCur <- myTB[[timeName]]
      if( (time_0 < time_t) && (time_0 %in% tempiCur)   && (time_t %in% tempiCur) ){
        myDes1 <- dplyr::filter(myDes1,(!!tempiCur >= !!time_0 ) &   (!!tempiCur <= !!time_t))
      }else{# if error
        obj_out <-  convergEU_glb()$tmpl_out
        obj_out$err <- "Error: wrong time window."
        return(obj_out)
        }
      }else{};
    #
   }else{
    tmp <- convergEU_glb()$tmpl_out
    tmp$err <- "Error: declared time variable absent."
    return(tmp)
  }
  # make calculations
  if(typeInd == "lowBest"){
      ord_MS <- dplyr::select(myDes1,-!!timeName)
  }else{# reverse sign
    ord_MS <- -1*dplyr::select(myDes1,-!!timeName)
  }
  for(auxMS in 1:nrow(ord_MS)) {
        ord_MS[auxMS,] <-  as.list(rank(unlist(ord_MS[auxMS,]),
                                        ties.method ="min"))
  }
  ord_MS <- dplyr::bind_cols(myDes1[timeName],ord_MS)
  obj_out$res <- ord_MS
  return(obj_out)

}

