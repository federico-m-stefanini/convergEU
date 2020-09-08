#' Unweighted average of countries
#'
#' The computation is based on clusters defined in a objects
#' created by invoking *convergEU_glb()*.
#' At now only cluster labels contained into *convergEU_glb()* are possible.
#'
#'
#'
#' The cluster specification is based on labels:  "EU27_2020", "EU27_2007", "EU25", "EU19",
#' "EU15", "EU12","EA", "Eurozone", "all".
#' The option cluster = "all"  indicates that all countries in the dataset
#' have to be considered.
#'
#'
#' @param  myTB  time by member states dataset.
#' @param  timeName name of the variable that contains time.
#' @param  cluster the label defining a cluster; one string selected within
#'                the following: "EU12" , "EU15" ,"EU19","EU25" ,"EU27_2007", "EU28", "EU27_2020",
#'                "Eurozone","EA", "all" (for all countries in the dataset).
#' @return  The dataset with the average of clustered countries.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Unweighted average of Member States for cluster "EU12":
#' myAC1<-average_clust(emp_20_64_MS,timeName = "time",cluster = "EU12")
#'
#' #  Visualize results for Italy:
#' myAC1$res[,c(1,17)]
#'
#' # Visualize results for the first five member states:
#' myAC1$res[,c(1:6)]
#'
#' # Example 2
#' # Unweighted average of Member States for cluster "EU25":
#' myAC2<-average_clust(emp_20_64_MS,timeName = "time",cluster = "EU25")
#'
#' # Visualize results for France:
#' myAC2$res[,c(1,13)]
#'
#' # Visualize results for the first six member states:
#' myAC2$res[,c(1:7)]
#'
#' # Example 3
#' # Unweighted average of countries for cluster "EU27":
#' myAC<-average_clust(emp_20_64_MS,timeName = "time",cluster = "EU27")
#'
#' # Visualize results for Germany:
#' myAC$res[,c(1,7)]
#'
#' # Visualize results for the first five member states:
#' myAC$res[,c(1:6)]
#'
#' @export
#'
#'
average_clust <- function(myTB, timeName = "time", cluster="EU27"){
  out_obj <- convergEU_glb()$tmpl_out
  # Checked data?
  res <-  check_data(myTB)
  if(!is.null(res$err)) return(res);
  # check if timeName is present
  if(!(timeName %in% names(myTB))){
    tmp <- convergEU_glb()$tmpl_out
    tmp$err <- "Error: Time variable not in the dataframe."
    return(tmp)
  }else{ };
  ## test
  if(cluster %in%  convergEU_glb()$labels_clusters){
      testMS <- check_country(myTB, clusterCode=cluster)
      if( is.null(testMS$res) | (testMS$res == FALSE)){
        out_obj$err <- "Error: at least one country missing"
        return(out_obj);
      }else{};

  }else if(cluster == "all"){
         # nothing to do
   }else{
     out_obj$err <- "Error: badly specified countries."
     return(out_obj);

   };

  # checks passed let's   average
  posit <- which(names(myTB) == timeName)
  tmp <- myTB[,-posit]
  if(cluster != "all"){
     ##MScorrente <- unlist(convergEU_glb()[[cluster]]$memberStates[,2])
     MScorrente <-convergEU_glb()[[cluster]]$memberStates[["codeMS"]]
  }else{
    MScorrente <-     names(myTB)[-posit]
  }
  tmp2 <- dplyr::select(tmp,MScorrente)#tmp[,MScorrente]
  media <- apply(tmp2,1,mean)
  ##myTB[,cluster] <- media
  myTB <- dplyr::mutate(myTB,!!cluster := media)
  out_obj$res <- myTB
  return(out_obj)
}


