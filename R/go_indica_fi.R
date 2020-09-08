#' Create an indicator fiche for a given aggregation of countries.
#'
#' An auxiliary function to compile a rmarkdown file to produce the indicator fiche in
#' html format within the output directory.
#'
#' Note that most of function arguments are passed as strings of characters
#' instead of object names. For example, if the object of a dataset
#' in the workspace is myTB, the parameter is set like  workDF='myTB'
#' instead of workDF=myTB as one may expect.
#' Furthermore, the dataset must be complete, that is without missing values.
#' Note also that Internet connection should be available when invoking the function
#' to properly rendering the results in the html file. The fiches have been tested with
#' the browsers Mozilla Firefox and Google Chrome.
#'
#' @param time_0 starting time.
#' @param time_t ending time.
#' @param timeName name of the variable containing times (years).
#' @param workDF name (string) of the dataset in the global environment containing
#'                    all countries contributing to average.
#' @param indicaT name of the  considered indicator.
#' @param indiType type of indicator "lowBest" or "highBest" (default).
#' @param seleMeasure set of measures of convergence; this is a
#'            subset of the following collection of strings: "beta","delta",
#'             "gamma","sigma"; "all" is a shortcut for the whole set.
#' @param seleAggre  selection of member states, default 'EU27' ('custom'
#'                    if not pre-coded).
#' @param x_angle axis orientation for time labels, default 45.
#' @param data_res_download  should data and results be downloaded, default FALSE.
#' @param auth  author of this report, default 'A.Student'.
#' @param dataNow date of production of this country fiche, default is
#'                 current time.
#' @param outFile name of the output file (without path), without extension.
#' @param outDir  output directory, eventually not existing (only one level allowed).
#' @param pdf_out should the output be saved as PDF file? The default is  FALSE.
#' @param workTB   a tibble containing data.
#' @param selfContained  TRUE if just one file is desired
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#'
#' @export
#'
#'
go_indica_fi <-  function(
  time_0 = NA,
  time_t = NA,
  timeName = NA,
  workDF = NA ,
  indicaT = NA, # 'emp_20_64_MS'
  indiType = c('highBest','lowBest')[1],
  seleMeasure = "all",
  seleAggre = 'EU27',
  x_angle =  45,
  data_res_download =  FALSE,
  auth = 'A.Student',
  dataNow =  Sys.time(), #'2019/01/31',
  outFile = NA,
  outDir = NA,
  pdf_out = FALSE,
  workTB = NULL,
  selfContained = FALSE
){
  if(is.na(workDF) & (!is.null(workTB))){
    curTB <-  workTB
  }else if(!is.na(workDF) & is.null(workTB)){
    curTB <- get(workDF,envir = .GlobalEnv)
    workTB <- curTB
  }else{
    stop("Error while specifying data.")
  }
  # check if missing values are present
  if( any(!stats::complete.cases(curTB))){
    obj_out <- convergEU_glb()$tmpl_out
    obj_out$err <- paste("Error: one or more missing values (NAs) in the dataframe. ",
                    "Please perform imputation of missing values (NA) before making fiches.")
    return(obj_out)
  }
  #
  sourceFilecss <- system.file("extdata", "EUF.css", package = "convergEU")
  sourceFile1 <- system.file("extdata", "indica_fi_2.Rmd", package = "convergEU")
  sourceFile2 <- system.file("extdata", "eurofound.jpg", package = "convergEU")
  # conditional files
  sourceFile71 <- system.file("extdata", "indica_fi_2_sigma.Rmd", package = "convergEU")
  sourceFile72 <- system.file("extdata", "indica_fi_2_beta.Rmd", package = "convergEU")
  sourceFile73 <- system.file("extdata", "indica_fi_2_nobeta.Rmd", package = "convergEU")
  sourceFile74 <- system.file("extdata", "indica_fi_2_delta.Rmd", package = "convergEU")
  sourceFile75 <- system.file("extdata", "indica_fi_2_gamma.Rmd", package = "convergEU")
  #
  if (is.na(outFile)) {
    outFile2 <- paste0("indica-fie-",seleAggre,"-", indicaT, "-", time_0,"-",time_t, ".html")
    outFile <- paste0("indica-fi-",seleAggre,"-", indicaT, "-", time_0,"-",time_t)
  }else{
    outFile2 <- paste0(outFile,".html")
  }
  if (is.na(outDir)) {
    outDir <- file.path(getwd(),"out_dir_counvergEU")
  }
  # check
  resDE <- dir.exists(outDir)
  if (!resDE) {
    dir.create(outDir, FALSE)
  }
  # full path
  outPF <- file.path(outDir,outFile2)
  outFcss <- file.path(outDir,"EUF.css")
  sourcePF1 <- file.path(outDir,"indica_fi_2.Rmd")
  sourcePF2 <- file.path(outDir,"eurofound.jpg")
  sourcePF71 <- file.path(outDir,"indica_fi_2_sigma.Rmd")
  sourcePF72 <- file.path(outDir,"indica_fi_2_beta.Rmd")
  sourcePF73 <- file.path(outDir,"indica_fi_2_nobeta.Rmd")
  sourcePF74 <- file.path(outDir,"indica_fi_2_delta.Rmd")
  sourcePF75 <- file.path(outDir,"indica_fi_2_gamma.Rmd")
    file.copy(from = sourceFile1,
              to = sourcePF1,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile2,
              to = sourcePF2,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile71,
              to = sourcePF71,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile72,
              to = sourcePF72,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile73,
              to = sourcePF73,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile74,
              to = sourcePF74,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile75,
              to = sourcePF75,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilecss,
              to = outFcss,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
  #
  if(selfContained){
     myOutOpt <- list(self_contained = TRUE,
                      mathjax ="default")
  }else{
    myOutOpt <- list(self_contained = FALSE,
         mathjax = 'local')
  }
  # go with rendering
  rmarkdown::render(sourcePF1,
                    params = list(
                      dataNow = dataNow,
                      workingDF = workDF,
                      time_0 = time_0,
                      time_t = time_t,
                      timeName = timeName,
                      indiType = indiType,
                      indicaT = indicaT,
                      seleMeasure = seleMeasure,
                      seleAggre = seleAggre,
                      x_angle =  x_angle,
                      data_res_download =  data_res_download,
                      auth = auth,
                      outFile = outFile,
                      outDir = outDir,
                      pdf_out = FALSE,
                      workTB = workTB
                    ),
                    output_options = myOutOpt,
                    output_file = outPF,
                    encoding = "UTF-8")
}

