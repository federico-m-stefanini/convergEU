#' Downloader of social scoreboard indicators
#'
#'
#' This is an "envelope function" to automate the download from Eurostat
#' of all the indicators involved in the social scoreboard.
#'
#'
#' Note that the downloaded datasets may have auxiliary columns to be later removed
#' and they may contain missing values, thus before further calculation taking place,
#' imputation or truncation of missing values must be performed.
#' Extra columns include EU12 and other similar weighted averages.
#'
#' @param fromTime starting time
#' @param toTime ending time
#' @param rm.EU  is TRUE remove all variables (columns) starting with
#'              "EU" and "EA", that is averages for different groups of countries.
#' @return a list with as many components as indicators.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#'
#' @export
#'
dow_soc_scor_boa <- function(
          fromTime = 1999,
          toTime = 2018,
          rm.EU = FALSE)
{
   res <-   list()
   scoreBoaTB <- convergEU_glb()$scoreBoaTB
   structureDB <- dplyr::tribble(
   ~gender, ~ageInterv,~uniqueIdentif, ~condiz,
    #1
      NA,  "Y18-24",  4,  'wstatus: POP; unit: PC',
     "T",  "Y25-64",  1,  'unit: PC',
      NA,  "Y30-34",  4,  'isced11: ED5-8',
      NA,  "Y20-64",  1,  'indic_em: EMP_LFS',
      NA,  "Y20-64", 1,   'worktime: PT; unit: PC_EMP',
     # 6
     NA,       NA,  4,  'unit: PC; nace_r2: B-S_X_O',
     'T',      NA,  1,  'indic_il: S80_S20',
     'T',      NA,  1, 'unit: PC',
      NA,      NA,  6, 'unit: PC; indic_il: LI_R_MD60',
      NA,      NA,  1, 'age: TOTAL  sex: TOTAL UNIT: PC',
    # 11
      NA,      NA,  1, 'unit: PC_Y_LT60',
      NA,      NA,  1, 'unit: PC; tenure: OWN_L',
      NA,      NA,  1, 'unit: PC; tenure: OWN_L',
      NA,      NA,  1, 'typtrai: NO_FED_NFE; wstatus: NEMP; unit: PC',
      NA,      NA,  1, 'unit: PC_POP; indic_em: EMP_LFS',
    # 16
      'T',     NA,   1,  'unit: PC_ACT ',
      'T',     NA,   1, 'unit: PC_POP; indic_em: ACT',
      'T',     NA,   1, 'unit: PC_ACT',
      'T',     NA,   1, 'indic_em: LTU; unit: PC_ACT',
       NA,     NA,   1, 'duration: M0-11; unit: PC',
     # 21
   NA,     NA,   2, 'duration: M12-23; unit: PC',# nota non 11
   NA,     NA,   3, 'duration: M24-59; unit: PC',
   NA,     NA,   4, 'duration: M_GE60; unit: PC',
   NA,     NA,   1, 'sex: total',
   NA,     NA,   1, 'unit: CP_MNAC; direct: PAID; na_item: B6G_R_HAB_2008; sector: S14_S15',
#26
   NA,     NA,   1, 'wstatus: EMP',
   NA,     NA,   1, 'indic_il: LI_R_MD60BT; unit: PC',
   NA,     NA,   3, 'cofog99: social protection',
   NA,     NA,   1, 'cofog99: health',
   NA,     NA,   2, 'cofog99: education',
   # 31
   NA,     NA,   1, 'indic_il: R_PN_WK',
   NA,     NA,   1, 'duration: H_GE1 ',
   NA,     NA,   1, 'unit: PC; quantile: TOTAL; reason: TOOEFW',
   NA,     NA,   3, 'unit: YR; indic_he: M_65_DFLE->sex healty life years in absolute value - males',
   NA,     NA,   4, 'unit: YR; indic_he: F_65_DFLE-> sex healty life years in absolute value - females',
   NA,     NA,   2, 'unit: PC_CHE; icha11_hf: HF3',
   NA,     NA,   2, 'indic_is: I_DSK_BAB; ind_type: IND_TOTAL; unit: PC_IND'
   )
  rowTot <- nrow(structureDB)
  for(aux in 1:rowTot){#  37
      mymes <- paste("Downloading Indicator: ",aux, " of ",rowTot,"\n")
      message(mymes)
      IDindica <- as.character(unlist(scoreBoaTB[aux,"IndicatorID"]))
      NameIndica <- as.character(unlist(scoreBoaTB[["Names"]])[aux])
      res[[IDindica]] <-  down_lo_EUS(
                 NameIndica,
                 fromTime = fromTime,
                 toTime   = toTime,
                 gender= structureDB$gender[aux],
                 ageInterv = structureDB$ageInterv[aux],
                 rawDump=FALSE,
                 uniqueIdentif = structureDB$uniqueIdentif[aux])
      if(rm.EU){
            name_vars <- names(res[[IDindica]]$res)
            eliminandi <-  ((sapply(name_vars,stringr::str_detect,"EU") |
               sapply(name_vars,stringr::str_detect,"EA")) &
               sapply(name_vars,stringr::str_length)>2 );
            res[[IDindica]]$res <- res[[IDindica]]$res[,!eliminandi]
      }else{};
   }
return(res)
}

