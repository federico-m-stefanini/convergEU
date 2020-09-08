#' Global objects for convergEU package
#'
#' This is a list of constants and setups for the package. In this function that generates global static objects
#' and tables, cluster of countries are stored with their corresponding labels as well as indicators information
#' and labels.
#'
#' Note that EU27 refers to Member States after the 1st February 2020, while EU28 is a valid
#' tag up to 31 March 2020. String EU27_2020 and EU27_2007 as defined by Eurofound are also
#' available.
#'
#' @return  a list of constants and objects for package convergEU
#' @details The following clusters of countries are stored: EU12, EU15, EU19, EU25,
#'           EU27, EA, Eurozone. Current Member States are elements of EU27_2020.
#'          The cluster geoRefEUF is composed of both Member States and other countries
#'          (neighboring countries). The component "metaEUstat" contains the indicators'
#'          information, while the component "paralintags" is for defining patterns for the Member States.
#'
#' @references{ \url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Member States in the cluster Eurozone:
#' convergEU_glb()$Eurozone
#'
#' # Cluster EU12 of Member States:
#' convergEU_glb()$EU12
#'
#' # Cluster EU19 of Member States:
#' convergEU_glb()$EU19
#'
#' # Cluster EU27 of Member States after 31 jan 2020:
#' convergEU_glb()$EU27
#'
#' # Cluster EU28 of Member States up to jan 2020:
#' convergEU_glb()$EU28
#'
#' # The countries in the cluster geoRefEUF:
#' convergEU_glb()$geoRefEUF
#'
#' # Metainformation on indicators of the European Union:
#' convergEU_glb()$metaEUStat
#'
#' @export
#'
#'
convergEU_glb <- function(){
  # global constants
  rounDigits <- 4 # rounding
  epsilonV <-  0.0001 # small constant for rounding tresholding
  #
  # Labels and codes for MS
  EUcodes <- structure(list(
           pae = c("1-AT", "10-ES", "11-FI", "12-FR", "13-HR",
                       "14-HU", "15-IE", "16-IT", "17-LT", "18-LU", "19-LV", "2-BE",
                       "20-MT", "21-NL", "22-PL", "23-PT", "24-RO", "25-SE", "26-SI",
                       "27-SK", "28-UK", "3-BG", "4-CY", "5-CZ", "6-DE", "7-DK", "8-EE",
                       "9-EL"),
            paeF = c("1-AT", "10-ES", "11-FI", "12-FR", "13-HR",
                                         "14-HU", "15-IE", "16-IT", "17-LT", "18-LU", "19-LV", "2-BE",
                                         "20-MT", "21-NL", "22-PL", "23-PT", "24-RO", "25-SE", "26-SI",
                                         "27-SK", "28-UK", "3-BG", "4-CY", "5-CZ", "6-DE", "7-DK", "8-EE",
                                         "9-EL"),
            paeN = c(1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 3, 4, 5, 6, 7, 8, 9),
            paeS = c("AT", "ES", "FI", "FR", "HR", "HU", "IE", "IT",
                        "LT", "LU", "LV", "BE", "MT", "NL", "PL", "PT", "RO", "SE",
                        "SI", "SK", "UK", "BG", "CY", "CZ", "DE", "DK", "EE", "EL"
               )), row.names = c(NA, -28L), class = c("tbl_df", "tbl", "data.frame"
               ))

  # COUNTRIES AREA EURO
  ############
  ## Sources
  ## https://ec.europa.eu/eurostat/statistics-explained/index.php/Tutorial:Country_codes_and_protocol_order
  ## https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Country_codes
  ## https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Country_codes
  ##

  EU12 <-  list(
      dates=c("01-11-1993","31/12/1994"),
      memberStates=  dplyr::tibble(MS=c("Belgium",
        "Denmark", "France", "Germany", "Greece", "Ireland", "Italy",
        "Luxembourg", "Netherlands", "Portugal", "Spain", "United-Kingdom"),
        codeMS=c("BE", "DK", "FR", "DE", "EL", "IE", "IT",
        "LU", "NL", "PT", "ES", "UK") ))

  EU15 <-  list(
    dates=c("01-01-1995","30/04/2004"),
    memberStates=  dplyr::tibble(
          MS=c("Belgium",
          "Denmark", "France", "Germany", "Greece", "Ireland", "Italy",
          "Luxembourg", "Netherlands", "Portugal", "Spain", "United-Kingdom",
          "Austria", "Finland", "Sweden"
          ),
          codeMS=c("BE", "DK", "FR", "DE", "EL", "IE", "IT",
                   "LU", "NL", "PT", "ES", "UK",
                   "AT","FI","SE") ))
  ## this equal to Eurozone tag
  EU19 <- Eurozone <- list(dates=c(NA,NA),
               memberStates= dplyr::tibble(
                 MS = c("Austria", "Belgium", "Cyprus", "Estonia",
                        "Finland", "France", "Germany", "Greece", "Ireland", "Italy",
                        "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                        "Portugal", "Slovakia", "Slovenia", "Spain"),
                 codeMS = c("AT","BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE",
                            "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK",
                            "SI", "ES")))


  EU25 <-  list(
    dates = c("01/05/2004","31/12/2006"),
    memberStates= dplyr::bind_rows(EU15$memberStates,
                  dplyr::tibble(
          MS = c("Cyprus", "Czech-Republic", "Estonia", "Hungary", "Latvia",
                 "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia"),
          codeMS = c("CY", "CZ", "EE", "HU", "LV", "LT", "MT", "PL", "SK", "SI")
          )))

  EU27 <- EU27_2020 <-  list(
    dates = c("01/02/2020","00/00/0000"),
    memberStates=  structure(list(MS = c("Belgium", "Denmark", "France", "Germany",
   "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal",
   "Spain", "Austria", "Finland", "Sweden", "Cyprus",
   "Czech-Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
   "Malta", "Poland", "Slovakia", "Slovenia", "Bulgaria", "Romania",
   "Croatia"), codeMS = c("BE", "DK", "FR", "DE", "EL", "IE", "IT",
    "LU", "NL", "PT", "ES",  "AT", "FI", "SE", "CY", "CZ", "EE",
    "HU", "LV", "LT", "MT", "PL", "SK", "SI", "BG", "RO", "HR")),
   row.names = c(NA,-27L), class = c("tbl_df", "tbl", "data.frame"))
  )

  EU27_2019 <- EU27_2007 <-    list(
      dates = c("30/06/2013","31/01/2020"),
      memberStates = structure(list(MS = c("Belgium", "Denmark", "France", "Germany",
     "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal",
     "Spain", "United-Kingdom", "Austria", "Finland", "Sweden", "Cyprus",
     "Czech-Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
     "Malta", "Poland", "Slovakia", "Slovenia", "Bulgaria", "Romania"
      ), codeMS = c("BE", "DK", "FR", "DE", "EL", "IE", "IT", "LU",
                    "NL", "PT", "ES", "UK", "AT", "FI", "SE", "CY", "CZ", "EE", "HU",
                    "LV", "LT", "MT", "PL", "SK", "SI", "BG", "RO")), row.names = c(NA,
      -27L), class = c("tbl_df", "tbl", "data.frame"))
      )


  EU28 <-  list(
    dates = c("01/06/2013","01/02/2020"),
    memberStates=  structure(list(MS = c("Belgium", "Denmark", "France", "Germany",
   "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal",
   "Spain", "United-Kingdom", "Austria", "Finland", "Sweden", "Cyprus",
   "Czech-Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
   "Malta", "Poland", "Slovakia", "Slovenia", "Bulgaria", "Romania",
   "Croatia"), codeMS = c("BE", "DK", "FR", "DE", "EL", "IE", "IT",
    "LU", "NL", "PT", "ES", "UK", "AT", "FI", "SE", "CY", "CZ", "EE",
    "HU", "LV", "LT", "MT", "PL", "SK", "SI", "BG", "RO", "HR")),
    row.names = c(NA,-28L), class = c("tbl_df", "tbl", "data.frame"))
    )
   labeclust <- c( "Eurozone", "EA", "EU12", "EU15", "EU19", "EU25",
                                    "EU27_2007", "EU27_2019", "EU27_2020", "EU27", "EU28")


  ###################################################
  # template of output objects with metainformation
  tmp_out <- list(res=NULL, # results
                  msg=NULL, # messages
                  err=NULL  # errors
                  )

  ###########################################
  ## Eurofound dataset geo labels
  geoRefEUF <- dplyr::tibble(geo = c("AD", "AL", "AM", "AT", "AZ",
                                     "BE", "BG", "BY", "CH", "CY", "CZ", "DE", "DE_TOT", "DK", "EA18",
                                   "EA19", "EE", "EEA30", "EEA31", "EFTA", "EL", "ES", "EU27", "EU28",
                                   "FI", "FR", "FX", "GE", "HR", "HU", "IE", "IS", "IT", "LI", "LT",
                                   "LU", "LV", "MD", "ME", "MK", "MT", "NL", "NO", "PL", "PT", "RO",
                                   "RS", "RU", "SE", "SI", "SK", "SM", "TR", "UA", "UK", "XK", "EA",
                                   "EA11", "EA12", "EA13", "EA15", "EA16", "EA17", "EU15", "EU25",
                                   "BA", "CN_X_HK", "JP", "KR", "US", "CPC1", "EU", "NMS10", "EEA",
                                   "AU", "CA", "MK"),
                             geo_label =  c("Andorra", "Albania", "Armenia", "Austria", "Azerbaijan",
                                         "Belgium", "Bulgaria", "Belarus", "Switzerland", "Cyprus",
                                         "Czech Republic", "Germany (until 1990 former territory of the FRG)",
                                         "Germany including former GDR", "Denmark", "Euro area (18 countries)",
                                         "Euro area (19 countries)", "Estonia",
                                         "European Economic Area (EU27 plus IS, LI, NO)",
                                         "European Economic Area (EU28 plus IS, LI, NO)",
                                         "European Free Trade Association","Greece", "Spain",
                                         "European Union (27 countries)", "European Union (28 countries)",
                                         "Finland", "France", "France (metropolitan)", "Georgia", "Croatia",
                                         "Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania",
                                         "Luxembourg", "Latvia", "Moldova", "Montenegro",
                                         "Former Yugoslav Republic of Macedonia, the",
                                         "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                                         "Serbia", "Russia", "Sweden", "Slovenia", "Slovakia", "San Marino",
                                         "Turkey", "Ukraine", "United Kingdom",
                                         "Kosovo (under United Nations Security Council Resolution 1244/99)",
                                         "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)",
                                         "Euro area (11 countries)", "Euro area (12 countries)", "Euro area (13 countries)",
                                         "Euro area (15 countries)", "Euro area (16 countries)", "Euro area (17 countries)",
                                         "European Union (15 countries)", "European Union (25 countries)",
                                         "Bosnia and Herzegovina", "China except Hong Kong", "Japan",
                                         "South Korea", "United States",
                                         "Candidate and potential candidate countries except Turkey and Kosovo",
                                         "European Union (EU6-1972, EU9-1980, EU10-1985, EU12-1994, EU15-2004, EU25-2006, EU27-2013, EU28)",
                                         "New Member States (10 countries)",
                                         "European Economic Area (EEA18-2004, EEA28-2006, EEA30-2013, EEA31)",
                                         "Australia","Canada","FYR of Macedonia"))

  ## fine Eurofound geo labels
  ##########################################

  ###########################################
  ## Metainformation to access Eurostat database
  oriMetaEUStat <- dplyr::tibble(
    Official_code = c("lfsa_argaed", "lfsa_ergaed",
                      "lfsa_ergacob", "lfsa_urgan", "une_educ_a", "lfsa_upgan", "edat_lfse_20",
                      "une_rt_a", "ilc_lvhl36", "ilc_lvhl30", "lfsa_egad", "lfsa_eppgai",
                      "lfsi_emp_a (own calculation. See tesem060)",
                      "lfsi_pt_a (own calculation. See tepsr_lm210)",
                      "lfsa_egised (own calculation)", "earn_gr_gpgr2",
                      "Own calculation. lfsa_esgan",
                      "Own calculation. lfsa_esgan", "lfsa_etgar", "prc_ppp_ind", "gov_10dd_edpt1",
                      "nasa_10_ki", "gov_10a_main", "nama_10_lp_ulc", "nama_10_lp_ulc",
                      "nama_10_lp_ulc", "earn_nt_net", "spr_exp_sum", "gov_10a_exp",
                      "ilc_pnp3", "spr_exp_pens", "spr_pns_ben", "edat_lfse_14", "trng_lfse_01",
                      "edat_lfse_03", "gov_10a_exp", "hlth_silc_08", "gov_10a_exp",
                      "isoc_sk_dskl_i]", "isoc_bde15b_h", "isoc_ci_im_i", "isoc_ec_ibuy",
                      "isoc_ci_it_en2", "rd_p_perslf", "rd_e_gerdtot", "isoc_ciegi_ac",
                      "gov_10a_exp", "Own calculation. lfsa_eegan2", "prc_hicp_aind",
                      "ilc_di12", "ilc_di11", "lfst_r_lmder", "demo_mlexpec", "hlth_hlye",
                      "y16_deprindex", "Own calculation. hlth_dm060"),
    #
    Code_in_database = c("act_15-64_p",
                         "emp_20_64_p", "emp_nonat_p", "unem_15_74_p", "unem_pri_p", "ltunemp_p",
                         "neet_p", "youthunemp_p", "temptoperm_p", "parttofull_p", "jobdur_00_11_p",
                         "involpart_p", "geg_p", "ggparttime_p", "managers_p", "gpg_p",
                         "selfemp_p", "selfempwe_p", "temporary_p", "gdppc_pps", "governdebt_p",
                         "housedebt_p", "governk_p", "labourcost_i", "labourprod_i", "compemp_pps",
                         "earnings_eur", "expspr_eur", "expspr_p", "replacepen_p", "exppens_p",
                         "pensbenf_pop", "esleavers_p", "lll_p", "terteduc_p", "expedu_p",
                         "unmetneed_p", "exphlth_p", "digskill_p", "housebband_p", "mobaccess_p",
                         "intpurchase_p", "enterpbband_p", "persRID_p", "expRID_p", "eGovern_p",
                         "exppubserv_p", "padmempl_p", "inflation_p", "gini", "s80s20",
                         "regcohes_p", "lifexp_y", "hlye_y", "depriv_i", "diffends_p"),
    #
    indicator = c("Activity rate", "Employment rate", "Employment rate of non-native workers",
                  "Unemployment rate by age", "Unemployment rate by education",
                  "Long-term unemployment rate", "NEET's rate", "Youth unemployment rate",
                  "Transition rate from temporary to permanent jobs",
                  "Transition rate from part-time to full-time jobs",
                  "Employment in current job by duration",
                  "Involuntary part-time employment as percentage of the total part-time employment",
                  "Gender employment gap", "Gender gap in part-time employment",
                  "Proportion of employment in managerial occupations", "Gender pay gap",
                  "Self employment rate", "Self employment with employees",
                  "Percentage of employees with an involuntary temporary work",
                  "Real GDP per capita in PPS (EU28)",
                  "Government consolidated gross debt to GDP ratio",
                  "Household gross debt to income ratio",
                  "General government gross fixed capital formation to GDP ratio",
                  "Nominal unit labour cost based on hours worked (Index, 2010=100)",
                  "Real labour productivity per hour worked (Index, 2010=100)",
                  "Compensation per employee per hours worked (pps)", "Annual net earnings (euros)",
                  "Total Expenditure on social protection in euro per inhabitant",
                  "General government expenditure in social protection as percentage of GDP",
                  "Aggregate replacement ratio for pensions",
                  "Total expenditure on pensions as percentage of GDP",
                  "Pensions beneficiaries at 31st December",
                  "Early leavers from education and training as % of the population aged 18-24",
                  "Percentage of adult participation in learning",
                  "Tertiary education attainment",
                  "General government expenditure in education as percentage of GDP",
                  "Self-reported unmet needs of medical care",
                  "General government expenditure in health as percentage of GDP",
                  "Percentage of individuals who have basic or above basic overall digital skills",
                  "Households with broadband access as percentage of all households",
                  "Percentage of individuals using mobile devices to access the internet on the move",
                  "Percentage of individuals using the internet for ordering goods or services",
                  "Percentage of enterprises with broadband access (fixed or mobile)",
                  "Total R&D personnel as percentage of total employment - numerator in full-time equivalent (FTE)",
                  "Expenditure in R&D as percentage of GDP",
                  "Percentage of individuals using internet in interaction with public services (eGovernance)",
                  "Government expenditure on general public services as percentage of GDP",
                  "Percentage of employees in Public administration and defence (compulsory social security) as percentage of total number of employees",
                  "Inflation rate", "Gini coefficient of equivalised disposable income",
                  "Income quintile share ratio",
                  "Dispersion in regional employment rates",
                  "Life expectancy at birth", "Healthy life years at the age of 65",
                  "Deprivation Index", "Percentage of individuals with difficulties in making ends meet"
    ),
    Official_code_purified = c("lfsa_argaed", "lfsa_ergaed",
                                "lfsa_ergacob", "lfsa_urgan",
                                "une_educ_a", "lfsa_upgan", "edat_lfse_20", "une_rt_a", "ilc_lvhl36",
                                "ilc_lvhl30", "lfsa_egad", "lfsa_eppgai",
                                "lfsi_emp_a",
                                "lfsi_pt_a",
                                "lfsa_egised",
                                "earn_gr_gpgr2",
                                "lfsa_esgan", "lfsa_esgan",
                                "lfsa_etgar", "prc_ppp_ind", "gov_10dd_edpt1", "nasa_10_ki",
                                "gov_10a_main", "nama_10_lp_ulc", "nama_10_lp_ulc", "nama_10_lp_ulc",
                                "earn_nt_net", "spr_exp_sum", "gov_10a_exp", "ilc_pnp3", "spr_exp_pens",
                                "spr_pns_ben", "edat_lfse_14", "trng_lfse_01", "edat_lfse_03",
                                "gov_10a_exp", "hlth_silc_08", "gov_10a_exp",

                                "isoc_sk_dskl_i",

                                "isoc_bde15b_h", "isoc_ci_im_i", "isoc_ec_ibuy", "isoc_ci_it_en2",
                                "rd_p_perslf", "rd_e_gerdtot", "isoc_ciegi_ac", "gov_10a_exp",

                                "lfsa_eegan2",
                                "prc_hicp_aind",

                                "ilc_di12", # missing

                                "ilc_di11", "lfst_r_lmder", "demo_mlexpec", "hlth_hlye",
                                "y16_deprindex",
                                "hlth_dm060")
  )
  oriMetaEUStat <- dplyr::mutate(oriMetaEUStat, subSelection = rep(NA, nrow(oriMetaEUStat)))
  oriMetaEUStat <- dplyr::mutate(oriMetaEUStat, selectorUser = rep(NA, nrow(oriMetaEUStat)))
  ## different   vars within   dataset
  # 1 gov_10a_exp   expspr_p         General government expenditure in social protect
  # 2 gov_10a_exp   expedu_p         General government expenditure in education as
  # 3 gov_10a_exp   exphlth_p        General government expenditure in health as
  # 4 gov_10a_exp   exppubserv_p
  estraSub <- which(oriMetaEUStat$Official_code_purified  == "gov_10a_exp")
  oriMetaEUStat$subSelection[ estraSub ] <- c("expspr_p", "expedu_p","exphlth_p","exppubserv_p")
  #
  estraSub <- which(oriMetaEUStat$Official_code_purified == "lfsa_esgan"  )
  oriMetaEUStat$subSelection[ estraSub ] <- c("selfemp_p","selfempwe_p")
  #   Official_code     Code_in_database indicator     Official_code   subSelection
  #   <chr>             <chr>            <chr>         <chr>             <chr>
  # 1 Own calculation. selfemp_p        Self employm lfsa_esgan        NA
  # 2 Own calculation. selfempwe_p      Self employm lfsa_esgan        NA
  #
  estraSub <- which(oriMetaEUStat$Official_code_purified == "nama_10_lp_ulc"  )
  oriMetaEUStat$subSelection[ estraSub ] <- c("labourcost_i","labourprod_i","compemp_pps" )
  #   Official_code  Code_in_database indicator         Official_code_p subSelection
  #   <chr>          <chr>            <chr>             <chr>            <chr>
  # 1 nama_10_lp_ulc labourcost_i     Nominal unit lab  nama_10_lp_ulc   NA
  # 2 nama_10_lp_ulc labourprod_i     Real labour prod  nama_10_lp_ulc   NA
  # 3 nama_10_lp_ulc compemp_pps      Compensation per  nama_10_lp_ulc   NA
  #
  # the user has to exploit this "SelectorUser"
  oriMetaEUStat$selectorUser <- oriMetaEUStat$Official_code_purified
  estrattoreSS <- !is.na(oriMetaEUStat$subSelection)
  oriMetaEUStat$selectorUser[estrattoreSS] <- oriMetaEUStat$subSelection[estrattoreSS]
  # esporto
  metaEUStat <- oriMetaEUStat
  ## fine
  ###########################################

  ################# map from parameters of the linear model to patterns
  ## first 4 columns deprecated
  paralintags <- dplyr::tribble(
    ~slope.EU,~interc.m,~slope.m,~constr,~tag, ~num.tag,
    TRUE,TRUE,  TRUE,  NA,   "Outperforming", 4,
    TRUE,TRUE,  FALSE, TRUE, "Flattening",2,
    TRUE,TRUE,  FALSE, FALSE,"Inversion",3,
    TRUE,FALSE, TRUE,  NA,   "Catching up",1,
    TRUE,FALSE, FALSE, TRUE, "Slower pace",5,
    TRUE,FALSE, FALSE, FALSE,"Diving",6,
    FALSE,FALSE,TRUE,  TRUE, "Recovering",11,
    FALSE,FALSE,TRUE,  FALSE,"Reacting better",12, # "better reaction",# check name
    FALSE,FALSE,FALSE, NA,"Falling away", 9,
    FALSE,TRUE, FALSE, NA, "Underperforming", 10,
    FALSE,TRUE,TRUE,FALSE,"Defending better",7,
    FALSE,TRUE,TRUE,TRUE,"Escaping", 8,
    # after meeting 17 May 2019
    NA,NA,NA,NA, "Parallel-better",13,
    NA,NA,NA,NA, "Parallel-equal",14,
    NA,NA,NA,NA, "Parallel-worse",15,
    NA,NA,NA,NA, "Crossing",16,
    NA,NA,NA,NA,  "Crossing reversed",17,
    NA,NA,NA,NA,  "Unspecified",18
  )
  ##### end of map


  ### Social Indicators within scoreboard
  scoreBoaTB <- structure(list(Sele = c(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                        9, 9, 9), IndicatorID = c("I.01.01.00", "I.01.02.00", "I.01.04.00",
                                                  "I.02.01.00", "I.02.02.00", "I.02.03.00", "I.03.01.00", "I.04.01.00",
                                                  "I.04.02.00", "I.04.03.00", "I.04.04.00", "I.04.05.00", "I.04.06.00",
                                                  "I.05.01.00", "II.06.01.00", "II.06.02.00", "II.06.03.00", "II.06.04.00",
                                                  "II.07.01.00", "II.07.03.00", "II.07.04.00", "II.07.05.00", "II.07.06.00",
                                                  "II.07.07.00", "II.08.01.00", "II.08.04.00", "III.09.01.00",
                                                  "III.09.02.00", "III.09.03.00", "III.09.04.00", "III.09.05.00",
                                                  "III.10.01.00", "III.11.01.00", "III.11.02.00", "III.11.03.00",
                                                  "III.11.04.00", "III.12.01.00"),
               Source = c("edat_lfse_14", "trng_lfse_01",
                      "Eurostat: edat_lfse_03", "Eurostat: TESEM060", "Eurostat: TEPSR_LM210",
                      "Eurostat: earn_gr_gpgr2", "Eurostat: ilc_di11", "Eurostat: ilc_peps01",
                      "Eurostat: ilc_li02", "Eurostat: ilc_mddd11", "Eurostat: TEPSR_LM430",
                      "Eurostat: TEPSR_LM440", "Eurostat: TEPSR_LM440", "Eurostat: TESEM150",
                      "Eurostat: TESEM010", "Eurostat: TESEM120", "Eurostat: TEPSR_WC130",
                      "Eurostat: TESEM140", "Eurostat: TESEM130", "Eurostat: TEPSR_WC220",
                      "Eurostat: TEPSR_WC220", "Eurostat: TEPSR_WC220", "Eurostat: TEPSR_WC220",
                      "Eurostat: TEPSR_WC230", "Eurostat: TEPSR_WC310", "Eurostat: TESOV110",
                      "Eurostat: TESPM050", "Eurostat: TEPSR_SP110", "Eurostat: TEPSR_SP110",
                      "Eurostat: TEPSR_SP110", "Eurostat: TESPN070", "Eurostat: TEPSR_SP210",
                      "Eurostat: TESPM110", "tepsr_sp320", "tepsr_sp320", "Eurostat: TEPSR_SP310",
                      "Eurostat: TEPSR_SP410"),
               Preset = c("age: 18-24; activyty: population",
                    "age 25-64;", "age: 30-34; ISCED: 5-8", "age: 20-64; indic_EM: EMP_LFS",
                    "age:20-64;", "NACE_R2: B-S_X_O", "age: TOTAL", "AGE: TOTAL; UNIT: PC",
                   "UNIT: PC;  INDIC_IL: LI_R_MD60; AGE: TOTAL", "age: TOTAL; sex: TOTAL; UNIT: PC",
                   NA, "unit: % of Owners", "unit: % of tennants", "sex: total",
                   "sex: TOTAL", "sex: total", "sex: TOTAL", "sex: TOTAL", "sex: TOTAL",
                   "duration: from 0 to 11", "duration: from 11 to 23", "duration: from 24 to 59",
                   "duration: 60 or over", "sex: total", NA, "sex: total", NA, "cofog99: social protection",
                   "cofog99: health", "cofog99: education", "sex: total", NA, "sex total",
                   "sex healty life years in absolute value - males", "sex healty life years in absolute value - females",
                   NA, "types: all individual"),
               Tags = c(edat_lfse_14 = "edat_lfse_14",
                    trng_lfse_01 = "trng_lfse_01", `Eurostat: edat_lfse_03` = "edat_lfse_03",
                    `Eurostat: TESEM060` = "TESEM060", `Eurostat: TEPSR_LM210` = "TEPSR_LM210",
                    `Eurostat: earn_gr_gpgr2` = "earn_gr_gpgr2", `Eurostat: ilc_di11` = "ilc_di11",
                    `Eurostat: ilc_peps01` = "ilc_peps01", `Eurostat: ilc_li02` = "ilc_li02",
                    `Eurostat: ilc_mddd11` = "ilc_mddd11", `Eurostat: TEPSR_LM430` = "TEPSR_LM430",
                    `Eurostat: TEPSR_LM440` = "TEPSR_LM440", `Eurostat: TEPSR_LM440` = "TEPSR_LM440",
                    `Eurostat: TESEM150` = "TESEM150", `Eurostat: TESEM010` = "TESEM010",
                    `Eurostat: TESEM120` = "TESEM120", `Eurostat: TEPSR_WC130` = "TEPSR_WC130",
                    `Eurostat: TESEM140` = "TESEM140", `Eurostat: TESEM130` = "TESEM130",
                    `Eurostat: TEPSR_WC220` = "TEPSR_WC220", `Eurostat: TEPSR_WC220` = "TEPSR_WC220",
                    `Eurostat: TEPSR_WC220` = "TEPSR_WC220", `Eurostat: TEPSR_WC220` = "TEPSR_WC220",
                    `Eurostat: TEPSR_WC230` = "TEPSR_WC230", `Eurostat: TEPSR_WC310` = "TEPSR_WC310",
                    `Eurostat: TESOV110` = "TESOV110", `Eurostat: TESPM050` = "TESPM050",
                    `Eurostat: TEPSR_SP110` = "TEPSR_SP110", `Eurostat: TEPSR_SP110` = "TEPSR_SP110",
                    `Eurostat: TEPSR_SP110` = "TEPSR_SP110", `Eurostat: TESPN070` = "TESPN070",
                    `Eurostat: TEPSR_SP210` = "TEPSR_SP210", `Eurostat: TESPM110` = "TESPM110",
                    tepsr_sp320 = "tepsr_sp320", tepsr_sp320 = "tepsr_sp320", `Eurostat: TEPSR_SP310` = "TEPSR_SP310",
                    `Eurostat: TEPSR_SP410` = "TEPSR_SP410"),
               Names = c(edat_lfse_14 = "edat_lfse_14",
                        trng_lfse_01 = "trng_lfse_01", `Eurostat: edat_lfse_03` = "edat_lfse_03",
                      `Eurostat: TESEM060` = "tesem060", `Eurostat: TEPSR_LM210` = "tepsr_lm210",
                      `Eurostat: earn_gr_gpgr2` = "earn_gr_gpgr2", `Eurostat: ilc_di11` = "ilc_di11",
                      `Eurostat: ilc_peps01` = "ilc_peps01", `Eurostat: ilc_li02` = "ilc_li02",
                      `Eurostat: ilc_mddd11` = "ilc_mddd11", `Eurostat: TEPSR_LM430` = "tepsr_lm430",
                      `Eurostat: TEPSR_LM440` = "tepsr_lm440", `Eurostat: TEPSR_LM440` = "tepsr_lm440",
                      `Eurostat: TESEM150` = "tesem150", `Eurostat: TESEM010` = "tesem010",
                      `Eurostat: TESEM120` = "tesem120", `Eurostat: TEPSR_WC130` = "tepsr_wc130",
                      `Eurostat: TESEM140` = "tesem140", `Eurostat: TESEM130` = "tesem130",
                      `Eurostat: TEPSR_WC220` = "tepsr_wc220", `Eurostat: TEPSR_WC220` = "tepsr_wc220",
                      `Eurostat: TEPSR_WC220` = "tepsr_wc220", `Eurostat: TEPSR_WC220` = "tepsr_wc220",
                      `Eurostat: TEPSR_WC230` = "tepsr_wc230", `Eurostat: TEPSR_WC310` = "tepsr_wc310",
                      `Eurostat: TESOV110` = "tesov110", `Eurostat: TESPM050` = "tespm050",
                      `Eurostat: TEPSR_SP110` = "tepsr_sp110", `Eurostat: TEPSR_SP110` = "tepsr_sp110",
                      `Eurostat: TEPSR_SP110` = "tepsr_sp110", `Eurostat: TESPN070` = "tespn070",
                      `Eurostat: TEPSR_SP210` = "tepsr_sp210", `Eurostat: TESPM110` = "tespm110",
                        tepsr_sp320 = "tepsr_sp320", tepsr_sp320 = "tepsr_sp320",
                      `Eurostat: TEPSR_SP310` = "tepsr_sp310",
                      `Eurostat: TEPSR_SP410` = "tepsr_sp410"),
Specifiche = list(c("age: 18-24",
           " activyty: population"), "age 25-64", c("age: 30-34", " ISCED: 5-8"
          ), c("age: 20-64", " indic_EM: EMP_LFS"), "age:20-64", "NACE_R2: B-S_X_O",
        "age: TOTAL", c("AGE: TOTAL", " UNIT: PC"), c("UNIT: PC",
        "  INDIC_IL: LI_R_MD60", " AGE: TOTAL"), c("age: TOTAL",
        " sex: TOTAL", " UNIT: PC"), NA_character_, "unit: % of Owners",
        "unit: % of tennants", "sex: total", "sex: TOTAL", "sex: total",
        "sex: TOTAL", "sex: TOTAL", "sex: TOTAL", "duration: from 0 to 11",
        "duration: from 11 to 23", "duration: from 24 to 59", "duration: 60 or over",
        "sex: total", NA_character_, "sex: total", NA_character_,
        "cofog99: social protection", "cofog99: health", "cofog99: education",
        "sex: total", NA_character_, "sex total",
        "sex healty life years in absolute value - males",
        "sex healty life years in absolute value - females", NA_character_,
        "types: all individual")), class = c("tbl_df", "tbl", "data.frame"
        ), row.names = c(NA, -37L))

  return(
    list(
      EUcodes=EUcodes,
      Eurozone = Eurozone,
      EA = Eurozone,
      EU12=EU12,
      EU15=EU15,
      EU19=EU19,
      EU25=EU25,
      EU27_2007= EU27_2007,# pre brexit
      EU27_2019= EU27_2019, # pre Brexit
      EU27_2020= EU27_2020, # after brexit
      EU27=EU27, # EU from 2020 Feb 02
      EU28=EU28,
      geoRefEUF=geoRefEUF, # eurofound dataset geo labels (tibble)
      metaEUStat=metaEUStat, # metainformation to use eurostat
      tmpl_out= tmp_out,  # template for the output list
      paralintags = paralintags,
      rounDigits = rounDigits,
      epsilonV =   epsilonV,
      scoreBoaTB = scoreBoaTB,
      labels_clusters = labeclust
      )
)
}


