
context("Mapping to 39 patterns,  algorithm on october 2020")



# options(tibble.print_max=Inf, tibble.print_min=Inf)
# debug(ms_pattern_39)

test_that("Simplest test ", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
   )
  
  
  res <- ms_pattern_39(testTB, "time")
  expect_equal( res$res$mat_num_tags[[4]][3], 11)
  # 2003, 2004
  #emme0EU<-mean(0.9,   2.9,    4.1,)
  #emme1EU <- mean(c(1.3,   2.9,    4.0))
  #emme2EU <- mean(c(1.2,   3.1,    4.1))
  #gradiEU <- (emme2EU -emme1EU)/(2004-2003)
  

})



 
test_that("Simplest test ms_pattern_ori", {


  resT <- ms_pattern_ori(emp_20_64_MS,"time","highBest")
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[2,3])),1)
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[2,5])),21)
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[1,5])),4)

  expect_equal(resT$res$mat_num_tags$Catching_up[2],
               sum(as.numeric(resT$res$mat_num_tags[2,2:15]) == 1));

})


 

test_that("Test patterns of the legend in both fiches", {
    # vector of remapping to old numericla codes
    current_map <- c(c(16,13,11,15,18,20,3,1,10,5,6,7,14,23,4,9,27,17,33,32),
                    c(2,8,12,19,21,22,24,25,26,28,29,30,31,34,35:40))
    
    myTB20 <- structure(list(Time = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 
    2, 2, 2, 2, 2, 2, 2), EU = c(10, 10, 10, 10, 10, 10, 10, 10, 
    10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Grp = c(1, 2, 3, 4, 5, 
    6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), MS = c(12, 12, 
    12, 12, 16, -12, -2, 4, 6, 8, 22, 12, 6, 2, 2, -2, -2, -2, -4, 
    -6)), row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"
    ))
    tbG10 <- structure(list(Time = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 
    2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 
    2, 2, 2, 2, 2, 2), EU = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 
    10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), Grp = c(1, 2, 3, 
    4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
    13, 14, 15, 16, 17, 18, 19, 20, 11, 12, 13, 14, 15, 16, 17, 18, 
    19, 20), MS = c(12, 12, 12, 12, 16, -12, -2, 4, 6, 8, 22, 12, 
    6, 2, 2, -2, -2, -2, -4, -6, 18, 12, 6, 2, 1, -8, -2, -2, -4, 
    -2, 12, 12, 12, 12, 17, 9, 8, 3, -4, -16)), row.names = c(NA, 
    -40L), class = c("tbl_df", "tbl", "data.frame"))
    
    test_set_1 <-  dplyr::bind_cols(
      dplyr::rename(tbG10[1:10,], EU1=EU,MS1=MS, Grp1=Grp),
      dplyr::select(dplyr::rename(tbG10[11:20,],EU2=EU,MS2=MS, Grp2=Grp),
             -Time,-Grp2))
    test_set_2 <-  dplyr::bind_cols(
      dplyr::rename(tbG10[21:30,], EU1=EU,MS1=MS, Grp1=Grp),
      dplyr::select(dplyr::rename(tbG10[31:40,],EU2=EU,MS2=MS, Grp2=Grp),
                    -Time,-Grp2))
   for(auxTest in 1:2){
      test_set <-  list(test_set_1,test_set_2)[[auxTest]]
       for(aux in 1:10){
         res1 <-  as.numeric(test_set[aux,])
         mypattern <- map_2_patt_39(vaMS= res1[c(4,6)],#c(vaMS1,vaMS2),
                                    vaEU= res1[c(2,5)],#c(vaEU1,vaEU2),
                                    vaT = res1[1])
         mypattern_recoded <- map_2_patt_39(vaMS= res1[c(4,6)],#c(vaMS1,vaMS2),
                                    vaEU= res1[c(2,5)],#c(vaEU1,vaEU2),
                                    vaT = res1[1],
                                    remap=TRUE)
         #print(mypattern)
         reference_code <- aux+10*(auxTest-1)
         expect_equal(mypattern, reference_code);
         expect_equal(mypattern_recoded, current_map[reference_code]);
       }
   }
    
    

    tbG2 <- structure(list(Pat = c("EUT1", "EUT1", "EUT1", "EUT1", "EUT1", 
              "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", 
              "EUT2", "EUT2", "EUT2", "EUT1", "EUT1", "EUT1", "EUT1", "EUT1", 
              "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", 
              "EUT2", "EUT2", "EUT2", "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", 
              "EUT2"), EU = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
              10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 
              10, 10, 0, 5, 10, 10, 5, 0), Time = c(1, 1, 1, 1, 1, 1, 1, 1, 
              2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 
              2, 2, 2, 1, 1, 1, 2, 2, 2), MS = c(6, 13, 13, 18, 18, 7, 7, 1, 
              18, 18, 13, 13, 6, 1, 7, 8, 13, 7, 5, 5, 18, 7, 4, -2, -3, 3, 
              5, 14, 7, 7, 7, 12, 0, 5, 10, 10, 5, 0), PatMS = c("M1", "M1", 
              "M1", "M1", "M1", "M1", "M1", "M1", "M2", "M2", "M2", "M2", "M2", 
              "M2", "M2", "M2", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", 
              "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M1", "M1", "M1", 
              "M2", "M2", "M2"), Grp = c(21L, 22L, 23L, 24L, 25L, 26L, 27L, 
              28L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 
              33L, 34L, 35L, 36L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 
              38L, 39L, 37L, 38L, 39L)), row.names = c(NA, -38L), class = c("tbl_df", 
              "tbl", "data.frame"))

    test_set_3 <- dplyr::select(
        dplyr::bind_cols(
          dplyr::rename(
            tbG2[1:8,], EU1=EU,MS1=MS, Grp1=Grp),
          dplyr::select(dplyr::rename(
            tbG2[9:16,],EU2=EU,MS2=MS, Grp2=Grp,Pat2=Pat,PatMS2=PatMS),
                        -Time,-Grp2,-Pat2,-PatMS2)  
          ) ,
      -Pat,-PatMS)
    
    test_set_4 <- dplyr::select(
      dplyr::bind_cols(
            dplyr::rename(
              tbG2[17:24,], EU1=EU,MS1=MS, Grp1=Grp),
            dplyr::select(dplyr::rename(
              tbG2[25:32,],EU2=EU,MS2=MS, Grp2=Grp),
                          -Time,-Grp2,-Pat,-Grp2,-PatMS)) ,
      -Pat,-PatMS)

    
    for(auxTest in 1:2){
      test_set <-  list(test_set_3,test_set_4)[[auxTest]]
      for(aux in 1:8){
        res1 <-  as.numeric(test_set[aux,])
        mypattern <- map_2_patt_39(vaMS= res1[c(3,6)],#c(vaMS1,vaMS2),
                                   vaEU= res1[c(1,5)],#c(vaEU1,vaEU2),
                                   vaT = res1[2])
        mypattern_recoded <- map_2_patt_39(vaMS= res1[c(3,6)],#c(vaMS1,vaMS2),
                                   vaEU= res1[c(1,5)],#c(vaEU1,vaEU2),
                                   vaT = res1[2],
                                   remap=TRUE)
        #print(mypattern)
        # expect_equal(mypattern, 20+aux+8*(auxTest-1));
        reference_code <- 20+aux+8*(auxTest-1)
        expect_equal(mypattern, reference_code);
        expect_equal(mypattern_recoded, current_map[reference_code]);
        }
    }
    
    # last chunk
    test_set_5 <- dplyr::select(
      dplyr::bind_cols(
        dplyr::rename(
          tbG2[33:35,], EU1=EU,MS1=MS, Grp1=Grp),
        dplyr::select(dplyr::rename(
          tbG2[36:38,],EU2=EU,MS2=MS, Grp2=Grp),
          -Time,-Grp2,-Pat,-Grp2,-PatMS) ) ,
      -Pat,-PatMS)
    
    test_set <-  test_set_5
    for(aux in 1:3){# aux <- 1
      res1 <-  as.numeric(test_set[aux,])
      mypattern <- map_2_patt_39(vaMS= res1[c(3,6)],#c(vaMS1,vaMS2),
                                 vaEU= res1[c(1,5)],#c(vaEU1,vaEU2),
                                 vaT = res1[2])
      mypattern_recoded <- map_2_patt_39(vaMS= res1[c(3,6)],#c(vaMS1,vaMS2),
                                 vaEU= res1[c(1,5)],#c(vaEU1,vaEU2),
                                 vaT = res1[2],
                                 remap = TRUE)
      #print(mypattern)
      reference_code <- 36+aux
      #expect_equal(mypattern, 36+aux);
      expect_equal(mypattern, reference_code);
      expect_equal(mypattern_recoded, current_map[reference_code]);
    }
    
})


 
 



