
context("Tagging patterns with labels and numbers, original algorithm")




# debug(ms_pattern)

test_that("Simplest test gradients and deltas squared", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
   )
  # 2003, 2004
  emme1EU <- mean(c(1.3,   2.9,    4.0))
  emme2EU <- mean(c(1.2,   3.1,    4.1))
  gradiEU <- (emme2EU -emme1EU)/(2004-2003)
  resT <-   coeu_grad(emme2EU,emme1EU, 1.2, 1.3, 2004, 2003)
  expect_equal( resT$GraEU, gradiEU)
  expect_equal( resT$time_len, 2004-2003)
  expect_equal( resT$GraMS, 1.2-1.3)
  expect_equal( resT$Delta2,
                (1.2 - emme2EU)^2 - (1.3 - emme1EU)^2);



})



# debug(ms_pattern_ori)


test_that("Simplest test ms_pattern_ori", {


  resT <- ms_pattern_ori(emp_20_64_MS,"time","highBest")
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[2,3])),1)
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[2,5])),21)
  expect_equal(as.numeric(unlist(resT$res$mat_num_tags[1,5])),4)

  expect_equal(resT$res$mat_num_tags$Catching_up[2],
               sum(as.numeric(resT$res$mat_num_tags[2,2:15]) == 1));

})


# debug(ms_pattern_ori)
test_that("Test highBest", {
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     19,   20,    21,
    2005,     29,   30,    31
  )
  # mean(c(0.8,   2.7,    3.9))
  # mean(c(1.2,   3.2,    4.2))

  # critical pattern: parallelism
  resT1 <- ms_pattern_ori(testTB,"time","highBest")
  #?ms_pattern_ori

  expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                "Parallel-better-under")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                "Parallel-better-over")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                "Parallel-better-over")

  expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                18)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                13)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                13)


  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     19,   20,    21,
    2005,     19,   20,    21
  )
  # mean(c(0.8,   2.7,    3.9))
  # mean(c(1.2,   3.2,    4.2))

  # critical pattern: parallelism
  resT1 <- ms_pattern_ori(testTB,"time","highBest")
  #?ms_pattern_ori

  expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                "Parallel-equal-under")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                "Parallel-equal-over")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                "Parallel-equal-over")

  expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                17)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                 14)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                 14)



  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     29,   30,    31,
    2005,     19,   20,    21
  )
  # mean(c(0.8,   2.7,    3.9))
  # mean(c(1.2,   3.2,    4.2))

  # critical pattern: parallelism
  resT1 <- ms_pattern_ori(testTB,"time","highBest")
  #?ms_pattern_ori

  expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                "Parallel-worse-under")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                "Parallel-worse-over")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                "Parallel-worse-over")

  expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                16)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                 15)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                 15)



  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     29,   30,    31,
    2005,     21,   20,    19
  )
  # mean(c(0.8,   2.7,    3.9))
  # mean(c(1.2,   3.2,    4.2))

  # critical pattern: parallelism
  resT1 <- ms_pattern_ori(testTB,"time","highBest")
  #?ms_pattern_ori

  expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                "Crossing reversed")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                "Parallel-worse-over")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                "Crossing")

  expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                20)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                 15)
  expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                 19)


  # now the first 12 pateterns
  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     17,   20,    23,
    2005,     29,   30,    31
  )
  # mean(c(0.8,   2.7,    3.9))
  # mean(c(1.2,   3.2,    4.2))

  #
  resT1 <- ms_pattern_ori(testTB,"time","highBest")
  #?ms_pattern_ori

  expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                "Catching up")
  expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                "Flattening")

  expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                1)
   expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                 2)

   # now the first 12 pateterns: pattern 3
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     33,   20,    7,
     2005,     31,   30,    29
   )

   #
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   #?ms_pattern_ori

   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Inversion")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 3)

   # now the first 12 pateterns: pattern 4
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     23,   20,    17,
     2005,     35,   30,    25
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Outperforming")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 4)


   # now the first 12 pateterns: pattern 5
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     17,   20,    23,
     2005,     22,   30,    38
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Slower pace")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 5)


   # now the first 12 pateterns: pattern 6
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     18,   20,    22,
     2005,     16,   30,    44
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Diving")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 6)


   # now the first 12 pateterns: pattern 7 e 9
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     38,   30,    22,
     2005,     35,   20,    5
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Defending better")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 7)
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                 "Falling away")
   expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                  9)

   # now the first 12 pateterns: pattern 8
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     38,   30,    22,
     2005,     45,   20,    -5
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Escaping")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 8)

   # now the first 12 pateterns: pattern 10
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     38,   30,    22,
     2005,     25,   20,    15
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Underperforming")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 10)

   # now the first 12 pateterns: pattern 11
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     15,   30,    45,
     2005,     19,   20,    21
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Recovering")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 11)


   # now the first 12 pateterns: pattern 12
   testTB <- dplyr::tribble(
     ~time, ~countryA ,  ~countryB,  ~countryC,
     2000,     15,   30,    45,
     2005,     10,   20,    30
   )
   resT1 <- ms_pattern_ori(testTB,"time","highBest")
   expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                 "Reacting better")
   expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                 12)

})






# debug(ms_pattern_ori)
test_that("Test lowBest", {

    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     19,   20,    21,
      2005,     29,   30,    31
    )
    # mean(c( 19,   20,    21))
    # mean(c(29,   30,    31))

    # critical pattern: parallelism
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    #?ms_pattern_ori

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Parallel-worse-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                  "Parallel-worse-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Parallel-worse-under")

    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  15)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                   15)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   16)


    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     19,   20,    21,
      2005,     19,   20,    21
    )
    # mean(c(0.8,   2.7,    3.9))
    # mean(c(1.2,   3.2,    4.2))

    # critical pattern: parallelism
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    #?ms_pattern_ori

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Parallel-equal-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                  "Parallel-equal-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Parallel-equal-under")

    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  14)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                   14)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   17)



    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     29,   30,    31,
      2005,     19,   20,    21
    )
   resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    # ms_pattern_ori(testTB,"time","highBest")

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Parallel-better-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[2,2])),
                  "Parallel-better-over")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Parallel-better-under")

    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  13)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[2,2])),
                   13)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   18)



    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     29,   30,    31,
      2005,     21,   20,    19
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    # ms_pattern_ori(testTB,"time","highBest")

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Crossing")

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Crossing reversed")

    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  19)

    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   20)


    # now the first 12 pateterns
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     17,   20,    23,
      2005,     29,   30,    31
    )
    # mean(c(0.8,   2.7,    3.9))
    # mean(c(1.2,   3.2,    4.2))

    #
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    #?ms_pattern_ori

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Underperforming")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Reacting better")

    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  10)
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   12)

    # now the first 12 pateterns: pattern 3
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     33,   20,    7,
      2005,     31,   30,    29
    )

    #
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    #?ms_pattern_ori

    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Recovering")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  11)

    # now the first 12 pateterns: pattern 4
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     23,   20,    17,
      2005,     35,   30,    25
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Falling away")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  9)


    # now the first 12 pateterns: pattern 5
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     17,   20,    23,
      2005,     22,   30,    38
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Defending better")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  7)


    # now the first 12 pateterns: pattern 6
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     18,   20,    22,
      2005,     16,   30,    44
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Escaping")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  8)


    # now the first 12 pateterns: pattern 7 e 9
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     38,   30,    22,
      2005,     35,   20,    5
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Slower pace")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  5)
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[3,2])),
                  "Outperforming")
    expect_equal(  as.numeric(unlist(resT1$res$mat_without_summaries[3,2])),
                   4)

    # now the first 12 pateterns: pattern 8
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     38,   30,    22,
      2005,     45,   20,    -5
    )
    # ms_pattern_ori(testTB,"time","highBest")
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Diving")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  6)

    # now the first 12 pateterns: pattern 10
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     38,   30,    22,
      2005,     25,   20,    15
    )

    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Catching up")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  1)

    # now the first 12 pateterns: pattern 11
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     15,   30,    45,
      2005,     19,   20,    21
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Inversion")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  3)


    # now the first 12 pateterns: pattern 12
    testTB <- dplyr::tribble(
      ~time, ~countryA ,  ~countryB,  ~countryC,
      2000,     15,   30,    45,
      2005,     10,   20,    30
    )
    resT1 <- ms_pattern_ori(testTB,"time","lowBest")
    expect_equal( as.character(unlist(resT1$res$mat_label_tags[1,2])),
                  "Flattening")
    expect_equal( as.numeric(unlist(resT1$res$mat_without_summaries[1,2])),
                  2)

  })









test_that("Failure  tests", {

   testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )
  testRes <- ms_pattern_ori(testTB, timeName="pippo",
                        typeIn = c('highBest','lowBest')[2])

  expect_null( testRes$res)

  testRes <- ms_pattern_ori(testTB, timeName="pippo",
                        typeIn = 'kamikaze')
  expect_null( testRes$res)

  testRes <- ms_pattern_ori(testTB, timeName="pippo",
                        typeIn = c('highBest','lowBest')[2])
  expect_null( testRes$res)

  testRes <- ms_pattern_ori(testTB, timeName="pippo",
                        typeIn = c('highBest','lowBest')[2])
  expect_equal( testRes$err, "Error: the dataset failed to pass preliminary checks.  Error: timeName variable absent.")


  testTB <- dplyr::tribble(
    ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     NA,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )
  testRes <- ms_pattern_ori(testTB, timeName="pippo",
                        typeIn = c('highBest','lowBest')[1]
                        )
  expect_equal( testRes$err,
    "Error: the dataset failed to pass preliminary checks.  Error: one or more missing values in the dataframe.")


})




