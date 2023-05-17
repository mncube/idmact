test_that("idmact_comp works", {
  # Test all list inputs

  # Inputs
  raw = list(list(1, 2, 3, 4, 5), list(1, 1, 1, 1, 1))
  inc = list(1 , 1)
  map_raw = list(list(1, 2, 3, 4, 5))
  map_scale = list(list(20, 21, 22, 23, 24))
    # # Debugging inputs
    # df = NULL
    # df_map = NULL
    # mcent = "mean"
    # na.rm.mcent = TRUE

  comp_mean <- idmact_comp(raw = raw,
                           inc = inc,
                           map_raw = map_raw,
                           map_scale = map_scale)

  expect_equal(comp_mean$composite_results$deltac, 0.8)

  # Test raw scores in df map in lists

  #Inputs
  df <- data.frame(Id = c(1:20),
                   Score1 = rep(11:15, 4),
                   Score2 = rep(11:14, 5))
  raw = list("Score1", "Score2")
  inc = list(1 , 1)
  map_raw <- list(list(11, 12, 13, 14, 15))
  map_scale <- list(list(21, 22, 23, 24, 25))


  comp_dfraws_mean <- idmact_comp(df = df,
                                  raw = raw,
                                  inc = inc,
                                  map_raw = map_raw,
                                  map_scale = map_scale)

  expect_equal(comp_dfraws_mean$composite_results$deltac, 0.9)

  # Test raw scores and map in same df

  df <- data.frame(Id = c(1:20),
                   Score1 = rep(11:15, 4),
                   Score2 = rep(12:15, 5),
                   rawmap = rep(11:15, 4),
                   scalemap = rep(21:25, 4))
  raw = list("Score1", "Score2")
  inc = list(1 , 1)
  map_raw <- list("rawmap")
  map_scale <- list("scalemap")

  comp_samedf_mean <- idmact_comp(df = df,
                                  raw = raw,
                                  inc = inc,
                                  map_raw = map_raw,
                                  map_scale = map_scale)

  expect_equal(comp_samedf_mean$composite_results$deltac, 0.75)

  # Test raw scores and map in seperate dfs

  df <- data.frame(Id = c(1:20),
                   Score1 = rep(11:15, 4),
                   Score2 = rep(12:15, 5))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4))
  raw = list("Score1", "Score2")
  inc = list(1 , 1)
  map_raw <- list("rawmap")
  map_scale <- list("scalemap")

  comp_samedf_mean <- idmact_comp(df = df,
                                  df_map = df_map,
                                  raw = raw,
                                  inc = inc,
                                  map_raw = map_raw,
                                  map_scale = map_scale)

  expect_equal(comp_samedf_mean$composite_results$deltac, 0.75)

})


test_that("idmact_comp sanity check: reverse maps with symmetric constant raw
          scores gives a deltac of 0", {

            # Test 2s and 4s
            raw1 = list(2,2,2)
            map_raw1 = list(1,2, 3, 4, 5)
            map_scale1 = list(10, 20, 30, 40, 50)

            raw2 = list(4,4,4)
            map_raw2 = list(1,2, 3, 4, 5)
            map_scale2 = list(50, 40, 30, 20, 10)

            inc = list(1 , 1)

            expect_equal(idmact_comp(raw = list(raw1, raw2),
                                     map_raw = list(map_raw1, map_raw2),
                                     map_scale = list(map_scale1, map_scale2),
                                     inc = inc)$composite_results$deltac, 0)
            })

test_that("README Composite Example", {
  # Create 100 raw scores
  set.seed(279)
  raw_scores <- as.list(sample(1:100, 100, replace = TRUE))

  # Map between raw scores and scale scores for each form

  ## Each form has scale scores ranging from 1 to 12
  map_scale <- c(1:12)

  ## Each assessment has raw scores ranging from 1 - 100
  map_raw_formA <- list(1:5, 6:20, 21:25, 26:40, 41:45, 46:50, 51:55,
                        56:75, 76:80, 81:85, 86:90, 91:100)

  map_raw_formB <- list(1:10, 11:20, 21:30, 31:40, 41:50, 51:55, 56:65,
                        66:75, 76:85, 86:90, 91:95, 96:100)

  formA <- map_elongate(map_raw = map_raw_formA,
                        map_scale = map_scale)

  formB <- map_elongate(map_raw = map_raw_formB,
                        map_scale = map_scale)

  resA <- idmact_subj(raw = raw_scores,
                      map_raw = formA$map_raw,
                      map_scale = formA$map_scale)


  resB <- idmact_subj(raw = raw_scores,
                      map_raw = formB$map_raw,
                      map_scale = formB$map_scale)

  # Create 100 raw scores
  set.seed(250)
  raw_scores_s2 <- as.list(sample(1:100, 100, replace = TRUE))

  # Subject to will use the same ranges for raw scores and scale scores as was used
  # in the previous example, but the map will be slightly different.
  map_raw_formA_s2 <- list(1:10, 11:25, 26:30, 31:40, 41:45, 46:50, 51:60,
                           61:75, 76:80, 81:85, 86:90, 91:100)

  map_raw_formB_s2 <- list(1:10, 11:16, 17:25, 26:35, 36:45, 46:55, 56:60,
                           61:75, 76:85, 86:90, 91:95, 96:100)

  formA_s2 <- map_elongate(map_raw = map_raw_formA_s2,
                           map_scale = map_scale)

  formB_s2 <- map_elongate(map_raw = map_raw_formB_s2,
                           map_scale = map_scale)

  resA_comp <- idmact_comp(raw = list(raw_scores, raw_scores_s2),
                           inc = list(1, 1),
                           map_raw = list(formA$map_raw, formA_s2$map_raw),
                           map_scale = list(formA$map_scale, formA_s2$map_scale))

  resB_comp <- idmact_comp(raw = list(raw_scores, raw_scores_s2),
                           inc = list(1, 1),
                           map_raw = list(formB$map_raw, formB_s2$map_raw),
                           map_scale = list(formB$map_scale, formB_s2$map_scale))

  resA_s2 <- idmact_subj(raw = raw_scores_s2,
                      map_raw = formA_s2$map_raw,
                      map_scale = formA_s2$map_scale)


  resB_s2 <- idmact_subj(raw = raw_scores_s2,
                      map_raw = formB_s2$map_raw,
                      map_scale = formB_s2$map_scale)

  # 3.  Sum the adjusted scale scores for each subject area, divide this sum by
  #the number of subject areas, and round to the nearest integer in order to
  #obtain each observation/examinee's adjusted composite scale score.
  mean_scale_unadj_A <- round((unlist(resA$scale$unadj) + unlist(resA_s2$scale$unadj))/2)
  mean_scale_adj_A <- round((unlist(resA$scale$adj) + unlist(resA_s2$scale$adj))/2)
  mean_scale_unadj_B <- round((unlist(resB$scale$unadj) + unlist(resB_s2$scale$unadj))/2)
  mean_scale_adj_B <- round((unlist(resB$scale$adj) + unlist(resB_s2$scale$adj))/2)

  # 4.  Calculate the adjusted mean composite scale score across all observations
  # (m_adj).
  m_adj_A <- mean(mean_scale_adj_A)
  m_adj_B <- mean(mean_scale_adj_B)

  # 5.  Calculate the unadjusted mean composite scale score across all observations
  # (m_unadj).
  m_unadj_A <- mean(mean_scale_unadj_A)
  m_unadj_B <- mean(mean_scale_unadj_B)

  # 6.  Compute the difference between the adjusted and unadjusted mean composite
  # scale scores to obtain delta composite: deltac = m_adj - m_unadj
  deltacA <- m_adj_A - m_unadj_A
  deltacB <- m_adj_B - m_unadj_B

  # Compare mostly-hand computation to idmact_comp computation
  expect_equal(resA_comp$composite_results$deltac, deltacA)
  expect_equal(resB_comp$composite_results$deltac, deltacB)

})


test_that("idmact_comp's mcen_obs works as expected", {
  # Test all list inputs

  # Inputs
  raw = list(list(1, 2, 3, 4, 5), list(1, 1, 1, 1, 1))
  inc = list(1 , 1)
  map_raw = list(list(1, 2, 3, 4, 5))
  map_scale = list(list(20, 21, 22, 23, 24))

  comp_mean1 <- idmact_comp(raw = raw,
                           inc = inc,
                           map_raw = map_raw,
                           map_scale = map_scale)

  comp_mean2 <- idmact_comp(raw = raw,
                            inc = inc,
                            map_raw = map_raw,
                            map_scale = map_scale,
                            mcent_obs = function(x) round(mean(x)))



  expect_equal(comp_mean1$composite_results$deltac,
               comp_mean2$composite_results$deltac)
  })
