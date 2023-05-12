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
