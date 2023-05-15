test_that("idmact_subj works", {

  # Test list input
  raw = list(1, 2, 3, 4, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale)$deltas, 0.8)

  # Test data frame scores and list maps
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))


  expect_equal(idmact_subj(df = df, raw = "Score",
                           map_raw = list(11, 12, 13, 14, 15),
                           map_scale = list(21, 22, 23, 24, 25))$deltas,
               0.8)

  # Test scores and maps in one data frame
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4),
                   rawmap = rep(11:15, 4),
                   scalemap = rep(21:25, 4))

  expect_equal(idmact_subj(df = df, raw = "Score",
                            map_raw = "rawmap",
                            map_scale = "scalemap")$deltas, 0.8)

  # Test scores and maps in separate data frames
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4))
  expect_equal(idmact_subj(df = df, df_map = df_map, raw = "Score",
                           map_raw = "rawmap",
                           map_scale = "scalemap")$deltas, 0.8)

  # Test beta when measure of central tendency is median
  raw = list(1, 2, 3, 4, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale,
                           mcent = "median")$deltas, 1)

  # Test missing data
  raw = list(1, 2, 3, NA, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale,
                           mcent = function(x)median(x, na.rm = TRUE))$deltas, 1)


})


test_that("idmact_subj sanity check: small hand calculation", {

  # List computations
  raw = list(2,2,2)
  map_raw = list(1,2,3)
  map_scale = list(10, 20, 30)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale)$deltas, 10)

  # One data frame computation
  df <- data.frame(raw = c(2,2,2),
                   rawmap = c(1,2,3),
                   scalemap = c(10, 20, 30))

  expect_equal(idmact_subj(df = df,
                           raw = "raw",
                           map_raw = "rawmap",
                           map_scale = "scalemap")$deltas, 10)

  # Two data frame computation
  df <- data.frame(raw = c(2,2,2))
  df_map <- data.frame(rawmap = c(1,2,3),
                       scalemap = c(10, 20, 30))

  expect_equal(idmact_subj(df = df,
                           df_map = df_map,
                           raw = "raw",
                           map_raw = "rawmap",
                           map_scale = "scalemap")$deltas, 10)


})


test_that("idmact_subj sanity check: reverse maps give equal magnitude opposite
          delta for symmetric constant raw scores", {

            # Test 2s and 4s
            raw1 = list(2,2,2)
            map_raw1 = list(1,2, 3, 4, 5)
            map_scale1 = list(10, 20, 30, 40, 50)

            raw2 = list(4,4,4)
            map_raw2 = list(1,2, 3, 4, 5)
            map_scale2 = list(50, 40, 30, 20, 10)

            expect_equal(idmact_subj(raw = raw1,
                                     map_raw = map_raw1,
                                     map_scale = map_scale1)$deltas,
                         -idmact_subj(raw = raw2,
                                      map_raw = map_raw2,
                                      map_scale = map_scale2)$deltas)
})


test_that("README Single Subject Example", {
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

  expect_equal(resA$m_scale$adj - resA$m_scale$unadj,
               resA$deltas)

  expect_equal(resB$m_scale$adj - resB$m_scale$unadj,
               resB$deltas)
})


test_that("README Single Subject Example does not throw error when number of
          raw scores does not equal the map length", {
  # Create 100 raw scores
  set.seed(279)
  raw_scores <- as.list(sample(1:100, 233, replace = TRUE))

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

  expect_equal(round(resA$deltas, 3),
               round(0.0944206,3))

  expect_equal(round(resB$deltas, 3),
               round(0.1030043, 3))
})


test_that("Additional-Workflows Example with df and df_map does not throw error
when number of raw scores does not equal the map length", {
  # Create 100 raw scores
  set.seed(279)
  raw_scores <- as.list(sample(1:100, 233, replace = TRUE))

  # Map between raw scores and scale scores for each form

  ## Each form has scale scores ranging from 1 to 12
  map_scale <- c(1:12)

  # First make sure the information in map_raw_formA and map_raw_formB is the same
  # length as the information in map_scale
  map_raw_formA <- as.character(alist(1-5, 6-20, 21-25, 26-40, 41-45, 46-50, 51-55,
                                      56-75, 76-80, 81-85, 86-90, 91-100))

  map_raw_formB <- as.character(alist(1-10, 11-20, 21-30, 31-40, 41-50, 51-55, 56-65,
                                      66-75, 76-85, 86-90, 91-95, 96-100))

  # Then form the df_raw data frames
  df_raw_A <- data.frame(raw = map_raw_formA,
                         scale = map_scale)
  df_raw_A <- map_elongate_df(df_raw_A, "raw", "scale")

  df_raw_B <- data.frame(raw = map_raw_formB,
                         scale = map_scale)
  df_raw_B <- map_elongate_df(df_raw_B, "raw", "scale")

  # Now the df data frame only needs to hold the raw scores
  df <- data.frame(scores = unlist(raw_scores))

  resA <- idmact_subj(df = df,
                      df_map = df_raw_A,
                      raw = "scores",
                      map_raw = "raw",
                      map_scale = "scale")

  resB <- idmact_subj(df = df,
                      df_map = df_raw_B,
                      raw = "scores",
                      map_raw = "raw",
                      map_scale = "scale")

  expect_equal(round(resA$deltas, 3),
                         round(0.0944206,3))
  expect_equal(round(resB$deltas, 3),
                         round(0.1030043, 3))
  })


