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
