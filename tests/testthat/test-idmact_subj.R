test_that("idmact_subj works", {

  # Test list input
  raw = list(1, 2, 3, 4, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale)$beta, 0.8)

  # Test data frame scores and list maps
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))


  expect_equal(idmact_subj(df = df, raw = "Score",
                           map_raw = list(11, 12, 13, 14, 15),
                           map_scale = list(21, 22, 23, 24, 25))$beta,
               0.8)

  # Test scores and maps in one data frame
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4),
                   rawmap = rep(11:15, 4),
                   scalemap = rep(21:25, 4))

  expect_equal(idmact_subj(df = df, raw = "Score",
                            map_raw = "rawmap",
                            map_scale = "scalemap")$betas, 0.8)

  # Test scores and maps in separate data frames
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4))
  expect_equal(idmact_subj(df = df, df_map = df_map, raw = "Score",
                           map_raw = "rawmap",
                           map_scale = "scalemap")$betas, 0.8)

  # Test beta when measure of central tendency is median
  raw = list(1, 2, 3, 4, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale,
                           mcent = "median")$beta, 1)

  # Test missing data
  raw = list(1, 2, 3, NA, 5)
  map_raw = list(1, 2, 3, 4, 5)
  map_scale = list(20, 21, 22, 23, 24)

  expect_equal(idmact_subj(raw = raw,
                           map_raw = map_raw,
                           map_scale = map_scale,
                           mcent = function(x)median(x, na.rm = TRUE))$beta, 1)


})
