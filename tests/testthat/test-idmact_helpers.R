test_that("adjust_raw_scores works", {

  # Test with df = NULL
  raw_scores <- list(1, 2, 3) #Set raw scores
  adj_scores <- adjust_raw_scores(raw = raw_scores) #Increment raw scores
  # Sum over incremented raw scores
  expect_equal(sum(unlist(adj_scores)), 9)

  # Test with df

  # Create test data
  df <- data.frame(Id = list(1:20),
                   Score = rep(11:15, 4))
  # Increment scores by 1
  df$Score1 <- adjust_raw_scores(df, "Score")

  # Test that a total of 20 was added to Score (once for each row)
  expect_equal(sum(df$Score1), sum(df$Score) + 20)

  # Increment scores by 3
  df$Score3 <- adjust_raw_scores(df, "Score", inc = 3)

  # Test that a total of 60 was added to Score (three for each row)
  expect_equal(sum(df$Score3), sum(df$Score) + 60)
})

test_that("adjust_raw_scores works with anonymous functions", {
  # Test with df = NULL
  raw_scores <- list(1, 2, 3) #Set raw scores
  adj_scores <- adjust_raw_scores(raw = raw_scores, inc = function(x)sum(x + 1)) #Increment raw scores
  # Sum over incremented raw scores
  expect_equal(sum(unlist(adj_scores)), 9)

  # Create test data
  df <- data.frame(Id = list(1:20),
                   Score = rep(11:15, 4))
  # Increment scores by 1
  df$Score1 <- adjust_raw_scores(df, "Score", function(x)sum(x + 1))

  # Test that a total of 20 was added to Score (once for each row)
  expect_equal(sum(df$Score1), sum(df$Score) + 20)

})


test_that("map_scores works", {

  #Test with df = NULL

  bij_scale <- map_scores(conv = list(1, 2, 3, 4, 5),
                             map_raw = list(1, 2, 3, 4, 5),
                             map_scale = list(20, 21, 22, 23, 24))


  expect_equal(sum(do.call(c, bij_scale)), 110)
  expect_equal(length(bij_scale), 5)

  overmax_scale <- map_scores(conv = list(1, 2, 3, 4, 6),
                          map_raw = list(1, 2, 3, 4, 5),
                          map_scale = list(20, 21, 22, 23, 24))


  expect_equal(sum(do.call(c, overmax_scale)), 110)
  expect_equal(length(overmax_scale), 5)
  expect_equal(max(unlist(overmax_scale)), 24)

  # Test with df

  # Create test data
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))


  # Test on list maps
  df$adjScale <- map_scores(df = df, conv = "Score",
                            map_raw = list(11, 12, 13, 14, 15),
                            map_scale = list(21, 22, 23, 24, 25))

  expect_equal(sum(unlist(df$adjScale)), sum(unlist(df$Score)) + 200)

  # Test on df column maps
  df$rawmap <- rep(11:15, 4)
  df$scalemap <- rep(21:25, 4)
  df$adjScale <- map_scores(df = df, conv = "Score",
                            map_raw = "rawmap",
                            map_scale = "scalemap")

  expect_equal(sum(unlist(df$adjScale)), sum(unlist(df$Score)) + 200)

  # Test on df-maps
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4))
  df$adjScale <- map_scores(df = df, df_map = df_map, conv = "Score",
                            map_raw = "rawmap",
                            map_scale = "scalemap")

  expect_equal(sum(unlist(df$adjScale)), sum(unlist(df$Score)) + 200)

})

test_that("map_scores parameter checks work", {
  # Mismatched map size
  expect_error(map_scores(conv = list(1, 2, 3, 4, 5),
                          map_raw = list(1, 2, 3, 4),
                          map_scale = list(20, 21, 22, 23, 24)),
               "map_raw and map_scale must have the same length.")

  # conv (if a data frame column) is found in the correct column
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4),
                       Score = rep(11:15, 4))
  expect_error(map_scores(df = df, df_map = df_map, conv = "Score",
                          map_raw = "rawmap",
                          map_scale = "scalemap"),
               "If conv is a quoted data frame column, it must be found in df and not in df_map.")

  # map_raw and map_scale (if column names) are found in the same data frame
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4),
                   scalemap = rep(21:25, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4))

  expect_error(map_scores(df = df, df_map = df_map, conv = "Score",
                          map_raw = "rawmap",
                          map_scale = "scalemap"),
               "If map_raw and map_scale are both quoted data frame columns, they must be found in the same data frame (either df or df_map).",
               fixed = TRUE)

  # Check that map_raw/map_scale is only in one data frame
  df <- data.frame(Id = c(1:20),
                   Score = rep(11:15, 4),
                   scalemap = rep(21:25, 4))
  df_map <- data.frame(rawmap = rep(11:15, 4),
                       scalemap = rep(21:25, 4))
  expect_error(map_scores(df = df, df_map = df_map, conv = "Score",
             map_raw = "rawmap",
             map_scale = "scalemap"),
             "If map_raw and/or map_scale is a quoted data frame column, it must be found in only one data frame (either df or df_map).",
             fixed = TRUE)

})
