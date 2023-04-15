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
