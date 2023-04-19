test_that("map_elongate works", {

  # Use map_elongate to prep map for processing
  new_map <- map_elongate(map_raw = list(1:5, 6:10),
                          map_scale = list(20, 21))

  # Test that lengths are as expected
  expect_equal(length(new_map$map_raw), length(new_map$map_scale))

  # Use map_elongate on concatenation format
  new_map2 <- map_elongate(map_raw = list(c(1,2,3,4,5), c(6,7,8,9,10)),
                           map_scale = list(20, 21))

  # Test that lengths are as expected
  expect_equal(length(new_map2$map_raw), length(new_map2$map_scale))
})

test_that("map_elongate_df works", {

  # Create a data frame
  df_map <- data.frame(
    raw = c("1-5", "6-10"),
    scale = c(10, 11)
  )

  # Elongate the data frame
  elongated_df <- map_elongate_df(df_map, "raw", "scale")

  # Check that the resulting map has two columns
  expect_equal(length(elongated_df), 2)

  # Check that the resulting map has 10 rows
  expect_equal(nrow(elongated_df), 10)
})
