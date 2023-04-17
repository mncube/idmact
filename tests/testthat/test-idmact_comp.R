test_that("idmact_comp works", {
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

  expect_equal(comp_mean$beta_comp, 0.8)

  # # Test that it works with round2
  # comp_mean_r2 <- idmact_comp(raw = raw,
  #                          inc = inc,
  #                          map_raw = map_raw,
  #                          map_scale = map_scale,
  #                          rounder = "round2")
  #
  # expect_equal(comp_mean_r2$beta_comp, 0.8)




})
