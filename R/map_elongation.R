#' Elongate Mapping of Raw Scores to Scale Scores
#'
#' This function elongates the mapping of raw scores to scale scores. Each element
#' in 'map_raw' and 'map_scale' is repeated according to the length of the
#' corresponding 'map_raw' element, resulting in equal-length lists of map_raw'
#' and 'map_scale' scores.
#'
#' @param map_raw A list where each element is a number or a numeric vector
#' representing a set of raw scores. Each vector in 'map_raw' should have a
#' corresponding element in 'map_scale'.
#' @param map_scale A list where each element represents a scale score
#' corresponding to the raw scores in 'map_raw'. Each element in 'map_scale' is
#' repeated times the length of the corresponding element in 'map_raw'.
#'
#' @return A named list with two components: 'map_raw' and 'map_scale'. Each
#' component is a list representing the elongated mapping of raw scores to scale
#' scores.
#' @export
#'
#' @examples
#' # Elongate the mapping of raw scores (1:5 and 6:10) to scale scores (20 and 21)
#' map_elongate(map_raw = list(1:5, 6:10),
#'              map_scale = list(20, 21))
map_elongate <- function(map_raw, map_scale) {
  # Initialize empty lists for elongated_map_raw and elongated_map_scale
  elongated_map_raw <- list()
  elongated_map_scale <- list()

  # Check if the lengths of map_raw and map_scale are equal
  if (length(map_raw) != length(map_scale)) {
    rlang::abort("The lengths of map_raw and map_scale should be equal.")
  }

  # Iterate through each element in map_raw and map_scale
  for (i in seq_along(map_raw)) {
    # Check if the current map_raw element is a numeric vector
    if (is.numeric(map_raw[[i]])) {
      # Calculate the length of the current map_raw element
      len_map_raw <- length(map_raw[[i]])

      # Append the current map_raw element len_map_raw times to elongated_map_raw
      elongated_map_raw <- c(elongated_map_raw, rep(map_raw[[i]], 1))

      # Append the current map_scale element len_map_raw times to elongated_map_scale
      elongated_map_scale <- c(elongated_map_scale, rep(map_scale[[i]], len_map_raw))
    } else {
      rlang::abort("Each element of map_raw should be a numeric vector.")
    }
  }

  # Return the elongated map
  return(list(map_raw = elongated_map_raw, map_scale = elongated_map_scale))
}

#' Elongate Data Frame Mapping of Raw Scores to Scale Scores
#'
#' This function takes a data frame that maps raw scores to scale scores, and
#' elongates it. Each raw score range in the 'map_raw' column is split into
#' individual scores, and each corresponding scale score in the 'map_scale'
#' column is repeated for each individual raw score. The function returns
#' a new data frame with each raw score paired with its corresponding scale score.
#'
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores. The 'map_raw' column should contain ranges of raw scores as strings
#' (in the form, "1-5"), and the 'map_scale' column should contain the
#' corresponding scale scores.
#' @param map_raw A string representing the column name in 'df_map' that contains
#' the raw score ranges.
#' @param map_scale A string representing the column name in 'df_map' that contains
#' the scale scores.
#'
#' @return An elongated data frame where each row represents a raw score and its
#' corresponding scale score.
#' @export
#'
#' @examples
#' # Create a data frame mapping raw score ranges to scale scores
#' df_map <- data.frame(
#'  raw = c("1-5", "6-10"),
#'  scale = c(10, 11)
#' )
#' # Elongate the data frame
#' elongated_df <- map_elongate_df(df_map, "raw", "scale")
#' print(elongated_df)
map_elongate_df <- function(df_map, map_raw, map_scale) {
  # Ensure that the raw_col and scale_col exist in the data frame
  if (!(map_raw %in% colnames(df_map)) || !(map_scale %in% colnames(df_map))) {
    rlang::abort("Both map_raw and map_scale must be valid column names in df_map.")
  }

  # Initialize empty data frame for the elongated map
  elongated_df <- data.frame()

  # Iterate through each row in the input data frame
  for (i in seq_len(nrow(df_map))) {
    # Extract the raw and scale values from the current row
    raw_value <- df_map[[map_raw]][i]
    scale_value <- df_map[[map_scale]][i]

    # Split raw_value into a list of numeric vectors
    raw_list <- strsplit(raw_value, '-')
    raw_list <- lapply(raw_list, function(x) as.numeric(x))

    # Create a list of raw ranges
    raw_ranges <- lapply(raw_list, function(x) seq(from = x[1], to = x[2]))

    # Use the map_elongate function to elongate the current row's map
    elongated_map <- map_elongate(raw_ranges, list(scale_value))

    # Convert the elongated map to a data frame and append it to elongated_df
    elongated_row <- data.frame(map_raw = c(unlist(elongated_map$map_raw)), map_scale = c(unlist(elongated_map$map_scale)))
    elongated_df <- rbind(elongated_df, elongated_row)
  }

  #Get original column names
  names(elongated_df) <- c(map_raw, map_scale)

  # Return the elongated data frame
  return(elongated_df)
}
