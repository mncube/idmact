#' Elongate Map
#'
#' @param map_raw A list containing the domain of raw scores for the raw-to-scale
#' score mapping
#' @param map_scale A list containing the domain of raw scores for the raw-to-scale
#' score mapping
#'
#' @return A nested list consisting of a list of raw scores and a list of scale
#' scores.
#' @export
#'
#' @examples
#' map_elongate(map_raw = list(1:5, 6:10),
#' map_scale = list(20, 21))
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


#' Elongate df_map
#'
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores
#' @param map_raw A quoted column name from df_map containing the domain of raw
#' scores for the raw-to-scale score mapping
#' @param map_scale A quoted column name from df_map containing the range of
#' scale scores for the raw-to-scale score mapping
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df_map <- data.frame(
#'  raw = c("1-5", "6-10"),
#'  scale = c(10, 11))
#' # Elongate the data frame
#' elongated_df <- map_elongate_df(df_map, "raw", "scale")
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
