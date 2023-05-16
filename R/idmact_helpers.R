#' Adjust Raw Scores
#'
#' This function adjusts raw scores either by a fixed increment or according to
#' a specified function. It can adjust scores stored in a list or within a specific
#' column of a data frame.
#'
#' @param df An optional data frame containing a column for raw scores. If 'df'
#' is provided, 'raw' should be the name of the column in 'df' that contains the
#' raw scores. If 'df' is NULL, 'raw' should be a list of raw scores. Default is
#' NULL.
#' @param raw Either a string representing the name of the column in 'df' that
#' contains the raw scores, or a list of raw scores if 'df' is NULL.
#' @param inc Either a numeric value that will be added to each raw score to
#' calculate the adjusted raw score, or a function that will be applied to each raw
#' score to calculate the adjusted score. The function should take a single numeric
#' argument and return a single numeric value.
#'
#' @return If 'df' is NULL, the function returns a list containing the the
#' adjusted raw scores.  If 'df' is provided, the function returns a vector
#' containing the adjusted raw scores.
#' @export
#'
#' @examples
#' # Create raw data
#' df <- data.frame(Id = 1:20,
#' RawScore = rep(11:15, 4))
#' # Increment scores by 2
#' df$AdjScore <- adjust_raw_scores(df, "RawScore", inc = 2)
#'
#' # Adjust scores using a function
#' adjust_raw_scores(df = NULL, raw = list(11:15), inc = function(x) {x^2})
adjust_raw_scores <- function(df = NULL, raw, inc = 1){
  if (is.function(inc)) {
    if (is.null(df)) {
      raw <- lapply(raw, inc)
    } else {
      df$raw <- sapply(df[[raw]], inc)
    }
  } else {
    if (is.null(df)) {
      raw <- lapply(raw, function(x) sum(x, inc))
    } else {
      df$raw <- df[[raw]] + inc
    }
  }
}

#' Map Scores
#'
#' This function converts raw scores into scale scores using a provided mapping.
#' The mapping can be provided directly as lists through 'map_raw' and 'map_scale',
#' or indirectly via columns within either 'df' or 'df_map' data frames.
#'
#' @param df An optional data frame containing a column for raw scores. If 'df'
#' is provided and 'conv' is a character, 'conv' should represent the name of the
#' column in 'df' containing the raw scores.
#' @param df_map An optional data frame that maps raw scores to their corresponding
#' scale scores. If 'df_map' is provided, map_raw' and 'map_scale' should represent
#' the names of the columns in 'df_map' that describe how raw scores map to scale
#' scores.
#' @param conv Either a list of raw scores to be converted or a string representing
#' the name of a column in 'df' containing the raw scores.
#' @param map_raw Either a list containing the domain of raw scores for the
#' raw-to-scale score mapping, or a string representing the name of a column in
#' either 'df' or 'df_map' containing these values.
#' @param map_scale Either a list containing the range of scale scores for the
#' raw-to-scale score mapping, or a string representing the name of a column in
#' either 'df' or 'df_map' containing these values.
#' @param na.rm.max Logical. Should missing values be removed when computing the
#' maximum raw and scale values in the mapping? Default is TRUE.
#'
#' @return A list of scale scores.
#' @export
#'
#' @examples
#' # Convert raw scores to scale scores using lists
#' map_scores(conv = list(1, 2, 3, 4, 5),
#'            map_raw = list(1, 2, 3, 4, 5),
#'            map_scale = list(20, 21, 22, 23, 24))
#'
#' # Convert raw scores to scale scores using a data frame
#' df <- data.frame(Id = 1:5, RawScore = 1:5)
#' df_map <- data.frame(Raw = 1:5, Scale = 20:24)
#' df$ScaleScore <- map_scores(df, df_map, conv = "RawScore", map_raw = "Raw", map_scale = "Scale")
map_scores <- function(df = NULL, df_map = NULL, conv, map_raw, map_scale,
                       na.rm.max = TRUE) {

  # Check that map_raw and map_scale are the same length
  if (length(map_raw) != length(map_scale)) {
    rlang::abort("map_raw and map_scale must have the same length.")
  }

  # Check that if conv is a quoted data frame column, it is found in df and not in df_map
  if (is.character(conv) && (is.null(df) || !conv %in% names(df) || (conv %in% names(df_map)))) {
    rlang::abort("If conv is a quoted data frame column, it must be found in df and not in df_map.")
  }

  # Check that if map_raw and map_scale are both quoted data frame columns, they are found in the same data frame (either df or df_map)
  if (is.character(map_raw) && is.character(map_scale)) {
    if (!((map_raw %in% names(df) && map_scale %in% names(df)) || (map_raw %in% names(df_map) && map_scale %in% names(df_map)))) {
      rlang::abort("If map_raw and map_scale are both quoted data frame columns, they must be found in the same data frame (either df or df_map).")
    }
  }

  # Check that if map_raw and/or map_scale is a quoted data frame column, it is only found in one data frame (either df or df_map)
  for (arg in list(map_raw, map_scale)) {
    if (is.character(arg) && arg %in% names(df) && arg %in% names(df_map)) {
      rlang::abort("If map_raw and/or map_scale is a quoted data frame column, it must be found in only one data frame (either df or df_map).")
    }
  }

  # Helper function: Get information based on provided parametr values
  map_out <- function(map_obj, func = "max"){

    if (func == "max"){
      if (is.list(map_obj)){
        out <- max(unlist(map_obj), na.rm = na.rm.max)
      } else {
        if (is.null(df_map)){
          out <- max(df[[map_obj]], na.rm = na.rm.max)
        } else {
          out <- max(df_map[[map_obj]], na.rm = na.rm.max)
        }
      }
    }

    if (func == "get"){
      if (is.list(map_obj)){
        out <- map_obj
      } else {
        if (is.null(df_map)){
          #out <- as.list(unique(df[[map_obj]]))
          out <- as.list(df[[map_obj]])
        } else {
          #out <- as.list(unique(df_map[[map_obj]]))
          out <- as.list(df_map[[map_obj]])
        }
      }
    }
    return(out)
  }

  #Helper function: Map scores
  mapper <- function(x, map_raw, map_scale) {
    #Get max values
    max_raw <- map_out(map_raw, func = "max")
    max_scale <- map_out(map_scale, func = "max")

    #Map scores
      sapply(x, function(score) {
        if (is.na(score)){
          return(NA)
        } else if (score > max_raw) {
          return(max_scale)
        } else {
          idx <- which(map_raw == score)
          if (length(idx) > 0) {
            if(length(idx) > 1){
              idx <- idx[[1]]
            }
            return(map_scale[idx])
          } else {
            return(NA)
          }
        }
      })
  }

  # Get map between raw and scale values
  map_raw_values <- map_out(map_raw, func = "get")
  map_scale_values <- map_out(map_scale, func = "get")


  # Map scores based on input type
  if (is.null(df)) {
    out <- mapper(conv, map_raw_values, map_scale_values)
  } else {
    df$out <- mapper(df[[conv]], map_raw_values, map_scale_values)
  }

  #Return mapped scores
  return(if (is.null(df)) out else df$out)
}
