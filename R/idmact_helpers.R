#' Convert raw scores to adjusted raw scores
#'
#' @param df An optional data frame containing a variable for raw scores
#' @param raw A list of raw scores, or a quoted column name from the data frame
#' where raw scores are stored.
#' @param inc A value used to increment raw scores in order to calculate adjusted
#' scores, or an anonymous function
#'
#' @return A list of adjusted raw scores
#' @export
#'
#' @examples
#' # Create raw data
#' df <- data.frame(Id = list(1:20),
#' RawScore = rep(11:15, 4))
#' # Increment scores by 2
#' df$AdjScore <- adjust_raw_scores(df, "RawScore", inc = 2)
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

#' Convert Raw Scores to Scale Scores
#'
#' @param df An optional data frame containing a variable for raw scores
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores
#' @param conv A list of raw scores, or a quoted column name from the data frame
#' where raw scores are stored.
#' @param map_raw A list containing the domain of raw scores for the raw-to-scale
#' score mapping, or a quoted column name from either df or df_map that represents
#' this domain
#' @param map_scale A list containing the range of scale scores for the
#' raw-to-scale score mapping, or a quoted column name from either df or df_map
#' that represents this range
#' @param na.rm.max Pass the na.rm argument to the max function for computing the
#' maximum raw and scale values in the mapping, taking into account the handling
#' of missing values
#'
#' @return A list if 'conv' is a list, or a data frame if 'conv' is a column name
#' @export
#'
#' @examples
#' (bij_scale <- map_scores(conv = list(1, 2, 3, 4, 5),
#'                         map_raw = list(1, 2, 3, 4, 5),
#'                         map_scale = list(20, 21, 22, 23, 24)))
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
          out <- as.list(unique(df[[map_obj]]))
        } else {
          out <- as.list(unique(df_map[[map_obj]]))
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
