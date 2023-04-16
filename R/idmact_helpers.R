#' Convert raw scores to adjusted raw scores
#'
#' @param df A data frame
#' @param raw A list of raw scores or the quoted data frame column name where
#' raw scores are stored.
#' @param inc Increment raw scores by inc to obtain adjusted scores
#'
#' @return A list
#' @export
#'
#' @examples
#' # Create raw data
#' df <- data.frame(Id = list(1:20),
#' RawScore = rep(11:15, 4))
#' # Increment scores by 2
#' df$AdjScore <- adjust_raw_scores(df, "RawScore", inc = 2)
adjust_raw_scores <- function(df = NULL, raw, inc = 1){
  if(is.null(df)){
    raw <- lapply(raw, function(x)sum(x,inc))
  } else {
    df$raw <- df[[raw]] + inc
  }

}

map_scores <- function(df = NULL, df_map = NULL, conv, map_raw, map_scale) {

  # Helper function: Get information based on provided parametr values
  map_out <- function(map_obj, func = "max"){

    if (func == "max"){
      if (is.list(map_obj)){
        out <- max(unlist(map_obj))
      } else {
        if (is.null(df_map)){
          out <- max(df[[map_obj]])
        } else {
          out <- max(df_map[[map_obj]])
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
        if (score > max_raw) {
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
