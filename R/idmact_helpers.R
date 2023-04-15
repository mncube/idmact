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

map_scores <- function(df = NULL, conv, map_raw, map_scale) {

  # Create mapper function to help map raw scores to scale scores
  # mapper <- function(x, map_raw, map_scale) {
  #   sapply(x, function(score) {
  #     idx <- which(map_raw == score)
  #     if (length(idx) > 0) {
  #       return(map_scale[idx])
  #     } else {
  #       return(NA)
  #     }
  #   })
  # }

  mapper <- function(x, map_raw, map_scale) {
    if (is.list(map_raw)){
      max_raw <- max(unlist(map_raw))
    } else {
      max_raw <- max(df[[map_raw]])
    }

    if (is.list(map_scale)){
      max_scale <- max(unlist(map_scale))
    } else {
      max_scale <- max(df[[map_scale]])
    }

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

  # Get map_raw_values based on input type
  if (is.list(map_raw)){
    map_raw_values <- map_raw
  } else {
    map_raw_values <- as.list(unique(df[[map_raw]]))
  }

  # Get map_scale_values based on input type
  if (is.list(map_scale)){
    map_scale_values <- map_scale
  } else {
    map_scale_values <- as.list(unique(df[[map_scale]]))
  }

  # Map scores based on input type
  if (is.null(df)) {
    out <- mapper(conv, map_raw_values, map_scale_values)
  } else {
    df$out <- mapper(df[[conv]], map_raw_values, map_scale_values)
  }

  #Return mapped scores
  return(if (is.null(df)) out else df$out)
}
