#' idmact Helper Functions
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
