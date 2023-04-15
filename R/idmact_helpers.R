#' idmact Helper Functions
#'
#' Increment raw score into an adjusted raw score as explained in step one of
#' Schiel (1998).
#'
#' @param df A data frame
#' @param raw A list of raw scores or the quoted data frame column name where
#' raw scores are stored.
#' @param inc Increment raw scores by inc to obtain adjusted scores
#'
#' @return A list
#' @export
#'
#' @references
#'
#' Schiel, J. Interpreting Differences Between Mean ACT Assessment Scores (1998)
#' <https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf>
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
