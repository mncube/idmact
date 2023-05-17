#' Interpret Differences in Mean ACT Scores at the Subject Level
#'
#' This function implements an algorithm to analyze differences in subject level
#' scores.It first adjusts raw scores for each student, maps these adjusted raw
#' scores to scale scores using the provided raw-to-scale score mapping (note:
#' perfect raw scores are always converted to the maximum allowable scale score
#' despite the adjustment in the previous step), then calculates the mean adjusted
#' and unadjusted scale scores across all observations. The difference (delta)
#' between these two mean scores is then computed. The adjustment and calculation
#' method follows Schiel's (1998) methodology, but can also be customized with
#' user-defined parameters and functions.
#'
#' @param df An optional data frame containing the variable for raw scores.
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores.
#' @param raw A list of raw scores, or a string representing the column name in
#' 'df' containing raw scores.
#' @param inc A numeric value used to increment raw scores to calculate adjusted
#' scores, or a function to perform this incrementing operation.
#' @param map_raw A list containing the domain of raw scores for the raw-to-scale
#' score mapping, or a string representing the column name in 'df' or 'df_map'
#' that contains this domain.
#' @param map_scale A list containing the range of scale scores for the
#' raw-to-scale score mapping, or a string representing the column name in 'df'
#' or 'df_map' that contains this range.
#' @param mcent_subj A function that defines the measure of central tendency to be used.
#' The default is 'mean'.
#' @param na.rm.max A logical value. If TRUE, missing values are removed before calculating the
#' maximum raw and scale values in the mapping. If FALSE, missing values are not
#' removed.
#'
#' @return A list containing the following elements:
#' * 'deltas': the difference between the mean adjusted and unadjusted scale
#' scores,
#' * 'm_scale': a list with the mean adjusted ('adj') and unadjusted ('unadj')
#' scale scores,
#' * 'scale': a list with the individual adjusted ('adj') and unadjusted ('unadj')
#' scale scores,
#' * 'raw': a list with the individual adjusted ('adj') and unadjusted ('unadj')
#' raw scores.
#' @export
#'
#' @references
#' Schiel, J. C. (1998). Interpreting differences in ACT composite scores
#' (ACT Research Report Series 98-1). ACT, Inc. URL:
#' https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf
#'
#' @examples
#' raw_scores = list(1, 2, 3, 4, 5)
#' map_raw_scores = list(1, 2, 3, 4, 5)
#' map_scale_scores = list(20, 21, 22, 23, 24)
#' idmact_subj(raw = raw_scores,
#'             map_raw = map_raw_scores,
#'             map_scale = map_scale_scores)
idmact_subj <- function(df = NULL, df_map = NULL, raw,
                        inc = 1, map_raw, map_scale,
                        mcent_subj = function(x) mean(x, na.rm = TRUE),
                        na.rm.max = TRUE){

  # Adjusted raw scores
  adj <- adjust_raw_scores(df, raw , inc)

  # Scale scores
  unadj_scale <- map_scores(df, df_map, conv = raw, map_raw, map_scale,
                            na.rm.max = na.rm.max)
  if (is.null(df)){
    adj_scale <- map_scores(df, df_map, conv = adj, map_raw, map_scale,
                            na.rm.max = na.rm.max)
  } else{
    raw <- df[[raw]]
    df$adj <- adj
    adj_scale <- map_scores(df, df_map, conv = "adj", map_raw, map_scale,
                            na.rm.max = na.rm.max)
  }


  # Summarize scale scores
  m_unadj_scale <- do.call(mcent_subj, list(unlist(unadj_scale)))
  m_adj_scale <- do.call(mcent_subj, list(unlist(adj_scale)))

  # Delta subject
  deltas <- m_adj_scale - m_unadj_scale

  # Output object
  out <- list("deltas" = deltas,
              "m_scale" = list("adj" = m_adj_scale,
                               "unadj" = m_unadj_scale),
              "scale" = list("adj" = adj_scale,
                             "unadj" = unadj_scale),
              "raw" = list("adj" = adj,
                           "unadj" = raw))

  # Return output
  return(out)

}
