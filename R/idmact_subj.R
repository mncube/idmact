#' Interpreting Differences in Mean ACT Scores at the Subject Level
#'
#' Use the idmact_subj() function to interpret differences in subject level scores.
#' The algorithm is implemented as: 1) Increment raw scores for each student
#' in order to obtain adjusted raw scores; 2) Map adjusted raw scores to adjusted
#' scale scores using the form's raw score to scale score map (note: perfect raw
#' scores are always converted to the maximum allowable scale score despite the
#' adjustment in step one); 3) Calculate the adjusted mean scale score across all
#' observations (m_adj); 4) Calculate the unadjusted mean scale score across all
#' observations (m_unadj); 5) Compute the difference between the adjusted and
#' unadjusted mean scale scores to obtain delta subject: deltas = m_adj - m_unadj
#'
#' The default idmact_subj() parameter values for inc and mcent_subj provide an
#' implementation of the algorithm aligned with the main method presented in Schiel
#' (1998) <https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf>;
#' however, these parameters can also take arbitrary anonymous functions for users
#' wishing to use modified implementations of the algorithm.
#'
#' @param df An optional data frame containing a variable for raw scores
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores
#' @param raw A list of raw scores, or a quoted column name from the data frame
#' where raw scores are stored.
#' @param inc A value used to increment raw scores in order to calculate adjusted
#' scores, or an anonymous function
#' @param map_raw A list containing the domain of raw scores for the raw-to-scale
#' score mapping, or a quoted column name from either df or df_map that represents
#' this domain
#' @param map_scale A list containing the range of scale scores for the
#' raw-to-scale score mapping, or a quoted column name from either df or df_map
#' that represents this range
#' @param mcent_subj An anonymous function that defines the measure of central
#' tendency to be used
#' @param na.rm.max Pass the na.rm argument to the map_scores' max function for
#' computing the maximum raw and scale values in the mapping, taking into account
#' the handling of missing values
#'
#' @return A nested list containing deltas, a summary of scale scores as a list
#' (adjusted and unadjusted), a list of adjusted and unadjusted scale scores,
#' and a list of adjusted and unadjusted raw scores
#' @export
#'
#' @examples
#' raw = list(1, 2, 3, 4, 5)
#' map_raw = list(1, 2, 3, 4, 5)
#' map_scale = list(20, 21, 22, 23, 24)
#' idmact_subj(raw = raw,
#'             map_raw = map_raw,
#'             map_scale = map_scale)
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
