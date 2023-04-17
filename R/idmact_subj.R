#' Interpreting Subject Level Differences in Mean ACT Scores
#'
#' @param df A data frame containing raw scores (optional)
#' @param df_map A data frame containing the map between raw and scale scores
#' @param raw A list of raw scores or the quoted data frame column name where
#' raw scores are stored.
#' @param inc Increment raw scores by inc to obtain adjusted scores
#' @param map_raw A list containing the domain of raw scores for the raw score to
#' scale score map, or the corresponding column name (quoted) in df or df_map
#' @param map_scale A list containing the image of scale scores for the raw score
#' to scale score map, or the corresponding column name (quoted) in df or df_map
#' @param mcent A measure of central tendency
#' @param na.rm.mcent Pass na.rm argument to function selected with mcent in order
#' to summarize scale score
#'
#' @return A list containing betas, scale score summary (adjusted and unadjusted),
#' scale scores (adjusted and unadjusted), and raw scores (adjusted nd unadjusted)
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
                        inc = 1, map_raw, map_scale, mcent = "mean",
                        na.rm.mcent = TRUE){

  # Adjusted raw scores
  adj <- adjust_raw_scores(df, raw , inc)

  # Scale scores
  unadj_scale <- map_scores(df, df_map, conv = raw, map_raw, map_scale)
  if (is.null(df)){
    adj_scale <- map_scores(df, df_map, conv = adj, map_raw, map_scale)
  } else{
    raw <- df[[raw]]
    df$adj <- adj
    adj_scale <- map_scores(df, df_map, conv = "adj", map_raw, map_scale)
  }


  # Mean scale scores
  # m_unadj_scale <- mean(unlist(unadj_scale))
  # m_adj_scale <- mean(unlist(adj_scale))
  m_unadj_scale <- do.call(mcent, c(list(unlist(unadj_scale)), na.rm = na.rm.mcent))
  m_adj_scale <- do.call(mcent, c(list(unlist(adj_scale)), na.rm = na.rm.mcent))

  # Beta subject
  betas <- m_adj_scale - m_unadj_scale

  # Output object
  out <- list("betas" = betas,
              "m_scale" = list("adj" = m_adj_scale,
                               "unadj" = m_unadj_scale),
              "scale" = list("adj" = adj_scale,
                             "unadj" = unadj_scale),
              "raw" = list("adj" = adj,
                           "unadj" = raw))

  # Return output
  return(out)

}
