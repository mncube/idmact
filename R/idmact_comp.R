#' Interpreting Differences in Mean ACT Scores at the Composite Level
#'
#' Use the idmact_comp() function to interpret differences in composite level
#' scores.  The algorithm is implemented as: 1) Increment the raw score for one
#' or more subjects for each student to obtain adjusted raw scores; 2) Map
#' adjusted raw scores to adjusted scale scores using the form's raw score to
#' scale score map (note: perfect raw scores are always converted to the maximum
#' allowable scale score despite the adjustment in step one); 3) Sum the adjusted
#' scale scores for each subject area, divide this sum by the number of subject
#' areas, and round to the nearest integer in order to obtain each
#' observation/examinee's adjusted composite scale score; 4) Calculate the
#' adjusted mean composite scale score across all observations (m_adj);
#' 5) Calculate the unadjusted mean composite scale score across all observations
#' (m_unadj); 6) Compute the difference between the adjusted and unadjusted mean
#' composite scale scores to obtain delta composite: deltac = m_adj - m_unadj
#'
#' The default idmact_comp() parameter values for inc, mcent_subj, mcent_obs, and
#' mcent_comp provide an implementation of the algorithm aligned with the main
#' method presented in Schiel (1998)
#' <https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf>;
#' however, these parameters can also take arbitrary anonymous functions for users
#' wishing to use modified implementations of the algorithm.
#'
#' @param df An optional data frame containing a variable for raw scores
#' @param df_map A data frame that maps raw scores to their corresponding scale
#' scores
#' @param raw A list containing either a list of raw scores for each subject, or
#' quoted column names from the data frame where raw scores for each subject are
#' stored
#' @param inc A value used to increment raw scores in order to calculate adjusted
#' scores, or an anonymous function
#' @param map_raw A nested list where each sublist contains the domain of raw
#' scores for a subject's raw-to-scale score mapping, or quoted column names from
#' either df or df_map representing the subject area domains
#' @param map_scale A nested list where each sublist contains the range of scale
#' scores for a subject's raw-to-scale score mapping, or quoted column names from
#' either df or df_map representing the subject area ranges
#' @param mcent_subj An anonymous function specifying the measure of central
#' tendency used to summarize scale scores at the subject level
#' @param mcent_obs An anonymous function used to summarize scale scores at the
#' observation level (i.e., summary within each observation/examinee across
#' subjects)
#' @param mcent_comp An anonymous function defining the measure of central
#' tendency used to summarize composite level scale scores (i.e., summarizing
#' exam level scale scores across observations)
#'
#' @return A nested list containing both composite and subject level results.
#' Composite results include deltac, a list of summarized composite scale scores
#' (adjusted and unadjusted), and composite level scale scores (adjusted and
#' unadjusted). Subject level results consist of the outcomes obtained from
#' idmact_subj for each subject.
#'
#' @export
#'
#' @examples
#' raw = list(list(1, 2, 3, 4, 5), list(1, 1, 1, 1, 1))
#' inc = list(1 , 1)
#' map_raw = list(list(1, 2, 3, 4, 5))
#' map_scale = list(list(20, 21, 22, 23, 24))
#' comp_mean <- idmact_comp(raw = raw,
#'                          inc = inc,
#'                          map_raw = map_raw,
#'                          map_scale = map_scale)
idmact_comp <- function(df = NULL, df_map = NULL, raw,
                        inc, map_raw, map_scale,
                        mcent_subj = function(x) mean(x, na.rm = TRUE),
                        mcent_obs = function(x) round(sum(x) / length(x)),
                        mcent_comp = function(x) mean(x, na.rm = TRUE)){

  # Find the maximum length among raw, inc, map_raw, and map_scale
  max_len <- max(length(raw), length(inc), length(map_raw), length(map_scale))

  # Check and recycle parameters if necessary
  recycle_param <- function(param, max_len) {
    if (length(param) < max_len) {
      param <- rep(param, length.out = max_len)
    }
    return(param)
  }

  raw <- recycle_param(raw, max_len)
  inc <- recycle_param(inc, max_len)
  map_raw <- recycle_param(map_raw, max_len)
  map_scale <- recycle_param(map_scale, max_len)

  # Initialize lists to store results for each subject
  subjr <- vector(mode = "list", length = max_len)
  adj_scale_scores <- vector(mode = "list", length = max_len)
  unadj_scale_scores <- vector(mode = "list", length = max_len)

  # Iterate over subjects
  for (subj in seq_len(max_len)) {

    # Apply idmact_subj for each subject
    subj_results <- idmact_subj(df, df_map, raw = raw[[subj]],
                                inc = inc[[subj]], map_raw = map_raw[[subj]],
                                map_scale = map_scale[[subj]],
                                mcent_subj = mcent_subj)

    # Store results for each subject
    subjr[[subj]] <- subj_results
    adj_scale_scores[[subj]] <- subj_results$scale$adj
    unadj_scale_scores[[subj]] <- subj_results$scale$unadj
  }

  # Calculate composite score for each examinee
    adj_comp_scores <- apply(sapply(adj_scale_scores, unlist), 1, mcent_obs)
    unadj_comp_scores <- apply(sapply(unadj_scale_scores, unlist), 1, mcent_obs)

  # Calculate the adjusted and unadjusted mcent ACT Composite scale score
  m_adj_comp <- do.call(mcent_comp, list(adj_comp_scores))
  m_unadj_comp <- do.call(mcent_comp, list(unadj_comp_scores))

  # Calculate the difference between the adjusted and unadjusted mcent Composite scores
  delta_comp <- m_adj_comp - m_unadj_comp

  # Output object
  out <- list("composite_results" = list("deltac" = delta_comp,
                                         "mscale" = list("adj" = m_adj_comp,
                                                         "unadj" = m_unadj_comp),
                                         "scale" = list("adj" = adj_comp_scores,
                                                        "unadj" = unadj_comp_scores)),
              "subject_results" = subjr)

  # Return output
  return(out)
}

