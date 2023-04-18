#' Interpreting Composite Differences in Mean ACT Scores
#'
#' @param df A data frame containing raw scores (optional)
#' @param df_map A data frame containing the map between raw and scale scores
#' @param raw A nested list of raw scores or the list of quoted data frame column
#' name where raw scores for each subject are stored.
#' @param inc A list of numbers. Increment raw scores for each subject by inc to
#' obtain adjusted scores
#' @param map_raw A nested list containing the domain of raw scores for each subject's
#' raw score to scale score map, or the corresponding list of column names (quoted)
#' in df or df_map
#' @param map_scale A nested list containing the image of scale scores for each
#' subject's raw score to scale score map, or the corresponding list of column
#' names (quoted) in df or df_map
#' @param mcent_subj A measure of central tendency to summarize subject level
#' scale scores
#' @param mcent_obs An anonymous function to summarize observation level
#' scale scores (i.e., summary within observation/examinee across subjects)
#' @param mcent_comp A anonymous function defining the measure of central tendency
#' used to summarize composite level scale scores (i.e., summarize exam level scale
#' score across observations)
#'
#' @return A nested list
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
  beta_comp <- m_adj_comp - m_unadj_comp

  # Output object
  out <- list("composite_results" = list("betac" = beta_comp,
                                         "mscale" = list("adj" = m_adj_comp,
                                                         "unadj" = m_unadj_comp),
                                         "scale" = list("adj" = adj_comp_scores,
                                                        "unadj" = unadj_comp_scores)),
              "subject_results" = subjr)

  # Return output
  return(out)
}

