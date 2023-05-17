#' Interpreting Differences in Mean ACT Scores at the Composite Level
#'
#' The idmact_comp() function calculates and interprets differences in ACT composite
#' scores. The function operates in the following steps:
#' 1. Increment raw scores for one or more subjects for each student to obtain
#' adjusted raw scores.
#' 2. Map adjusted raw scores to adjusted scale scores using the form's raw score
#' to scale score map.
#' 3. Obtain each examinee's adjusted composite scale score by averaging the
#' adjusted scale scores across subjects.
#' 4. Calculate the adjusted and unadjusted mean composite scale scores across
#' all observations.
#' 5. Compute the difference between the adjusted and unadjusted mean composite
#' scale scores.
#'
#' By default, the function parameters align with the method presented in Schiel
#' (1998). However, you can specify arbitrary anonymous functions for different
#' implementations.
#'
#' @param df A data frame containing raw scores (optional). If provided, the 'raw'
#' parameter should contain column names from this data frame.
#' @param df_map A data frame mapping raw scores to scale scores.
#' @param raw A list of raw scores for each subject, or column names from 'df'
#' where raw scores are stored.
#' @param inc A value or function used to increment raw scores for adjusted score
#' calculation. This can be a single value or a list of values for each subject.
#' @param map_raw Column names from 'df' or 'df_map' representing the domain of
#' raw scores, or a list of such domains.
#' @param map_scale Column names from 'df' or 'df_map' representing the range of
#' scale scores, or a list of such ranges.
#' @param mcent_subj A function summarizing scale scores at the subject level
#' (default is mean with NA removal).
#' @param mcent_obs A function summarizing scale scores at the examinee level
#' (default is round(mean)).
#' @param mcent_comp A function summarizing composite level scale scores
#' (default is mean with NA removal).
#' @param na.rm.max A boolean indicating whether to remove NA values when computing
#' maximum raw and scale values in the mapping.
#'
#' @return A list containing composite and subject level results.
#' "composite_results" includes the difference between the adjusted and unadjusted
#' mean composite scale scores (deltac), the mean adjusted and unadjusted composite
#' scale scores (mscale), and a list of individual adjusted and unadjusted composite
#' scale scores (scale). "subject_results" includes the outcomes from idmact_subj
#' for each subject.
#'
#' @export
#'
#' @references
#' Schiel, J. C. (1998). Interpreting differences in ACT composite scores
#' (ACT Research Report Series 98-1). ACT, Inc. URL:
#' https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf
#'
#' @examples
#' # Example 1: Using df and df_map
#' df <- data.frame(raw1 = c(1, 2, 3), raw2 = c(1, 1, 1))
#' df_map <- data.frame(map_raw1 = c(1, 2, 3),
#'                      map_scale1 = c(20, 21, 22),
#'                      map_raw2 = c(1, 1, 1),
#'                      map_scale2 = c(20, 20, 20))
#' comp_mean <- idmact_comp(df = df,
#'                          df_map = df_map,
#'                          raw = c("raw1", "raw2"),
#'                          inc = 1,
#'                          map_raw = c("map_raw1", "map_raw2"),
#'                          map_scale = c("map_scale1", "map_scale2"))
#'
#' # Example 2: Using lists
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
                        mcent_comp = function(x) mean(x, na.rm = TRUE),
                        na.rm.max = TRUE){

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
                                mcent_subj = mcent_subj,
                                na.rm.max = na.rm.max)

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

