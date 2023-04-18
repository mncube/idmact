idmact_comp <- function(df = NULL, df_map = NULL, raw,
                        inc, map_raw, map_scale, mcent_subj = "mean",
                        na.rm.mcent_subj = TRUE,
                        mcent_obs = function(x) round(sum(x) / length(x)),
                        mcent_comp = "mean",
                        na.rm.mcent_comp = TRUE){

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
  betas <- list()
  m_scale_adj <- list()
  m_scale_unadj <- list()
  adj_scale_scores <- list()
  unadj_scale_scores <- list()

  # Iterate over subjects
  for (subj in seq_len(max_len)) {

    # Apply idmact_subj for each subject
    subj_results <- idmact_subj(df, df_map, raw = raw[[subj]],
                                inc = inc[[subj]], map_raw = map_raw[[subj]],
                                map_scale = map_scale[[subj]],
                                mcent_subj = mcent_subj, na.rm.mcent_subj = na.rm.mcent_subj)

    # Store results for each subject
    betas[[subj]] <- subj_results$betas
    m_scale_adj[[subj]] <- subj_results$m_scale$adj
    m_scale_unadj[[subj]] <- subj_results$m_scale$unadj
    adj_scale_scores[[subj]] <- subj_results$scale$adj
    unadj_scale_scores[[subj]] <- subj_results$scale$unadj
  }

  # Calculate composite score for each examinee
    adj_comp_scores <- apply(sapply(adj_scale_scores, unlist), 1, mcent_obs)
    unadj_comp_scores <- apply(sapply(unadj_scale_scores, unlist), 1, mcent_obs)

  # Calculate the adjusted and unadjusted mean ACT Composite scale score
  m_adj_comp <- do.call(mcent_comp, c(list(adj_comp_scores), na.rm = na.rm.mcent_comp))
  m_unadj_comp <- do.call(mcent_comp, c(list(unadj_comp_scores), na.rm = na.rm.mcent_comp))

  # Calculate the difference between the adjusted and unadjusted mean Composite scores
  beta_comp <- m_adj_comp - m_unadj_comp

  # Output object
  out <- list("beta_comp" = beta_comp,
              "m_comp" = list("adj" = m_adj_comp,
                              "unadj" = m_unadj_comp),
              "subject_results" = betas,
              "composite_scores" = list("adj" = adj_comp_scores,
                                        "unadj" = unadj_comp_scores))

  # Return output
  return(out)
}

