% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idmact_comp.R
\name{idmact_comp}
\alias{idmact_comp}
\title{Interpreting Differences in Mean ACT Scores at the Composite Level}
\usage{
idmact_comp(
  df = NULL,
  df_map = NULL,
  raw,
  inc,
  map_raw,
  map_scale,
  mcent_subj = function(x) mean(x, na.rm = TRUE),
  mcent_obs = function(x) round(sum(x)/length(x)),
  mcent_comp = function(x) mean(x, na.rm = TRUE),
  na.rm.max = TRUE
)
}
\arguments{
\item{df}{A data frame containing raw scores (optional). If provided, the 'raw'
parameter should contain column names from this data frame.}

\item{df_map}{A data frame mapping raw scores to scale scores.}

\item{raw}{A list of raw scores for each subject, or column names from 'df'
where raw scores are stored.}

\item{inc}{A value or function used to increment raw scores for adjusted score
calculation. This can be a single value or a list of values for each subject.}

\item{map_raw}{Column names from 'df' or 'df_map' representing the domain of
raw scores, or a list of such domains.}

\item{map_scale}{Column names from 'df' or 'df_map' representing the range of
scale scores, or a list of such ranges.}

\item{mcent_subj}{A function summarizing scale scores at the subject level
(default is mean with NA removal).}

\item{mcent_obs}{A function summarizing scale scores at the examinee level
(default is round(mean)).}

\item{mcent_comp}{A function summarizing composite level scale scores
(default is mean with NA removal).}

\item{na.rm.max}{A boolean indicating whether to remove NA values when computing
maximum raw and scale values in the mapping.}
}
\value{
A list containing composite and subject level results.
"composite_results" includes the difference between the adjusted and unadjusted
mean composite scale scores (deltac), the mean adjusted and unadjusted composite
scale scores (mscale), and a list of individual adjusted and unadjusted composite
scale scores (scale). "subject_results" includes the outcomes from idmact_subj
for each subject.
}
\description{
The idmact_comp() function calculates and interprets differences in ACT composite
scores. The function operates in the following steps:
\enumerate{
\item Increment raw scores for one or more subjects for each student to obtain
adjusted raw scores.
\item Map adjusted raw scores to adjusted scale scores using the form's raw score
to scale score map.
\item Obtain each examinee's adjusted composite scale score by averaging the
adjusted scale scores across subjects.
\item Calculate the adjusted and unadjusted mean composite scale scores across
all observations.
\item Compute the difference between the adjusted and unadjusted mean composite
scale scores.
}
}
\details{
By default, the function parameters align with the method presented in Schiel
(1998). However, you can specify arbitrary anonymous functions for different
implementations.
}
\examples{
# Example 1: Using df and df_map
df <- data.frame(raw1 = c(1, 2, 3), raw2 = c(1, 1, 1))
df_map <- data.frame(map_raw1 = c(1, 2, 3),
                     map_scale1 = c(20, 21, 22),
                     map_raw2 = c(1, 1, 1),
                     map_scale2 = c(20, 20, 20))
comp_mean <- idmact_comp(df = df,
                         df_map = df_map,
                         raw = c("raw1", "raw2"),
                         inc = 1,
                         map_raw = c("map_raw1", "map_raw2"),
                         map_scale = c("map_scale1", "map_scale2"))

# Example 2: Using lists
raw = list(list(1, 2, 3, 4, 5), list(1, 1, 1, 1, 1))
inc = list(1 , 1)
map_raw = list(list(1, 2, 3, 4, 5))
map_scale = list(list(20, 21, 22, 23, 24))
comp_mean <- idmact_comp(raw = raw,
                         inc = inc,
                         map_raw = map_raw,
                         map_scale = map_scale)
}
\references{
Schiel, J. C. (1998). Interpreting differences in ACT composite scores
(ACT Research Report Series 98-1). ACT, Inc. URL:
https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf
}
