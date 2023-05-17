
<!-- README.md is generated from README.Rmd. Please edit that file -->

# idmact

<!-- badges: start -->
<!-- badges: end -->

The goal of idmact is to provide an implementation of the [Schiel
(1998)](https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf)
algorithm for interpreting differences between mean ACT assessment
scores.

## Installation

You can install the released version of idmact from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("idmact")
```

You can install the development version of idmact from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mncube/idmact")
```

## Main Algorithm

The idmact package provides tools designed to examine the significance
of differences between mean ACT scale scores over time. The fundamental
algorithm for composite scale scores consists of the following six
steps:

1.  Add one unit to the raw score of one or more subjects for each
    student to derive adjusted raw scores.

2.  Convert adjusted raw scores to adjusted scale scores using the
    form’s raw score to scale score map. Note that perfect raw scores
    are always converted to the maximum allowable scale score,
    irrespective of the adjustment in step one.

3.  For each student, sum the adjusted scale scores across all subject
    areas, divide this sum by the number of subject areas, and round to
    the nearest integer. This produces each student’s adjusted composite
    scale score.

4.  Calculate the adjusted mean composite scale score across all
    students (m_adj).

5.  Calculate the unadjusted mean composite scale score across all
    observations (m_unadj).

6.  Compute the difference between the adjusted and unadjusted mean
    composite scale scores to get the delta composite: deltac = m_adj -
    m_unadj

While this algorithm was initially developed to address the challenges
in interpreting differences between mean ACT scale scores, it can also
be beneficial in other contexts. These may include situations where
different forms of an assessment have varying raw score to scale score
maps, particularly when the relationship between raw and scale scores is
complex and/or proprietary.

## Single Subject Example

In the subsequent example, we’ll use the algorithm described above to
interpret differences between two forms of a hypothetical assessment.
Both forms share the same range of raw scores and scale scores. However,
the conversion map from raw scores to scale scores differs between the
two assessments. To keep this example straightforward, we’ll simulate a
single data set of raw scores and then use the algorithm to compare mean
differences in scale scores between the two assessments. This initial
example will concentrate on a single subject area, hence step 3 of the
algorithm will not be included.

### Generate Data and Maps

``` r
library(idmact)

# Create 100 raw scores
set.seed(279)
raw_scores <- as.list(sample(1:100, 100, replace = TRUE))

# Map between raw scores and scale scores for each form

## Each form has scale scores ranging from 1 to 12
map_scale <- c(1:12)

## Each assessment has raw scores ranging from 1 - 100
map_raw_formA <- list(1:5, 6:20, 21:25, 26:40, 41:45, 46:50, 51:55,
                   56:75, 76:80, 81:85, 86:90, 91:100)

map_raw_formB <- list(1:10, 11:20, 21:30, 31:40, 41:50, 51:55, 56:65,
                   66:75, 76:85, 86:90, 91:95, 96:100)
```

### Format Map

In the raw score to scale score map presented above, vectors/lists were
utilized to efficiently map each of the 100 raw scores to each of the 12
scale scores. Use the map_elongate function to expand the raw and scale
sections of the map into a format where each portion is a list of 100.
This is the map format required for the idmact_subj function, which
implements the subject-level algorithm.

``` r
formA <- map_elongate(map_raw = map_raw_formA,
                      map_scale = map_scale)

formB <- map_elongate(map_raw = map_raw_formB,
                      map_scale = map_scale)
```

### Run Subject Level Algorithm

Utilize the raw data and the maps stored in formA and formB to calculate
and compare the subject-level delta (deltas). In the algorithm below,
each raw score is increased by 1 using the default value of the inc
parameter.

``` r
resA <- idmact_subj(raw = raw_scores,
                    map_raw = formA$map_raw,
                    map_scale = formA$map_scale)


resB <- idmact_subj(raw = raw_scores,
                    map_raw = formB$map_raw,
                    map_scale = formB$map_scale)
```

### Compare Results

``` r
cat("Form A subject level delta:", resA$deltas, "\n")
#> Form A subject level delta: 0.11
cat("Form B subject level delta:", resB$deltas)
#> Form B subject level delta: 0.1
```

In this section, the function idmact_subj is used to calculate the
‘delta’ for each form (A and B). The ‘delta’ is the difference between
the mean adjusted scale score and the mean unadjusted scale score. This
‘delta’ value provides an estimate of how much the mean scale score
would increase if every student were to answer one additional item
correctly on the test.

In the provided example:

For Form A, the subject level delta is 0.11, meaning the mean scale
score is expected to increase by 0.11 if every student answers one
additional item correctly.

For Form B, the subject level delta is 0.1, suggesting that the mean
scale score would increase by 0.1 under the same conditions.

The results indicate that Form A is more responsive to increases in raw
scores, as a one unit increase in raw score leads to a larger increase
in the mean scale score for Form A compared to Form B.

## Composite Example

The idmact_comp function can be used to calculate the delta for
composite scores (deltac). In the example below, raw scores for an
additional subject area (‘s2’) will be created and combined with the
data from the previous example to demonstrate idmact_comp.

### Generate Data and Maps

``` r
# Create 100 raw scores
set.seed(250)
raw_scores_s2 <- as.list(sample(1:100, 100, replace = TRUE))

# Subject two will use the same ranges for raw scores and scale scores as in
# the previous example, but the map will be slightly different. 
map_raw_formA_s2 <- list(1:10, 11:25, 26:30, 31:40, 41:45, 46:50, 51:60,
                         61:75, 76:80, 81:85, 86:90, 91:100)

map_raw_formB_s2 <- list(1:10, 11:16, 17:25, 26:35, 36:45, 46:55, 56:60,
                         61:75, 76:85, 86:90, 91:95, 96:100)

formA_s2 <- map_elongate(map_raw = map_raw_formA_s2,
                         map_scale = map_scale)

formB_s2 <- map_elongate(map_raw = map_raw_formB_s2,
                         map_scale = map_scale)
```

### Run Composite Level Algorithm

In the algorithm below, each raw score for each subject is incremented
by 1.

``` r
resA_comp <- idmact_comp(raw = list(raw_scores, raw_scores_s2),
                         inc = list(1, 1),
                         map_raw = list(formA$map_raw, formA_s2$map_raw),
                         map_scale = list(formA$map_scale, formA_s2$map_scale))

resB_comp <- idmact_comp(raw = list(raw_scores, raw_scores_s2),
                         inc = list(1, 1),
                         map_raw = list(formB$map_raw, formB_s2$map_raw),
                         map_scale = list(formB$map_scale, formB_s2$map_scale))
```

### Compare Composite Results

``` r
cat("Form A composite level delta:", resA_comp$composite_results$deltac, "\n")
#> Form A composite level delta: 0.08
cat("Form B composite level delta:", resB_comp$composite_results$deltac)
#> Form B composite level delta: 0.08
```

In the composite example, two subjects are considered instead of one,
and the idmact_comp function is used to calculate the composite level
delta (deltac). The composite level delta is calculated in a similar way
to the subject level delta, but it considers the total adjusted and
unadjusted scale scores across all subjects, rather than just one.

In the provided example:

For both Form A and Form B, the composite level delta is 0.08. This
means that if every student were to answer one additional item correctly
on each form, the mean composite scale score is expected to increase by
0.08.

This suggests that, when considering multiple subjects, both Form A and
Form B respond similarly to increases in raw scores.

### See Subject Area 2 Results

``` r
cat("Form A subject area 2 delta:", resA_comp$subject_results[[2]]$deltas, "\n")
#> Form A subject area 2 delta: 0.05
cat("Form B subject area 2 delta:", resB_comp$subject_results[[2]]$deltas)
#> Form B subject area 2 delta: 0.08
```

This section presents the ‘delta’ for the second subject area
specifically. The ‘delta’ for the second subject area is calculated in
the same way as the subject level delta mentioned in the first section,
but it only considers the scores for the second subject.

In the provided example:

For Form A, the subject area 2 delta is 0.05, suggesting that the mean
scale score for the second subject would increase by 0.05 if every
student answered one additional item correctly.

For Form B, the subject area 2 delta is 0.08, meaning that the mean
scale score for the second subject would increase by 0.08 under the same
conditions.

These results indicate that, for the second subject area specifically,
Form B is more responsive to increases in raw scores.
