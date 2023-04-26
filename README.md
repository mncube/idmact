
<!-- README.md is generated from README.Rmd. Please edit that file -->

# idmact

<!-- badges: start -->
<!-- badges: end -->

The goal of idmact is to provide an implementation of the Schiel (1998)
<https://www.act.org/content/dam/act/unsecured/documents/ACT_RR98-01.pdf>
algorithm for interpreting differences beween mean ACT assessment
scores.

## Installation

You can install the development version of idmact from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mncube/idmact")
```

## Main Algorithm

The tools presented in the idmact package were developed to help
investigate the substantiveness of differences between mean ACT scale
scores across time. The basic form of the algorithm for composite scale
scores has the following five steps:

1.  Add one raw score unit to one or more subjects for each student to
    obtain adjusted raw scores.

2.  Map adjusted raw scores to adjusted scale scores using the form’s
    raw score to scale score map (note: perfect raw scores are always
    converted to the maximum allowable scale score despite the
    adjustment in step one).

3.  Sum the adjusted scale scores for each subject area, divide this sum
    by the number of subject areas, and round to the nearest integer in
    order to obtain each observation/examinee’s adjusted composite scale
    score.

4.  Calculate the adjusted mean composite scale score across all
    observations (m_adj).

5.  Calculate the unadjusted mean composite scale score across all
    observations (m_unadj).

6.  Compute the difference between the adjusted and unadjusted mean
    composite scale scores to obtain delta composite: deltac = m_adj -
    m_unadj

While this algorithm was developed with difficulties associated with
interpreting differences between mean ACT scale scores, the algorithm
can also be useful in other cases different forms of an assessment have
different raw score to scale score maps and when the mathematical
relation between the raw scores and scale scores is complex and/or
closed source.

## Single Subject Example

In the example that follows, the algorithm presented above will be used
to interpret differences between two forms of dummy a assessment. Each
form of the assessment has the same range of raw scores and the same
range of scale scores; however, the map between raw scores and scale
scores is different for each assessment. To keep things simple, a single
data set of raw scores will be simulated and on this data set, the
algorithm will be used to compare mean differences in scale scores
between the assessments. This first example will focus on a single
subject area, so step 3 of the algorithm will be excluded.

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

In the raw score to scale score map presented above, vectors were used
to save time when presenting how each of the 100 raw scores maps onto
each of the 12 scale scores. Use the map_elongate function to stretch
the raw and scale portions of the map out into a format where each part
of the map is a list of 100. This is the map format needed to use the
idmact_subj function which implements the subject-level algorithm.

``` r
formA <- map_elongate(map_raw = map_raw_formA,
                      map_scale = map_scale)

formB <- map_elongate(map_raw = map_raw_formB,
                      map_scale = map_scale)
```

### Run Subject Level Algorithm

Use the raw data and the maps stored in formA and formB to get and
compare the subject level delta (deltas). In the algorithm below, each
raw score is incremented by 1 using the default value of the inc
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

The higher value for form A provides evidence that if each student were
to answer an additional item correctly, the mean scale score for Form A
would be expected to increase more than the mean scale score for Form B.

## Composite Example

The idmact_comp funtion can be used to obtain delta for composite
scores. In the example below, raw scores for one additional subject
(say, ‘s2’) area will be created and combined with the data in the
previous example in order to illustrate idmact_comp.

### Generate Data and Maps

``` r
# Create 100 raw scores
set.seed(250)
raw_scores_s2 <- as.list(sample(1:100, 100, replace = TRUE))

# Subject to will use the same ranges for raw scores and scale scores as was used
# in the previous example, but the map will be slightly different. 
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
by 1. The raw scores are recycled such that for each form and each
subject, the same raw scores are being used.

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

### See Subject Area 2 Results

``` r
cat("Form A subject area 2 delta:", resA_comp$subject_results[[2]]$deltas, "\n")
#> Form A subject area 2 delta: 0.05
cat("Form B subject area 2 delta:", resB_comp$subject_results[[2]]$deltas)
#> Form B subject area 2 delta: 0.08
```
