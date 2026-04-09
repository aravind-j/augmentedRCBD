# Combined Analysis of Augmented Randomised Complete Block Design in Multiple Environments

`augmentedRCBD.menv` is an extension of
[`augmentedRCBD`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.md)
designed for the combined/pooled analysis of data from augmented
randomised complete block design across multiple environments such as
locations and/or seasons. This function performs analysis under the
following two scenarios.

- *Scenario 1*::

  Replicated test treatments across environments.

- *Scenario 2*::

  Non-replicated test treatments across environments.

## Usage

``` r
augmentedRCBD.menv(
  block,
  treatment,
  env,
  y,
  checks = NULL,
  method.comp = c("lsd", "tukey", "none"),
  scenario = c(1, 2),
  alpha = 0.05,
  group = TRUE,
  console = TRUE,
  simplify = FALSE,
  truncate.means = TRUE
)
```

## Arguments

- block:

  Vector of blocks (as a factor).

- treatment:

  Vector of treatments/genotypes (as a factor).

- env:

  Vector of environments (as a factor).

- y:

  Numeric vector of response variable (Trait).

- checks:

  Character vector of the checks present in `treatment` levels.

- method.comp:

  Method for comparison of treatments (`"lsd"` for least significant
  difference or `"tukey"` for Tukey's honest significant difference). If
  `"none"`, no comparisons will be made, the ANOVA output will be given
  as a data frame and the adjusted means will be computed directly from
  treatment and block effects instead of using
  [`emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- scenario:

  Either `1` or `2` (see **Details**).

- alpha:

  Type I error probability (Significance level) to be used for multiple
  comparisons.

- group:

  If `TRUE`, genotypes will be grouped according to `"method.comp"`.
  Default is `TRUE`.

- console:

  If `TRUE`, output will be printed to console. Default is `TRUE`.
  Default is `TRUE`.

- simplify:

  If `TRUE`, ANOVA output will be given as a data frame instead of a
  `summary.aov` object. Default is `TRUE`.

- truncate.means:

  If `TRUE`, the negative adjusted means will be truncated to zero.
  Default is `TRUE`.

## Value

A list of class `augmentedRCBD.menv` containing the following
components:

- `Details`:

  Details of the augmented design used.

- `Means`:

  A data frame with the "Means", "Block", "SE", "Mix", "Max" and
  "Adjusted Means" for each "Treatment".

- `ANOVA, Treatment Adjusted`:

  An object of class `summary.aov` for ANOVA table with treatments
  adjusted.

- `ANOVA, Block Adjusted`:

  An object of class `summary.aov` for ANOVA table with block adjusted.

- `Block effects`:

  A vector of block effects.

- `Treatment effects`:

  A vector of treatment effects.

- `Std. Errors`:

  A data frame of standard error of difference between various
  combinations along with critical difference and tukey's honest
  significant difference (when `method.comp = "tukey"`) at `alpha`.

- `Overall adjusted mean`:

  Overall adjusted mean.

- `CV`:

  Coefficient of variation.

- `Comparison method`:

  The method for comparison of treatments.

- `Comparisons`:

  A data frame of pairwise comparisons of treatments. This is computed
  only if argument `group` is `TRUE`

- `Groups`:

  A data frame with compact letter display of pairwise comparisons of
  treatments. Means with at least one letter common are not
  significantly different statistically. This is computed only if
  argument `group` is `TRUE`

- `warning`:

  A vector of warning messages (if any) captured during model fitting.

## Details

The method of analysis is determined by whether the test treatments are
replicated across different environments as indicated by the `scenario`
argument. The design is balanced in terms of the checks being replicated
across all the blocks and environments.

- Scenario `1`::

  The test treatments are replicated across all environments. Here the
  Treatment \u00D7 Environment interaction is fully estimable as it is
  balanced.

- Scenario `2`::

  The test treatments are replicated across all environments. Here the
  Treatment \u00D7 Environment interaction is only partially estimable
  as it is unbalanced..

## Note

- Data should preferably be balanced i.e. all the check genotypes should
  be present in all the blocks. If not, a warning is issued.

- There should not be any missing values.

- The number of test genotypes can vary within a block.

- When a large number of treatments or genotypes are involved, the
  analysis becomes memory intensive because calculating adjusted means
  necessitates the creation of a reference grid matrix by `emmeans`,
  which can grow exponentially in size

## See also

[`augmentedRCBD`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.md)

## Examples

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scenario 1: Test treatments are replicated across all environments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data
blk1 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
          4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
          7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
trt1 <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10,
          1, 2, 3, 4, 8, 11, 5, 1, 2, 3, 4, 12, 9, 1, 2, 3, 4, 7, 6, 10,
          1, 2, 3, 4, 7, 9, 12, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 8, 11, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
        78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
        76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
        78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
env1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
          3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
data1 <- data.frame(env1, blk1, trt1, y1)
chks1 <- c(1, 2, 3, 4)

# Convert block, treatment and environment to factors
data1$blk1 <- as.factor(data1$blk1)
data1$trt1 <- as.factor(data1$trt1)
data1$env1 <- as.factor(data1$env1)

# Contingency tables of factors
table(data1$env1, data1$trt1)
#>    
#>     1 2 3 4 5 6 7 8 9 10 11 12
#>   1 3 3 3 3 1 1 1 1 1  1  1  1
#>   2 3 3 3 3 1 1 1 1 1  1  1  1
#>   3 3 3 3 3 1 1 1 1 1  1  1  1
table(data1$env1, data1$blk1)
#>    
#>     1 2 3 4 5 6 7 8 9
#>   1 7 6 7 0 0 0 0 0 0
#>   2 0 0 0 7 6 7 0 0 0
#>   3 0 0 0 0 0 0 7 6 7
table(data1$blk1, data1$trt1)
#>    
#>     1 2 3 4 5 6 7 8 9 10 11 12
#>   1 1 1 1 1 0 0 1 0 0  0  1  1
#>   2 1 1 1 1 1 0 0 0 1  0  0  0
#>   3 1 1 1 1 0 1 0 1 0  1  0  0
#>   4 1 1 1 1 1 0 0 1 0  0  1  0
#>   5 1 1 1 1 0 0 0 0 1  0  0  1
#>   6 1 1 1 1 0 1 1 0 0  1  0  0
#>   7 1 1 1 1 0 0 1 0 1  0  0  1
#>   8 1 1 1 1 1 1 0 0 0  0  0  0
#>   9 1 1 1 1 0 0 0 1 0  1  1  0

# Results
out1 <- augmentedRCBD.menv(block = data1$blk1, treatment = data1$trt1,
                           env = data1$env1, y = data1$y1, checks = chks1,
                           scenario = 1, method.comp = "lsd", alpha = 0.05,
                           group = TRUE, console = TRUE)
#> Error in augmentedRCBD.menv(block = data1$blk1, treatment = data1$trt1,     env = data1$env1, y = data1$y1, checks = chks1, scenario = 1,     method.comp = "lsd", alpha = 0.05, group = TRUE, console = TRUE): "block", "treatment", "env", and "y" are of unequal lengths.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test treatments are not replicated across all environments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data
blk2 <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
          4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
          7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9)
trt2 <- c(1, 2, 3, 4, 7, 10, 11, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 12,
          1, 2, 3, 4, 16, 19, 13, 1, 2, 3, 4, 20, 17, 1, 2, 3, 4, 15, 14, 18,
          1, 2, 3, 4, 22, 25, 27, 1, 2, 3, 4, 21, 23, 1, 2, 3, 4, 24, 26, 28)
y2 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77,
        78, 78, 70, 75, 74, 90, 80, 85, 78, 95, 86, 81, 78, 78, 76, 88,
        76, 79, 80, 76, 75, 74, 77, 75, 72, 91, 81, 86, 80, 94, 87, 83,
        78, 79, 77, 90, 74, 76, 82, 83, 86, 76, 73, 74, 69)
env2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
          3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
data2 <- data.frame(env2, blk2, trt2, y2)
chks2 <- c(1, 2, 3, 4)

# Convert block, treatment and environment to factors
data2$blk2 <- as.factor(data2$blk2)
data2$trt2 <- as.factor(data2$trt2)
data2$env2 <- as.factor(data2$env2)

# Contingency tables of factors
table(data2$env2, data2$trt2)
#>    
#>     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>   1 3 3 3 3 1 1 1 1 1  1  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   2 3 3 3 3 0 0 0 0 0  0  0  0  1  1  1  1  1  1  1  1  0  0  0  0  0  0  0  0
#>   3 3 3 3 3 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1
table(data2$env2, data2$blk2)
#>    
#>     1 2 3 4 5 6 7 8 9
#>   1 7 6 7 0 0 0 0 0 0
#>   2 0 0 0 7 6 7 0 0 0
#>   3 0 0 0 0 0 0 7 6 7
table(data2$blk2, data2$trt2)
#>    
#>     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>   1 1 1 1 1 0 0 1 0 0  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   2 1 1 1 1 1 0 0 0 1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   3 1 1 1 1 0 1 0 1 0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   4 1 1 1 1 0 0 0 0 0  0  0  0  1  0  0  1  0  0  1  0  0  0  0  0  0  0  0  0
#>   5 1 1 1 1 0 0 0 0 0  0  0  0  0  0  0  0  1  0  0  1  0  0  0  0  0  0  0  0
#>   6 1 1 1 1 0 0 0 0 0  0  0  0  0  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0
#>   7 1 1 1 1 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  1  0  1  0
#>   8 1 1 1 1 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  1  0  1  0  0  0  0  0
#>   9 1 1 1 1 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  1  0  1

# Results
out2 <- augmentedRCBD.menv(block = data2$blk2, treatment = data2$trt2,
                           env = data2$env2, y = data2$y2, checks = chks2,
                           scenario = 2, method.comp = "lsd", alpha = 0.05,
                           group = TRUE, console = TRUE)
#> Error in augmentedRCBD.menv(block = data2$blk2, treatment = data2$trt2,     env = data2$env2, y = data2$y2, checks = chks2, scenario = 2,     method.comp = "lsd", alpha = 0.05, group = TRUE, console = TRUE): "block", "treatment", "env", and "y" are of unequal lengths.
```
