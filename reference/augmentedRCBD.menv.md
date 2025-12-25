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

  Character vector of the checks present in `treatment` levels. If not
  specified, checks are inferred from the data on the basis of number of
  replications of treatments/genotypes.

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
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#>                                        
#> Number of blocks           "9"         
#> Number of treatments       "12"        
#> Number of environments     "3"         
#> Number of check treatments "4"         
#> Number of test treatments  "8"         
#> Check treatments           "1, 2, 3, 4"
#> 
#> 
#> ANOVA, Treatment Adjusted
#> =========================
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (eliminating Blocks)                            11  691.2   62.83
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test and Test vs. Check                       8  605.3   75.66
#> Block (within Environment, ignoring Treatments)            6  672.5  112.08
#> Environment × Treatment interaction                       22  398.7   18.12
#>   Interaction: Check × Environment                         6   17.6    2.94
#>   Interaction: Test and Test vs. Check × Environment      16  381.1   23.82
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (eliminating Blocks)                              2.070 0.0824 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test and Test vs. Check                        2.492 0.0513 .
#> Block (within Environment, ignoring Treatments)             3.692 0.0144 *
#> Environment × Treatment interaction                         0.597 0.8753  
#>   Interaction: Check × Environment                          0.097 0.9958  
#>   Interaction: Test and Test vs. Check × Environment        0.785 0.6846  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> ANOVA, Block Adjusted
#> =====================
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (ignoring Blocks)                               11  691.2   62.83
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test                                          7  561.2   80.17
#>   Treatment: Test vs. Check                                1   44.1   44.10
#> Block (within Environment, eliminating Treatments)         6  672.5  112.08
#> Environment × Treatment interaction                       22  398.7   18.12
#>   Interaction: Check × Environment                         6   17.6    2.94
#>   Interaction: Test × Environment                         14  340.2   24.30
#>   Interaction: Test vs. Check × Environment                2   40.9   20.47
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (ignoring Blocks)                                 2.070 0.0824 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test                                           2.640 0.0459 *
#>   Treatment: Test vs. Check                                 1.453 0.2437  
#> Block (within Environment, eliminating Treatments)          3.692 0.0144 *
#> Environment × Treatment interaction                         0.597 0.8753  
#>   Interaction: Check × Environment                          0.097 0.9958  
#>   Interaction: Test × Environment                           0.800 0.6596  
#>   Interaction: Test vs. Check × Environment                 0.674 0.5219  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Coefficient of Variation
#> ========================
#> 6.81942
#> 
#> Overall Adjusted Mean
#> =====================
#> 80.31944
#> 
#> Standard Errors
#> ===============
#>                                          Std. Error of Diff.   CD (5%)
#> Control Treatment Means                             1.499657  3.150662
#> Two Test Treatments (Same Block)                    4.498971  9.451987
#> Two Test Treatments (Different Blocks)              5.030003 10.567643
#> A Test Treatment and a Control Treatment            3.749143  7.876656
#> 
#> Treatment Means
#> ===============
#>  treatment Means   SE r   Min   Max Adjusted Means
#>          1 83.67 1.92 9 78.00 92.00          83.67
#>         10 71.67 1.45 3 69.00 74.00          74.17
#>         11 83.00 4.58 3 74.00 89.00          81.25
#>         12 80.33 2.19 3 76.00 83.00          78.75
#>          2 79.33 0.73 9 76.00 83.00          79.33
#>          3 81.22 1.61 9 75.00 87.00          81.22
#>          4 81.78 2.10 9 74.00 91.00          81.78
#>          5 78.00 2.08 3 74.00 81.00          77.08
#>          6 75.33 0.33 3 75.00 76.00          78.08
#>          7 89.00 6.03 3 77.00 96.00          88.67
#>          8 79.33 7.88 3 70.00 95.00          79.50
#>          9 81.33 2.85 3 78.00 87.00          80.33
#> 
#> 
#> Comparisons
#> ===========
#> 
#> Method : lsd
#> 
#>                   contrast estimate   SE df t.ratio p.value sig
#>    treatment1 - treatment2     4.33 2.60 18   1.668   0.113    
#>    treatment1 - treatment3     2.44 2.60 18   0.941   0.359    
#>    treatment1 - treatment4     1.89 2.60 18   0.727   0.476    
#>    treatment1 - treatment5     6.58 3.90 18   1.690   0.108    
#>    treatment1 - treatment6     5.58 3.90 18   1.433   0.169    
#>    treatment1 - treatment7    -5.00 3.90 18  -1.283   0.216    
#>    treatment1 - treatment8     4.17 3.90 18   1.069   0.299    
#>    treatment1 - treatment9     3.33 3.90 18   0.856   0.404    
#>   treatment1 - treatment10     9.50 3.90 18   2.438   0.025   *
#>   treatment1 - treatment11     2.42 3.90 18   0.620   0.543    
#>   treatment1 - treatment12     4.92 3.90 18   1.262   0.223    
#>    treatment2 - treatment3    -1.89 2.60 18  -0.727   0.476    
#>    treatment2 - treatment4    -2.44 2.60 18  -0.941   0.359    
#>    treatment2 - treatment5     2.25 3.90 18   0.577   0.571    
#>    treatment2 - treatment6     1.25 3.90 18   0.321   0.752    
#>    treatment2 - treatment7    -9.33 3.90 18  -2.395   0.028   *
#>    treatment2 - treatment8    -0.17 3.90 18  -0.043   0.966    
#>    treatment2 - treatment9    -1.00 3.90 18  -0.257   0.800    
#>   treatment2 - treatment10     5.17 3.90 18   1.326   0.201    
#>   treatment2 - treatment11    -1.92 3.90 18  -0.492   0.629    
#>   treatment2 - treatment12     0.58 3.90 18   0.150   0.883    
#>    treatment3 - treatment4    -0.56 2.60 18  -0.214   0.833    
#>    treatment3 - treatment5     4.14 3.90 18   1.062   0.302    
#>    treatment3 - treatment6     3.14 3.90 18   0.806   0.431    
#>    treatment3 - treatment7    -7.44 3.90 18  -1.911   0.072    
#>    treatment3 - treatment8     1.72 3.90 18   0.442   0.664    
#>    treatment3 - treatment9     0.89 3.90 18   0.228   0.822    
#>   treatment3 - treatment10     7.06 3.90 18   1.811   0.087    
#>   treatment3 - treatment11    -0.03 3.90 18  -0.007   0.994    
#>   treatment3 - treatment12     2.47 3.90 18   0.635   0.534    
#>    treatment4 - treatment5     4.69 3.90 18   1.205   0.244    
#>    treatment4 - treatment6     3.69 3.90 18   0.948   0.356    
#>    treatment4 - treatment7    -6.89 3.90 18  -1.768   0.094    
#>    treatment4 - treatment8     2.28 3.90 18   0.585   0.566    
#>    treatment4 - treatment9     1.44 3.90 18   0.371   0.715    
#>   treatment4 - treatment10     7.61 3.90 18   1.953   0.066    
#>   treatment4 - treatment11     0.53 3.90 18   0.135   0.894    
#>   treatment4 - treatment12     3.03 3.90 18   0.777   0.447    
#>    treatment5 - treatment6    -1.00 4.86 18  -0.206   0.839    
#>    treatment5 - treatment7   -11.58 5.03 18  -2.303   0.033   *
#>    treatment5 - treatment8    -2.42 4.86 18  -0.497   0.625    
#>    treatment5 - treatment9    -3.25 4.86 18  -0.669   0.512    
#>   treatment5 - treatment10     2.92 5.03 18   0.580   0.569    
#>   treatment5 - treatment11    -4.17 4.86 18  -0.857   0.402    
#>   treatment5 - treatment12    -1.67 5.03 18  -0.331   0.744    
#>    treatment6 - treatment7   -10.58 4.86 18  -2.178   0.043   *
#>    treatment6 - treatment8    -1.42 4.86 18  -0.292   0.774    
#>    treatment6 - treatment9    -2.25 5.03 18  -0.447   0.660    
#>   treatment6 - treatment10     3.92 4.68 18   0.836   0.414    
#>   treatment6 - treatment11    -3.17 5.03 18  -0.630   0.537    
#>   treatment6 - treatment12    -0.67 5.03 18  -0.133   0.896    
#>    treatment7 - treatment8     9.17 5.03 18   1.822   0.085    
#>    treatment7 - treatment9     8.33 4.86 18   1.715   0.104    
#>   treatment7 - treatment10    14.50 4.86 18   2.984   0.008  **
#>   treatment7 - treatment11     7.42 4.86 18   1.526   0.144    
#>   treatment7 - treatment12     9.92 4.68 18   2.118   0.048   *
#>    treatment8 - treatment9    -0.83 5.03 18  -0.166   0.870    
#>   treatment8 - treatment10     5.33 4.68 18   1.139   0.270    
#>   treatment8 - treatment11    -1.75 4.68 18  -0.374   0.713    
#>   treatment8 - treatment12     0.75 5.03 18   0.149   0.883    
#>   treatment9 - treatment10     6.17 5.03 18   1.226   0.236    
#>   treatment9 - treatment11    -0.92 5.03 18  -0.182   0.857    
#>   treatment9 - treatment12     1.58 4.68 18   0.338   0.739    
#>  treatment10 - treatment11    -7.08 4.86 18  -1.458   0.162    
#>  treatment10 - treatment12    -4.58 5.03 18  -0.911   0.374    
#>  treatment11 - treatment12     2.50 4.86 18   0.514   0.613    
#> 
#> Treatment Groups
#> ================
#> 
#> Method : lsd
#> 
#>  Treatment Adjusted Means   SE df lower.CL upper.CL Group
#>         10          74.17 3.44 18    66.95    81.39   1  
#>          5          77.08 3.44 18    69.86    84.30   12 
#>          6          78.08 3.44 18    70.86    85.30   12 
#>         12          78.75 3.44 18    71.53    85.97   12 
#>          2          79.33 1.84 18    75.47    83.19   12 
#>          8          79.50 3.44 18    72.28    86.72   123
#>          9          80.33 3.44 18    73.11    87.55   123
#>          3          81.22 1.84 18    77.36    85.08   123
#>         11          81.25 3.44 18    74.03    88.47   123
#>          4          81.78 1.84 18    77.92    85.64   123
#>          1          83.67 1.84 18    79.81    87.53    23
#>          7          88.67 3.44 18    81.45    95.89     3

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
#> 
#> Augmented Design Details
#> ========================
#>                                        
#> Number of blocks           "9"         
#> Number of treatments       "28"        
#> Number of environments     "3"         
#> Number of check treatments "4"         
#> Number of test treatments  "24"        
#> Check treatments           "1, 2, 3, 4"
#> 
#> 
#> ANOVA, Treatment Adjusted
#> =========================
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (eliminating Blocks)                            27 1550.0   57.41
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test and Test vs. Check                      24 1464.1   61.00
#> Block (within Environment, ignoring Treatments)            6  194.8   32.47
#> Environment × Treatment interaction                        6   17.6    2.94
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (eliminating Blocks)                              1.891 0.0817 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test and Test vs. Check                        2.009 0.0664 .
#> Block (within Environment, ignoring Treatments)             1.070 0.4162  
#> Environment × Treatment interaction                         0.097 0.9958  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> ANOVA, Block Adjusted
#> =====================
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (ignoring Blocks)                               27 1550.0   57.41
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test                                         23 1420.0   61.74
#>   Treatment: Test vs. Check                                1   44.1   44.10
#> Block (within Environment, eliminating Treatments)         6  194.8   32.47
#> Environment × Treatment interaction                        6   17.6    2.94
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (ignoring Blocks)                                 1.891 0.0817 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test                                           2.033 0.0643 .
#>   Treatment: Test vs. Check                                 1.453 0.2437  
#> Block (within Environment, eliminating Treatments)          1.070 0.4162  
#> Environment × Treatment interaction                         0.097 0.9958  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Coefficient of Variation
#> ========================
#> NA
#> 
#> Overall Adjusted Mean
#> =====================
#> 79.98214
#> 
#> Standard Errors
#> ===============
#>                                          Std. Error of Diff. CD (5%)
#> Control Treatment Means                                   NA      NA
#> Two Test Treatments (Same Block)                          NA      NA
#> Two Test Treatments (Different Blocks)                    NA      NA
#> A Test Treatment and a Control Treatment                  NA      NA
#> 
#> Treatment Means
#> ===============
#>  treatment Means   SE r   Min   Max Adjusted Means
#>          1 83.67 1.92 9 78.00 92.00          83.67
#>         10 89.00 <NA> 1 89.00 89.00          85.75
#>         11 82.00 <NA> 1 82.00 82.00          78.75
#>         12 74.00 <NA> 1 74.00 74.00          76.50
#>         13 81.00 <NA> 1 81.00 81.00          79.25
#>         14 75.00 <NA> 1 75.00 75.00          80.25
#>         15 77.00 <NA> 1 77.00 77.00          82.25
#>         16 95.00 <NA> 1 95.00 95.00          93.25
#>         17 79.00 <NA> 1 79.00 79.00          80.50
#>         18 72.00 <NA> 1 72.00 72.00          77.25
#>         19 86.00 <NA> 1 86.00 86.00          84.25
#>          2 79.33 0.73 9 76.00 83.00          79.33
#>         20 76.00 <NA> 1 76.00 76.00          77.50
#>         21 74.00 <NA> 1 74.00 74.00          74.50
#>         22 94.00 <NA> 1 94.00 94.00          91.00
#>         23 76.00 <NA> 1 76.00 76.00          76.50
#>         24 73.00 <NA> 1 73.00 73.00          72.75
#>         25 87.00 <NA> 1 87.00 87.00          84.00
#>         26 74.00 <NA> 1 74.00 74.00          73.75
#>         27 83.00 <NA> 1 83.00 83.00          80.00
#>         28 69.00 <NA> 1 69.00 69.00          68.75
#>          3 81.22 1.61 9 75.00 87.00          81.22
#>          4 81.78 2.10 9 74.00 91.00          81.78
#>          5 79.00 <NA> 1 79.00 79.00          77.50
#>          6 75.00 <NA> 1 75.00 75.00          77.50
#>          7 96.00 <NA> 1 96.00 96.00          92.75
#>          8 70.00 <NA> 1 70.00 70.00          72.50
#>          9 78.00 <NA> 1 78.00 78.00          76.50
#> 
#> 
#> Comparisons
#> ===========
#> 
#> Method : lsd
#> 
#>                   contrast estimate   SE df t.ratio p.value sig
#>    treatment1 - treatment2     4.33 2.29 24   1.896   0.070    
#>    treatment1 - treatment3     2.44 2.29 24   1.070   0.295    
#>    treatment1 - treatment4     1.89 2.29 24   0.826   0.417    
#>    treatment1 - treatment5     6.17 5.60 24   1.102   0.282    
#>    treatment1 - treatment6     6.17 5.60 24   1.102   0.282    
#>    treatment1 - treatment7    -9.08 5.60 24  -1.623   0.118    
#>    treatment1 - treatment8    11.17 5.60 24   1.995   0.058    
#>    treatment1 - treatment9     7.17 5.60 24   1.280   0.213    
#>   treatment1 - treatment10    -2.08 5.60 24  -0.372   0.713    
#>   treatment1 - treatment11     4.92 5.60 24   0.878   0.389    
#>   treatment1 - treatment12     7.17 5.60 24   1.280   0.213    
#>   treatment1 - treatment13     4.42 5.60 24   0.789   0.438    
#>   treatment1 - treatment14     3.42 5.60 24   0.610   0.547    
#>   treatment1 - treatment15     1.42 5.60 24   0.253   0.802    
#>   treatment1 - treatment16    -9.58 5.60 24  -1.712   0.100    
#>   treatment1 - treatment17     3.17 5.60 24   0.566   0.577    
#>   treatment1 - treatment18     6.42 5.60 24   1.146   0.263    
#>   treatment1 - treatment19    -0.58 5.60 24  -0.104   0.918    
#>   treatment1 - treatment20     6.17 5.60 24   1.102   0.282    
#>   treatment1 - treatment21     9.17 5.60 24   1.637   0.115    
#>   treatment1 - treatment22    -7.33 5.60 24  -1.310   0.203    
#>   treatment1 - treatment23     7.17 5.60 24   1.280   0.213    
#>   treatment1 - treatment24    10.92 5.60 24   1.950   0.063    
#>   treatment1 - treatment25    -0.33 5.60 24  -0.060   0.953    
#>   treatment1 - treatment26     9.92 5.60 24   1.771   0.089    
#>   treatment1 - treatment27     3.67 5.60 24   0.655   0.519    
#>   treatment1 - treatment28    14.92 5.60 24   2.665   0.014   *
#>    treatment2 - treatment3    -1.89 2.29 24  -0.826   0.417    
#>    treatment2 - treatment4    -2.44 2.29 24  -1.070   0.295    
#>    treatment2 - treatment5     1.83 5.60 24   0.327   0.746    
#>    treatment2 - treatment6     1.83 5.60 24   0.327   0.746    
#>    treatment2 - treatment7   -13.42 5.60 24  -2.397   0.025   *
#>    treatment2 - treatment8     6.83 5.60 24   1.221   0.234    
#>    treatment2 - treatment9     2.83 5.60 24   0.506   0.617    
#>   treatment2 - treatment10    -6.42 5.60 24  -1.146   0.263    
#>   treatment2 - treatment11     0.58 5.60 24   0.104   0.918    
#>   treatment2 - treatment12     2.83 5.60 24   0.506   0.617    
#>   treatment2 - treatment13     0.08 5.60 24   0.015   0.988    
#>   treatment2 - treatment14    -0.92 5.60 24  -0.164   0.871    
#>   treatment2 - treatment15    -2.92 5.60 24  -0.521   0.607    
#>   treatment2 - treatment16   -13.92 5.60 24  -2.486   0.020   *
#>   treatment2 - treatment17    -1.17 5.60 24  -0.208   0.837    
#>   treatment2 - treatment18     2.08 5.60 24   0.372   0.713    
#>   treatment2 - treatment19    -4.92 5.60 24  -0.878   0.389    
#>   treatment2 - treatment20     1.83 5.60 24   0.327   0.746    
#>   treatment2 - treatment21     4.83 5.60 24   0.863   0.396    
#>   treatment2 - treatment22   -11.67 5.60 24  -2.084   0.048   *
#>   treatment2 - treatment23     2.83 5.60 24   0.506   0.617    
#>   treatment2 - treatment24     6.58 5.60 24   1.176   0.251    
#>   treatment2 - treatment25    -4.67 5.60 24  -0.834   0.413    
#>   treatment2 - treatment26     5.58 5.60 24   0.997   0.329    
#>   treatment2 - treatment27    -0.67 5.60 24  -0.119   0.906    
#>   treatment2 - treatment28    10.58 5.60 24   1.890   0.071    
#>    treatment3 - treatment4    -0.56 2.29 24  -0.243   0.810    
#>    treatment3 - treatment5     3.72 5.60 24   0.665   0.512    
#>    treatment3 - treatment6     3.72 5.60 24   0.665   0.512    
#>    treatment3 - treatment7   -11.53 5.60 24  -2.059   0.050    
#>    treatment3 - treatment8     8.72 5.60 24   1.558   0.132    
#>    treatment3 - treatment9     4.72 5.60 24   0.844   0.407    
#>   treatment3 - treatment10    -4.53 5.60 24  -0.809   0.427    
#>   treatment3 - treatment11     2.47 5.60 24   0.442   0.663    
#>   treatment3 - treatment12     4.72 5.60 24   0.844   0.407    
#>   treatment3 - treatment13     1.97 5.60 24   0.352   0.728    
#>   treatment3 - treatment14     0.97 5.60 24   0.174   0.864    
#>   treatment3 - treatment15    -1.03 5.60 24  -0.184   0.856    
#>   treatment3 - treatment16   -12.03 5.60 24  -2.149   0.042   *
#>   treatment3 - treatment17     0.72 5.60 24   0.129   0.898    
#>   treatment3 - treatment18     3.97 5.60 24   0.710   0.485    
#>   treatment3 - treatment19    -3.03 5.60 24  -0.541   0.594    
#>   treatment3 - treatment20     3.72 5.60 24   0.665   0.512    
#>   treatment3 - treatment21     6.72 5.60 24   1.201   0.242    
#>   treatment3 - treatment22    -9.78 5.60 24  -1.747   0.093    
#>   treatment3 - treatment23     4.72 5.60 24   0.844   0.407    
#>   treatment3 - treatment24     8.47 5.60 24   1.513   0.143    
#>   treatment3 - treatment25    -2.78 5.60 24  -0.496   0.624    
#>   treatment3 - treatment26     7.47 5.60 24   1.335   0.194    
#>   treatment3 - treatment27     1.22 5.60 24   0.218   0.829    
#>   treatment3 - treatment28    12.47 5.60 24   2.228   0.036   *
#>    treatment4 - treatment5     4.28 5.60 24   0.764   0.452    
#>    treatment4 - treatment6     4.28 5.60 24   0.764   0.452    
#>    treatment4 - treatment7   -10.97 5.60 24  -1.960   0.062    
#>    treatment4 - treatment8     9.28 5.60 24   1.657   0.110    
#>    treatment4 - treatment9     5.28 5.60 24   0.943   0.355    
#>   treatment4 - treatment10    -3.97 5.60 24  -0.710   0.485    
#>   treatment4 - treatment11     3.03 5.60 24   0.541   0.594    
#>   treatment4 - treatment12     5.28 5.60 24   0.943   0.355    
#>   treatment4 - treatment13     2.53 5.60 24   0.452   0.656    
#>   treatment4 - treatment14     1.53 5.60 24   0.273   0.787    
#>   treatment4 - treatment15    -0.47 5.60 24  -0.084   0.933    
#>   treatment4 - treatment16   -11.47 5.60 24  -2.049   0.052    
#>   treatment4 - treatment17     1.28 5.60 24   0.228   0.821    
#>   treatment4 - treatment18     4.53 5.60 24   0.809   0.427    
#>   treatment4 - treatment19    -2.47 5.60 24  -0.442   0.663    
#>   treatment4 - treatment20     4.28 5.60 24   0.764   0.452    
#>   treatment4 - treatment21     7.28 5.60 24   1.300   0.206    
#>   treatment4 - treatment22    -9.22 5.60 24  -1.647   0.113    
#>   treatment4 - treatment23     5.28 5.60 24   0.943   0.355    
#>   treatment4 - treatment24     9.03 5.60 24   1.613   0.120    
#>   treatment4 - treatment25    -2.22 5.60 24  -0.397   0.695    
#>   treatment4 - treatment26     8.03 5.60 24   1.434   0.164    
#>   treatment4 - treatment27     1.78 5.60 24   0.318   0.754    
#>   treatment4 - treatment28    13.03 5.60 24   2.327   0.029   *
#>    treatment5 - treatment6    -0.00 7.67 24  -0.000   1.000    
#>    treatment5 - treatment7   -15.25 7.67 24  -1.989   0.058    
#>    treatment5 - treatment8     5.00 7.67 24   0.652   0.520    
#>    treatment5 - treatment9     1.00 6.86 24   0.146   0.885    
#>   treatment5 - treatment10    -8.25 7.67 24  -1.076   0.293    
#>   treatment5 - treatment11    -1.25 7.67 24  -0.163   0.872    
#>   treatment5 - treatment12     1.00 7.67 24   0.130   0.897    
#>   treatment5 - treatment13    -1.75 7.67 24  -0.228   0.821    
#>   treatment5 - treatment14    -2.75 7.67 24  -0.359   0.723    
#>   treatment5 - treatment15    -4.75 7.67 24  -0.620   0.541    
#>   treatment5 - treatment16   -15.75 7.67 24  -2.055   0.051    
#>   treatment5 - treatment17    -3.00 7.67 24  -0.391   0.699    
#>   treatment5 - treatment18     0.25 7.67 24   0.033   0.974    
#>   treatment5 - treatment19    -6.75 7.67 24  -0.881   0.387    
#>   treatment5 - treatment20    -0.00 7.67 24  -0.000   1.000    
#>   treatment5 - treatment21     3.00 7.67 24   0.391   0.699    
#>   treatment5 - treatment22   -13.50 7.67 24  -1.761   0.091    
#>   treatment5 - treatment23     1.00 7.67 24   0.130   0.897    
#>   treatment5 - treatment24     4.75 7.67 24   0.620   0.541    
#>   treatment5 - treatment25    -6.50 7.67 24  -0.848   0.405    
#>   treatment5 - treatment26     3.75 7.67 24   0.489   0.629    
#>   treatment5 - treatment27    -2.50 7.67 24  -0.326   0.747    
#>   treatment5 - treatment28     8.75 7.67 24   1.141   0.265    
#>    treatment6 - treatment7   -15.25 7.67 24  -1.989   0.058    
#>    treatment6 - treatment8     5.00 6.86 24   0.729   0.473    
#>    treatment6 - treatment9     1.00 7.67 24   0.130   0.897    
#>   treatment6 - treatment10    -8.25 7.67 24  -1.076   0.293    
#>   treatment6 - treatment11    -1.25 7.67 24  -0.163   0.872    
#>   treatment6 - treatment12     1.00 6.86 24   0.146   0.885    
#>   treatment6 - treatment13    -1.75 7.67 24  -0.228   0.821    
#>   treatment6 - treatment14    -2.75 7.67 24  -0.359   0.723    
#>   treatment6 - treatment15    -4.75 7.67 24  -0.620   0.541    
#>   treatment6 - treatment16   -15.75 7.67 24  -2.055   0.051    
#>   treatment6 - treatment17    -3.00 7.67 24  -0.391   0.699    
#>   treatment6 - treatment18     0.25 7.67 24   0.033   0.974    
#>   treatment6 - treatment19    -6.75 7.67 24  -0.881   0.387    
#>   treatment6 - treatment20    -0.00 7.67 24  -0.000   1.000    
#>   treatment6 - treatment21     3.00 7.67 24   0.391   0.699    
#>   treatment6 - treatment22   -13.50 7.67 24  -1.761   0.091    
#>   treatment6 - treatment23     1.00 7.67 24   0.130   0.897    
#>   treatment6 - treatment24     4.75 7.67 24   0.620   0.541    
#>   treatment6 - treatment25    -6.50 7.67 24  -0.848   0.405    
#>   treatment6 - treatment26     3.75 7.67 24   0.489   0.629    
#>   treatment6 - treatment27    -2.50 7.67 24  -0.326   0.747    
#>   treatment6 - treatment28     8.75 7.67 24   1.141   0.265    
#>    treatment7 - treatment8    20.25 7.67 24   2.642   0.014   *
#>    treatment7 - treatment9    16.25 7.67 24   2.120   0.045   *
#>   treatment7 - treatment10     7.00 6.86 24   1.021   0.317    
#>   treatment7 - treatment11    14.00 6.86 24   2.042   0.052    
#>   treatment7 - treatment12    16.25 7.67 24   2.120   0.045   *
#>   treatment7 - treatment13    13.50 7.67 24   1.761   0.091    
#>   treatment7 - treatment14    12.50 7.67 24   1.631   0.116    
#>   treatment7 - treatment15    10.50 7.67 24   1.370   0.183    
#>   treatment7 - treatment16    -0.50 7.67 24  -0.065   0.949    
#>   treatment7 - treatment17    12.25 7.67 24   1.598   0.123    
#>   treatment7 - treatment18    15.50 7.67 24   2.022   0.054    
#>   treatment7 - treatment19     8.50 7.67 24   1.109   0.278    
#>   treatment7 - treatment20    15.25 7.67 24   1.989   0.058    
#>   treatment7 - treatment21    18.25 7.67 24   2.381   0.026   *
#>   treatment7 - treatment22     1.75 7.67 24   0.228   0.821    
#>   treatment7 - treatment23    16.25 7.67 24   2.120   0.045   *
#>   treatment7 - treatment24    20.00 7.67 24   2.609   0.015   *
#>   treatment7 - treatment25     8.75 7.67 24   1.141   0.265    
#>   treatment7 - treatment26    19.00 7.67 24   2.479   0.021   *
#>   treatment7 - treatment27    12.75 7.67 24   1.663   0.109    
#>   treatment7 - treatment28    24.00 7.67 24   3.131   0.005  **
#>    treatment8 - treatment9    -4.00 7.67 24  -0.522   0.607    
#>   treatment8 - treatment10   -13.25 7.67 24  -1.728   0.097    
#>   treatment8 - treatment11    -6.25 7.67 24  -0.815   0.423    
#>   treatment8 - treatment12    -4.00 6.86 24  -0.583   0.565    
#>   treatment8 - treatment13    -6.75 7.67 24  -0.881   0.387    
#>   treatment8 - treatment14    -7.75 7.67 24  -1.011   0.322    
#>   treatment8 - treatment15    -9.75 7.67 24  -1.272   0.216    
#>   treatment8 - treatment16   -20.75 7.67 24  -2.707   0.012   *
#>   treatment8 - treatment17    -8.00 7.67 24  -1.044   0.307    
#>   treatment8 - treatment18    -4.75 7.67 24  -0.620   0.541    
#>   treatment8 - treatment19   -11.75 7.67 24  -1.533   0.138    
#>   treatment8 - treatment20    -5.00 7.67 24  -0.652   0.520    
#>   treatment8 - treatment21    -2.00 7.67 24  -0.261   0.796    
#>   treatment8 - treatment22   -18.50 7.67 24  -2.413   0.024   *
#>   treatment8 - treatment23    -4.00 7.67 24  -0.522   0.607    
#>   treatment8 - treatment24    -0.25 7.67 24  -0.033   0.974    
#>   treatment8 - treatment25   -11.50 7.67 24  -1.500   0.147    
#>   treatment8 - treatment26    -1.25 7.67 24  -0.163   0.872    
#>   treatment8 - treatment27    -7.50 7.67 24  -0.978   0.338    
#>   treatment8 - treatment28     3.75 7.67 24   0.489   0.629    
#>   treatment9 - treatment10    -9.25 7.67 24  -1.207   0.239    
#>   treatment9 - treatment11    -2.25 7.67 24  -0.294   0.772    
#>   treatment9 - treatment12    -0.00 7.67 24  -0.000   1.000    
#>   treatment9 - treatment13    -2.75 7.67 24  -0.359   0.723    
#>   treatment9 - treatment14    -3.75 7.67 24  -0.489   0.629    
#>   treatment9 - treatment15    -5.75 7.67 24  -0.750   0.460    
#>   treatment9 - treatment16   -16.75 7.67 24  -2.185   0.039   *
#>   treatment9 - treatment17    -4.00 7.67 24  -0.522   0.607    
#>   treatment9 - treatment18    -0.75 7.67 24  -0.098   0.923    
#>   treatment9 - treatment19    -7.75 7.67 24  -1.011   0.322    
#>   treatment9 - treatment20    -1.00 7.67 24  -0.130   0.897    
#>   treatment9 - treatment21     2.00 7.67 24   0.261   0.796    
#>   treatment9 - treatment22   -14.50 7.67 24  -1.892   0.071    
#>   treatment9 - treatment23    -0.00 7.67 24  -0.000   1.000    
#>   treatment9 - treatment24     3.75 7.67 24   0.489   0.629    
#>   treatment9 - treatment25    -7.50 7.67 24  -0.978   0.338    
#>   treatment9 - treatment26     2.75 7.67 24   0.359   0.723    
#>   treatment9 - treatment27    -3.50 7.67 24  -0.457   0.652    
#>   treatment9 - treatment28     7.75 7.67 24   1.011   0.322    
#>  treatment10 - treatment11     7.00 6.86 24   1.021   0.317    
#>  treatment10 - treatment12     9.25 7.67 24   1.207   0.239    
#>  treatment10 - treatment13     6.50 7.67 24   0.848   0.405    
#>  treatment10 - treatment14     5.50 7.67 24   0.717   0.480    
#>  treatment10 - treatment15     3.50 7.67 24   0.457   0.652    
#>  treatment10 - treatment16    -7.50 7.67 24  -0.978   0.338    
#>  treatment10 - treatment17     5.25 7.67 24   0.685   0.500    
#>  treatment10 - treatment18     8.50 7.67 24   1.109   0.278    
#>  treatment10 - treatment19     1.50 7.67 24   0.196   0.847    
#>  treatment10 - treatment20     8.25 7.67 24   1.076   0.293    
#>  treatment10 - treatment21    11.25 7.67 24   1.468   0.155    
#>  treatment10 - treatment22    -5.25 7.67 24  -0.685   0.500    
#>  treatment10 - treatment23     9.25 7.67 24   1.207   0.239    
#>  treatment10 - treatment24    13.00 7.67 24   1.696   0.103    
#>  treatment10 - treatment25     1.75 7.67 24   0.228   0.821    
#>  treatment10 - treatment26    12.00 7.67 24   1.565   0.131    
#>  treatment10 - treatment27     5.75 7.67 24   0.750   0.460    
#>  treatment10 - treatment28    17.00 7.67 24   2.218   0.036   *
#>  treatment11 - treatment12     2.25 7.67 24   0.294   0.772    
#>  treatment11 - treatment13    -0.50 7.67 24  -0.065   0.949    
#>  treatment11 - treatment14    -1.50 7.67 24  -0.196   0.847    
#>  treatment11 - treatment15    -3.50 7.67 24  -0.457   0.652    
#>  treatment11 - treatment16   -14.50 7.67 24  -1.892   0.071    
#>  treatment11 - treatment17    -1.75 7.67 24  -0.228   0.821    
#>  treatment11 - treatment18     1.50 7.67 24   0.196   0.847    
#>  treatment11 - treatment19    -5.50 7.67 24  -0.717   0.480    
#>  treatment11 - treatment20     1.25 7.67 24   0.163   0.872    
#>  treatment11 - treatment21     4.25 7.67 24   0.554   0.584    
#>  treatment11 - treatment22   -12.25 7.67 24  -1.598   0.123    
#>  treatment11 - treatment23     2.25 7.67 24   0.294   0.772    
#>  treatment11 - treatment24     6.00 7.67 24   0.783   0.441    
#>  treatment11 - treatment25    -5.25 7.67 24  -0.685   0.500    
#>  treatment11 - treatment26     5.00 7.67 24   0.652   0.520    
#>  treatment11 - treatment27    -1.25 7.67 24  -0.163   0.872    
#>  treatment11 - treatment28    10.00 7.67 24   1.305   0.204    
#>  treatment12 - treatment13    -2.75 7.67 24  -0.359   0.723    
#>  treatment12 - treatment14    -3.75 7.67 24  -0.489   0.629    
#>  treatment12 - treatment15    -5.75 7.67 24  -0.750   0.460    
#>  treatment12 - treatment16   -16.75 7.67 24  -2.185   0.039   *
#>  treatment12 - treatment17    -4.00 7.67 24  -0.522   0.607    
#>  treatment12 - treatment18    -0.75 7.67 24  -0.098   0.923    
#>  treatment12 - treatment19    -7.75 7.67 24  -1.011   0.322    
#>  treatment12 - treatment20    -1.00 7.67 24  -0.130   0.897    
#>  treatment12 - treatment21     2.00 7.67 24   0.261   0.796    
#>  treatment12 - treatment22   -14.50 7.67 24  -1.892   0.071    
#>  treatment12 - treatment23    -0.00 7.67 24  -0.000   1.000    
#>  treatment12 - treatment24     3.75 7.67 24   0.489   0.629    
#>  treatment12 - treatment25    -7.50 7.67 24  -0.978   0.338    
#>  treatment12 - treatment26     2.75 7.67 24   0.359   0.723    
#>  treatment12 - treatment27    -3.50 7.67 24  -0.457   0.652    
#>  treatment12 - treatment28     7.75 7.67 24   1.011   0.322    
#>  treatment13 - treatment14    -1.00 7.67 24  -0.130   0.897    
#>  treatment13 - treatment15    -3.00 7.67 24  -0.391   0.699    
#>  treatment13 - treatment16   -14.00 6.86 24  -2.042   0.052    
#>  treatment13 - treatment17    -1.25 7.67 24  -0.163   0.872    
#>  treatment13 - treatment18     2.00 7.67 24   0.261   0.796    
#>  treatment13 - treatment19    -5.00 6.86 24  -0.729   0.473    
#>  treatment13 - treatment20     1.75 7.67 24   0.228   0.821    
#>  treatment13 - treatment21     4.75 7.67 24   0.620   0.541    
#>  treatment13 - treatment22   -11.75 7.67 24  -1.533   0.138    
#>  treatment13 - treatment23     2.75 7.67 24   0.359   0.723    
#>  treatment13 - treatment24     6.50 7.67 24   0.848   0.405    
#>  treatment13 - treatment25    -4.75 7.67 24  -0.620   0.541    
#>  treatment13 - treatment26     5.50 7.67 24   0.717   0.480    
#>  treatment13 - treatment27    -0.75 7.67 24  -0.098   0.923    
#>  treatment13 - treatment28    10.50 7.67 24   1.370   0.183    
#>  treatment14 - treatment15    -2.00 6.86 24  -0.292   0.773    
#>  treatment14 - treatment16   -13.00 7.67 24  -1.696   0.103    
#>  treatment14 - treatment17    -0.25 7.67 24  -0.033   0.974    
#>  treatment14 - treatment18     3.00 6.86 24   0.438   0.666    
#>  treatment14 - treatment19    -4.00 7.67 24  -0.522   0.607    
#>  treatment14 - treatment20     2.75 7.67 24   0.359   0.723    
#>  treatment14 - treatment21     5.75 7.67 24   0.750   0.460    
#>  treatment14 - treatment22   -10.75 7.67 24  -1.402   0.174    
#>  treatment14 - treatment23     3.75 7.67 24   0.489   0.629    
#>  treatment14 - treatment24     7.50 7.67 24   0.978   0.338    
#>  treatment14 - treatment25    -3.75 7.67 24  -0.489   0.629    
#>  treatment14 - treatment26     6.50 7.67 24   0.848   0.405    
#>  treatment14 - treatment27     0.25 7.67 24   0.033   0.974    
#>  treatment14 - treatment28    11.50 7.67 24   1.500   0.147    
#>  treatment15 - treatment16   -11.00 7.67 24  -1.435   0.164    
#>  treatment15 - treatment17     1.75 7.67 24   0.228   0.821    
#>  treatment15 - treatment18     5.00 6.86 24   0.729   0.473    
#>  treatment15 - treatment19    -2.00 7.67 24  -0.261   0.796    
#>  treatment15 - treatment20     4.75 7.67 24   0.620   0.541    
#>  treatment15 - treatment21     7.75 7.67 24   1.011   0.322    
#>  treatment15 - treatment22    -8.75 7.67 24  -1.141   0.265    
#>  treatment15 - treatment23     5.75 7.67 24   0.750   0.460    
#>  treatment15 - treatment24     9.50 7.67 24   1.239   0.227    
#>  treatment15 - treatment25    -1.75 7.67 24  -0.228   0.821    
#>  treatment15 - treatment26     8.50 7.67 24   1.109   0.278    
#>  treatment15 - treatment27     2.25 7.67 24   0.294   0.772    
#>  treatment15 - treatment28    13.50 7.67 24   1.761   0.091    
#>  treatment16 - treatment17    12.75 7.67 24   1.663   0.109    
#>  treatment16 - treatment18    16.00 7.67 24   2.087   0.048   *
#>  treatment16 - treatment19     9.00 6.86 24   1.313   0.202    
#>  treatment16 - treatment20    15.75 7.67 24   2.055   0.051    
#>  treatment16 - treatment21    18.75 7.67 24   2.446   0.022   *
#>  treatment16 - treatment22     2.25 7.67 24   0.294   0.772    
#>  treatment16 - treatment23    16.75 7.67 24   2.185   0.039   *
#>  treatment16 - treatment24    20.50 7.67 24   2.674   0.013   *
#>  treatment16 - treatment25     9.25 7.67 24   1.207   0.239    
#>  treatment16 - treatment26    19.50 7.67 24   2.544   0.018   *
#>  treatment16 - treatment27    13.25 7.67 24   1.728   0.097    
#>  treatment16 - treatment28    24.50 7.67 24   3.196   0.004  **
#>  treatment17 - treatment18     3.25 7.67 24   0.424   0.675    
#>  treatment17 - treatment19    -3.75 7.67 24  -0.489   0.629    
#>  treatment17 - treatment20     3.00 6.86 24   0.438   0.666    
#>  treatment17 - treatment21     6.00 7.67 24   0.783   0.441    
#>  treatment17 - treatment22   -10.50 7.67 24  -1.370   0.183    
#>  treatment17 - treatment23     4.00 7.67 24   0.522   0.607    
#>  treatment17 - treatment24     7.75 7.67 24   1.011   0.322    
#>  treatment17 - treatment25    -3.50 7.67 24  -0.457   0.652    
#>  treatment17 - treatment26     6.75 7.67 24   0.881   0.387    
#>  treatment17 - treatment27     0.50 7.67 24   0.065   0.949    
#>  treatment17 - treatment28    11.75 7.67 24   1.533   0.138    
#>  treatment18 - treatment19    -7.00 7.67 24  -0.913   0.370    
#>  treatment18 - treatment20    -0.25 7.67 24  -0.033   0.974    
#>  treatment18 - treatment21     2.75 7.67 24   0.359   0.723    
#>  treatment18 - treatment22   -13.75 7.67 24  -1.794   0.085    
#>  treatment18 - treatment23     0.75 7.67 24   0.098   0.923    
#>  treatment18 - treatment24     4.50 7.67 24   0.587   0.563    
#>  treatment18 - treatment25    -6.75 7.67 24  -0.881   0.387    
#>  treatment18 - treatment26     3.50 7.67 24   0.457   0.652    
#>  treatment18 - treatment27    -2.75 7.67 24  -0.359   0.723    
#>  treatment18 - treatment28     8.50 7.67 24   1.109   0.278    
#>  treatment19 - treatment20     6.75 7.67 24   0.881   0.387    
#>  treatment19 - treatment21     9.75 7.67 24   1.272   0.216    
#>  treatment19 - treatment22    -6.75 7.67 24  -0.881   0.387    
#>  treatment19 - treatment23     7.75 7.67 24   1.011   0.322    
#>  treatment19 - treatment24    11.50 7.67 24   1.500   0.147    
#>  treatment19 - treatment25     0.25 7.67 24   0.033   0.974    
#>  treatment19 - treatment26    10.50 7.67 24   1.370   0.183    
#>  treatment19 - treatment27     4.25 7.67 24   0.554   0.584    
#>  treatment19 - treatment28    15.50 7.67 24   2.022   0.054    
#>  treatment20 - treatment21     3.00 7.67 24   0.391   0.699    
#>  treatment20 - treatment22   -13.50 7.67 24  -1.761   0.091    
#>  treatment20 - treatment23     1.00 7.67 24   0.130   0.897    
#>  treatment20 - treatment24     4.75 7.67 24   0.620   0.541    
#>  treatment20 - treatment25    -6.50 7.67 24  -0.848   0.405    
#>  treatment20 - treatment26     3.75 7.67 24   0.489   0.629    
#>  treatment20 - treatment27    -2.50 7.67 24  -0.326   0.747    
#>  treatment20 - treatment28     8.75 7.67 24   1.141   0.265    
#>  treatment21 - treatment22   -16.50 7.67 24  -2.152   0.042   *
#>  treatment21 - treatment23    -2.00 6.86 24  -0.292   0.773    
#>  treatment21 - treatment24     1.75 7.67 24   0.228   0.821    
#>  treatment21 - treatment25    -9.50 7.67 24  -1.239   0.227    
#>  treatment21 - treatment26     0.75 7.67 24   0.098   0.923    
#>  treatment21 - treatment27    -5.50 7.67 24  -0.717   0.480    
#>  treatment21 - treatment28     5.75 7.67 24   0.750   0.460    
#>  treatment22 - treatment23    14.50 7.67 24   1.892   0.071    
#>  treatment22 - treatment24    18.25 7.67 24   2.381   0.026   *
#>  treatment22 - treatment25     7.00 6.86 24   1.021   0.317    
#>  treatment22 - treatment26    17.25 7.67 24   2.250   0.034   *
#>  treatment22 - treatment27    11.00 6.86 24   1.604   0.122    
#>  treatment22 - treatment28    22.25 7.67 24   2.903   0.008  **
#>  treatment23 - treatment24     3.75 7.67 24   0.489   0.629    
#>  treatment23 - treatment25    -7.50 7.67 24  -0.978   0.338    
#>  treatment23 - treatment26     2.75 7.67 24   0.359   0.723    
#>  treatment23 - treatment27    -3.50 7.67 24  -0.457   0.652    
#>  treatment23 - treatment28     7.75 7.67 24   1.011   0.322    
#>  treatment24 - treatment25   -11.25 7.67 24  -1.468   0.155    
#>  treatment24 - treatment26    -1.00 6.86 24  -0.146   0.885    
#>  treatment24 - treatment27    -7.25 7.67 24  -0.946   0.354    
#>  treatment24 - treatment28     4.00 6.86 24   0.583   0.565    
#>  treatment25 - treatment26    10.25 7.67 24   1.337   0.194    
#>  treatment25 - treatment27     4.00 6.86 24   0.583   0.565    
#>  treatment25 - treatment28    15.25 7.67 24   1.989   0.058    
#>  treatment26 - treatment27    -6.25 7.67 24  -0.815   0.423    
#>  treatment26 - treatment28     5.00 6.86 24   0.729   0.473    
#>  treatment27 - treatment28    11.25 7.67 24   1.468   0.155    
#> 
#> Treatment Groups
#> ================
#> 
#> Method : lsd
#> 
#>  Treatment Adjusted Means   SE df lower.CL upper.CL  Group
#>         28          68.75 5.36 24    57.69    79.81  1    
#>          8          72.50 5.36 24    61.44    83.56  12   
#>         24          72.75 5.36 24    61.69    83.81  12   
#>         26          73.75 5.36 24    62.69    84.81  12   
#>         21          74.50 5.36 24    63.44    85.56  12   
#>          9          76.50 5.36 24    65.44    87.56  123  
#>         12          76.50 5.36 24    65.44    87.56  123  
#>         23          76.50 5.36 24    65.44    87.56  123  
#>         18          77.25 5.36 24    66.19    88.31  1234 
#>          5          77.50 5.36 24    66.44    88.56  12345
#>          6          77.50 5.36 24    66.44    88.56  12345
#>         20          77.50 5.36 24    66.44    88.56  12345
#>         11          78.75 5.36 24    67.69    89.81  12345
#>         13          79.25 5.36 24    68.19    90.31  12345
#>          2          79.33 1.62 24    76.00    82.67  12   
#>         27          80.00 5.36 24    68.94    91.06  12345
#>         14          80.25 5.36 24    69.19    91.31  12345
#>         17          80.50 5.36 24    69.44    91.56  12345
#>          3          81.22 1.62 24    77.89    84.56   234 
#>          4          81.78 1.62 24    78.44    85.11   2345
#>         15          82.25 5.36 24    71.19    93.31  12345
#>          1          83.67 1.62 24    80.33    87.00   2345
#>         25          84.00 5.36 24    72.94    95.06  12345
#>         19          84.25 5.36 24    73.19    95.31  12345
#>         10          85.75 5.36 24    74.69    96.81   2345
#>         22          91.00 5.36 24    79.94   102.06    345
#>          7          92.75 5.36 24    81.69   103.81     45
#>         16          93.25 5.36 24    82.19   104.31      5
```
