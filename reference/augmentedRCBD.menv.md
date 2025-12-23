# Combined Analysis of Augmented Randomised Complete Block Design in Multiple Environments

`augmentedRCBD.menv` is an extension of `augmentedRCBD` for the
combined/pooled analysis of data from augmented randomised complete
block design across multiple environments (locations and/or seasons)
under the following two scenarios.

- *Scenario 1*:

  Test treatments replicated across environments.

- *Scenario 1*:

  Test treatments are not replicated across environments.

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

  Either `1` or `2` (see **Description above**).

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
                           scenario = 2, method.comp = "lsd", alpha = 0.05,
                           group = TRUE, console = TRUE)
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> Details :
#> $`Number of blocks`
#> [1] 9
#> 
#> $`Number of treatments`
#> [1] 12
#> 
#> $`Number of environments`
#> [1] 3
#> 
#> $`Number of check treatments`
#> [1] 4
#> 
#> $`Number of test treatments`
#> [1] 8
#> 
#> $`Check treatments`
#> [1] "1" "2" "3" "4"
#> 
#> 
#> Means :
#>    treatment    Means        SE r Min Max Adjusted Means
#> 1          1 83.66667 1.9220938 9  78  92       83.66667
#> 2         10 71.66667 1.4529663 3  69  74       75.67979
#> 3         11 83.00000 4.5825757 3  74  89       80.81989
#> 4         12 80.33333 2.1858128 3  76  83       77.77646
#> 5          2 79.33333 0.7264832 9  76  83       79.33333
#> 6          3 81.22222 1.6139821 9  75  87       81.22222
#> 7          4 81.77778 2.1001176 9  74  91       81.77778
#> 8          5 78.00000 2.0816660 3  74  81       76.54375
#> 9          6 75.33333 0.3333333 3  75  76       78.82917
#> 10         7 89.00000 6.0277138 3  77  96       87.82774
#> 11         8 79.33333 7.8810603 3  70  95       79.96513
#> 12         9 81.33333 2.8480012 3  78  87       80.01761
#> 
#> ANOVA, Treatment Adjusted :
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (eliminating Blocks)                            11  691.2   62.83
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test and Test vs. Check                       8  605.3   75.66
#> Block (within Environment, ignoring Treatments)            6  672.5  112.08
#> Environment × Treatment interaction                       22  398.7   18.12
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (eliminating Blocks)                              2.070 0.0824 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test and Test vs. Check                        2.492 0.0513 .
#> Block (within Environment, ignoring Treatments)             3.692 0.0144 *
#> Environment × Treatment interaction                         0.597 0.8753  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> ANOVA, Block Adjusted :
#>                                                           Df Sum Sq Mean Sq
#> Environment                                                2   24.7   12.35
#> Treatment (ignoring Blocks)                               11  691.2   62.83
#>   Treatment: Check                                         3   85.9   28.63
#>   Treatment: Test                                          7  561.2   80.17
#>   Treatment: Test vs. Check                                1   44.1   44.10
#> Block (within Environment, eliminating Treatments)         6  672.5  112.08
#> Environment × Treatment interaction                       22  398.7   18.12
#> Residuals                                                 18  546.5   30.36
#>                                                           F value Pr(>F)  
#> Environment                                                 0.407 0.6718  
#> Treatment (ignoring Blocks)                                 2.070 0.0824 .
#>   Treatment: Check                                          0.943 0.4406  
#>   Treatment: Test                                           2.640 0.0459 *
#>   Treatment: Test vs. Check                                 1.453 0.2437  
#> Block (within Environment, eliminating Treatments)          3.692 0.0144 *
#> Environment × Treatment interaction                         0.597 0.8753  
#> Residuals                                                                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Block effects :
#>       1_1       1_2       1_3       2_4       2_5       2_6       3_7       3_8 
#>  4.796558  1.073107 -3.639155  4.524462 -1.465678 -5.619529  4.339741 -1.228820 
#>       3_9 
#> -2.780686 
#> attr(,"overall")
#> [1] 80.28829
#> 
#> Treatment effects :
#>          1          2          3          4          5          6          7 
#>  3.3783720 -0.9549613  0.9339275  1.4894831 -3.7445443 -1.4591269  7.5394484 
#>          8          9         10         11         12 
#> -0.3231683 -0.2706844 -4.6085047  0.5315940 -2.5118350 
#> attr(,"overall")
#> [1] 80.28829
#> 
#> Std. Errors :
#>                                          Std. Error of Diff. CD (5%)
#> Control Treatment Means                                   NA      NA
#> Two Test Treatments (Same Block)                          NA      NA
#> Two Test Treatments (Different Blocks)                    NA      NA
#> A Test Treatment and a Control Treatment                  NA      NA
#> 
#> Overall adjusted mean :
#> [1] 80.28829
#> 
#> CV :
#> [1] NA
#> 
#> Comparison method :
#> [1] "lsd"
#> 
#> Comparisons :
#>                     contrast     estimate       SE df     t.ratio   p.value
#> 1    treatment1 - treatment2   4.33333333 2.291591 40  1.89097178 0.7575477
#> 2    treatment1 - treatment3   2.44444444 2.291591 40  1.06670203 0.9945433
#> 3    treatment1 - treatment4   1.88888889 2.291591 40  0.82426975 0.9994425
#> 4    treatment1 - treatment5   7.12291632 3.390279 40  2.10098217 0.6249498
#> 5    treatment1 - treatment6   4.83749891 3.390989 40  1.42657463 0.9509357
#> 6    treatment1 - treatment7  -4.16107644 3.381748 40 -1.23045131 0.9829691
#> 7    treatment1 - treatment8   3.70154025 3.391475 40  1.09142484 0.9934050
#> 8    treatment1 - treatment9   3.64905642 3.401221 40  1.07286645 0.9942756
#> 9   treatment1 - treatment10   7.98687666 3.391497 40  2.35497088 0.4575174
#> 10  treatment1 - treatment11   2.84677800 3.381724 40  0.84181252 0.9993222
#> 11  treatment1 - treatment12   5.89020702 3.400703 40  1.73205585 0.8427258
#> 12   treatment2 - treatment3  -1.88888889 2.291591 40 -0.82426975 0.9994425
#> 13   treatment2 - treatment4  -2.44444444 2.291591 40 -1.06670203 0.9945433
#> 14   treatment2 - treatment5   2.78958299 3.390279 40  0.82281805 0.9994515
#> 15   treatment2 - treatment6   0.50416558 3.390989 40  0.14867803 1.0000000
#> 16   treatment2 - treatment7  -8.49440978 3.381748 40 -2.51183986 0.3620603
#> 17   treatment2 - treatment8  -0.63179308 3.391475 40 -0.18628857 1.0000000
#> 18   treatment2 - treatment9  -0.68427692 3.401221 40 -0.20118564 1.0000000
#> 19  treatment2 - treatment10   3.65354333 3.391497 40  1.07726568 0.9940782
#> 20  treatment2 - treatment11  -1.48655533 3.381724 40 -0.43958499 0.9999990
#> 21  treatment2 - treatment12   1.55687368 3.400703 40  0.45780941 0.9999984
#> 22   treatment3 - treatment4  -0.55555556 2.291591 40 -0.24243228 1.0000000
#> 23   treatment3 - treatment5   4.67847188 3.390279 40  1.37996651 0.9608529
#> 24   treatment3 - treatment6   2.39305447 3.390989 40  0.70570988 0.9998730
#> 25   treatment3 - treatment7  -6.60552089 3.381748 40 -1.95328588 0.7199751
#> 26   treatment3 - treatment8   1.25709581 3.391475 40  0.37066342 0.9999998
#> 27   treatment3 - treatment9   1.20461197 3.401221 40  0.35417040 0.9999999
#> 28  treatment3 - treatment10   5.54243222 3.391497 40  1.63421410 0.8858003
#> 29  treatment3 - treatment11   0.40233356 3.381724 40  0.11897290 1.0000000
#> 30  treatment3 - treatment12   3.44576257 3.400703 40  1.01325017 0.9964637
#> 31   treatment4 - treatment5   5.23402743 3.390279 40  1.54383371 0.9185305
#> 32   treatment4 - treatment6   2.94861002 3.390989 40  0.86954278 0.9990871
#> 33   treatment4 - treatment7  -6.04996533 3.381748 40 -1.78900530 0.8142007
#> 34   treatment4 - treatment8   1.81265136 3.391475 40  0.53447284 0.9999921
#> 35   treatment4 - treatment9   1.76016753 3.401221 40  0.51751041 0.9999944
#> 36  treatment4 - treatment10   6.09798777 3.391497 40  1.79802246 0.8094670
#> 37  treatment4 - treatment11   0.95788911 3.381724 40  0.28325463 1.0000000
#> 38  treatment4 - treatment12   4.00131813 3.400703 40  1.17661510 0.9879414
#> 39   treatment5 - treatment6  -2.28541741 4.204286 40 -0.54359233 0.9999907
#> 40   treatment5 - treatment7 -11.28399277 4.326977 40 -2.60782331 0.3092058
#> 41   treatment5 - treatment8  -3.42137607 4.211950 40 -0.81230219 0.9995136
#> 42   treatment5 - treatment9  -3.47385991 4.221229 40 -0.82294979 0.9994507
#> 43  treatment5 - treatment10   0.86396034 4.321290 40  0.19993111 1.0000000
#> 44  treatment5 - treatment11  -4.27613832 4.211436 40 -1.01536341 0.9964002
#> 45  treatment5 - treatment12  -1.23270931 4.342940 40 -0.28384211 1.0000000
#> 46   treatment6 - treatment7  -8.99857536 4.213051 40 -2.13588112 0.6017899
#> 47   treatment6 - treatment8  -1.13595866 4.196834 40 -0.27067039 1.0000000
#> 48   treatment6 - treatment9  -1.18844250 4.352471 40 -0.27305006 1.0000000
#> 49  treatment6 - treatment10   3.14937775 4.085295 40  0.77090574 0.9997029
#> 50  treatment6 - treatment11  -1.99072091 4.306653 40 -0.46224313 0.9999983
#> 51  treatment6 - treatment12   1.05270810 4.351738 40  0.24190521 1.0000000
#> 52   treatment7 - treatment8   7.86261670 4.315642 40  1.82188806 0.7966665
#> 53   treatment7 - treatment9   7.81013286 4.205532 40  1.85710953 0.7770878
#> 54  treatment7 - treatment10  12.14795310 4.206537 40  2.88787534 0.1840837
#> 55  treatment7 - treatment11   7.00785445 4.197783 40  1.66941808 0.8711998
#> 56  treatment7 - treatment12  10.05128346 4.086299 40  2.45975241 0.3926311
#> 57   treatment8 - treatment9  -0.05248384 4.358937 40 -0.01204051 1.0000000
#> 58  treatment8 - treatment10   4.28533641 4.078551 40  1.05070079 0.9951907
#> 59  treatment8 - treatment11  -0.85476225 4.079409 40 -0.20953093 1.0000000
#> 60  treatment8 - treatment12   2.18866676 4.351790 40  0.50293484 0.9999958
#> 61  treatment9 - treatment10   4.33782024 4.359247 40  0.99508482 0.9969726
#> 62  treatment9 - treatment11  -0.80227841 4.330793 40 -0.18524978 1.0000000
#> 63  treatment9 - treatment12   2.24115060 4.085336 40  0.54858410 0.9999897
#> 64 treatment10 - treatment11  -5.14009866 4.190835 40 -1.22650952 0.9833796
#> 65 treatment10 - treatment12  -2.09666964 4.345792 40 -0.48245978 0.9999973
#> 66 treatment11 - treatment12   3.04342901 4.220871 40  0.72104291 0.9998436
#> 
#> Groups :
#>    treatment   emmean       SE df lower.CL upper.CL .group
#> 10        10 75.67979 2.979355 40 66.65044 84.70914      1
#> 5          5 76.54375 2.977969 40 67.51860 85.56890      1
#> 12        12 77.77646 2.989830 40 68.71536 86.83756      1
#> 6          6 78.82917 2.978777 40 69.80157 87.85677      1
#> 2          2 79.33333 1.620399 40 74.42249 84.24418      1
#> 8          8 79.96513 2.979330 40 70.93585 88.99440      1
#> 9          9 80.01761 2.990420 40 70.95473 89.08049      1
#> 11        11 80.81989 2.968226 40 71.82427 89.81551      1
#> 3          3 81.22222 1.620399 40 76.31138 86.13307      1
#> 4          4 81.77778 1.620399 40 76.86693 86.68862      1
#> 1          1 83.66667 1.620399 40 78.75582 88.57751      1
#> 7          7 87.82774 2.968253 40 78.83204 96.82345      1
#> 
#> warnings :
#> NULL
#> 

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
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> Details :
#> $`Number of blocks`
#> [1] 9
#> 
#> $`Number of treatments`
#> [1] 28
#> 
#> $`Number of environments`
#> [1] 3
#> 
#> $`Number of check treatments`
#> [1] 4
#> 
#> $`Number of test treatments`
#> [1] 24
#> 
#> $`Check treatments`
#> [1] "1" "2" "3" "4"
#> 
#> 
#> Means :
#>    treatment    Means        SE r Min Max Adjusted Means
#> 1          1 83.66667 1.9220938 9  78  92       83.66667
#> 2         10 89.00000        NA 1  89  89       85.75000
#> 3         11 82.00000        NA 1  82  82       78.75000
#> 4         12 74.00000        NA 1  74  74       76.50000
#> 5         13 81.00000        NA 1  81  81       79.25000
#> 6         14 75.00000        NA 1  75  75       80.25000
#> 7         15 77.00000        NA 1  77  77       82.25000
#> 8         16 95.00000        NA 1  95  95       93.25000
#> 9         17 79.00000        NA 1  79  79       80.50000
#> 10        18 72.00000        NA 1  72  72       77.25000
#> 11        19 86.00000        NA 1  86  86       84.25000
#> 12         2 79.33333 0.7264832 9  76  83       79.33333
#> 13        20 76.00000        NA 1  76  76       77.50000
#> 14        21 74.00000        NA 1  74  74       74.50000
#> 15        22 94.00000        NA 1  94  94       91.00000
#> 16        23 76.00000        NA 1  76  76       76.50000
#> 17        24 73.00000        NA 1  73  73       72.75000
#> 18        25 87.00000        NA 1  87  87       84.00000
#> 19        26 74.00000        NA 1  74  74       73.75000
#> 20        27 83.00000        NA 1  83  83       80.00000
#> 21        28 69.00000        NA 1  69  69       68.75000
#> 22         3 81.22222 1.6139821 9  75  87       81.22222
#> 23         4 81.77778 2.1001176 9  74  91       81.77778
#> 24         5 79.00000        NA 1  79  79       77.50000
#> 25         6 75.00000        NA 1  75  75       77.50000
#> 26         7 96.00000        NA 1  96  96       92.75000
#> 27         8 70.00000        NA 1  70  70       72.50000
#> 28         9 78.00000        NA 1  78  78       76.50000
#> 
#> ANOVA, Treatment Adjusted :
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
#> ANOVA, Block Adjusted :
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
#> Block effects :
#>   1_1   1_2   1_3   2_4   2_5   2_6   3_7   3_8   3_9 
#>  3.25  1.50 -2.50  1.75 -1.50 -5.25  3.00 -0.50  0.25 
#> attr(,"overall")
#> [1] 79.98214
#> 
#> Treatment effects :
#>            1            2            3            4            5            6 
#>   3.68452381  -0.64880952   1.24007937   1.79563492  -2.48214286  -2.48214286 
#>            7            8            9           10           11           12 
#>  12.76785714  -7.48214286  -3.48214286   5.76785714  -1.23214286  -3.48214286 
#>           13           14           15           16           17           18 
#>  -0.73214286   0.26785714   2.26785714  13.26785714   0.51785714  -2.73214286 
#>           19           20           21           22           23           24 
#>   4.26785714  -2.48214286  -5.48214286  11.01785714  -3.48214286  -7.23214286 
#>           25           26           27           28 
#>   4.01785714  -6.23214286   0.01785714 -11.23214286 
#> attr(,"overall")
#> [1] 79.98214
#> 
#> Std. Errors :
#>                                          Std. Error of Diff. CD (5%)
#> Control Treatment Means                                   NA      NA
#> Two Test Treatments (Same Block)                          NA      NA
#> Two Test Treatments (Different Blocks)                    NA      NA
#> A Test Treatment and a Control Treatment                  NA      NA
#> 
#> Overall adjusted mean :
#> [1] 79.98214
#> 
#> CV :
#> [1] NA
#> 
#> Comparison method :
#> [1] "lsd"
#> 
#> Comparisons :
#>                      contrast      estimate       SE df       t.ratio   p.value
#> 1     treatment1 - treatment2  4.333333e+00 2.285443 24  1.896058e+00 0.9658870
#> 2     treatment1 - treatment3  2.444444e+00 2.285443 24  1.069571e+00 0.9999908
#> 3     treatment1 - treatment4  1.888889e+00 2.285443 24  8.264869e-01 0.9999999
#> 4     treatment1 - treatment5  6.166667e+00 5.598170 24  1.101550e+00 0.9999842
#> 5     treatment1 - treatment6  6.166667e+00 5.598170 24  1.101550e+00 0.9999842
#> 6     treatment1 - treatment7 -9.083333e+00 5.598170 24 -1.622554e+00 0.9939411
#> 7     treatment1 - treatment8  1.116667e+01 5.598170 24  1.994699e+00 0.9451063
#> 8     treatment1 - treatment9  7.166667e+00 5.598170 24  1.280180e+00 0.9997933
#> 9    treatment1 - treatment10 -2.083333e+00 5.598170 24 -3.721454e-01 1.0000000
#> 10   treatment1 - treatment11  4.916667e+00 5.598170 24  8.782632e-01 0.9999998
#> 11   treatment1 - treatment12  7.166667e+00 5.598170 24  1.280180e+00 0.9997933
#> 12   treatment1 - treatment13  4.416667e+00 5.598170 24  7.889483e-01 1.0000000
#> 13   treatment1 - treatment14  3.416667e+00 5.598170 24  6.103185e-01 1.0000000
#> 14   treatment1 - treatment15  1.416667e+00 5.598170 24  2.530589e-01 1.0000000
#> 15   treatment1 - treatment16 -9.583333e+00 5.598170 24 -1.711869e+00 0.9885123
#> 16   treatment1 - treatment17  3.166667e+00 5.598170 24  5.656611e-01 1.0000000
#> 17   treatment1 - treatment18  6.416667e+00 5.598170 24  1.146208e+00 0.9999679
#> 18   treatment1 - treatment19 -5.833333e-01 5.598170 24 -1.042007e-01 1.0000000
#> 19   treatment1 - treatment20  6.166667e+00 5.598170 24  1.101550e+00 0.9999842
#> 20   treatment1 - treatment21  9.166667e+00 5.598170 24  1.637440e+00 0.9932206
#> 21   treatment1 - treatment22 -7.333333e+00 5.598170 24 -1.309952e+00 0.9997022
#> 22   treatment1 - treatment23  7.166667e+00 5.598170 24  1.280180e+00 0.9997933
#> 23   treatment1 - treatment24  1.091667e+01 5.598170 24  1.950042e+00 0.9553753
#> 24   treatment1 - treatment25 -3.333333e-01 5.598170 24 -5.954327e-02 1.0000000
#> 25   treatment1 - treatment26  9.916667e+00 5.598170 24  1.771412e+00 0.9831434
#> 26   treatment1 - treatment27  3.666667e+00 5.598170 24  6.549760e-01 1.0000000
#> 27   treatment1 - treatment28  1.491667e+01 5.598170 24  2.664561e+00 0.6238201
#> 28    treatment2 - treatment3 -1.888889e+00 2.285443 24 -8.264869e-01 0.9999999
#> 29    treatment2 - treatment4 -2.444444e+00 2.285443 24 -1.069571e+00 0.9999908
#> 30    treatment2 - treatment5  1.833333e+00 5.598170 24  3.274880e-01 1.0000000
#> 31    treatment2 - treatment6  1.833333e+00 5.598170 24  3.274880e-01 1.0000000
#> 32    treatment2 - treatment7 -1.341667e+01 5.598170 24 -2.396617e+00 0.7837997
#> 33    treatment2 - treatment8  6.833333e+00 5.598170 24  1.220637e+00 0.9999054
#> 34    treatment2 - treatment9  2.833333e+00 5.598170 24  5.061178e-01 1.0000000
#> 35   treatment2 - treatment10 -6.416667e+00 5.598170 24 -1.146208e+00 0.9999679
#> 36   treatment2 - treatment11  5.833333e-01 5.598170 24  1.042007e-01 1.0000000
#> 37   treatment2 - treatment12  2.833333e+00 5.598170 24  5.061178e-01 1.0000000
#> 38   treatment2 - treatment13  8.333333e-02 5.598170 24  1.488582e-02 1.0000000
#> 39   treatment2 - treatment14 -9.166667e-01 5.598170 24 -1.637440e-01 1.0000000
#> 40   treatment2 - treatment15 -2.916667e+00 5.598170 24 -5.210036e-01 1.0000000
#> 41   treatment2 - treatment16 -1.391667e+01 5.598170 24 -2.485931e+00 0.7333078
#> 42   treatment2 - treatment17 -1.166667e+00 5.598170 24 -2.084014e-01 1.0000000
#> 43   treatment2 - treatment18  2.083333e+00 5.598170 24  3.721454e-01 1.0000000
#> 44   treatment2 - treatment19 -4.916667e+00 5.598170 24 -8.782632e-01 0.9999998
#> 45   treatment2 - treatment20  1.833333e+00 5.598170 24  3.274880e-01 1.0000000
#> 46   treatment2 - treatment21  4.833333e+00 5.598170 24  8.633774e-01 0.9999999
#> 47   treatment2 - treatment22 -1.166667e+01 5.598170 24 -2.084014e+00 0.9200000
#> 48   treatment2 - treatment23  2.833333e+00 5.598170 24  5.061178e-01 1.0000000
#> 49   treatment2 - treatment24  6.583333e+00 5.598170 24  1.175980e+00 0.9999498
#> 50   treatment2 - treatment25 -4.666667e+00 5.598170 24 -8.336058e-01 0.9999999
#> 51   treatment2 - treatment26  5.583333e+00 5.598170 24  9.973497e-01 0.9999975
#> 52   treatment2 - treatment27 -6.666667e-01 5.598170 24 -1.190865e-01 1.0000000
#> 53   treatment2 - treatment28  1.058333e+01 5.598170 24  1.890499e+00 0.9668571
#> 54    treatment3 - treatment4 -5.555556e-01 2.285443 24 -2.430844e-01 1.0000000
#> 55    treatment3 - treatment5  3.722222e+00 5.598170 24  6.648998e-01 1.0000000
#> 56    treatment3 - treatment6  3.722222e+00 5.598170 24  6.648998e-01 1.0000000
#> 57    treatment3 - treatment7 -1.152778e+01 5.598170 24 -2.059205e+00 0.9276013
#> 58    treatment3 - treatment8  8.722222e+00 5.598170 24  1.558049e+00 0.9963817
#> 59    treatment3 - treatment9  4.722222e+00 5.598170 24  8.435296e-01 0.9999999
#> 60   treatment3 - treatment10 -4.527778e+00 5.598170 24 -8.087961e-01 1.0000000
#> 61   treatment3 - treatment11  2.472222e+00 5.598170 24  4.416126e-01 1.0000000
#> 62   treatment3 - treatment12  4.722222e+00 5.598170 24  8.435296e-01 0.9999999
#> 63   treatment3 - treatment13  1.972222e+00 5.598170 24  3.522977e-01 1.0000000
#> 64   treatment3 - treatment14  9.722222e-01 5.598170 24  1.736679e-01 1.0000000
#> 65   treatment3 - treatment15 -1.027778e+00 5.598170 24 -1.835917e-01 1.0000000
#> 66   treatment3 - treatment16 -1.202778e+01 5.598170 24 -2.148520e+00 0.8979398
#> 67   treatment3 - treatment17  7.222222e-01 5.598170 24  1.290104e-01 1.0000000
#> 68   treatment3 - treatment18  3.972222e+00 5.598170 24  7.095573e-01 1.0000000
#> 69   treatment3 - treatment19 -3.027778e+00 5.598170 24 -5.408514e-01 1.0000000
#> 70   treatment3 - treatment20  3.722222e+00 5.598170 24  6.648998e-01 1.0000000
#> 71   treatment3 - treatment21  6.722222e+00 5.598170 24  1.200789e+00 0.9999282
#> 72   treatment3 - treatment22 -9.777778e+00 5.598170 24 -1.746603e+00 0.9855759
#> 73   treatment3 - treatment23  4.722222e+00 5.598170 24  8.435296e-01 0.9999999
#> 74   treatment3 - treatment24  8.472222e+00 5.598170 24  1.513391e+00 0.9975401
#> 75   treatment3 - treatment25 -2.777778e+00 5.598170 24 -4.961939e-01 1.0000000
#> 76   treatment3 - treatment26  7.472222e+00 5.598170 24  1.334762e+00 0.9996011
#> 77   treatment3 - treatment27  1.222222e+00 5.598170 24  2.183253e-01 1.0000000
#> 78   treatment3 - treatment28  1.247222e+01 5.598170 24  2.227911e+00 0.8662699
#> 79    treatment4 - treatment5  4.277778e+00 5.598170 24  7.641386e-01 1.0000000
#> 80    treatment4 - treatment6  4.277778e+00 5.598170 24  7.641386e-01 1.0000000
#> 81    treatment4 - treatment7 -1.097222e+01 5.598170 24 -1.959966e+00 0.9532199
#> 82    treatment4 - treatment8  9.277778e+00 5.598170 24  1.657288e+00 0.9921537
#> 83    treatment4 - treatment9  5.277778e+00 5.598170 24  9.427684e-01 0.9999992
#> 84   treatment4 - treatment10 -3.972222e+00 5.598170 24 -7.095573e-01 1.0000000
#> 85   treatment4 - treatment11  3.027778e+00 5.598170 24  5.408514e-01 1.0000000
#> 86   treatment4 - treatment12  5.277778e+00 5.598170 24  9.427684e-01 0.9999992
#> 87   treatment4 - treatment13  2.527778e+00 5.598170 24  4.515365e-01 1.0000000
#> 88   treatment4 - treatment14  1.527778e+00 5.598170 24  2.729066e-01 1.0000000
#> 89   treatment4 - treatment15 -4.722222e-01 5.598170 24 -8.435296e-02 1.0000000
#> 90   treatment4 - treatment16 -1.147222e+01 5.598170 24 -2.049281e+00 0.9305054
#> 91   treatment4 - treatment17  1.277778e+00 5.598170 24  2.282492e-01 1.0000000
#> 92   treatment4 - treatment18  4.527778e+00 5.598170 24  8.087961e-01 1.0000000
#> 93   treatment4 - treatment19 -2.472222e+00 5.598170 24 -4.416126e-01 1.0000000
#> 94   treatment4 - treatment20  4.277778e+00 5.598170 24  7.641386e-01 1.0000000
#> 95   treatment4 - treatment21  7.277778e+00 5.598170 24  1.300028e+00 0.9997359
#> 96   treatment4 - treatment22 -9.222222e+00 5.598170 24 -1.647364e+00 0.9927029
#> 97   treatment4 - treatment23  5.277778e+00 5.598170 24  9.427684e-01 0.9999992
#> 98   treatment4 - treatment24  9.027778e+00 5.598170 24  1.612630e+00 0.9943859
#> 99   treatment4 - treatment25 -2.222222e+00 5.598170 24 -3.969551e-01 1.0000000
#> 100  treatment4 - treatment26  8.027778e+00 5.598170 24  1.434000e+00 0.9988386
#> 101  treatment4 - treatment27  1.777778e+00 5.598170 24  3.175641e-01 1.0000000
#> 102  treatment4 - treatment28  1.302778e+01 5.598170 24  2.327149e+00 0.8200544
#> 103   treatment5 - treatment6 -1.776357e-15 7.665610 24 -2.317307e-16 1.0000000
#> 104   treatment5 - treatment7 -1.525000e+01 7.665610 24 -1.989405e+00 0.9464012
#> 105   treatment5 - treatment8  5.000000e+00 7.665610 24  6.522638e-01 1.0000000
#> 106   treatment5 - treatment9  1.000000e+00 6.856330 24  1.458506e-01 1.0000000
#> 107  treatment5 - treatment10 -8.250000e+00 7.665610 24 -1.076235e+00 0.9999897
#> 108  treatment5 - treatment11 -1.250000e+00 7.665610 24 -1.630660e-01 1.0000000
#> 109  treatment5 - treatment12  1.000000e+00 7.665610 24  1.304528e-01 1.0000000
#> 110  treatment5 - treatment13 -1.750000e+00 7.665610 24 -2.282923e-01 1.0000000
#> 111  treatment5 - treatment14 -2.750000e+00 7.665610 24 -3.587451e-01 1.0000000
#> 112  treatment5 - treatment15 -4.750000e+00 7.665610 24 -6.196506e-01 1.0000000
#> 113  treatment5 - treatment16 -1.575000e+01 7.665610 24 -2.054631e+00 0.9289493
#> 114  treatment5 - treatment17 -3.000000e+00 7.665610 24 -3.913583e-01 1.0000000
#> 115  treatment5 - treatment18  2.500000e-01 7.665610 24  3.261319e-02 1.0000000
#> 116  treatment5 - treatment19 -6.750000e+00 7.665610 24 -8.805562e-01 0.9999998
#> 117  treatment5 - treatment20 -4.884981e-15 7.665610 24 -6.372593e-16 1.0000000
#> 118  treatment5 - treatment21  3.000000e+00 7.665610 24  3.913583e-01 1.0000000
#> 119  treatment5 - treatment22 -1.350000e+01 7.665610 24 -1.761112e+00 0.9841890
#> 120  treatment5 - treatment23  1.000000e+00 7.665610 24  1.304528e-01 1.0000000
#> 121  treatment5 - treatment24  4.750000e+00 7.665610 24  6.196506e-01 1.0000000
#> 122  treatment5 - treatment25 -6.500000e+00 7.665610 24 -8.479430e-01 0.9999999
#> 123  treatment5 - treatment26  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 124  treatment5 - treatment27 -2.500000e+00 7.665610 24 -3.261319e-01 1.0000000
#> 125  treatment5 - treatment28  8.750000e+00 7.665610 24  1.141462e+00 0.9999701
#> 126   treatment6 - treatment7 -1.525000e+01 7.665610 24 -1.989405e+00 0.9464012
#> 127   treatment6 - treatment8  5.000000e+00 6.856330 24  7.292531e-01 1.0000000
#> 128   treatment6 - treatment9  1.000000e+00 7.665610 24  1.304528e-01 1.0000000
#> 129  treatment6 - treatment10 -8.250000e+00 7.665610 24 -1.076235e+00 0.9999897
#> 130  treatment6 - treatment11 -1.250000e+00 7.665610 24 -1.630660e-01 1.0000000
#> 131  treatment6 - treatment12  1.000000e+00 6.856330 24  1.458506e-01 1.0000000
#> 132  treatment6 - treatment13 -1.750000e+00 7.665610 24 -2.282923e-01 1.0000000
#> 133  treatment6 - treatment14 -2.750000e+00 7.665610 24 -3.587451e-01 1.0000000
#> 134  treatment6 - treatment15 -4.750000e+00 7.665610 24 -6.196506e-01 1.0000000
#> 135  treatment6 - treatment16 -1.575000e+01 7.665610 24 -2.054631e+00 0.9289493
#> 136  treatment6 - treatment17 -3.000000e+00 7.665610 24 -3.913583e-01 1.0000000
#> 137  treatment6 - treatment18  2.500000e-01 7.665610 24  3.261319e-02 1.0000000
#> 138  treatment6 - treatment19 -6.750000e+00 7.665610 24 -8.805562e-01 0.9999998
#> 139  treatment6 - treatment20 -3.108624e-15 7.665610 24 -4.055287e-16 1.0000000
#> 140  treatment6 - treatment21  3.000000e+00 7.665610 24  3.913583e-01 1.0000000
#> 141  treatment6 - treatment22 -1.350000e+01 7.665610 24 -1.761112e+00 0.9841890
#> 142  treatment6 - treatment23  1.000000e+00 7.665610 24  1.304528e-01 1.0000000
#> 143  treatment6 - treatment24  4.750000e+00 7.665610 24  6.196506e-01 1.0000000
#> 144  treatment6 - treatment25 -6.500000e+00 7.665610 24 -8.479430e-01 0.9999999
#> 145  treatment6 - treatment26  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 146  treatment6 - treatment27 -2.500000e+00 7.665610 24 -3.261319e-01 1.0000000
#> 147  treatment6 - treatment28  8.750000e+00 7.665610 24  1.141462e+00 0.9999701
#> 148   treatment7 - treatment8  2.025000e+01 7.665610 24  2.641668e+00 0.6382215
#> 149   treatment7 - treatment9  1.625000e+01 7.665610 24  2.119857e+00 0.9081522
#> 150  treatment7 - treatment10  7.000000e+00 6.856330 24  1.020954e+00 0.9999962
#> 151  treatment7 - treatment11  1.400000e+01 6.856330 24  2.041909e+00 0.9326126
#> 152  treatment7 - treatment12  1.625000e+01 7.665610 24  2.119857e+00 0.9081522
#> 153  treatment7 - treatment13  1.350000e+01 7.665610 24  1.761112e+00 0.9841890
#> 154  treatment7 - treatment14  1.250000e+01 7.665610 24  1.630660e+00 0.9935569
#> 155  treatment7 - treatment15  1.050000e+01 7.665610 24  1.369754e+00 0.9994081
#> 156  treatment7 - treatment16 -5.000000e-01 7.665610 24 -6.522638e-02 1.0000000
#> 157  treatment7 - treatment17  1.225000e+01 7.665610 24  1.598046e+00 0.9949910
#> 158  treatment7 - treatment18  1.550000e+01 7.665610 24  2.022018e+00 0.9380862
#> 159  treatment7 - treatment19  8.500000e+00 7.665610 24  1.108849e+00 0.9999822
#> 160  treatment7 - treatment20  1.525000e+01 7.665610 24  1.989405e+00 0.9464012
#> 161  treatment7 - treatment21  1.825000e+01 7.665610 24  2.380763e+00 0.7923291
#> 162  treatment7 - treatment22  1.750000e+00 7.665610 24  2.282923e-01 1.0000000
#> 163  treatment7 - treatment23  1.625000e+01 7.665610 24  2.119857e+00 0.9081522
#> 164  treatment7 - treatment24  2.000000e+01 7.665610 24  2.609055e+00 0.6586181
#> 165  treatment7 - treatment25  8.750000e+00 7.665610 24  1.141462e+00 0.9999701
#> 166  treatment7 - treatment26  1.900000e+01 7.665610 24  2.478603e+00 0.7375910
#> 167  treatment7 - treatment27  1.275000e+01 7.665610 24  1.663273e+00 0.9918068
#> 168  treatment7 - treatment28  2.400000e+01 7.665610 24  3.130866e+00 0.3462901
#> 169   treatment8 - treatment9 -4.000000e+00 7.665610 24 -5.218111e-01 1.0000000
#> 170  treatment8 - treatment10 -1.325000e+01 7.665610 24 -1.728499e+00 0.9871716
#> 171  treatment8 - treatment11 -6.250000e+00 7.665610 24 -8.153298e-01 1.0000000
#> 172  treatment8 - treatment12 -4.000000e+00 6.856330 24 -5.834025e-01 1.0000000
#> 173  treatment8 - treatment13 -6.750000e+00 7.665610 24 -8.805562e-01 0.9999998
#> 174  treatment8 - treatment14 -7.750000e+00 7.665610 24 -1.011009e+00 0.9999968
#> 175  treatment8 - treatment15 -9.750000e+00 7.665610 24 -1.271914e+00 0.9998138
#> 176  treatment8 - treatment16 -2.075000e+01 7.665610 24 -2.706895e+00 0.5970867
#> 177  treatment8 - treatment17 -8.000000e+00 7.665610 24 -1.043622e+00 0.9999942
#> 178  treatment8 - treatment18 -4.750000e+00 7.665610 24 -6.196506e-01 1.0000000
#> 179  treatment8 - treatment19 -1.175000e+01 7.665610 24 -1.532820e+00 0.9970816
#> 180  treatment8 - treatment20 -5.000000e+00 7.665610 24 -6.522638e-01 1.0000000
#> 181  treatment8 - treatment21 -2.000000e+00 7.665610 24 -2.609055e-01 1.0000000
#> 182  treatment8 - treatment22 -1.850000e+01 7.665610 24 -2.413376e+00 0.7746304
#> 183  treatment8 - treatment23 -4.000000e+00 7.665610 24 -5.218111e-01 1.0000000
#> 184  treatment8 - treatment24 -2.500000e-01 7.665610 24 -3.261319e-02 1.0000000
#> 185  treatment8 - treatment25 -1.150000e+01 7.665610 24 -1.500207e+00 0.9978156
#> 186  treatment8 - treatment26 -1.250000e+00 7.665610 24 -1.630660e-01 1.0000000
#> 187  treatment8 - treatment27 -7.500000e+00 7.665610 24 -9.783957e-01 0.9999983
#> 188  treatment8 - treatment28  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 189  treatment9 - treatment10 -9.250000e+00 7.665610 24 -1.206688e+00 0.9999220
#> 190  treatment9 - treatment11 -2.250000e+00 7.665610 24 -2.935187e-01 1.0000000
#> 191  treatment9 - treatment12 -3.552714e-15 7.665610 24 -4.634613e-16 1.0000000
#> 192  treatment9 - treatment13 -2.750000e+00 7.665610 24 -3.587451e-01 1.0000000
#> 193  treatment9 - treatment14 -3.750000e+00 7.665610 24 -4.891979e-01 1.0000000
#> 194  treatment9 - treatment15 -5.750000e+00 7.665610 24 -7.501034e-01 1.0000000
#> 195  treatment9 - treatment16 -1.675000e+01 7.665610 24 -2.185084e+00 0.8839651
#> 196  treatment9 - treatment17 -4.000000e+00 7.665610 24 -5.218111e-01 1.0000000
#> 197  treatment9 - treatment18 -7.500000e-01 7.665610 24 -9.783957e-02 1.0000000
#> 198  treatment9 - treatment19 -7.750000e+00 7.665610 24 -1.011009e+00 0.9999968
#> 199  treatment9 - treatment20 -1.000000e+00 7.665610 24 -1.304528e-01 1.0000000
#> 200  treatment9 - treatment21  2.000000e+00 7.665610 24  2.609055e-01 1.0000000
#> 201  treatment9 - treatment22 -1.450000e+01 7.665610 24 -1.891565e+00 0.9666726
#> 202  treatment9 - treatment23 -5.773160e-15 7.665610 24 -7.531247e-16 1.0000000
#> 203  treatment9 - treatment24  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 204  treatment9 - treatment25 -7.500000e+00 7.665610 24 -9.783957e-01 0.9999983
#> 205  treatment9 - treatment26  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 206  treatment9 - treatment27 -3.500000e+00 7.665610 24 -4.565847e-01 1.0000000
#> 207  treatment9 - treatment28  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 208 treatment10 - treatment11  7.000000e+00 6.856330 24  1.020954e+00 0.9999962
#> 209 treatment10 - treatment12  9.250000e+00 7.665610 24  1.206688e+00 0.9999220
#> 210 treatment10 - treatment13  6.500000e+00 7.665610 24  8.479430e-01 0.9999999
#> 211 treatment10 - treatment14  5.500000e+00 7.665610 24  7.174902e-01 1.0000000
#> 212 treatment10 - treatment15  3.500000e+00 7.665610 24  4.565847e-01 1.0000000
#> 213 treatment10 - treatment16 -7.500000e+00 7.665610 24 -9.783957e-01 0.9999983
#> 214 treatment10 - treatment17  5.250000e+00 7.665610 24  6.848770e-01 1.0000000
#> 215 treatment10 - treatment18  8.500000e+00 7.665610 24  1.108849e+00 0.9999822
#> 216 treatment10 - treatment19  1.500000e+00 7.665610 24  1.956791e-01 1.0000000
#> 217 treatment10 - treatment20  8.250000e+00 7.665610 24  1.076235e+00 0.9999897
#> 218 treatment10 - treatment21  1.125000e+01 7.665610 24  1.467594e+00 0.9983876
#> 219 treatment10 - treatment22 -5.250000e+00 7.665610 24 -6.848770e-01 1.0000000
#> 220 treatment10 - treatment23  9.250000e+00 7.665610 24  1.206688e+00 0.9999220
#> 221 treatment10 - treatment24  1.300000e+01 7.665610 24  1.695886e+00 0.9896944
#> 222 treatment10 - treatment25  1.750000e+00 7.665610 24  2.282923e-01 1.0000000
#> 223 treatment10 - treatment26  1.200000e+01 7.665610 24  1.565433e+00 0.9961524
#> 224 treatment10 - treatment27  5.750000e+00 7.665610 24  7.501034e-01 1.0000000
#> 225 treatment10 - treatment28  1.700000e+01 7.665610 24  2.217697e+00 0.8706180
#> 226 treatment11 - treatment12  2.250000e+00 7.665610 24  2.935187e-01 1.0000000
#> 227 treatment11 - treatment13 -5.000000e-01 7.665610 24 -6.522638e-02 1.0000000
#> 228 treatment11 - treatment14 -1.500000e+00 7.665610 24 -1.956791e-01 1.0000000
#> 229 treatment11 - treatment15 -3.500000e+00 7.665610 24 -4.565847e-01 1.0000000
#> 230 treatment11 - treatment16 -1.450000e+01 7.665610 24 -1.891565e+00 0.9666726
#> 231 treatment11 - treatment17 -1.750000e+00 7.665610 24 -2.282923e-01 1.0000000
#> 232 treatment11 - treatment18  1.500000e+00 7.665610 24  1.956791e-01 1.0000000
#> 233 treatment11 - treatment19 -5.500000e+00 7.665610 24 -7.174902e-01 1.0000000
#> 234 treatment11 - treatment20  1.250000e+00 7.665610 24  1.630660e-01 1.0000000
#> 235 treatment11 - treatment21  4.250000e+00 7.665610 24  5.544243e-01 1.0000000
#> 236 treatment11 - treatment22 -1.225000e+01 7.665610 24 -1.598046e+00 0.9949910
#> 237 treatment11 - treatment23  2.250000e+00 7.665610 24  2.935187e-01 1.0000000
#> 238 treatment11 - treatment24  6.000000e+00 7.665610 24  7.827166e-01 1.0000000
#> 239 treatment11 - treatment25 -5.250000e+00 7.665610 24 -6.848770e-01 1.0000000
#> 240 treatment11 - treatment26  5.000000e+00 7.665610 24  6.522638e-01 1.0000000
#> 241 treatment11 - treatment27 -1.250000e+00 7.665610 24 -1.630660e-01 1.0000000
#> 242 treatment11 - treatment28  1.000000e+01 7.665610 24  1.304528e+00 0.9997210
#> 243 treatment12 - treatment13 -2.750000e+00 7.665610 24 -3.587451e-01 1.0000000
#> 244 treatment12 - treatment14 -3.750000e+00 7.665610 24 -4.891979e-01 1.0000000
#> 245 treatment12 - treatment15 -5.750000e+00 7.665610 24 -7.501034e-01 1.0000000
#> 246 treatment12 - treatment16 -1.675000e+01 7.665610 24 -2.185084e+00 0.8839651
#> 247 treatment12 - treatment17 -4.000000e+00 7.665610 24 -5.218111e-01 1.0000000
#> 248 treatment12 - treatment18 -7.500000e-01 7.665610 24 -9.783957e-02 1.0000000
#> 249 treatment12 - treatment19 -7.750000e+00 7.665610 24 -1.011009e+00 0.9999968
#> 250 treatment12 - treatment20 -1.000000e+00 7.665610 24 -1.304528e-01 1.0000000
#> 251 treatment12 - treatment21  2.000000e+00 7.665610 24  2.609055e-01 1.0000000
#> 252 treatment12 - treatment22 -1.450000e+01 7.665610 24 -1.891565e+00 0.9666726
#> 253 treatment12 - treatment23 -2.220446e-15 7.665610 24 -2.896633e-16 1.0000000
#> 254 treatment12 - treatment24  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 255 treatment12 - treatment25 -7.500000e+00 7.665610 24 -9.783957e-01 0.9999983
#> 256 treatment12 - treatment26  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 257 treatment12 - treatment27 -3.500000e+00 7.665610 24 -4.565847e-01 1.0000000
#> 258 treatment12 - treatment28  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 259 treatment13 - treatment14 -1.000000e+00 7.665610 24 -1.304528e-01 1.0000000
#> 260 treatment13 - treatment15 -3.000000e+00 7.665610 24 -3.913583e-01 1.0000000
#> 261 treatment13 - treatment16 -1.400000e+01 6.856330 24 -2.041909e+00 0.9326126
#> 262 treatment13 - treatment17 -1.250000e+00 7.665610 24 -1.630660e-01 1.0000000
#> 263 treatment13 - treatment18  2.000000e+00 7.665610 24  2.609055e-01 1.0000000
#> 264 treatment13 - treatment19 -5.000000e+00 6.856330 24 -7.292531e-01 1.0000000
#> 265 treatment13 - treatment20  1.750000e+00 7.665610 24  2.282923e-01 1.0000000
#> 266 treatment13 - treatment21  4.750000e+00 7.665610 24  6.196506e-01 1.0000000
#> 267 treatment13 - treatment22 -1.175000e+01 7.665610 24 -1.532820e+00 0.9970816
#> 268 treatment13 - treatment23  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 269 treatment13 - treatment24  6.500000e+00 7.665610 24  8.479430e-01 0.9999999
#> 270 treatment13 - treatment25 -4.750000e+00 7.665610 24 -6.196506e-01 1.0000000
#> 271 treatment13 - treatment26  5.500000e+00 7.665610 24  7.174902e-01 1.0000000
#> 272 treatment13 - treatment27 -7.500000e-01 7.665610 24 -9.783957e-02 1.0000000
#> 273 treatment13 - treatment28  1.050000e+01 7.665610 24  1.369754e+00 0.9994081
#> 274 treatment14 - treatment15 -2.000000e+00 6.856330 24 -2.917013e-01 1.0000000
#> 275 treatment14 - treatment16 -1.300000e+01 7.665610 24 -1.695886e+00 0.9896944
#> 276 treatment14 - treatment17 -2.500000e-01 7.665610 24 -3.261319e-02 1.0000000
#> 277 treatment14 - treatment18  3.000000e+00 6.856330 24  4.375519e-01 1.0000000
#> 278 treatment14 - treatment19 -4.000000e+00 7.665610 24 -5.218111e-01 1.0000000
#> 279 treatment14 - treatment20  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 280 treatment14 - treatment21  5.750000e+00 7.665610 24  7.501034e-01 1.0000000
#> 281 treatment14 - treatment22 -1.075000e+01 7.665610 24 -1.402367e+00 0.9991600
#> 282 treatment14 - treatment23  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 283 treatment14 - treatment24  7.500000e+00 7.665610 24  9.783957e-01 0.9999983
#> 284 treatment14 - treatment25 -3.750000e+00 7.665610 24 -4.891979e-01 1.0000000
#> 285 treatment14 - treatment26  6.500000e+00 7.665610 24  8.479430e-01 0.9999999
#> 286 treatment14 - treatment27  2.500000e-01 7.665610 24  3.261319e-02 1.0000000
#> 287 treatment14 - treatment28  1.150000e+01 7.665610 24  1.500207e+00 0.9978156
#> 288 treatment15 - treatment16 -1.100000e+01 7.665610 24 -1.434980e+00 0.9988272
#> 289 treatment15 - treatment17  1.750000e+00 7.665610 24  2.282923e-01 1.0000000
#> 290 treatment15 - treatment18  5.000000e+00 6.856330 24  7.292531e-01 1.0000000
#> 291 treatment15 - treatment19 -2.000000e+00 7.665610 24 -2.609055e-01 1.0000000
#> 292 treatment15 - treatment20  4.750000e+00 7.665610 24  6.196506e-01 1.0000000
#> 293 treatment15 - treatment21  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 294 treatment15 - treatment22 -8.750000e+00 7.665610 24 -1.141462e+00 0.9999701
#> 295 treatment15 - treatment23  5.750000e+00 7.665610 24  7.501034e-01 1.0000000
#> 296 treatment15 - treatment24  9.500000e+00 7.665610 24  1.239301e+00 0.9998782
#> 297 treatment15 - treatment25 -1.750000e+00 7.665610 24 -2.282923e-01 1.0000000
#> 298 treatment15 - treatment26  8.500000e+00 7.665610 24  1.108849e+00 0.9999822
#> 299 treatment15 - treatment27  2.250000e+00 7.665610 24  2.935187e-01 1.0000000
#> 300 treatment15 - treatment28  1.350000e+01 7.665610 24  1.761112e+00 0.9841890
#> 301 treatment16 - treatment17  1.275000e+01 7.665610 24  1.663273e+00 0.9918068
#> 302 treatment16 - treatment18  1.600000e+01 7.665610 24  2.087244e+00 0.9189744
#> 303 treatment16 - treatment19  9.000000e+00 6.856330 24  1.312656e+00 0.9996924
#> 304 treatment16 - treatment20  1.575000e+01 7.665610 24  2.054631e+00 0.9289493
#> 305 treatment16 - treatment21  1.875000e+01 7.665610 24  2.445989e+00 0.7563654
#> 306 treatment16 - treatment22  2.250000e+00 7.665610 24  2.935187e-01 1.0000000
#> 307 treatment16 - treatment23  1.675000e+01 7.665610 24  2.185084e+00 0.8839651
#> 308 treatment16 - treatment24  2.050000e+01 7.665610 24  2.674282e+00 0.6176904
#> 309 treatment16 - treatment25  9.250000e+00 7.665610 24  1.206688e+00 0.9999220
#> 310 treatment16 - treatment26  1.950000e+01 7.665610 24  2.543829e+00 0.6987544
#> 311 treatment16 - treatment27  1.325000e+01 7.665610 24  1.728499e+00 0.9871716
#> 312 treatment16 - treatment28  2.450000e+01 7.665610 24  3.196093e+00 0.3135599
#> 313 treatment17 - treatment18  3.250000e+00 7.665610 24  4.239715e-01 1.0000000
#> 314 treatment17 - treatment19 -3.750000e+00 7.665610 24 -4.891979e-01 1.0000000
#> 315 treatment17 - treatment20  3.000000e+00 6.856330 24  4.375519e-01 1.0000000
#> 316 treatment17 - treatment21  6.000000e+00 7.665610 24  7.827166e-01 1.0000000
#> 317 treatment17 - treatment22 -1.050000e+01 7.665610 24 -1.369754e+00 0.9994081
#> 318 treatment17 - treatment23  4.000000e+00 7.665610 24  5.218111e-01 1.0000000
#> 319 treatment17 - treatment24  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 320 treatment17 - treatment25 -3.500000e+00 7.665610 24 -4.565847e-01 1.0000000
#> 321 treatment17 - treatment26  6.750000e+00 7.665610 24  8.805562e-01 0.9999998
#> 322 treatment17 - treatment27  5.000000e-01 7.665610 24  6.522638e-02 1.0000000
#> 323 treatment17 - treatment28  1.175000e+01 7.665610 24  1.532820e+00 0.9970816
#> 324 treatment18 - treatment19 -7.000000e+00 7.665610 24 -9.131694e-01 0.9999996
#> 325 treatment18 - treatment20 -2.500000e-01 7.665610 24 -3.261319e-02 1.0000000
#> 326 treatment18 - treatment21  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 327 treatment18 - treatment22 -1.375000e+01 7.665610 24 -1.793726e+00 0.9806966
#> 328 treatment18 - treatment23  7.500000e-01 7.665610 24  9.783957e-02 1.0000000
#> 329 treatment18 - treatment24  4.500000e+00 7.665610 24  5.870374e-01 1.0000000
#> 330 treatment18 - treatment25 -6.750000e+00 7.665610 24 -8.805562e-01 0.9999998
#> 331 treatment18 - treatment26  3.500000e+00 7.665610 24  4.565847e-01 1.0000000
#> 332 treatment18 - treatment27 -2.750000e+00 7.665610 24 -3.587451e-01 1.0000000
#> 333 treatment18 - treatment28  8.500000e+00 7.665610 24  1.108849e+00 0.9999822
#> 334 treatment19 - treatment20  6.750000e+00 7.665610 24  8.805562e-01 0.9999998
#> 335 treatment19 - treatment21  9.750000e+00 7.665610 24  1.271914e+00 0.9998138
#> 336 treatment19 - treatment22 -6.750000e+00 7.665610 24 -8.805562e-01 0.9999998
#> 337 treatment19 - treatment23  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 338 treatment19 - treatment24  1.150000e+01 7.665610 24  1.500207e+00 0.9978156
#> 339 treatment19 - treatment25  2.500000e-01 7.665610 24  3.261319e-02 1.0000000
#> 340 treatment19 - treatment26  1.050000e+01 7.665610 24  1.369754e+00 0.9994081
#> 341 treatment19 - treatment27  4.250000e+00 7.665610 24  5.544243e-01 1.0000000
#> 342 treatment19 - treatment28  1.550000e+01 7.665610 24  2.022018e+00 0.9380862
#> 343 treatment20 - treatment21  3.000000e+00 7.665610 24  3.913583e-01 1.0000000
#> 344 treatment20 - treatment22 -1.350000e+01 7.665610 24 -1.761112e+00 0.9841890
#> 345 treatment20 - treatment23  1.000000e+00 7.665610 24  1.304528e-01 1.0000000
#> 346 treatment20 - treatment24  4.750000e+00 7.665610 24  6.196506e-01 1.0000000
#> 347 treatment20 - treatment25 -6.500000e+00 7.665610 24 -8.479430e-01 0.9999999
#> 348 treatment20 - treatment26  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 349 treatment20 - treatment27 -2.500000e+00 7.665610 24 -3.261319e-01 1.0000000
#> 350 treatment20 - treatment28  8.750000e+00 7.665610 24  1.141462e+00 0.9999701
#> 351 treatment21 - treatment22 -1.650000e+01 7.665610 24 -2.152471e+00 0.8964807
#> 352 treatment21 - treatment23 -2.000000e+00 6.856330 24 -2.917013e-01 1.0000000
#> 353 treatment21 - treatment24  1.750000e+00 7.665610 24  2.282923e-01 1.0000000
#> 354 treatment21 - treatment25 -9.500000e+00 7.665610 24 -1.239301e+00 0.9998782
#> 355 treatment21 - treatment26  7.500000e-01 7.665610 24  9.783957e-02 1.0000000
#> 356 treatment21 - treatment27 -5.500000e+00 7.665610 24 -7.174902e-01 1.0000000
#> 357 treatment21 - treatment28  5.750000e+00 7.665610 24  7.501034e-01 1.0000000
#> 358 treatment22 - treatment23  1.450000e+01 7.665610 24  1.891565e+00 0.9666726
#> 359 treatment22 - treatment24  1.825000e+01 7.665610 24  2.380763e+00 0.7923291
#> 360 treatment22 - treatment25  7.000000e+00 6.856330 24  1.020954e+00 0.9999962
#> 361 treatment22 - treatment26  1.725000e+01 7.665610 24  2.250310e+00 0.8564590
#> 362 treatment22 - treatment27  1.100000e+01 6.856330 24  1.604357e+00 0.9947361
#> 363 treatment22 - treatment28  2.225000e+01 7.665610 24  2.902574e+00 0.4751483
#> 364 treatment23 - treatment24  3.750000e+00 7.665610 24  4.891979e-01 1.0000000
#> 365 treatment23 - treatment25 -7.500000e+00 7.665610 24 -9.783957e-01 0.9999983
#> 366 treatment23 - treatment26  2.750000e+00 7.665610 24  3.587451e-01 1.0000000
#> 367 treatment23 - treatment27 -3.500000e+00 7.665610 24 -4.565847e-01 1.0000000
#> 368 treatment23 - treatment28  7.750000e+00 7.665610 24  1.011009e+00 0.9999968
#> 369 treatment24 - treatment25 -1.125000e+01 7.665610 24 -1.467594e+00 0.9983876
#> 370 treatment24 - treatment26 -1.000000e+00 6.856330 24 -1.458506e-01 1.0000000
#> 371 treatment24 - treatment27 -7.250000e+00 7.665610 24 -9.457825e-01 0.9999991
#> 372 treatment24 - treatment28  4.000000e+00 6.856330 24  5.834025e-01 1.0000000
#> 373 treatment25 - treatment26  1.025000e+01 7.665610 24  1.337141e+00 0.9995900
#> 374 treatment25 - treatment27  4.000000e+00 6.856330 24  5.834025e-01 1.0000000
#> 375 treatment25 - treatment28  1.525000e+01 7.665610 24  1.989405e+00 0.9464012
#> 376 treatment26 - treatment27 -6.250000e+00 7.665610 24 -8.153298e-01 1.0000000
#> 377 treatment26 - treatment28  5.000000e+00 6.856330 24  7.292531e-01 1.0000000
#> 378 treatment27 - treatment28  1.125000e+01 7.665610 24  1.467594e+00 0.9983876
#> 
#> Groups :
#>    treatment   emmean       SE df lower.CL  upper.CL .group
#> 28        28 68.75000 5.359840 24 49.97624  87.52376      1
#> 8          8 72.50000 5.359840 24 53.72624  91.27376      1
#> 24        24 72.75000 5.359840 24 53.97624  91.52376      1
#> 26        26 73.75000 5.359840 24 54.97624  92.52376      1
#> 21        21 74.50000 5.359840 24 55.72624  93.27376      1
#> 9          9 76.50000 5.359840 24 57.72624  95.27376      1
#> 12        12 76.50000 5.359840 24 57.72624  95.27376      1
#> 23        23 76.50000 5.359840 24 57.72624  95.27376      1
#> 18        18 77.25000 5.359840 24 58.47624  96.02376      1
#> 5          5 77.50000 5.359840 24 58.72624  96.27376      1
#> 6          6 77.50000 5.359840 24 58.72624  96.27376      1
#> 20        20 77.50000 5.359840 24 58.72624  96.27376      1
#> 11        11 78.75000 5.359840 24 59.97624  97.52376      1
#> 13        13 79.25000 5.359840 24 60.47624  98.02376      1
#> 2          2 79.33333 1.616052 24 73.67283  84.99383      1
#> 27        27 80.00000 5.359840 24 61.22624  98.77376      1
#> 14        14 80.25000 5.359840 24 61.47624  99.02376      1
#> 17        17 80.50000 5.359840 24 61.72624  99.27376      1
#> 3          3 81.22222 1.616052 24 75.56172  86.88272      1
#> 4          4 81.77778 1.616052 24 76.11728  87.43828      1
#> 15        15 82.25000 5.359840 24 63.47624 101.02376      1
#> 1          1 83.66667 1.616052 24 78.00617  89.32717      1
#> 25        25 84.00000 5.359840 24 65.22624 102.77376      1
#> 19        19 84.25000 5.359840 24 65.47624 103.02376      1
#> 10        10 85.75000 5.359840 24 66.97624 104.52376      1
#> 22        22 91.00000 5.359840 24 72.22624 109.77376      1
#> 7          7 92.75000 5.359840 24 73.97624 111.52376      1
#> 16        16 93.25000 5.359840 24 74.47624 112.02376      1
#> 
#> warnings :
#> NULL
#> 
```
