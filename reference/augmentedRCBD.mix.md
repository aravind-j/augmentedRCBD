# Analysis of Augmented Randomised Complete Block Design in Single and Multiple Environments Using Mixed Models

The function `augmentedRCBD.mix` implements analysis of variance of an
augmented randomised block design (Federer, 1956; Federer, 1961) and the
generation as well as comparison of the adjusted means of the
treatments/genotypes using mixed-effect models. The analysis can be
performed for cases where the design is in a single environment or is
across multiple environments such as locations and/or seasons with
either 1) test treatments are replicated across environments
(`scenario = "I"`) or 2) test treatments are not replicated across
environments (`scenario = "II"`).

## Usage

``` r
augmentedRCBD.mix(
  block,
  treatment,
  env = NULL,
  y,
  checks = NULL,
  env.random = FALSE,
  check.random = FALSE,
  test.random = TRUE,
  drop.nonsig.interaction = TRUE,
  scenario = c("I", "II"),
  scenario.violation.threshold = 0.1,
  df_method = c("kenward-roger", "satterthwaite"),
  console = TRUE
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

- env.random:

  logical. If `TRUE`, `env` is considered as a random effect. Default is
  `FALSE`.

- check.random:

  logical. If `TRUE`, `check` treatments are considered as random
  effects. Default is `FALSE`.

- test.random:

  logical. If `TRUE`, `test` treatments are considered as random
  effects. Default is `TRUE`.

- drop.nonsig.interaction:

  logical. If `TRUE`, "test treatment \\\times\\ environment"
  interaction effect is dropped from the model if found to be
  non-significant.

- scenario:

  Either `1` or `2` (see **Details**).

- scenario.violation.threshold:

  Threshold proportion of number of accessions violating `scenario`
  requirements to trigger an error. Default of `0.1`.

- df_method:

  Degrees-of-freedom method for estimation of BLUE means.

- console:

  If `TRUE`, output will be printed to console. Default is `TRUE`.
  Default is `TRUE`.

## Value

A list of class `augmentedRCBD.mix` containing the following components:

## Details

The model to be fitted as well as the method for estimation of treatment
means is determined by the arguments `scenario`, `env.random`,
`check.random`, `test.random`, `drop.nonsig.interaction`.

- **1. Single Environment:** *Random Effects* - check, test:

  *Model:*

  :   `y ~ (1|block) + (1|treatment)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUP)

- **2. Single Environment:** *Fixed Effects* - check; *Random Effects* -
  test:

  *Model:*

  :   `y ~ check + (1|block) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP)

- **3. Single Environment:** *Fixed Effects* - check, test:

  *Model:*

  :   `y ~ treatment + (1|block)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUE)

- **4. Multiple Environments - Scenario I:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  *Model:*

  :   `y ~ (1|env) + (1|treatment) + (1|env:block) + (1|env:treatment:check)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUP)

- **5. Multiple Environments - Scenario I:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  TRUE:

  *Model:*

  :   `y ~ (1|env) + (1|treatment) + (1|env:block) + (1|env:treatment)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUP)

- **6. Multiple Environments - Scenario I:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ check + (1|env) + (1|env:block) + (1|env:check) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP)

- **7. Multiple Environments - Scenario I:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  *Model:*

  :   `y ~ check + (1|env) + (1|env:block) + (1|env:treatment) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP)

- **8. Multiple Environments - Scenario I:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ treatment + (1|env) + (1|env:block2) + (1|env:check)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUE)

- **9. Multiple Environments - Scenario I:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  *Model:*

  :   `y ~ treatment + (1|env) + (1|env:block2) + (1|env:treatment)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUE)

- **10. Multiple Environments - Scenario I:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ env + (1|treatment) + (1|env:block) + (1|env:treatment:check)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUP)

- **11. Multiple Environments - Scenario I:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  *Model:*

  :   `y ~ env + (1|treatment) + (1|env:block) + (1|env:treatment)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUP)

- **12. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ env + check + env:check + (1|env:block) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP)

- **13. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  *Model:*

  :   `y ~ env + check + env:check + (1|env:block) + (1|treatment:test) + (1|env:treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP)

- **14. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  *Model:*

  :   `y ~ env + treatment + env:check + (1|env:block)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUE)

- **15. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  TRUE:

  *Model:*

  :   `y ~ env + treatment + env:treatment + (1|env:block)`

  *Mean Estimate:*

  :   Check and Test treatments (BLUE)

- **16. Multiple Environments - Scenario II:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  *Model:*

  :   `y ~ (1|env) + (1|treatment) + (1|env:block) + (1|env:treatment:check)`

  *Mean Estimate:*

  :   Check treatment (BLUP) and Test treatment (BLUP within
      Environment)

- **17. Multiple Environments - Scenario II:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ check + (1|env) + (1|env:block) + (1|env:check) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP within
      Environment)

- **18. Multiple Environments - Scenario II:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ treatment + (1|env) + (1|env:block2) + (1|env:check)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUE within
      Environment)

- **19. Multiple Environments - Scenario II:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ env + (1|treatment) + (1|env:block) + (1|env:treatment:check)`

  *Mean Estimate:*

  :   Check treatment (BLUP) and Test treatment (BLUP within
      Environment)

- **20. Multiple Environments - Scenario II:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  *Model:*

  :   `y ~ env + check + env:check + (1|env:block) + (1|treatment:test)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUP within
      Environment)

- **21. Multiple Environments - Scenario II:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  *Model:*

  :   `y ~ env + treatment + env:check + (1|env:block)`

  *Mean Estimate:*

  :   Check treatment (BLUE) and Test treatment (BLUE within
      Environment)

## Note

- Making checks random but tests fixed breaks the nesting/partition
  logic and creates a non-identifiable treatment variance decomposition.
  So this combination is not implemented in this function.

## See also

[`augmentedRCBD`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.md),
[`augmentedRCBD.menv`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.menv.md)

## Examples

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Single Environment: Random Effects - check, test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311,
        250, 240, 268, 287, 226, 395, 450)
data <- data.frame(blk, trt, y1, y2)

# Convert block and treatment to factors
data$blk <- as.factor(data$blk)
data$trt <- as.factor(data$trt)

# 01. Random Effects - check, test
out1 <- augmentedRCBD.mix(block = data$blk, treatment = data$trt,
                          y = y1, checks =  c("1", "2", "3", "4"),
                          check.random = TRUE, test.random = TRUE,
                          console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data', but variables found in environment of formula: try specifying 'formula' as a formula rather than a string in the original model

# 02. Fixed Effects - check; Random Effects - test
out2 <- augmentedRCBD.mix(block = data$blk, treatment = data$trt,
                          y = y1, checks =  c("1", "2", "3", "4"),
                          check.random = FALSE, test.random = TRUE,
                          console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data', but variables found in environment of formula: try specifying 'formula' as a formula rather than a string in the original model

# 03. Random Effects - Fixed Effects - check, test
out3 <- augmentedRCBD.mix(block = data$blk, treatment = data$trt,
                          y = y1, checks =  c("1", "2", "3", "4"),
                          check.random = FALSE, test.random = FALSE,
                          console = TRUE)
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ treatment + (1 | block)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> 
#> REML criterion at convergence: 58.9
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.1234 -0.2299  0.0000  0.0000  1.4439 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  block    (Intercept)  1.944   1.394   
#>  Residual             26.972   5.193   
#> Number of obs: 20, groups:  block, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)   
#> (Intercept)   81.014      1.550   1.341  52.258   0.0035 **
#> treatment1     3.653      3.041   6.031   1.201   0.2747   
#> treatment2    -2.014      3.041   6.031  -0.662   0.5323   
#> treatment3     0.986      3.041   6.031   0.324   0.7567   
#> treatment4     2.319      3.041   6.031   0.763   0.4744   
#> treatment5    -2.182      5.040   7.677  -0.433   0.6770   
#> treatment6    -5.287      5.015   7.413  -1.054   0.3250   
#> treatment7    14.427      5.015   7.413   2.876   0.0224 * 
#> treatment8   -10.287      5.015   7.413  -2.051   0.0772 . 
#> treatment9    -3.182      5.040   7.677  -0.631   0.5462   
#> treatment10   -6.287      5.015   7.413  -1.253   0.2481   
#> treatment11    7.426      5.015   7.413   1.481   0.1799   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) trtmn1 trtmn2 trtmn3 trtmn4 trtmn5 trtmn6 trtmn7 trtmn8
#> treatment1  -0.213                                                        
#> treatment2  -0.213  0.028                                                 
#> treatment3  -0.213  0.028  0.028                                          
#> treatment4  -0.213  0.028  0.028  0.028                                   
#> treatment5   0.052 -0.076 -0.076 -0.076 -0.076                            
#> treatment6   0.069 -0.084 -0.084 -0.084 -0.084 -0.127                     
#> treatment7   0.069 -0.084 -0.084 -0.084 -0.084 -0.127 -0.132              
#> treatment8   0.069 -0.084 -0.084 -0.084 -0.084 -0.127 -0.072 -0.132       
#> treatment9   0.052 -0.076 -0.076 -0.076 -0.076 -0.062 -0.127 -0.127 -0.127
#> treatment10  0.069 -0.084 -0.084 -0.084 -0.084 -0.127 -0.072 -0.132 -0.072
#> treatment11  0.069 -0.084 -0.084 -0.084 -0.084 -0.127 -0.132 -0.072 -0.132
#>             trtmn9 trtm10
#> treatment1               
#> treatment2               
#> treatment3               
#> treatment4               
#> treatment5               
#> treatment6               
#> treatment7               
#> treatment8               
#> treatment9               
#> treatment10 -0.127       
#> treatment11 -0.127 -0.132
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4
#> 1        0          
#>                                                                                  opt_message
#> 1 NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#>   opt_warnings singular    block Residual      AIC      BIC
#> 1                 FALSE 1.944444 26.97222 86.93248 100.8727
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> treatment 470.46  42.769    11 6.3919  1.5857 0.2883
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ treatment + (1 | block)
#>             npar  logLik    AIC      LRT Df Pr(>Chisq)
#> <none>        14 -29.466 86.932                       
#> (1 | block)   13 -29.491 84.983 0.050141  1     0.8228
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 84.66667 3.104656 7.892933 BLUE
#> 2          2 79.00000 3.104656 7.892933 BLUE
#> 3          3 82.00000 3.104656 7.892933 BLUE
#> 4          4 83.33333 3.104656 7.892933 BLUE
#> 5          5 78.83213 6.158823 7.969123 BLUE
#> 6          6 75.72742 6.158823 7.969123 BLUE
#> 7          7 95.44045 6.158823 7.969123 BLUE
#> 8          8 70.72742 6.158823 7.969123 BLUE
#> 9          9 77.83213 6.158823 7.969123 BLUE
#> 10        10 74.72742 6.158823 7.969123 BLUE
#> 11        11 88.44045 6.158823 7.969123 BLUE
#> 12        12 81.44045 6.158823 7.969123 BLUE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Multiple Environments - Scenario 1:
# Test treatments are replicated across all environments
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


# 04. Random Effects - env, check, test;
# With dropping of non-significant env:test interaction
out4 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = TRUE, test.random = TRUE,
                          drop.nonsig.interaction = TRUE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 05. Random Effects - env, check, test;
# Without dropping of non-significant env:test interaction
out5 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = TRUE, test.random = TRUE,
                          drop.nonsig.interaction = FALSE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 06. Fixed Effects - check; Random Effects - env, test;
# With dropping of non-significant env:test interaction
out6 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = FALSE, test.random = TRUE,
                          drop.nonsig.interaction = TRUE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 07. Fixed Effects - check; Random Effects - env, test;
# Without dropping of non-significant env:test interaction
out7 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = FALSE, test.random = TRUE,
                          drop.nonsig.interaction = FALSE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 08. Fixed Effects - check, test; Random Effects - env;
# With dropping of non-significant env:test interaction
out8 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = FALSE, test.random = FALSE,
                          drop.nonsig.interaction = TRUE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 09.Fixed Effects - check, test; Random Effects - env;
# Without dropping of non-significant env:test interaction
out9 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                          treatment = data1$trt1,
                          y = data1$y1, checks =  c("1", "2", "3", "4"),
                          env.random = TRUE,
                          check.random = FALSE, test.random = FALSE,
                          drop.nonsig.interaction = FALSE,
                          scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 10. Fixed Effects - env; Random Effects - check, test;
# With dropping of non-significant env:test interaction
out10 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = TRUE, test.random = TRUE,
                           drop.nonsig.interaction = TRUE,
                           scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 11. Fixed Effects - env; Random Effects - check, test;
# Without dropping of non-significant env:test interaction
out11 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = TRUE, test.random = TRUE,
                           drop.nonsig.interaction = FALSE,
                           scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 12. Fixed Effects - env, check; Random Effects - test;
# With dropping of non-significant env:test interaction
out12 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = TRUE,
                           drop.nonsig.interaction = TRUE,
                           scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data', but variables found in environment of formula: try specifying 'formula' as a formula rather than a string in the original model

# 13. Fixed Effects - env, check; Random Effects - test;
# Without dropping of non-significant env:test interaction
out13 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = TRUE,
                           drop.nonsig.interaction = FALSE,
                           scenario = "I", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data', but variables found in environment of formula: try specifying 'formula' as a formula rather than a string in the original model

# 14. Fixed Effects - env, check, test;
# With dropping of non-significant env:test interaction
out14 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = FALSE,
                           drop.nonsig.interaction = TRUE,
                           scenario = "I", console = TRUE)
#> fixed-effect model matrix is rank deficient so dropping 4 columns / coefficients
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + treatment + env:treatment + (1 | env:block2)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> 
#> REML criterion at convergence: 204.9
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -1.081 -0.330  0.000  0.000  1.469 
#> 
#> Random effects:
#>  Groups     Name        Variance Std.Dev.
#>  env:block2 (Intercept)  0.5278  0.7265  
#>  Residual               30.3611  5.5101  
#> Number of obs: 60, groups:  env:block2, 9
#> 
#> Fixed effects:
#>                   Estimate Std. Error        df t value Pr(>|t|)    
#> (Intercept)       80.33243    0.84579   3.62711  94.980 2.72e-07 ***
#> env1               0.67163    1.19612   3.62711   0.562  0.60731    
#> env2              -0.30375    1.19612   3.62711  -0.254  0.81327    
#> treatment1         3.33424    1.86224  18.11368   1.790  0.09011 .  
#> treatment2        -0.99910    1.86224  18.11368  -0.537  0.59814    
#> treatment3         0.88979    1.86224  18.11368   0.478  0.63850    
#> treatment4         1.44535    1.86224  18.11368   0.776  0.44769    
#> treatment5        -2.39203    3.03466  23.49801  -0.788  0.43844    
#> treatment6        -4.82031    3.03315  23.25712  -1.589  0.12552    
#> treatment7         8.64590    3.03165  22.97380   2.852  0.00903 ** 
#> treatment8        -0.98826    3.03165  22.97380  -0.326  0.74739    
#> treatment9         0.93589    3.03466  23.49801   0.308  0.76050    
#> treatment10       -8.50323    3.03165  22.97380  -2.805  0.01006 *  
#> treatment11        2.55380    3.03165  22.97380   0.842  0.40826    
#> env1:treatment1    0.32837    2.63360  18.11368   0.125  0.90215    
#> env2:treatment1   -0.69625    2.63360  18.11368  -0.264  0.79448    
#> env1:treatment2   -1.00497    2.63360  18.11368  -0.382  0.70720    
#> env2:treatment2   -1.02958    2.63360  18.11368  -0.391  0.70040    
#> env1:treatment3    0.10614    2.63360  18.11368   0.040  0.96829    
#> env2:treatment3   -2.25181    2.63360  18.11368  -0.855  0.40370    
#> env1:treatment4    0.88392    2.63360  18.11368   0.336  0.74100    
#> env2:treatment4   -1.47403    2.63360  18.11368  -0.560  0.58254    
#> env1:treatment5    0.33920    4.29272  23.60172   0.079  0.93768    
#> env2:treatment5    3.14122    4.28953  23.25712   0.732  0.47130    
#> env1:treatment6   -0.97246    4.28846  23.12059  -0.227  0.82260    
#> env2:treatment6    0.02459    4.28846  23.12059   0.006  0.99547    
#> env1:treatment7    6.18751    4.28739  22.97380   1.443  0.16247    
#> env2:treatment7  -11.44162    4.28739  22.97380  -2.669  0.01373 *  
#> env1:treatment8   -9.80451    4.28739  22.97380  -2.287  0.03175 *  
#> env2:treatment8   15.73745    4.28739  22.97380   3.671  0.00127 ** 
#> env1:treatment9   -3.98871    4.29272  23.60172  -0.929  0.36220    
#> env2:treatment9   -1.97541    4.29272  23.60172  -0.460  0.64960    
#> env1:treatment10   1.71046    4.28739  22.97380   0.399  0.69361    
#> env2:treatment10   0.70751    4.28739  22.97380   0.165  0.87037    
#> env1:treatment11   5.27961    4.28739  22.97380   1.231  0.23062    
#> env2:treatment11   3.19539    4.28739  22.97380   0.745  0.46365    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 36 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4
#> 1        0          
#>                                                                                  opt_message
#> 1 NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#>   opt_warnings singular env.block2 Residual      AIC      BIC
#> 1                 FALSE  0.5277775 30.36111 280.8878 360.4729
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>               Sum Sq Mean Sq NumDF   DenDF F value  Pr(>F)  
#> env             9.60   4.801     2  3.6271  0.1581 0.85933  
#> treatment     666.86  60.624    11 19.6446  1.9968 0.08737 .
#> env:treatment 826.77  37.580    22 18.9500  1.2378 0.32180  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + treatment + env:treatment + (1 | env:block2)
#>                  npar  logLik    AIC      LRT Df Pr(>Chisq)
#> <none>             38 -102.44 280.89                       
#> (1 | env:block2)   37 -102.45 278.90 0.010281  1     0.9192
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 1.852592 23.97900 BLUE
#> 2          2 79.33333 1.852592 23.97900 BLUE
#> 3          3 81.22222 1.852592 23.97900 BLUE
#> 4          4 81.77778 1.852592 23.97900 BLUE
#> 5          5 77.94040 3.419139 23.98953 BLUE
#> 6          6 75.51212 3.419139 23.98953 BLUE
#> 7          7 88.97833 3.419139 23.98953 BLUE
#> 8          8 79.34417 3.419139 23.98953 BLUE
#> 9          9 81.26832 3.419139 23.98953 BLUE
#> 10        10 71.82920 3.419139 23.98953 BLUE
#> 11        11 82.88623 3.419139 23.98953 BLUE
#> 12        12 80.23040 3.419139 23.98953 BLUE

# 15. Fixed Effects - env, check, test;
# Without dropping of non-significant env:test interaction
out15 <- augmentedRCBD.mix(env = data1$env1, block = data1$blk1,
                           treatment = data1$trt1,
                           y = data1$y1, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = FALSE,
                           drop.nonsig.interaction = FALSE,
                           scenario = "I", console = TRUE)
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + treatment + env:treatment + (1 | env:block2)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> 
#> REML criterion at convergence: 204.9
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -1.081 -0.330  0.000  0.000  1.469 
#> 
#> Random effects:
#>  Groups     Name        Variance Std.Dev.
#>  env:block2 (Intercept)  0.5278  0.7265  
#>  Residual               30.3611  5.5101  
#> Number of obs: 60, groups:  env:block2, 9
#> 
#> Fixed effects:
#>                   Estimate Std. Error        df t value Pr(>|t|)    
#> (Intercept)       80.33243    0.84579   3.62711  94.980 2.72e-07 ***
#> env1               0.67163    1.19612   3.62711   0.562  0.60731    
#> env2              -0.30375    1.19612   3.62711  -0.254  0.81327    
#> treatment1         3.33424    1.86224  18.11368   1.790  0.09011 .  
#> treatment2        -0.99910    1.86224  18.11368  -0.537  0.59814    
#> treatment3         0.88979    1.86224  18.11368   0.478  0.63850    
#> treatment4         1.44535    1.86224  18.11368   0.776  0.44769    
#> treatment5        -2.39203    3.03466  23.49801  -0.788  0.43844    
#> treatment6        -4.82031    3.03315  23.25712  -1.589  0.12552    
#> treatment7         8.64590    3.03165  22.97380   2.852  0.00903 ** 
#> treatment8        -0.98826    3.03165  22.97380  -0.326  0.74739    
#> treatment9         0.93589    3.03466  23.49801   0.308  0.76050    
#> treatment10       -8.50323    3.03165  22.97380  -2.805  0.01006 *  
#> treatment11        2.55380    3.03165  22.97380   0.842  0.40826    
#> env1:treatment1    0.32837    2.63360  18.11368   0.125  0.90215    
#> env2:treatment1   -0.69625    2.63360  18.11368  -0.264  0.79448    
#> env1:treatment2   -1.00497    2.63360  18.11368  -0.382  0.70720    
#> env2:treatment2   -1.02958    2.63360  18.11368  -0.391  0.70040    
#> env1:treatment3    0.10614    2.63360  18.11368   0.040  0.96829    
#> env2:treatment3   -2.25181    2.63360  18.11368  -0.855  0.40370    
#> env1:treatment4    0.88392    2.63360  18.11368   0.336  0.74100    
#> env2:treatment4   -1.47403    2.63360  18.11368  -0.560  0.58254    
#> env1:treatment5    0.33920    4.29272  23.60172   0.079  0.93768    
#> env2:treatment5    3.14122    4.28953  23.25712   0.732  0.47130    
#> env1:treatment6   -0.97246    4.28846  23.12059  -0.227  0.82260    
#> env2:treatment6    0.02459    4.28846  23.12059   0.006  0.99547    
#> env1:treatment7    6.18751    4.28739  22.97380   1.443  0.16247    
#> env2:treatment7  -11.44162    4.28739  22.97380  -2.669  0.01373 *  
#> env1:treatment8   -9.80451    4.28739  22.97380  -2.287  0.03175 *  
#> env2:treatment8   15.73745    4.28739  22.97380   3.671  0.00127 ** 
#> env1:treatment9   -3.98871    4.29272  23.60172  -0.929  0.36220    
#> env2:treatment9   -1.97541    4.29272  23.60172  -0.460  0.64960    
#> env1:treatment10   1.71046    4.28739  22.97380   0.399  0.69361    
#> env2:treatment10   0.70751    4.28739  22.97380   0.165  0.87037    
#> env1:treatment11   5.27961    4.28739  22.97380   1.231  0.23062    
#> env2:treatment11   3.19539    4.28739  22.97380   0.745  0.46365    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 36 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4
#> 1        0          
#>                                                                                  opt_message
#> 1 NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#>   opt_warnings singular env.block2 Residual      AIC      BIC
#> 1                 FALSE  0.5277775 30.36111 280.8878 360.4729
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>               Sum Sq Mean Sq NumDF   DenDF F value  Pr(>F)  
#> env             9.60   4.801     2  3.6271  0.1581 0.85933  
#> treatment     666.86  60.624    11 19.6446  1.9968 0.08737 .
#> env:treatment 826.77  37.580    22 18.9500  1.2378 0.32180  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + treatment + env:treatment + (1 | env:block2)
#>                  npar  logLik    AIC      LRT Df Pr(>Chisq)
#> <none>             38 -102.44 280.89                       
#> (1 | env:block2)   37 -102.45 278.90 0.010281  1     0.9192
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 1.852592 23.97900 BLUE
#> 2          2 79.33333 1.852592 23.97900 BLUE
#> 3          3 81.22222 1.852592 23.97900 BLUE
#> 4          4 81.77778 1.852592 23.97900 BLUE
#> 5          5 77.94040 3.419139 23.98953 BLUE
#> 6          6 75.51212 3.419139 23.98953 BLUE
#> 7          7 88.97833 3.419139 23.98953 BLUE
#> 8          8 79.34417 3.419139 23.98953 BLUE
#> 9          9 81.26832 3.419139 23.98953 BLUE
#> 10        10 71.82920 3.419139 23.98953 BLUE
#> 11        11 82.88623 3.419139 23.98953 BLUE
#> 12        12 80.23040 3.419139 23.98953 BLUE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Multiple Environments - Scenario 2:
# Test treatments are replicated across all environments
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

# 16. Random Effects - env, check, test
out16 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = TRUE,
                           check.random = TRUE, test.random = TRUE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 17. Fixed Effects - check; Random Effects - env, test
out17 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = TRUE,
                           check.random = FALSE, test.random = TRUE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 18. Fixed Effects - check, test; Random Effects - env
out18 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = TRUE,
                           check.random = FALSE, test.random = FALSE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 19. Fixed Effects - env; Random Effects - check, test
out19 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = TRUE, test.random = TRUE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data': object 'mod_final' not found

# 20. Fixed Effects - env, check; Random Effects - test
out20 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = TRUE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> Error: bad 'data', but variables found in environment of formula: try specifying 'formula' as a formula rather than a string in the original model

# 21. Fixed Effects - env, check, test
out21 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = FALSE,
                           scenario = "II", console = TRUE)
#> fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients
#> fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + treatment + env:check + (1 | env:block2)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> 
#> REML criterion at convergence: 172.5
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -1.081 -0.330  0.000  0.000  1.469 
#> 
#> Random effects:
#>  Groups     Name        Variance Std.Dev.
#>  env:block2 (Intercept)  0.5278  0.7265  
#>  Residual               30.3611  5.5101  
#> Number of obs: 60, groups:  env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept)  77.7171     7.4589  18.5947  10.419 3.38e-09 ***
#> env1          1.5556     2.6200  23.9790   0.594    0.558    
#> env2         -1.7778     2.6200  23.9790  -0.679    0.504    
#> treatment1    5.7274    10.7954  18.0056   0.531    0.602    
#> treatment2    3.0607    10.7954  18.0056   0.284    0.780    
#> treatment3    5.0607    10.7954  18.0056   0.469    0.645    
#> treatment4    4.0607     7.6622  18.0111   0.530    0.603    
#> treatment5   -6.3214    13.3543  19.0234  -0.473    0.641    
#> treatment6  -10.0614    13.3530  18.9222  -0.753    0.460    
#> treatment7   10.5648    13.3530  18.9222   0.791    0.439    
#> treatment8  -15.0614    13.3530  18.9222  -1.128    0.273    
#> treatment9   -7.3214    13.3543  19.0234  -0.548    0.590    
#> treatment10   3.5648    13.3530  18.9222   0.267    0.792    
#> treatment11  -3.4352    13.3530  18.9222  -0.257    0.800    
#> treatment12 -11.0614    13.3530  18.9222  -0.828    0.418    
#> treatment13   2.5052    13.3530  18.9222   0.188    0.853    
#> treatment14  -3.0397    13.3530  18.9222  -0.228    0.822    
#> treatment15  -1.0397    13.3530  18.9222  -0.078    0.939    
#> treatment16  16.5052    13.3530  18.9222   1.236    0.232    
#> treatment17   0.7165    13.3543  19.0234   0.054    0.958    
#> treatment18  -6.0397    13.3530  18.9222  -0.452    0.656    
#> treatment19   7.5052    13.3530  18.9222   0.562    0.581    
#> treatment20  -2.2835    13.3543  19.0234  -0.171    0.866    
#> treatment21  -3.8472    11.1410  19.4648  -0.345    0.734    
#> treatment22  15.9252    11.1394  19.3211   1.430    0.169    
#> treatment23  -1.8472    11.1410  19.4648  -0.166    0.870    
#> treatment24  -4.8960    11.1394  19.3211  -0.440    0.665    
#> treatment25   8.9252    11.1394  19.3211   0.801    0.433    
#> treatment26  -3.8960    11.1394  19.3211  -0.350    0.730    
#> treatment27   4.9252    11.1394  19.3211   0.442    0.663    
#> env1:check1  -0.3333     6.3625  18.0000  -0.052    0.959    
#> env2:check1   1.0000     6.3625  18.0000   0.157    0.877    
#> env1:check2  -3.3333     6.3625  18.0000  -0.524    0.607    
#> env2:check2  -1.0000     6.3625  18.0000  -0.157    0.877    
#> env1:check3  -2.3333     6.3625  18.0000  -0.367    0.718    
#> env2:check3  -2.3333     6.3625  18.0000  -0.367    0.718    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 36 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> fit warnings:
#> fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4
#> 1        0          
#>                                                                                  opt_message
#> 1 NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
#>   opt_warnings singular env.block2 Residual      AIC      BIC
#> 1                 FALSE  0.5277775 30.36111 248.4733 328.0584
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>            Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env          5.90   2.949     2 23.979  0.0971 0.9078
#> treatment 1422.58  52.688    27 17.098  1.7354 0.1186
#> env:check   17.61   2.935     6 18.000  0.0967 0.9958
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + treatment + env:check + (1 | env:block2)
#>                  npar  logLik    AIC      LRT Df Pr(>Chisq)
#> <none>             38 -86.237 248.47                       
#> (1 | env:block2)   37 -86.242 246.48 0.010281  1     0.9192
#> 
#> Model Formula
#> =========================
#>     treatment     mean       SE       df type
#> 1           1 84.66667 3.208784 23.97900 BLUE
#> 29          1 82.66667 3.208784 23.97900 BLUE
#> 57          1 83.66667 3.208784 23.97900 BLUE
#> 86          2 79.00000 3.208784 23.97900 BLUE
#> 114         2 78.00000 3.208784 23.97900 BLUE
#> 142         2 81.00000 3.208784 23.97900 BLUE
#> 171         3 82.00000 3.208784 23.97900 BLUE
#> 199         3 78.66667 3.208784 23.97900 BLUE
#> 227         3 83.00000 3.208784 23.97900 BLUE
#> 256         4 83.33333 3.208784 23.97900 BLUE
#> 284         4 80.00000 3.208784 23.97900 BLUE
#> 312         4 82.00000 3.208784 23.97900 BLUE
#> 341         5 78.95124 5.922122 23.98953 BLUE
#> 342         6 75.21129 5.922122 23.98953 BLUE
#> 343         7 95.83747 5.922122 23.98953 BLUE
#> 344         8 70.21129 5.922122 23.98953 BLUE
#> 345         9 77.95124 5.922122 23.98953 BLUE
#> 346        10 88.83747 5.922122 23.98953 BLUE
#> 347        11 81.83747 5.922122 23.98953 BLUE
#> 348        12 74.21129 5.922122 23.98953 BLUE
#> 377        13 80.77787 5.922122 23.98953 BLUE
#> 378        14 75.23296 5.922122 23.98953 BLUE
#> 379        15 77.23296 5.922122 23.98953 BLUE
#> 380        16 94.77787 5.922122 23.98953 BLUE
#> 381        17 78.98916 5.922122 23.98953 BLUE
#> 382        18 72.23296 5.922122 23.98953 BLUE
#> 383        19 85.77787 5.922122 23.98953 BLUE
#> 384        20 75.98916 5.922122 23.98953 BLUE
#> 413        21 74.09210 5.922122 23.98953 BLUE
#> 414        22 93.86456 5.922122 23.98953 BLUE
#> 415        23 76.09210 5.922122 23.98953 BLUE
#> 416        24 73.04334 5.922122 23.98953 BLUE
#> 417        25 86.86456 5.922122 23.98953 BLUE
#> 418        26 74.04334 5.922122 23.98953 BLUE
#> 419        27 82.86456 5.922122 23.98953 BLUE
#> 420        28 69.04334 5.922122 23.98953 BLUE

```
