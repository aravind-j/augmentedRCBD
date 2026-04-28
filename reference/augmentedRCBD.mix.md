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
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ (1 | treatment) + (1 | block)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 122.8
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.39758 -0.63355 -0.09756  0.32441  1.98295 
#> 
#> Random effects:
#>  Groups    Name        Variance Std.Dev.
#>  treatment (Intercept)  0.00    0.000   
#>  block     (Intercept) 22.17    4.709   
#>  Residual              26.27    5.125   
#> Number of obs: 20, groups:  treatment, 12; block, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error     df t value Pr(>|t|)   
#> (Intercept)   81.500      2.951  2.031   27.61  0.00121 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular treatment    block Residual
#> 1 Normal exit from bobyqa                  TRUE         0 22.17234 26.26546
#>        AIC      BIC
#> 1 130.7885 134.7715
#> 
#> ANOVA, Fixed Effects
#> =========================
#> NULL
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ 1 + (1 | treatment) + (1 | block)
#>                 npar  logLik    AIC    LRT Df Pr(>Chisq)  
#> <none>             4 -61.394 130.79                       
#> (1 | treatment)    3 -61.394 128.79 0.0000  1    1.00000  
#> (1 | block)        3 -64.072 134.14 5.3557  1    0.02065 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment mean SE df type
#> 1          1 81.5  0 NA BLUP
#> 2          2 81.5  0 NA BLUP
#> 3          3 81.5  0 NA BLUP
#> 4          4 81.5  0 NA BLUP
#> 5          5 81.5  0 NA BLUP
#> 6          6 81.5  0 NA BLUP
#> 7          7 81.5  0 NA BLUP
#> 8          8 81.5  0 NA BLUP
#> 9          9 81.5  0 NA BLUP
#> 10        10 81.5  0 NA BLUP
#> 11        11 81.5  0 NA BLUP
#> 12        12 81.5  0 NA BLUP

# 02. Fixed Effects - check; Random Effects - test
out2 <- augmentedRCBD.mix(block = data$blk, treatment = data$trt,
                          y = y1, checks =  c("1", "2", "3", "4"),
                          check.random = FALSE, test.random = TRUE,
                          console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ check + (1 | block) + (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 106.2
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.2205 -0.3482 -0.2066  0.4002  1.6731 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  treatment:test (Intercept) 10.52    3.243   
#>  block          (Intercept) 17.69    4.206   
#>  Residual                   24.84    4.984   
#> Number of obs: 20, groups:  treatment:test, 12; block, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  81.8763     3.0135  3.0523  27.170 9.66e-05 ***
#> check1        2.7904     3.8030  0.9570   0.734    0.601    
#> check2       -2.8763     3.8030  0.9570  -0.756    0.592    
#> check3        0.1237     3.8030  0.9570   0.033    0.979    
#> check4        1.4570     3.8030  0.9570   0.383    0.769    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>        (Intr) check1 check2 check3
#> check1  0.050                     
#> check2  0.050 -0.300              
#> check3  0.050 -0.300 -0.300       
#> check4  0.050 -0.300 -0.300 -0.300
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4             opt_message opt_warnings singular
#> 1        0           Normal exit from bobyqa                 FALSE
#>   treatment.test    block Residual      AIC      BIC
#> 1       10.51697 17.68735  24.8361 122.1983 130.1642
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>       Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
#> check 32.778  8.1946     4     2  0.3299  0.842
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ check + (1 | block) + (1 | treatment:test)
#>                      npar  logLik    AIC     LRT Df Pr(>Chisq)  
#> <none>                  8 -53.099 122.20                        
#> (1 | block)             7 -54.569 123.14 2.94061  1    0.08638 .
#> (1 | treatment:test)    7 -53.164 120.33 0.13036  1    0.71806  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 84.66667 4.969050 2.152436 BLUE
#> 2          2 79.00000 4.969050 2.152436 BLUE
#> 3          3 82.00000 4.969050 2.152436 BLUE
#> 4          4 83.33333 4.969050 2.152436 BLUE
#> 5          5 79.95499 2.786081       NA BLUP
#> 6          6 79.91155 2.779019       NA BLUP
#> 7          7 83.91242 2.779019       NA BLUP
#> 8          8 78.42413 2.779019       NA BLUP
#> 9          9 79.65750 2.786081       NA BLUP
#> 10        10 79.61407 2.779019       NA BLUP
#> 11        11 81.83003 2.779019       NA BLUP
#> 12        12 79.74765 2.779019       NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_wo_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 375.9
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.5350 -0.6249 -0.1477  0.4048  2.2955 
#> 
#> Random effects:
#>  Groups              Name        Variance Std.Dev.
#>  env:treatment:check (Intercept)  0.000   0.000   
#>  treatment           (Intercept)  3.305   1.818   
#>  env:block2          (Intercept) 13.438   3.666   
#>  env                 (Intercept)  0.000   0.000   
#>  Residual                        23.625   4.861   
#> Number of obs: 60, groups:  
#> env:treatment:check, 36; treatment, 12; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error     df t value Pr(>|t|)    
#> (Intercept)   80.559      1.488 10.011   54.12 1.09e-13 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment.check treatment
#> 1 Normal exit from bobyqa                  TRUE                   0  3.304911
#>   env.block2 env Residual      AIC      BIC
#> 1   13.43794   0   23.625 387.8948 400.4608
#> 
#> ANOVA, Fixed Effects
#> =========================
#> NULL
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ 1 + (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#>                           npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                       6 -187.95 387.89                          
#> (1 | env)                    5 -187.95 385.89  0.0000  1  0.9999998    
#> (1 | treatment)              5 -188.43 386.85  0.9602  1  0.3271461    
#> (1 | env:block2)             5 -193.89 397.77 11.8762  1  0.0005685 ***
#> (1 | env:treatment:check)    5 -187.95 385.89  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE df type
#> 1          1 82.29086 1.270503 NA BLUP
#> 2          2 79.87577 1.270503 NA BLUP
#> 3          3 80.92850 1.270503 NA BLUP
#> 4          4 81.23813 1.270503 NA BLUP
#> 5          5 79.52941 1.559247 NA BLUP
#> 6          6 79.87221 1.558127 NA BLUP
#> 7          7 82.64335 1.556813 NA BLUP
#> 8          8 80.42125 1.557021 NA BLUP
#> 9          9 80.45872 1.559491 NA BLUP
#> 10        10 78.90556 1.557019 NA BLUP
#> 11        11 80.75118 1.556806 NA BLUP
#> 12        12 79.78956 1.558369 NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 375.9
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.5350 -0.6249 -0.1477  0.4048  2.2955 
#> 
#> Random effects:
#>  Groups        Name        Variance Std.Dev.
#>  env:treatment (Intercept)  0.000   0.000   
#>  treatment     (Intercept)  3.305   1.818   
#>  env:block2    (Intercept) 13.438   3.666   
#>  env           (Intercept)  0.000   0.000   
#>  Residual                  23.625   4.861   
#> Number of obs: 60, groups:  
#> env:treatment, 36; treatment, 12; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error     df t value Pr(>|t|)    
#> (Intercept)   80.559      1.488 10.011   54.12 1.09e-13 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment treatment
#> 1 Normal exit from bobyqa                  TRUE             0  3.304911
#>   env.block2 env Residual      AIC      BIC
#> 1   13.43794   0   23.625 387.8948 400.4608
#> 
#> ANOVA, Fixed Effects
#> =========================
#> NULL
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ 1 + (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment)
#>                     npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                 6 -187.95 387.89                          
#> (1 | env)              5 -187.95 385.89  0.0000  1  0.9999998    
#> (1 | treatment)        5 -188.43 386.85  0.9602  1  0.3271461    
#> (1 | env:block2)       5 -193.89 397.77 11.8762  1  0.0005685 ***
#> (1 | env:treatment)    5 -187.95 385.89  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE df type
#> 1          1 82.29086 1.270503 NA BLUP
#> 2          2 79.87577 1.270503 NA BLUP
#> 3          3 80.92850 1.270503 NA BLUP
#> 4          4 81.23813 1.270503 NA BLUP
#> 5          5 79.52941 1.559247 NA BLUP
#> 6          6 79.87221 1.558127 NA BLUP
#> 7          7 82.64335 1.556813 NA BLUP
#> 8          8 80.42125 1.557021 NA BLUP
#> 9          9 80.45872 1.559491 NA BLUP
#> 10        10 78.90556 1.557019 NA BLUP
#> 11        11 80.75118 1.556806 NA BLUP
#> 12        12 79.78956 1.558369 NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:check) +      (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_wo_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 360.8
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.50439 -0.63853 -0.03649  0.35541  2.43950 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  env:check      (Intercept)  0.000   0.000   
#>  treatment:test (Intercept)  7.712   2.777   
#>  env:block2     (Intercept) 12.826   3.581   
#>  env            (Intercept)  0.000   0.000   
#>  Residual                   23.085   4.805   
#> Number of obs: 60, groups:  
#> env:check, 15; treatment:test, 12; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13454    1.77398  7.68157  45.736 1.22e-10 ***
#> check1       2.53213    2.80861  2.82615   0.902    0.437    
#> check2      -1.80121    2.80861  2.82615  -0.641    0.570    
#> check3       0.08768    2.80861  2.82615   0.031    0.977    
#> check4       0.64324    2.80861  2.82615   0.229    0.834    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>        (Intr) check1 check2 check3
#> check1  0.067                     
#> check2  0.067 -0.303              
#> check3  0.067 -0.303 -0.303       
#> check4  0.067 -0.303 -0.303 -0.303
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.check treatment.test
#> 1 Normal exit from bobyqa                  TRUE         0       7.712414
#>   env.block2 env Residual      AIC      BIC
#> 1   12.82638   0 23.08469 380.8263 401.7698
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>       Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> check 38.542  9.6356     4 2.9602  0.4174 0.7912
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:check) + (1 | treatment:test)
#>                      npar  logLik    AIC     LRT Df Pr(>Chisq)   
#> <none>                 10 -180.41 380.83                         
#> (1 | env)               9 -180.41 378.83  0.0000  1    1.00000   
#> (1 | env:block2)        9 -185.76 389.53 10.7030  1    0.00107 **
#> (1 | env:check)         9 -180.41 378.83  0.0000  1    1.00000   
#> (1 | treatment:test)    9 -181.19 380.39  1.5587  1    0.21186   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 3.420897 3.351039 BLUE
#> 2          2 79.33333 3.420897 3.351039 BLUE
#> 3          3 81.22222 3.420897 3.351039 BLUE
#> 4          4 81.77778 3.420897 3.351039 BLUE
#> 5          5 78.35828 2.044951       NA BLUP
#> 6          6 78.91023 2.042787       NA BLUP
#> 7          7 83.70619 2.039857       NA BLUP
#> 8          8 79.82877 2.040670       NA BLUP
#> 9          9 79.98128 2.045910       NA BLUP
#> 10        10 77.26612 2.040657       NA BLUP
#> 11        11 80.45126 2.039824       NA BLUP
#> 12        12 78.87954 2.043726       NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:treatment) +      (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 360.8
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.50439 -0.63853 -0.03649  0.35541  2.43950 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  env:treatment  (Intercept)  0.000   0.000   
#>  treatment:test (Intercept)  7.712   2.777   
#>  env:block2     (Intercept) 12.826   3.581   
#>  env            (Intercept)  0.000   0.000   
#>  Residual                   23.085   4.805   
#> Number of obs: 60, groups:  
#> env:treatment, 36; treatment:test, 12; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13454    1.77398  7.68157  45.736 1.22e-10 ***
#> check1       2.53213    2.80861  2.82615   0.902    0.437    
#> check2      -1.80121    2.80861  2.82615  -0.641    0.570    
#> check3       0.08768    2.80861  2.82615   0.031    0.977    
#> check4       0.64324    2.80861  2.82615   0.229    0.834    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>        (Intr) check1 check2 check3
#> check1  0.067                     
#> check2  0.067 -0.303              
#> check3  0.067 -0.303 -0.303       
#> check4  0.067 -0.303 -0.303 -0.303
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment treatment.test
#> 1 Normal exit from bobyqa                  TRUE             0       7.712414
#>   env.block2 env Residual      AIC      BIC
#> 1   12.82638   0 23.08469 380.8263 401.7698
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>       Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> check 38.542  9.6356     4 2.9602  0.4174 0.7912
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:treatment) + (1 | treatment:test)
#>                      npar  logLik    AIC     LRT Df Pr(>Chisq)   
#> <none>                 10 -180.41 380.83                         
#> (1 | env)               9 -180.41 378.83  0.0000  1    1.00000   
#> (1 | env:block2)        9 -185.76 389.53 10.7030  1    0.00107 **
#> (1 | env:treatment)     9 -180.41 378.83  0.0000  1    1.00000   
#> (1 | treatment:test)    9 -181.19 380.39  1.5587  1    0.21186   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE      df type
#> 1          1 83.66667 3.420897 3.40063 BLUE
#> 2          2 79.33333 3.420897 3.40063 BLUE
#> 3          3 81.22222 3.420897 3.40063 BLUE
#> 4          4 81.77778 3.420897 3.40063 BLUE
#> 5          5 78.35828 2.044951      NA BLUP
#> 6          6 78.91023 2.042787      NA BLUP
#> 7          7 83.70619 2.039857      NA BLUP
#> 8          8 79.82877 2.040670      NA BLUP
#> 9          9 79.98128 2.045910      NA BLUP
#> 10        10 77.26612 2.040657      NA BLUP
#> 11        11 80.45126 2.039824      NA BLUP
#> 12        12 78.87954 2.043726      NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_wo_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 321.1
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.48346 -0.70487  0.01713  0.38514  2.44862 
#> 
#> Random effects:
#>  Groups     Name        Variance Std.Dev.
#>  env:check  (Intercept)  0.00    0.000   
#>  env:block2 (Intercept) 10.77    3.282   
#>  env        (Intercept)  0.00    0.000   
#>  Residual               23.66    4.864   
#> Number of obs: 60, groups:  env:check, 15; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  80.2956     1.3088  8.5982  61.351 1.16e-12 ***
#> treatment1    3.3710     1.6452 39.9670   2.049  0.04707 *  
#> treatment2   -0.9623     1.6452 39.9670  -0.585  0.56189    
#> treatment3    0.9266     1.6452 39.9670   0.563  0.57645    
#> treatment4    1.4821     1.6452 39.9670   0.901  0.37305    
#> treatment5   -3.3723     2.8017 42.9237  -1.204  0.23532    
#> treatment6   -2.4937     2.7906 42.7117  -0.894  0.37654    
#> treatment7    7.8618     2.7755 42.3215   2.833  0.00704 ** 
#> treatment8   -0.5720     2.7804 42.5238  -0.206  0.83798    
#> treatment9    0.1712     2.8085 43.2029   0.061  0.95167    
#> treatment10  -5.7575     2.7800 42.5096  -2.071  0.04446 *  
#> treatment11   1.1022     2.7746 42.2854   0.397  0.69318    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) trtmn1 trtmn2 trtmn3 trtmn4 trtmn5 trtmn6 trtmn7 trtmn8
#> treatment1  -0.138                                                        
#> treatment2  -0.138  0.029                                                 
#> treatment3  -0.138  0.029  0.029                                          
#> treatment4  -0.138  0.029  0.029  0.029                                   
#> treatment5   0.030 -0.072 -0.072 -0.072 -0.072                            
#> treatment6   0.039 -0.079 -0.079 -0.079 -0.079 -0.096                     
#> treatment7   0.047 -0.085 -0.085 -0.085 -0.085 -0.149 -0.109              
#> treatment8   0.047 -0.085 -0.085 -0.085 -0.085 -0.102 -0.103 -0.155       
#> treatment9   0.030 -0.071 -0.071 -0.071 -0.071 -0.094 -0.147 -0.100 -0.153
#> treatment10  0.048 -0.086 -0.086 -0.086 -0.086 -0.146 -0.060 -0.111 -0.062
#> treatment11  0.047 -0.086 -0.086 -0.086 -0.086 -0.105 -0.148 -0.111 -0.064
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
#> treatment10 -0.153       
#> treatment11 -0.147 -0.108
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.check env.block2 env
#> 1 Normal exit from bobyqa                  TRUE         0   10.77185   0
#>   Residual      AIC      BIC
#> 1 23.65965 353.1243 386.6338
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
#> treatment 451.26  41.024    11 41.469  1.7339 0.09929 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:check)
#>                  npar  logLik    AIC   LRT Df Pr(>Chisq)   
#> <none>             16 -160.56 353.12                       
#> (1 | env)          15 -160.56 351.12 0.000  1   1.000000   
#> (1 | env:block2)   15 -164.17 358.34 7.213  1   0.007238 **
#> (1 | env:check)    15 -160.56 351.12 0.000  1   1.000000   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE        df type
#> 1          1 83.66667 1.955945  9.380281 BLUE
#> 2          2 79.33333 1.955945  9.380281 BLUE
#> 3          3 81.22222 1.955945  9.380281 BLUE
#> 4          4 81.77778 1.955945  9.380281 BLUE
#> 5          5 76.92337 3.163385 33.641987 BLUE
#> 6          6 77.80193 3.163808 33.652457 BLUE
#> 7          7 88.15743 3.155753 33.499934 BLUE
#> 8          8 79.72363 3.163422 33.667519 BLUE
#> 9          9 80.46687 3.171783 33.825748 BLUE
#> 10        10 74.53810 3.163433 33.667827 BLUE
#> 11        11 81.39787 3.155737 33.499637 BLUE
#> 12        12 78.53853 3.171499 33.817383 BLUE

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:treatment)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 321.1
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.48346 -0.70487  0.01713  0.38514  2.44862 
#> 
#> Random effects:
#>  Groups        Name        Variance Std.Dev.
#>  env:treatment (Intercept)  0.00    0.000   
#>  env:block2    (Intercept) 10.77    3.282   
#>  env           (Intercept)  0.00    0.000   
#>  Residual                  23.66    4.864   
#> Number of obs: 60, groups:  env:treatment, 36; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  80.2956     1.3088  8.5982  61.351 1.16e-12 ***
#> treatment1    3.3710     1.6452 39.9670   2.049  0.04707 *  
#> treatment2   -0.9623     1.6452 39.9670  -0.585  0.56189    
#> treatment3    0.9266     1.6452 39.9670   0.563  0.57645    
#> treatment4    1.4821     1.6452 39.9670   0.901  0.37305    
#> treatment5   -3.3723     2.8017 42.9237  -1.204  0.23532    
#> treatment6   -2.4937     2.7906 42.7117  -0.894  0.37654    
#> treatment7    7.8618     2.7755 42.3215   2.833  0.00704 ** 
#> treatment8   -0.5720     2.7804 42.5238  -0.206  0.83798    
#> treatment9    0.1712     2.8085 43.2029   0.061  0.95167    
#> treatment10  -5.7575     2.7800 42.5096  -2.071  0.04446 *  
#> treatment11   1.1022     2.7746 42.2854   0.397  0.69318    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) trtmn1 trtmn2 trtmn3 trtmn4 trtmn5 trtmn6 trtmn7 trtmn8
#> treatment1  -0.138                                                        
#> treatment2  -0.138  0.029                                                 
#> treatment3  -0.138  0.029  0.029                                          
#> treatment4  -0.138  0.029  0.029  0.029                                   
#> treatment5   0.030 -0.072 -0.072 -0.072 -0.072                            
#> treatment6   0.039 -0.079 -0.079 -0.079 -0.079 -0.096                     
#> treatment7   0.047 -0.085 -0.085 -0.085 -0.085 -0.149 -0.109              
#> treatment8   0.047 -0.085 -0.085 -0.085 -0.085 -0.102 -0.103 -0.155       
#> treatment9   0.030 -0.071 -0.071 -0.071 -0.071 -0.094 -0.147 -0.100 -0.153
#> treatment10  0.048 -0.086 -0.086 -0.086 -0.086 -0.146 -0.060 -0.111 -0.062
#> treatment11  0.047 -0.086 -0.086 -0.086 -0.086 -0.105 -0.148 -0.111 -0.064
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
#> treatment10 -0.153       
#> treatment11 -0.147 -0.108
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment env.block2 env
#> 1 Normal exit from bobyqa                  TRUE             0   10.77185   0
#>   Residual      AIC      BIC
#> 1 23.65965 353.1243 386.6338
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
#> treatment 451.26  41.024    11 41.469  1.7339 0.09929 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:treatment)
#>                     npar  logLik    AIC   LRT Df Pr(>Chisq)   
#> <none>                16 -160.56 353.12                       
#> (1 | env)             15 -160.56 351.12 0.000  1   1.000000   
#> (1 | env:block2)      15 -164.17 358.34 7.213  1   0.007238 **
#> (1 | env:treatment)   15 -160.56 351.12 0.000  1   1.000000   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE        df type
#> 1          1 83.66667 1.955945  7.376537 BLUE
#> 2          2 79.33333 1.955945  7.376537 BLUE
#> 3          3 81.22222 1.955945  7.376537 BLUE
#> 4          4 81.77778 1.955945  7.376537 BLUE
#> 5          5 76.92337 3.164642 33.592744 BLUE
#> 6          6 77.80193 3.165037 33.603731 BLUE
#> 7          7 88.15743 3.157459 33.445174 BLUE
#> 8          8 79.72363 3.165176 33.619369 BLUE
#> 9          9 80.46687 3.173055 33.783634 BLUE
#> 10        10 74.53810 3.165186 33.619694 BLUE
#> 11        11 81.39787 3.157447 33.444860 BLUE
#> 12        12 78.53853 3.172753 33.774900 BLUE

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_wo_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 369.4
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.5341 -0.5766 -0.1672  0.4757  2.2693 
#> 
#> Random effects:
#>  Groups              Name        Variance Std.Dev.
#>  env:treatment:check (Intercept)  0.000   0.000   
#>  treatment           (Intercept)  3.077   1.754   
#>  env:block2          (Intercept) 18.531   4.305   
#>  Residual                        23.760   4.874   
#> Number of obs: 60, groups:  
#> env:treatment:check, 36; treatment, 12; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  80.5653     1.6621  7.3369  48.472 1.81e-10 ***
#> env1          0.7355     2.2166  5.9363   0.332    0.751    
#> env2         -0.8470     2.2166  5.9365  -0.382    0.716    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>      (Intr) env1  
#> env1  0.000       
#> env2  0.000 -0.500
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment.check treatment
#> 1 Normal exit from bobyqa                  TRUE                   0  3.076909
#>   env.block2 Residual      AIC      BIC
#> 1    18.5308 23.75951 383.4009 398.0613
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>     Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env 4.0964  2.0482     2 5.9364  0.0862 0.9185
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#>                           npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                       7 -184.70 383.40                          
#> (1 | treatment)              6 -185.13 382.26  0.8618  1  0.3532338    
#> (1 | env:block2)             6 -191.66 395.33 13.9265  1  0.0001901 ***
#> (1 | env:treatment:check)    6 -184.70 381.40  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE df type
#> 1          1 82.23451 1.254103 NA BLUP
#> 2          2 79.90224 1.254103 NA BLUP
#> 3          3 80.91887 1.254103 NA BLUP
#> 4          4 81.21788 1.254103 NA BLUP
#> 5          5 79.57354 1.521652 NA BLUP
#> 6          6 79.96497 1.520450 NA BLUP
#> 7          7 82.51007 1.519063 NA BLUP
#> 8          8 80.44859 1.519266 NA BLUP
#> 9          9 80.44819 1.521893 NA BLUP
#> 10        10 79.05562 1.519263 NA BLUP
#> 11        11 80.71496 1.519056 NA BLUP
#> 12        12 79.79421 1.520690 NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 369.4
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.5341 -0.5766 -0.1672  0.4757  2.2693 
#> 
#> Random effects:
#>  Groups        Name        Variance Std.Dev.
#>  env:treatment (Intercept)  0.000   0.000   
#>  treatment     (Intercept)  3.077   1.754   
#>  env:block2    (Intercept) 18.531   4.305   
#>  Residual                  23.760   4.874   
#> Number of obs: 60, groups:  env:treatment, 36; treatment, 12; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  80.5653     1.6621  7.3369  48.472 1.81e-10 ***
#> env1          0.7355     2.2166  5.9363   0.332    0.751    
#> env2         -0.8470     2.2166  5.9365  -0.382    0.716    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>      (Intr) env1  
#> env1  0.000       
#> env2  0.000 -0.500
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment treatment
#> 1 Normal exit from bobyqa                  TRUE             0  3.076909
#>   env.block2 Residual      AIC      BIC
#> 1    18.5308 23.75951 383.4009 398.0613
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>     Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env 4.0964  2.0482     2 5.9364  0.0862 0.9185
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment)
#>                     npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                 7 -184.70 383.40                          
#> (1 | treatment)        6 -185.13 382.26  0.8618  1  0.3532338    
#> (1 | env:block2)       6 -191.66 395.33 13.9265  1  0.0001901 ***
#> (1 | env:treatment)    6 -184.70 381.40  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE df type
#> 1          1 82.23451 1.254103 NA BLUP
#> 2          2 79.90224 1.254103 NA BLUP
#> 3          3 80.91887 1.254103 NA BLUP
#> 4          4 81.21788 1.254103 NA BLUP
#> 5          5 79.57354 1.521652 NA BLUP
#> 6          6 79.96497 1.520450 NA BLUP
#> 7          7 82.51007 1.519063 NA BLUP
#> 8          8 80.44859 1.519266 NA BLUP
#> 9          9 80.44819 1.521893 NA BLUP
#> 10        10 79.05562 1.519263 NA BLUP
#> 11        11 80.71496 1.519056 NA BLUP
#> 12        12 79.79421 1.520690 NA BLUP

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
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_wo_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 329.4
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.2844 -0.5023 -0.1338  0.3901  2.0383 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  treatment:test (Intercept)  6.024   2.454   
#>  env:block2     (Intercept) 17.691   4.206   
#>  Residual                   26.998   5.196   
#> Number of obs: 60, groups:  treatment:test, 12; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13327    1.86685  6.90079  43.460 1.13e-09 ***
#> env1         0.74197    2.23210  6.19031   0.332    0.751    
#> env2        -1.25472    2.23210  6.19036  -0.562    0.594    
#> check1       2.53339    2.63322  2.24482   0.962    0.428    
#> check2      -1.79994    2.63322  2.24482  -0.684    0.558    
#> check3       0.08895    2.63322  2.24482   0.034    0.976    
#> check4       0.64450    2.63322  2.24482   0.245    0.827    
#> env1:check1  0.25803    2.15655 33.66923   0.120    0.905    
#> env2:check1  0.25472    2.15656 33.67000   0.118    0.907    
#> env1:check2 -1.07530    2.15655 33.66923  -0.499    0.621    
#> env2:check2 -0.07861    2.15656 33.67000  -0.036    0.971    
#> env1:check3  0.03581    2.15655 33.66923   0.017    0.987    
#> env2:check3 -1.30083    2.15656 33.67000  -0.603    0.550    
#> env1:check4  0.81359    2.15655 33.66923   0.377    0.708    
#> env2:check4 -0.52306    2.15656 33.67000  -0.243    0.810    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 15 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4             opt_message opt_warnings singular
#> 1        0           Normal exit from bobyqa                 FALSE
#>   treatment.test env.block2 Residual      AIC      BIC
#> 1       6.024114   17.69102  26.9985 365.4463 403.1445
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env        8.626  4.3130     2  6.190  0.1598 0.8558
#> check     50.443 12.6108     4  2.289  0.4671 0.7649
#> env:check 57.291  7.1614     8 33.696  0.2653 0.9729
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test)
#>                      npar  logLik    AIC     LRT Df Pr(>Chisq)   
#> <none>                 18 -164.72 365.45                         
#> (1 | env:block2)       17 -169.84 373.68 10.2346  1   0.001378 **
#> (1 | treatment:test)   17 -165.11 364.22  0.7767  1   0.378149   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 3.315059 3.125624 BLUE
#> 2          2 79.33333 3.315059 3.125624 BLUE
#> 3          3 81.22222 3.315059 3.125624 BLUE
#> 4          4 81.77778 3.315059 3.125624 BLUE
#> 5          5 78.63559 1.962439       NA BLUP
#> 6          6 79.13449 1.960477       NA BLUP
#> 7          7 82.85302 1.958030       NA BLUP
#> 8          8 79.82048 1.958552       NA BLUP
#> 9          9 79.87122 1.963058       NA BLUP
#> 10        10 77.79771 1.958543       NA BLUP
#> 11        11 80.26108 1.958009       NA BLUP
#> 12        12 78.95737 1.961087       NA BLUP

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
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test) +      (1 | env:treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 329.4
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.2844 -0.5023 -0.1338  0.3901  2.0383 
#> 
#> Random effects:
#>  Groups             Name        Variance  Std.Dev. 
#>  env:treatment:test (Intercept) 4.186e-15 6.470e-08
#>  treatment:test     (Intercept) 6.024e+00 2.454e+00
#>  env:block2         (Intercept) 1.769e+01 4.206e+00
#>  Residual                       2.700e+01 5.196e+00
#> Number of obs: 60, groups:  
#> env:treatment:test, 36; treatment:test, 12; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13327    1.86685  6.90079  43.460 1.13e-09 ***
#> env1         0.74197    2.23210  6.19031   0.332    0.751    
#> env2        -1.25472    2.23210  6.19036  -0.562    0.594    
#> check1       2.53339    2.63322  2.24482   0.962    0.428    
#> check2      -1.79994    2.63322  2.24482  -0.684    0.558    
#> check3       0.08895    2.63322  2.24482   0.034    0.976    
#> check4       0.64450    2.63322  2.24482   0.245    0.827    
#> env1:check1  0.25803    2.15655 33.66923   0.120    0.905    
#> env2:check1  0.25472    2.15656 33.67000   0.118    0.907    
#> env1:check2 -1.07530    2.15655 33.66923  -0.499    0.621    
#> env2:check2 -0.07861    2.15656 33.67000  -0.036    0.971    
#> env1:check3  0.03581    2.15655 33.66923   0.017    0.987    
#> env2:check3 -1.30083    2.15656 33.67000  -0.603    0.550    
#> env1:check4  0.81359    2.15655 33.66923   0.377    0.708    
#> env2:check4 -0.52306    2.15656 33.67000  -0.243    0.810    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 15 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment.test
#> 1 Normal exit from bobyqa                  TRUE       4.185786e-15
#>   treatment.test env.block2 Residual      AIC      BIC
#> 1       6.024113   17.69103  26.9985 367.4463 407.2389
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env        8.626  4.3130     2  6.190  0.1598 0.8558
#> check     50.443 12.6108     4  2.289  0.4671 0.7649
#> env:check 57.291  7.1614     8 33.696  0.2653 0.9729
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test) + (1 | env:treatment:test)
#>                          npar  logLik    AIC    LRT Df Pr(>Chisq)   
#> <none>                     19 -164.72 367.45                        
#> (1 | env:block2)           18 -168.90 373.79 8.3428  1   0.003872 **
#> (1 | treatment:test)       18 -165.09 366.18 0.7356  1   0.391085   
#> (1 | env:treatment:test)   18 -164.72 365.45 0.0000  1   1.000000   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 3.315059 3.059302 BLUE
#> 2          2 79.33333 3.315059 3.059302 BLUE
#> 3          3 81.22222 3.315059 3.059302 BLUE
#> 4          4 81.77778 3.315059 3.059302 BLUE
#> 5          5 78.63559 1.962439       NA BLUP
#> 6          6 79.13449 1.960477       NA BLUP
#> 7          7 82.85302 1.958030       NA BLUP
#> 8          8 79.82048 1.958552       NA BLUP
#> 9          9 79.87122 1.963058       NA BLUP
#> 10        10 77.79771 1.958543       NA BLUP
#> 11        11 80.26108 1.958009       NA BLUP
#> 12        12 78.95737 1.961087       NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 376.6
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.7191 -0.6154 -0.2276  0.4480  2.0900 
#> 
#> Random effects:
#>  Groups              Name        Variance Std.Dev.
#>  env:treatment:check (Intercept)  0.000   0.000   
#>  treatment           (Intercept)  1.695   1.302   
#>  env:block2          (Intercept) 13.614   3.690   
#>  env                 (Intercept)  0.000   0.000   
#>  Residual                        24.958   4.996   
#> Number of obs: 60, groups:  
#> env:treatment:check, 36; treatment, 28; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error     df t value Pr(>|t|)    
#> (Intercept)   80.581      1.440  9.074   55.98 7.76e-13 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment.check treatment
#> 1 Normal exit from bobyqa                  TRUE                   0  1.695004
#>   env.block2 env Residual      AIC      BIC
#> 1   13.61394   0  24.9579 388.6321 401.1981
#> 
#> ANOVA, Fixed Effects
#> =========================
#> NULL
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ 1 + (1 | env) + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#>                           npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                       6 -188.32 388.63                          
#> (1 | env)                    5 -188.32 386.63  0.0000  1  1.0000000    
#> (1 | treatment)              5 -188.43 386.85  0.2229  1  0.6368575    
#> (1 | env:block2)             5 -193.84 397.68 11.0471  1  0.0008883 ***
#> (1 | env:treatment:check)    5 -188.32 386.63  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE df type
#> 1          1 81.75159 1.053935 NA BLUP
#> 2          2 80.10771 1.053935 NA BLUP
#> 3          3 80.82427 1.053935 NA BLUP
#> 4          4 81.03503 1.053935 NA BLUP
#> 5          5 80.44431 1.265171 NA BLUP
#> 6          6 80.43961 1.264582 NA BLUP
#> 7          7 81.27513 1.264582 NA BLUP
#> 8          8 80.12164 1.264582 NA BLUP
#> 9          9 80.38071 1.265171 NA BLUP
#> 10        10 80.82996 1.264582 NA BLUP
#> 11        11 80.38479 1.264582 NA BLUP
#> 12        12 80.37602 1.264582 NA BLUP
#> 13        13 80.39980 1.264582 NA BLUP
#> 14        14 80.48610 1.264582 NA BLUP
#> 15        15 80.61330 1.264582 NA BLUP
#> 16        16 81.29014 1.264582 NA BLUP
#> 17        17 80.55879 1.265171 NA BLUP
#> 18        18 80.29532 1.264582 NA BLUP
#> 19        19 80.71778 1.264582 NA BLUP
#> 20        20 80.36801 1.265171 NA BLUP
#> 21        21 80.24645 1.265171 NA BLUP
#> 22        22 81.17597 1.264582 NA BLUP
#> 23        23 80.37364 1.265171 NA BLUP
#> 24        24 80.25215 1.264582 NA BLUP
#> 25        25 80.73080 1.264582 NA BLUP
#> 26        26 80.31575 1.264582 NA BLUP
#> 27        27 80.47642 1.264582 NA BLUP
#> 28        28 79.99777 1.264582 NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:check) +      (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 361.5
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.4007 -0.5591 -0.1435  0.4250  2.0065 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  treatment:test (Intercept) 12.69    3.563   
#>  env:check      (Intercept)  0.00    0.000   
#>  env:block2     (Intercept) 10.81    3.288   
#>  env            (Intercept)  0.00    0.000   
#>  Residual                   21.53    4.640   
#> Number of obs: 60, groups:  
#> treatment:test, 28; env:check, 15; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13607    1.91634  5.99805  42.339 1.17e-08 ***
#> check1       2.53060    3.39433  2.24198   0.746    0.526    
#> check2      -1.80274    3.39433  2.24198  -0.531    0.643    
#> check3       0.08615    3.39433  2.24198   0.025    0.982    
#> check4       0.64171    3.39433  2.24198   0.189    0.866    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>        (Intr) check1 check2 check3
#> check1  0.084                     
#> check2  0.084 -0.309              
#> check3  0.084 -0.309 -0.309       
#> check4  0.084 -0.309 -0.309 -0.309
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular treatment.test env.check
#> 1 Normal exit from bobyqa                  TRUE       12.69184         0
#>   env.block2 env Residual      AIC    BIC
#> 1   10.81295   0  21.5321 381.5266 402.47
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>       Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> check 27.314  6.8286     4 2.3058  0.3171 0.8501
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ check + (1 | env) + (1 | env:block2) + (1 | env:check) + (1 | treatment:test)
#>                      npar  logLik    AIC    LRT Df Pr(>Chisq)   
#> <none>                 10 -180.76 381.53                        
#> (1 | env)               9 -180.76 379.53 0.0000  1   1.000000   
#> (1 | env:block2)        9 -184.52 387.05 7.5193  1   0.006104 **
#> (1 | env:check)         9 -180.76 379.53 0.0000  1   1.000000   
#> (1 | treatment:test)    9 -181.19 380.39 0.8584  1   0.354174   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE       df type
#> 1          1 83.66667 4.035559 3.389111 BLUE
#> 2          2 79.33333 4.035559 3.389111 BLUE
#> 3          3 81.22222 4.035559 3.389111 BLUE
#> 4          4 81.77778 4.035559 3.389111 BLUE
#> 5          5 79.19710 2.903648       NA BLUP
#> 6          6 79.00884 2.897031       NA BLUP
#> 7          7 84.29258 2.897031       NA BLUP
#> 8          8 77.15460 2.897031       NA BLUP
#> 9          9 78.82625 2.903648       NA BLUP
#> 10        10 81.69665 2.897031       NA BLUP
#> 11        11 79.10072 2.897031       NA BLUP
#> 12        12 78.63799 2.897031       NA BLUP
#> 13        13 79.16036 2.897031       NA BLUP
#> 14        14 79.37853 2.897031       NA BLUP
#> 15        15 80.12022 2.897031       NA BLUP
#> 16        16 84.35221 2.897031       NA BLUP
#> 17        17 79.87531 2.903648       NA BLUP
#> 18        18 78.26599 2.897031       NA BLUP
#> 19        19 81.01459 2.897031       NA BLUP
#> 20        20 78.76277 2.903648       NA BLUP
#> 21        21 77.97738 2.903648       NA BLUP
#> 22        22 83.68680 2.897031       NA BLUP
#> 23        23 78.71907 2.903648       NA BLUP
#> 24        24 77.83822 2.897031       NA BLUP
#> 25        25 81.09087 2.897031       NA BLUP
#> 26        26 78.20907 2.897031       NA BLUP
#> 27        27 79.60748 2.897031       NA BLUP
#> 28        28 76.35484 2.897031       NA BLUP

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 209.4
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.3525 -0.3572  0.0000  0.0000  1.8303 
#> 
#> Random effects:
#>  Groups     Name        Variance Std.Dev.
#>  env:check  (Intercept)  0.00    0.000   
#>  env:block2 (Intercept)  1.78    1.334   
#>  env        (Intercept)  0.00    0.000   
#>  Residual               23.50    4.848   
#> Number of obs: 60, groups:  env:check, 15; env:block2, 9; env, 3
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  79.9958     0.9665  9.3687  82.768 9.68e-15 ***
#> treatment1    3.6708     1.7780 24.1747   2.065  0.04985 *  
#> treatment2   -0.6625     1.7780 24.1747  -0.373  0.71269    
#> treatment3    1.2264     1.7780 24.1747   0.690  0.49693    
#> treatment4    1.7819     1.7780 24.1747   1.002  0.32618    
#> treatment5   -1.3446     4.8828 31.1306  -0.275  0.78485    
#> treatment6   -4.4146     4.8728 30.8073  -0.906  0.37198    
#> treatment7   15.2485     4.8728 30.8073   3.129  0.00382 ** 
#> treatment8   -9.4146     4.8728 30.8073  -1.932  0.06259 .  
#> treatment9   -2.3446     4.8828 31.1306  -0.480  0.63446    
#> treatment10   8.2485     4.8728 30.8073   1.693  0.10059    
#> treatment11   1.2485     4.8728 30.8073   0.256  0.79948    
#> treatment12  -5.4146     4.8728 30.8073  -1.111  0.27508    
#> treatment13   0.5973     4.8728 30.8073   0.123  0.90324    
#> treatment14  -3.7752     4.8728 30.8073  -0.775  0.44439    
#> treatment15  -1.7752     4.8728 30.8073  -0.364  0.71811    
#> treatment16  14.5973     4.8728 30.8073   2.996  0.00537 ** 
#> treatment17  -0.6471     4.8828 31.1306  -0.133  0.89542    
#> treatment18  -6.7752     4.8728 30.8073  -1.390  0.17436    
#> treatment19   5.5973     4.8728 30.8073   1.149  0.25953    
#> treatment20  -3.6471     4.8828 31.1306  -0.747  0.46071    
#> treatment21  -5.8796     4.8828 31.1306  -1.204  0.23761    
#> treatment22  13.3066     4.8728 30.8073   2.731  0.01036 *  
#> treatment23  -3.8796     4.8828 31.1306  -0.795  0.43289    
#> treatment24  -7.0540     4.8728 30.8073  -1.448  0.15782    
#> treatment25   6.3066     4.8728 30.8073   1.294  0.20519    
#> treatment26  -6.0540     4.8728 30.8073  -1.242  0.22346    
#> treatment27   2.3066     4.8728 30.8073   0.473  0.63928    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 28 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.check env.block2 env
#> 1 Normal exit from bobyqa                  TRUE         0   1.780093   0
#>   Residual      AIC      BIC
#> 1 23.50463 273.4126 340.4317
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
#> treatment 1242.4  46.014    27 25.469  1.9577 0.04661 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ treatment + (1 | env) + (1 | env:block2) + (1 | env:check)
#>                  npar  logLik    AIC     LRT Df Pr(>Chisq)
#> <none>             32 -104.71 273.41                      
#> (1 | env)          31 -104.71 271.41 0.00000  1     1.0000
#> (1 | env:block2)   31 -104.82 271.63 0.21914  1     0.6397
#> (1 | env:check)    31 -104.71 271.41 0.00000  1     1.0000
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE        df type
#> 1          1 83.66667 1.676131  7.882789 BLUE
#> 2          2 79.33333 1.676131  7.882789 BLUE
#> 3          3 81.22222 1.676131  7.882789 BLUE
#> 4          4 81.77778 1.676131  7.882789 BLUE
#> 5          5 78.65125 5.501086 30.336883 BLUE
#> 6          6 75.58125 5.501086 30.336883 BLUE
#> 7          7 95.24437 5.501086 30.336883 BLUE
#> 8          8 70.58125 5.501086 30.336883 BLUE
#> 9          9 77.65125 5.501086 30.336883 BLUE
#> 10        10 88.24437 5.501086 30.336883 BLUE
#> 11        11 81.24437 5.501086 30.336883 BLUE
#> 12        12 74.58125 5.501086 30.336883 BLUE
#> 13        13 80.59312 5.501086 30.336883 BLUE
#> 14        14 76.22063 5.501086 30.336883 BLUE
#> 15        15 78.22063 5.501086 30.336883 BLUE
#> 16        16 94.59312 5.501086 30.336883 BLUE
#> 17        17 79.34875 5.501086 30.336883 BLUE
#> 18        18 73.22063 5.501086 30.336883 BLUE
#> 19        19 85.59312 5.501086 30.336883 BLUE
#> 20        20 76.34875 5.501086 30.336883 BLUE
#> 21        21 74.11625 5.501086 30.336883 BLUE
#> 22        22 93.30249 5.501086 30.336883 BLUE
#> 23        23 76.11625 5.501086 30.336883 BLUE
#> 24        24 72.94187 5.501086 30.336883 BLUE
#> 25        25 86.30249 5.501086 30.336883 BLUE
#> 26        26 73.94187 5.501086 30.336883 BLUE
#> 27        27 82.30249 5.501086 30.336883 BLUE
#> 28        28 68.94187 5.501086 30.336883 BLUE

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
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 370.1
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.7073 -0.6393 -0.1755  0.4327  2.0857 
#> 
#> Random effects:
#>  Groups              Name        Variance Std.Dev.
#>  env:treatment:check (Intercept)  0.000   0.000   
#>  treatment           (Intercept)  1.429   1.195   
#>  env:block2          (Intercept) 18.778   4.333   
#>  Residual                        25.171   5.017   
#> Number of obs: 60, groups:  
#> env:treatment:check, 36; treatment, 28; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  80.6029     1.6217  6.6253  49.702 8.97e-10 ***
#> env1          0.7362     2.2436  5.9724   0.328    0.754    
#> env2         -0.8719     2.2436  5.9724  -0.389    0.711    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>      (Intr) env1  
#> env1  0.000       
#> env2  0.000 -0.500
#> optimizer (bobyqa) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
#> 
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt                                       conv_lme4
#> 1        0 boundary (singular) fit: see help('isSingular')
#>               opt_message opt_warnings singular env.treatment.check treatment
#> 1 Normal exit from bobyqa                  TRUE                   0  1.428706
#>   env.block2 Residual      AIC      BIC
#> 1   18.77751 25.17089 384.0833 398.7437
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>     Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#> env 4.4023  2.2011     2 5.9724  0.0874 0.9174
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + (1 | treatment) + (1 | env:block2) + (1 | env:treatment:check)
#>                           npar  logLik    AIC     LRT Df Pr(>Chisq)    
#> <none>                       7 -185.04 384.08                          
#> (1 | treatment)              6 -185.13 382.26  0.1794  1  0.6718853    
#> (1 | env:block2)             6 -191.48 394.96 12.8756  1  0.0003329 ***
#> (1 | env:treatment:check)    6 -185.04 382.08  0.0000  1  1.0000000    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean        SE df type
#> 1          1 81.63881 0.9974662 NA BLUP
#> 2          2 80.17364 0.9974662 NA BLUP
#> 3          3 80.81230 0.9974662 NA BLUP
#> 4          4 81.00015 0.9974662 NA BLUP
#> 5          5 80.47684 1.1671588 NA BLUP
#> 6          6 80.48728 1.1666376 NA BLUP
#> 7          7 81.16592 1.1666376 NA BLUP
#> 8          8 80.21873 1.1666376 NA BLUP
#> 9          9 80.42313 1.1671588 NA BLUP
#> 10        10 80.78994 1.1666376 NA BLUP
#> 11        11 80.41396 1.1666376 NA BLUP
#> 12        12 80.43357 1.1666376 NA BLUP
#> 13        13 80.44487 1.1666376 NA BLUP
#> 14        14 80.54260 1.1666376 NA BLUP
#> 15        15 80.65003 1.1666376 NA BLUP
#> 16        16 81.19683 1.1666376 NA BLUP
#> 17        17 80.59601 1.1671588 NA BLUP
#> 18        18 80.38147 1.1666376 NA BLUP
#> 19        19 80.71343 1.1666376 NA BLUP
#> 20        20 80.43488 1.1671588 NA BLUP
#> 21        21 80.32286 1.1671588 NA BLUP
#> 22        22 81.08899 1.1666376 NA BLUP
#> 23        23 80.43028 1.1671588 NA BLUP
#> 24        24 80.33155 1.1666376 NA BLUP
#> 25        25 80.71301 1.1666376 NA BLUP
#> 26        26 80.38527 1.1666376 NA BLUP
#> 27        27 80.49816 1.1666376 NA BLUP
#> 28        28 80.11671 1.1666376 NA BLUP

# 20. Fixed Effects - env, check; Random Effects - test
out20 <- augmentedRCBD.mix(env = data2$env2, block = data2$blk2,
                           treatment = data2$trt2,
                           y = data2$y2, checks =  c("1", "2", "3", "4"),
                           env.random = FALSE,
                           check.random = FALSE, test.random = TRUE,
                           scenario = "II", console = TRUE)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> 
#> Augmented Design Details
#> ========================
#> NULL
#> 
#> Model Formula
#> =========================
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test)
#> Model Details
#> =========================
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: frmla_int
#>    Data: model.frame(mod_final)
#> Control: lmerControl(optimizer = "bobyqa")
#> 
#> REML criterion at convergence: 330.2
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.1920 -0.5147 -0.1671  0.3962  1.9464 
#> 
#> Random effects:
#>  Groups         Name        Variance Std.Dev.
#>  treatment:test (Intercept)  3.522   1.877   
#>  env:block2     (Intercept) 17.760   4.214   
#>  Residual                   28.225   5.313   
#> Number of obs: 60, groups:  treatment:test, 28; env:block2, 9
#> 
#> Fixed effects:
#>             Estimate Std. Error       df t value Pr(>|t|)    
#> (Intercept) 81.13170    1.75844  3.21727  46.139 1.19e-05 ***
#> env1         0.74220    2.24880  4.81716   0.330    0.755    
#> env2        -1.25321    2.24880  4.81716  -0.557    0.602    
#> check1       2.53497    2.26136  0.32012   1.121    0.652    
#> check2      -1.79836    2.26136  0.32012  -0.795    0.715    
#> check3       0.09053    2.26136  0.32012   0.040    0.981    
#> check4       0.64608    2.26136  0.32012   0.286    0.873    
#> env1:check1  0.25780    2.20767 20.10997   0.117    0.908    
#> env2:check1  0.25321    2.20767 20.10997   0.115    0.910    
#> env1:check2 -1.07554    2.20767 20.10997  -0.487    0.631    
#> env2:check2 -0.08012    2.20767 20.10997  -0.036    0.971    
#> env1:check3  0.03557    2.20767 20.10997   0.016    0.987    
#> env2:check3 -1.30235    2.20767 20.10997  -0.590    0.562    
#> env1:check4  0.81335    2.20767 20.10997   0.368    0.716    
#> env2:check4 -0.52457    2.20767 20.10997  -0.238    0.815    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation matrix not shown by default, as p = 15 > 12.
#> Use print(summary(x$Model), correlation=TRUE)  or
#>     vcov(summary(x$Model))        if you need it
#> 
#> Model Diagnostics
#> =========================
#>   conv_opt conv_lme4             opt_message opt_warnings singular
#> 1        0           Normal exit from bobyqa                 FALSE
#>   treatment.test env.block2 Residual      AIC      BIC
#> 1       3.522165   17.76004 28.22528 366.1819 403.8801
#> 
#> ANOVA, Fixed Effects
#> =========================
#> Type III Analysis of Variance Table with Satterthwaite's method
#>           Sum Sq Mean Sq NumDF   DenDF F value Pr(>F)
#> env        8.865  4.4325     2  4.8172  0.1570 0.8589
#> check     72.357 18.0893     4  2.0000  0.6409 0.6844
#> env:check 54.846  6.8558     8 21.2312  0.2429 0.9774
#> 
#> LRT, Random Effects
#> =========================
#> ANOVA-like table for random-effects: Single term deletions
#> 
#> Model:
#> y ~ env + check + env:check + (1 | env:block2) + (1 | treatment:test)
#>                      npar  logLik    AIC    LRT Df Pr(>Chisq)   
#> <none>                 18 -165.09 366.18                        
#> (1 | env:block2)       17 -169.01 372.02 7.8424  1   0.005104 **
#> (1 | treatment:test)   17 -165.11 364.22 0.0411  1   0.839259   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Model Formula
#> =========================
#>    treatment     mean       SE        df type
#> 1          1 83.66667 2.937966 0.7943792 BLUE
#> 2          2 79.33333 2.937966 0.7943792 BLUE
#> 3          3 81.22222 2.937966 0.7943792 BLUE
#> 4          4 81.77778 2.937966 0.7943792 BLUE
#> 5          5 79.51143 1.783427        NA BLUP
#> 6          6 79.50158 1.781885        NA BLUP
#> 7          7 80.94890 1.781885        NA BLUP
#> 8          8 78.94686 1.781885        NA BLUP
#> 9          9 79.40049 1.783427        NA BLUP
#> 10        10 80.17230 1.781885        NA BLUP
#> 11        11 79.39570 1.781885        NA BLUP
#> 12        12 79.39063 1.781885        NA BLUP
#> 13        13 79.31853 1.781885        NA BLUP
#> 14        14 79.48307 1.781885        NA BLUP
#> 15        15 79.70496 1.781885        NA BLUP
#> 16        16 80.87173 1.781885        NA BLUP
#> 17        17 79.59947 1.783427        NA BLUP
#> 18        18 79.15024 1.781885        NA BLUP
#> 19        19 79.87324 1.781885        NA BLUP
#> 20        20 79.26664 1.783427        NA BLUP
#> 21        21 79.33435 1.783427        NA BLUP
#> 22        22 80.92214 1.781885        NA BLUP
#> 23        23 79.55623 1.783427        NA BLUP
#> 24        24 79.31357 1.781885        NA BLUP
#> 25        25 80.14553 1.781885        NA BLUP
#> 26        26 79.42451 1.781885        NA BLUP
#> 27        27 79.70176 1.781885        NA BLUP
#> 28        28 78.86979 1.781885        NA BLUP

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
