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

  - *Model:* `y ~ (1|block) + (1|treatment)`

  - *Mean Estimate:* Check and Test treatments (BLUP)

- **2. Single Environment:** *Fixed Effects* - check; *Random Effects* -
  test:

  - *Model:* `y ~ check + (1|block) + (1|treatment:test)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP)

- **3. Single Environment:** *Fixed Effects* - check, test:

  - *Model:* `y ~ treatment + (1|block)`

  - *Mean Estimate:* Check and Test treatments (BLUE)

- **4. Multiple Environments - Scenario I:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  - *Model:*
    `y ~ (1|env) + (1|env:block) + (1|treatment) + (1|env:treatment:check)`

  - *Mean Estimate:* Check and Test treatments (BLUP)

- **5. Multiple Environments - Scenario I:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  TRUE:

  - *Model:*
    `y ~ (1|env) + (1|env:block) + (1|treatment) + (1|env:treatment)`

  - *Mean Estimate:* Check and Test treatments (BLUP)

- **6. Multiple Environments - Scenario I:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ check + (1|env) + (1|env:block) + (1|treatment:test) + (1|env:check)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP)

- **7. Multiple Environments - Scenario I:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  - *Model:*
    `y ~ check + (1|env) + (1|env:block) + (1|treatment:test) + (1|env:treatment)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP)

- **8. Multiple Environments - Scenario I:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:* `y ~ treatment + env:check + (1|env) + (1|env:block)`

  - *Mean Estimate:* Check and Test treatments (BLUE)

- **9. Multiple Environments - Scenario I:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  - *Model:* `y ~ treatment + env:treatment + (1|env) + (1|env:block)`

  - *Mean Estimate:* Check and Test treatments (BLUE)

- **10. Multiple Environments - Scenario I:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ env + (1|env:block) + (1|treatment) + (1|env:treatment:check)`

  - *Mean Estimate:* Check and Test treatments (BLUP)

- **11. Multiple Environments - Scenario I:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  - *Model:*
    `y ~ env + (1|env:block) + (1|treatment) + (1|env:treatment)`

  - *Mean Estimate:* Check and Test treatments (BLUP)

- **12. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ env + check + env:check + (1|env:block) + (1|treatment:test)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP)

- **13. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - TRUE:

  - *Model:*
    `y ~ env + check + env:check + (1|env:block) + (1|treatment:test) + (1|env:treatment:test)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP)

- **14. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  - *Model:* `y ~ env + treatment + env:check + (1|env:block)`

  - *Mean Estimate:* Check and Test treatments (BLUE)

- **15. Multiple Environments - Scenario I:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  TRUE:

  - *Model:* `y ~ env + treatment + env:treatment + (1|env:block)`

  - *Mean Estimate:* Check and Test treatments (BLUE)

- **16. Multiple Environments - Scenario II:** *Random Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  - *Model:*
    `y ~ (1|env) + (1|env:block) + (1|treatment) + (1|env:treatment:check)`

  - *Mean Estimate:* Check treatment (BLUP) and Test treatment (BLUP
    within Environment)

- **17. Multiple Environments - Scenario II:** *Fixed Effects* - check;
  *Random Effects* - env, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ check + (1|env) + (1|env:block) + (1|treatment:test) + (1|env:check)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP
    within Environment)

- **18. Multiple Environments - Scenario II:** *Fixed Effects* - check,
  test; *Random Effects* - env; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:* `y ~ treatment + env:check + (1|env) + (1|env:block)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUE
    within Environment)

- **19. Multiple Environments - Scenario II:** *Fixed Effects* - env;
  *Random Effects* - check, test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ env + (1|env:block) + (1|treatment) + (1|env:treatment:check)`

  - *Mean Estimate:* Check treatment (BLUP) and Test treatment (BLUP
    within Environment)

- **20. Multiple Environments - Scenario II:** *Fixed Effects* - env,
  check; *Random Effects* - test; *Test treatment \\\times\\ Environment
  Interaction* - FALSE:

  - *Model:*
    `y ~ env + check + env:check + (1|env:block) + (1|treatment:test)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUP
    within Environment)

- **21. Multiple Environments - Scenario II:** *Fixed Effects* - env,
  check, test; *Test treatment \\\times\\ Environment Interaction* -
  FALSE:

  - *Model:* `y ~ env + treatment + env:check + (1|env:block)`

  - *Mean Estimate:* Check treatment (BLUE) and Test treatment (BLUE
    within Environment)

## Note

- Making checks random but tests fixed breaks the nesting/partition
  logic and creates a non-identifiable treatment variance decomposition.
  So this combination is not implemented in this function.

## See also

[`augmentedRCBD`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.md),
`augmentedRCBD`
