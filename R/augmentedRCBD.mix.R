# trouBBlme4SolveR

augmentedRCBD.mix <- function(block, treatment, env = NULL,
                              y, checks = NULL,
                              env.random = TRUE,
                              check.random = TRUE,
                              test.random = TRUE,
                              scenario = c("I", "II")) {


  if (check.random && !test.random) {
    stop("Forbidden combination: check.random = TRUE & test.random = FALSE")
  }

  if (!menv && interaction) {
    warning("interaction ignored because menv = FALSE")
    interaction <- FALSE
  }

  # Automatic scenario validation ----

  if (menv) {

    ## Identify tests vs checks
    test_flag  <-
      ifelse(treatment %in% levels(treatment)[treatment %in% treatment],
             !(treatment %in% unique(treatment[treatment %in% treatment])),
             NA)
    test_flag <-
      ifelse(treatment %in% unique(treatment), 1, 1)  # placeholder safety
    test_flag <-
      ifelse(treatment %in% levels(treatment)[treatment %in% treatment], 1, 1)

    ## More robust: infer from factor 'check'
    test_flag <- ifelse(as.character(treatment) %in%
                          as.character(unique(treatment[check == "test"])),
                        1, 0)

    dat_tmp <- data.frame(trt = treatment,
                          env = env,
                          test = test_flag)

    test_env_counts <- aggregate(env ~ trt,
                                 data = dat_tmp[dat_tmp$test == 1, ],
                                 FUN = function(x) length(unique(x)))

    if (scenario == "I") {
      if (any(test_env_counts$env < 2)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env < 2])
        stop(
          "Scenario I violation: the following test treatments occur in only one environment:\n",
          paste(bad, collapse = ", ")
        )
      }
    }

    if (scenario == "II") {
      if (any(test_env_counts$env > 1)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env > 1])
        stop(
          "Scenario II violation: the following test treatments occur in multiple environments:\n",
          paste(bad, collapse = ", ")
        )
      }
    }
  }


}




build_formula <- function(menv = TRUE,
                          env.random = TRUE,
                          check.random = TRUE,
                          test.random = TRUE,
                          scenario = c("I", "II")) {

  scenario <- match.arg(scenario)

  # Forbidden combination
  if (check.random && !test.random) {
    stop("Forbidden: check.random = TRUE & test.random = FALSE")
  }

  fixed  <- character()
  random <- character()

  # Block term (always random) ----
  if (menv) {
    random <- c(random, sprintf("(1|%s:%s)", "env", "block2"))
  } else {
    random <- c(random, sprintf("(1|%s)", "block"))
  }

  # Environment main effect ----
  if (menv) {
    if (env.random) random <- c(random, sprintf("(1|%s)", "env"))
    else            fixed <- c(fixed, "env")
  }

  # --- Treatment structure
  if (check.random && test.random) {
    random <- c(random, "(1|treatment)")
  }

  if (!check.random && !test.random) {
    fixed <- c(fixed, "treatment")
  }

  if (!check.random && test.random) {
    fixed  <- c(fixed, "check")
    random <- c(random, "(1|treatment:test)")
  }

  # --- Env × treatment interactions
  if (menv) {
    if (scenario == "I") {
      if (!check.random && test.random) {
        fixed  <- c(fixed, "env:check")
        random <- c(random, "(1|env:treatment:test)")
      }
      if (check.random && test.random) {
        random <- c(random, "(1|env:treatment)")
      }
      if (!check.random && !test.random) {
        fixed <- c(fixed, "env:treatment")
      }
    }

    # Scenario 2 (tests unique per env)
    if (scenario == "II") {
      if (!check.random && !test.random) {
        fixed <- c(fixed, "env:check")  # test: env:treatment not estimable
      }
      if (!check.random && test.random) {
        fixed  <- c(fixed, "env:check")
        # test random within env already in (1|treatment:test)
      }
      if (check.random && test.random) {
        random <- c(random, "(1|env:check)")
      }
    }
  }

  rhs <- paste(c(fixed, random), collapse = " + ")
  formula <- as.formula(paste(deparse(substitute(y)), "~", rhs))

  return(formula)
}
