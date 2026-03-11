# trouBBlme4SolveR

augmentedRCBD.mix <- function(block, treatment, env = NULL,
                              y, checks = NULL,
                              env.random = TRUE,
                              check.random = TRUE,
                              test.random = TRUE,
                              interaction = TRUE,
                              scenario = c("I", "II")) {

  # Checks ----

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
          "Scenario I violation: ",
          "Test treatments appear in only one environment:\n",
          paste(bad, collapse = ", ")
        )
      }
    }

    if (scenario == "II") {
      if (any(test_env_counts$env > 1)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env > 1])
        stop(
          "Scenario II violation: ",
          "Test treatments appear in multiple environments:\n",
          paste(bad, collapse = ", ")
        )
      }
    }
  }

  #  Build formula ----

  menv <- ifelse(is.null(menv), TRUE, FALSE)

  frmla <- build_formula(menv = menv,
                         env.random = env.random,
                         check.random = check.random,
                         test.random = test.random,
                         scenario = scenario)

  # Fit the model ----
  # Decision on inclusion of env treatment interactions ----

}




build_formula <- function(block = "block2", treatment = "treatment",
                          test = "test", check = "check", env = "env",
                          y = "y",
                          menv = TRUE,
                          env.random = TRUE,
                          check.random = TRUE,
                          test.random = TRUE,
                          interaction = TRUE,
                          scenario = c("I", "II")) {


  scenario <- match.arg(scenario)


  # Forbidden combination
  if (check.random && !test.random) {
    stop("Forbidden: check.random = TRUE & test.random = FALSE")
  }

  fixed  <- character()
  random <- character()

  # Block term ----
  if (menv) {
    random <- c(random, sprintf("(1|%s:%s)", env, block))
  } else {
    random <- c(random, sprintf("(1|%s)", block))
  }

  # Environment main effect ----
  if (menv) {
    if (env.random) {
      random <- c(random, sprintf("(1|%s)", env))
    } else {
      fixed <- c(fixed, env)
    }
  }

  # Treatment structure ----
  if (check.random && test.random) {
    random <- c(random, sprintf("(1|%s)", treatment))
  } else if (!check.random && !test.random) {
    fixed <- c(fixed, treatment)
  } else if (!check.random && test.random) {
    fixed  <- c(fixed, check)
    random <- c(random, sprintf("(1|%s:%s)", treatment, test))
  }

  # Interaction (No test x env) ----
  if (menv && scenario == "I") {
    if (interaction) {  # only include interaction if TRUE
      if (!check.random && test.random) {
        fixed  <- c(fixed, sprintf("%s:%s", env, check))
        random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, test))
      }
      if (check.random && test.random) {
        random <- c(random, sprintf("(1|%s:%s)", env, treatment))
      }
      if (!check.random && !test.random) {
        fixed <- c(fixed, sprintf("%s:%s", env, treatment))
      }
    } else {  # interaction = FALSE for scenario I
      if (!check.random && test.random) {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
        # no env:treatment:test random
      }
      if (check.random && test.random) {
        random <- c(random, sprintf("(1|%s:%s:check)", env, treatment))
      }
      if (!check.random && !test.random) {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
      }
    }
  }

  # Scenario II: always no interaction (No test x env)
  # Only check x env is there
  if (menv && scenario == "II") {
    if (!check.random && test.random) {
      fixed <- c(fixed, sprintf("%s:%s", env, check))
    }
    if (check.random && test.random) {
      random <- c(random, sprintf("(1|%s:%s)", env, treatment))
    }
    if (!check.random && !test.random) {
      fixed <- c(fixed, sprintf("%s:%s", env, check))
    }
  }

  rhs <- paste(c(fixed, random), collapse = " + ")

  formula <- as.formula(paste(y, "~", rhs))
  return(formula)
}
