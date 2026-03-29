# For forbidden combos
# Checks and tests partition treatment. Making checks random but tests fixed breaks
# the nesting/partition logic and creates a non-identifiable treatment variance decomposition.

# trouBBlme4SolveR


#' Title
#'
#' @param block
#' @param treatment
#' @param env
#' @param y
#' @param checks
#' @param env.random
#' @param check.random
#' @param test.random
#' @param interaction
#' @param scenario
#'
#' @returns
#'
#' @import lme4
#' @importFrom lmerTest ranova
#' @export
#'
#' @examples
augmentedRCBD.mix <- function(block, treatment, env = NULL,
                              y, checks = NULL,
                              env.random = TRUE,
                              check.random = TRUE,
                              test.random = TRUE,
                              # interaction = TRUE,
                              drop.nonsig.interaction = TRUE,
                              scenario = c("I", "II"),
                              scenario.violation.threshold = 0.1,
                              console = TRUE) {

  # Checks ----

  if (check.random && !test.random) {
    stop("Forbidden combination: check.random = TRUE & test.random = FALSE")
  }

  if (is.null(env) && drop.nonsig.interaction) {
    warning("interaction ignored because menv = FALSE")
    drop.nonsig.interaction <- FALSE
  }

  # block
  if (!is.factor(block)) {
    stop('"block" should be of class "factor".')
  }
  # treatment
  if (!is.factor(treatment)) {
    stop('"treatment" should be of class "factor".')
  }
  # environment
    if (!is.null(env) && !is.factor(env)) {
      stop('"env" should be of class "factor".')
    }
  # y
  if (!(is.vector(y, mode = "integer") |
        is.vector(y, mode = "numeric"))) {
    stop('"y" should be a vector of class "numeric" or "integer".')
  }
  # Size equality of factors
  if (is.null(env)) {
    if (length(unique(lengths(list(y, treatment, block)))) == 1) {
      stop('"block", "treatment" and "y" are of unequal lengths.')
    }
  } else {
    if (!(length(unique(lengths(list(y, treatment, block, env)))) == 1)) {
      stop('"block", "treatment", "env", and "y" are of unequal lengths.')
    }
  }
  # Check for missing values
  if (TRUE %in% is.na(y)) {
    warning('"y" has missing value(s).')
  }

  # Prepare factors ----

  if (!is.null(env)) {
    block2 <- interaction(env, block, drop = TRUE, sep = "_")
  }

  tests <- levels(treatment)[!(levels(treatment) %in% checks)]

  test <- ifelse(treatment %in% checks, 0, 1)
  check <- ifelse(treatment %in% checks,
                  treatment, "test")

  # Fix treatment order so that checks are in the beginning ----
  treatment_org <- treatment
  treatment <- factor(treatment,
                      levels = c(checks, setdiff(levels(treatment),
                                                 checks)))
  if (!is.null(env)) {
   data_df <- data.frame(y, block2, treatment, env, test, check)
  } else {
   data_df <- data.frame(y, block, treatment, test, check)
  }

  # Force scenarion to be NULL when there is no env
  if (is.null(env)) {
    scenario = NULL
  }

  # Automatic scenario validation ----

  if (!is.null(env)) {

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

      if (all(test_env_counts$env < 2)) {
        stop(
          "Scenario I violation: ",
          "All test treatments appear in only one environment:\n"
        )
      }

      if (any(test_env_counts$env < 2)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env < 2])

        if ((length(bad) / length(tests)) > scenario.violation.threshold) {

          stop(sprintf(
            paste0(
              'Scenario I violation: %.0f%% of test treatments appear ',
              'in only one environment, exceeding the ',
              '"scenario.violation.threshold" (%.0f%%).'
            ),
            (length(bad) / length(tests)) * 100,
            scenario.violation.threshold * 100
          ))

        } else {
          warning(
            "Scenario I violation: ",
            "The following ", length(bad),
            " test treatments appear in only one environment:\n\n",
            paste(bad, collapse = ", "), "\n"
          )
        }

      }
    }

    if (scenario == "II") {
      if (any(test_env_counts$env > 1)) {
        bad <- as.character(test_env_counts$trt[test_env_counts$env > 1])

        stop(sprintf(
          paste0(
            'Scenario II violation: %.0f%% of test treatments appear ',
            'in multiple environments, exceeding the ',
            '"scenario.violation.threshold" (%.0f%%).'
          ),
          (length(bad) / length(tests)) * 100,
          scenario.violation.threshold * 100
        ))

        warning(
          "Scenario II violation: ",
          "The following ", length(bad),
          " test treatments appear in multiple environments:\n",
          paste(bad, collapse = ", "), "\n"
        )
      }
    }
  }


  #  Build formula ----

  menv <- ifelse(is.null(env), FALSE, TRUE)

  frmla_int <- build_formula(block = ifelse(is.null(env), "block", "block2"),
                             treatment = "treatment",
                             test = "test", check = "check",
                             env = "env", y = "y",
                             menv = menv,
                             env.random = env.random,
                             check.random = check.random,
                             test.random = test.random,
                             scenario = scenario, interaction = TRUE)
  frmla_wo_int <- build_formula(block = ifelse(is.null(env), "block", "block2"),
                                treatment = "treatment",
                                test = "test", check = "check",
                                env = "env", y = "y",
                                menv = menv,
                                env.random = env.random,
                                check.random = check.random,
                                test.random = test.random,
                                scenario = scenario, interaction = FALSE)

  # Fit the model ----
  mod_final <-
    lmer(frmla_int, data = data_df, REML = FALSE) # Use ML for testing

  if (drop.nonsig.interaction) {
    ## Decision on inclusion of env test-treatment interactions ----
    mod_wo_int <-
      lmer(frmla_wo_int, data = data_df, REML = FALSE) # Use ML for testing

    LRT <- anova(mod_wo_int, mod_final)
    LRT_p <- tibble::deframe(broom::tidy(LRT)[2, "p.value"])

    if (LRT_p > 0.05) {
      mod_final <- mod_wo_int
    }
  }

  # Use bobyqa if model is still singular
  if (isSingular(mod_final, tol = 1e-4)) {
    update(mod_final, control = lmerControl(optimizer = "bobyqa"),
           data = model.frame(mod_final))
  }


  ## Refit with REML ----
  mod_final <- update(mod_final, REML = TRUE, data = model.frame(mod_final))


  # Diagnostics ----

  vcov_df <- data.frame(VarCorr(mod_final))

  vcov_out <- vcov_df$vcov
  names(vcov_out) <- vcov_df$grp
  rm(vcov_df)

  mod_diag <-
    c(list(conv_opt = mod_final@optinfo$conv$opt,
           conv_lme4 = paste(mod_final@optinfo$conv$lme4, collapse = "\n"),
           opt_message = paste(mod_final@optinfo$message, collapse = "\n"),
           opt_warnings = paste(mod_final@optinfo$warnings, collapse = "\n"),
           singular = isSingular(mod_final)),
      as.list(vcov_out))


  AIC_val <- AIC(mod_final)
  BIC_val <- BIC(mod_final)

  mod_diag <- c(mod_diag, AIC = AIC_val, BIC = BIC_val)

  # ANOVA ----

  ## Fixed effects ----

  if (length(lme4::fixef(mod_final)) > 0) {
    # fixef_anova <-
    #   getS3method("anova", "lmerModLmerTest",
    #               envir = asNamespace("lmerTest"))(mod_final,
    #                                                type = 3,
    #                                                ddf = "Satterthwaite")

    fixef_anova <- stats::anova(mod_final,  type = 3,
                                ddf = "Satterthwaite")


  } else {
    fixef_anova <- NULL
  }

  ## Random effects ----

  ranef_anova <- lmerTest::ranova(mod_final)


  # Adjusted means ----

  check_mean <- if (check.random) {
    "BLUP"
  } else {
    "BLUE"
  }

  test_mean <- if (test.random) {
    "BLUP"
  } else {
    "BLUE"
  }

  test_mean <- if (scenario == "II") {
    paste0(test_mean, "_within_env")
  } else {
    test_mean
  }

  aug_adj_means <-
    get_aug_means(mod = mod_final, checks = checks,
                  tests = tests, env = env,
                  check_mean = check_mean,
                  test_mean = test_mean)


  # Final output ----

  output <- list(Details = NULL,
                 Model = mod_final,
                 `Model Diagnostics` = mod_diag,
                 `ANOVA, Fixed Effects` = fixef_anova,
                 `LRT, Random Effects` = ranef_anova,
                 `Means` = aug_adj_means)

  # Set Class
  class(output) <- "augmentedRCBD.mix"

  # if (console) {
  #   print.augmentedRCBD.mix(output)
  # }

  return(output)

}


# Function to get adjusted means ----
get_aug_means <- function(mod, checks, tests, env = NULL,
                          check_mean = c("BLUE","BLUP"),
                          test_mean = c("BLUE","BLUP",
                                        "BLUE_within_env",
                                        "BLUP_within_env")) {

  check_mean <- match.arg(check_mean)
  test_mean <- match.arg(test_mean)

  fe <- lme4::fixef(mod)
  re <- lme4::ranef(mod)

  intercept <- fe["(Intercept)"]

  ## Check means ----

  if (check_mean == "BLUE") {

    check_blue <- sapply(checks, function(ch) {

      coef_name <- paste0("check", ch)

      intercept +
        if (coef_name %in% names(fe)) fe[coef_name] else 0

    })

    adjm_check <- data.frame(
      treatment = checks,
      Mean = check_blue
    )
  }

  if (check_mean == "BLUP") {

    check_re <- re$treatment

    adjm_check <- data.frame(
      treatment = rownames(check_re),
      Mean = intercept + check_re[, "(Intercept)"]
    )

    adjm_check <- adjm_check[adjm_check$treatment %in% checks, ]
  }


  ## Test means ----

  if (test_mean == "BLUP") {

    test_re <- re$`treatment:test`

    adjm_test <- data.frame(
      treatment = gsub("^(.+)(:.+)$", "\\1", rownames(test_re)),
      Mean = intercept + test_re[, "(Intercept)"]
    )

    adjm_test <- adjm_test[adjm_test$treatment %in% tests, ]
  }


  if (test_mean == "BLUE") {

    test_blue <- sapply(tests, function(tr) {

      coef_name <- paste0("treatment", tr)

      intercept +
        if (coef_name %in% names(fe)) fe[coef_name] else 0

    })

    adjm_test <- data.frame(
      treatment = tests,
      Mean = test_blue
    )
  }

  ## Within-environment cases ----

  if (test_mean %in% c("BLUP_within_env","BLUE_within_env")) {

    env_levels <- levels(env)

    adj_list <- lapply(env_levels, function(e) {

      if (test_mean == "BLUP_within_env") {

        re_env <- re$`env:treatment:test`

        idx <- grepl(paste0("^", e, ":"), rownames(re_env))

        tmp <- re_env[idx, , drop = FALSE]

        data.frame(
          env = e,
          treatment = gsub("^[^:]+:([^:]+):.*$", "\\1", rownames(tmp)),
          Mean = intercept + tmp[, "(Intercept)"]
        )

      } else {

        sapply(tests, function(tr) {

          coef_name <- paste0("env", e, ":treatment", tr)

          intercept +
            if (coef_name %in% names(fe)) fe[coef_name] else 0

        })

      }

    })

    adjm_test <- do.call(rbind, adj_list)
  }

  ## Final output ----

  if (exists("adjm_check")) {
    out <- rbind(adjm_check, adjm_test)
  } else {
    out <- adjm_test
  }

  rownames(out) <- NULL

  return(out)
}


# Formula builder ----
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

  ## Forbidden configuration (Rule 99)  ----
  if (check.random && !test.random) {
    stop("Forbidden: check.random = TRUE & test.random = FALSE")
  }

  ## Scenario II never allows test x env interaction
  if (scenario == "II") interaction <- FALSE

  fixed  <- character()
  random <- character()

  ## Block term (Rule 1) ----

  if (menv) {
    random <- c(random, sprintf("(1|%s:%s)", env, block))
  } else {
    random <- c(random, sprintf("(1|%s)", block))
  }

  ## Environment main effect (Rule 2) ----

  if (menv) {
    if (env.random) {
      random <- c(random, sprintf("(1|%s)", env))
    } else {
      fixed <- c(fixed, env)
    }
  }

  ## Treatment main effects (Rules 3–6)  ----

  trt_case <- NULL

  if (check.random && test.random) {
    trt_case <- "both_random"
    random <- c(random, sprintf("(1|%s)", treatment))
  }

  if (!check.random && test.random) {
    trt_case <- "check_fixed"
    fixed  <- c(fixed, check)
    random <- c(random, sprintf("(1|%s:%s)", treatment, test))
  }

  if (!check.random && !test.random) {
    trt_case <- "both_fixed"
    fixed <- c(fixed, treatment)
  }

  ## Interactions (Rules 7–12)  ----

  if (menv) {

    if (scenario == "I") {

      if (interaction) {

        if (trt_case == "both_random") {
          random <- c(random, sprintf("(1|%s:%s)", env, treatment))
        }

        if (trt_case == "check_fixed") {
          fixed  <- c(fixed, sprintf("%s:%s", env, check))
          random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, test))
        }

        if (trt_case == "both_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, treatment))
        }

      } else {

        if (trt_case == "both_random") {
          random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, check))
        }

        if (trt_case == "check_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, check))
        }

        if (trt_case == "both_fixed") {
          fixed <- c(fixed, sprintf("%s:%s", env, check))
        }

      }
    }

    if (scenario == "II") {

      if (trt_case == "both_random") {
        random <- c(random, sprintf("(1|%s:%s:%s)", env, treatment, check))
      }

      if (trt_case == "check_fixed") {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
      }

      if (trt_case == "both_fixed") {
        fixed <- c(fixed, sprintf("%s:%s", env, check))
      }

    }
  }

  ## Build formula

  rhs <- paste(c(fixed, random), collapse = " + ")
  as.formula(paste(y, "~", rhs))

}


