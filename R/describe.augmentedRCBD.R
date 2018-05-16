#' Compute Descriptive Statistics from \code{augmentedRCBD} Output
#'
#' \code{describe.augmentedRCBD} computes descriptive statistics from the
#' adjusted means in an object of class \code{augmentedRCBD}.
#'
#' \code{describe.augmentedRCBD} computes the following descriptive statistics
#' from the adjusted means in an object of class \code{augmentedRCBD}.
#'
#' \itemize{ \item Count \item Mean \item Standard deviation \item Standard
#' error \item Minimum \item Maximum \item Skewness statistic along with p-value
#' from D'Agostino test of skewness (D'Agostino, 1970). \item Kurtosis statistic
#' along with p-value from Anscombe-Glynn test of kurtosis (Anscombe and Glynn,
#' 1983). }
#'
#' @param aug An object of class \code{augmentedRCBD}.
#'
#' @return A list with the following descriptive statistics:  \item{Count}{The
#'   number of treatments/genotypes.} \item{Mean}{The mean value.}
#'   \item{Std.Error}{The standard error.} \item{Std.Deviation}{The standard
#'   deviation.} \item{Min}{The minimum value} \item{Max}{The maximum value}
#'   \item{Skewness(statistic)}{The skewness estimator.}
#'   \item{Skewness(p.value)}{The p-value from D'Agostino test of skewness.}
#'   \item{Kurtosis(statistic)}{The kurtosis estimator.}
#'   \item{Kurtosis(p.value)}{The p-value from Anscombe-Glynn test of kurtosis.}
#'
#' @seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#' @references
#'
#' \insertRef{dagostino_transformation_1970}{augmentedRCBD}
#'
#' \insertRef{anscombe_distribution_1983}{augmentedRCBD}
#'
#' @export
#'
#' @examples
describe.augmentedRCBD <- function(aug) { # all treatments (test + checks)

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class augmentedRCBD')
  }

  adjmeans <- aug$Means$`Adjusted Means`
  Mean <- mean(adjmeans)
  Count <- length(adjmeans)
  Skewness <- moments::agostino.test(adjmeans, alternative = "two.sided")
  Kurtosis <- moments::anscombe.test(adjmeans, alternative = "two.sided")
  Range <- range(adjmeans)
  stddev = sd(adjmeans)
  stderror = stddev/sqrt(Count)

  out <- list(Count = Count, Mean = Mean, `Std.Error` = stderror,
              `Std.Deviation` = stddev, Min = Range[1], Max = Range[2],
              `Skewness(statistic)` = Skewness$statistic,
              `Skewness(p.value)` = Skewness$p.value,
              `Kurtosis(statistic)` = Kurtosis$statistic,
              `Kurtosis(p.value)` = Kurtosis$p.value)
  return(out)
}
