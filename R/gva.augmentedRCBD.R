#'Perform Genetic Variability Analysis on \code{augmentedRCBD} Output
#'
#'\code{gva.augmentedRCBD} performs genetic variability analysis on an object of
#'class \code{augmentedRCBD}.
#'
#'\code{gva.augmentedRCBD} performs genetic variability analysis from the ANOVA
#'results in an object of class \code{augmentedRCBD} and computes several
#'variability estimates.
#'
#'The phenotypic, genotypic and environmental variance
#'(\ifelse{html}{\out{<i>&sigma;<sup>2</sup><sub>p</sub></i>}}{\eqn{\sigma^{2}_{p}}},
#'\ifelse{html}{\out{<i>&sigma;<sup>2</sup><sub>g</sub></i>}}{\eqn{\sigma^{2}_{g}}}
#'and
#'\ifelse{html}{\out{<i>&sigma;<sup>2</sup><sub>e</sub></i>}}{\eqn{\sigma^{2}_{e}}}
#') are obtained from the ANOVA tables as follows:
#'
#'\ifelse{html}{\out{<p style="text-align:
#'center;"><em>&sigma;<sup>2</sup><sub>p</sub></em> = Sum of squares of test
#'treatments(genotypes)</p>}}{\deqn{\sigma^{2}_{p} = \textrm{Sum of squares of
#'test treatments(genotypes)}}} \ifelse{html}{\out{<p style="text-align:
#'center;"><em>&sigma;<sup>2</sup><sub>e</sub></em> = Sum of squares of
#'residuals(error)</p>}}{\deqn{\sigma^{2}_{e} = \textrm{Sum of squares of
#'residuals(error)}}} \ifelse{html}{\out{<p style="text-align:
#'center;"><em>&sigma;<sup>2</sup><sub>g</sub></em> =
#'<em>&sigma;<sup>2</sup><sub>p</sub></em> &minus;
#'<em>&sigma;<sup>2</sup><sub>e</sub></em></p>}}{\deqn{\sigma^{2}_{g} =
#'\sigma^{2}_{p} - \sigma^{2}_{e}}}
#'
#'Phenotypic and genotypic coefficients of variation
#'(\ifelse{html}{\out{<i>PCV</i>}}{\eqn{PCV}} and
#'\ifelse{html}{\out{<i>GCV</i>}}{\eqn{GCV}}) are estimated according to Burton
#'(1951, 1952) as follows:
#'
#'\ifelse{html}{\out{<p style="text-align: center;"><em>PCV =
#'<big>[</big><sup>&sigma;<sup>2</sup><sub>p</sub></sup> &frasl; <sub>&radic;
#'(<span style="text-decoration:overline">x</span>)</sub><big>]</big></em>
#'&times; 100</p>}}{\deqn{PCV = \frac{\sigma^{2}_{p}}{\sqrt{\overline{x}}}
#'\times 100}} \ifelse{html}{\out{<p style="text-align: center;"><em>GCV =
#'<big>[</big><sup>&sigma;<sup>2</sup><sub>g</sub></sup> &frasl; <sub>&radic;
#'(<span style="text-decoration:overline">x</span>)</sub><big>]</big></em>
#'&times; 100</p>}}{\deqn{GCV = \frac{\sigma^{2}_{g}}{\sqrt{\overline{x}}}
#'\times 100}}
#'
#'Where \ifelse{html}{\out{<i><span style="text-decoration:
#'overline;">x</span></i>}}{\eqn{\overline{x}}} is the mean.
#'
#'The estimates of \ifelse{html}{\out{<i>PCV</i>}}{\eqn{PCV}} and
#'\ifelse{html}{\out{<i>GCV</i>}}{\eqn{GCV}} are categorised according to
#'Sivasubramanian and Madhavamenon (1978) as follows:
#'
#'\tabular{ll}{ \strong{\emph{CV} (\%)} \tab \strong{Category} \cr x
#'\ifelse{html}{\out{<}}{\eqn{<}} 10 \tab Low \cr 10
#'\ifelse{html}{\out{&le;}}{\eqn{\le}} x \ifelse{html}{\out{<}}{\eqn{<}} 20 \tab
#'Medium \cr \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 20 \tab High }
#'
#'The broad-sense heritability
#'(\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}}) is calculated
#'according to method of Lush (1940) as follows:
#'
#'\ifelse{html}{\out{<p style="text-align: center;"><em>H<sup>2</sup> =
#'<sup>&sigma;<sup>2</sup><sub>g</sub></sup> &frasl;
#'<sub>&sigma;<sup>2</sup><sub>p</sub></sub></em></p>}}{\deqn{H^{2} =
#'\frac{\sigma^{2}_{g}}{\sigma^{2}_{p}}}}
#'
#'The estimates of broad-sense heritability
#'(\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}}) are cateogrised
#'according to Robinson (1966) as follows:
#'
#'\tabular{ll}{ \strong{\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}}}
#'\tab \strong{Category} \cr x \ifelse{html}{\out{<}}{\eqn{<}} 30 \tab Low \cr
#'30 \ifelse{html}{\out{&le;}}{\eqn{\le}} x \ifelse{html}{\out{<}}{\eqn{<}} 60
#'\tab Medium \cr \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 60 \tab High }
#'
#'Genetic advance (\ifelse{html}{\out{<i>GA</i>}}{\eqn{GA}}) and genetic advance
#'as per cent of mean (\ifelse{html}{\out{<i>GAM</i>}}{\eqn{GAM}}) are estimated
#'and categorised according to Johnson et al., (1955) as follows:
#'
#'\ifelse{html}{\out{<p style="text-align: center;"><em>GA = k &times;
#'&sigma;<sub>g</sub> &times; <big>[</big><sup>H<sup>2</sup></sup> &frasl;
#'</sub>100</sub><big>]</big></em></p>}}{\deqn{GA = k \times \sigma_{g} \times
#'\frac{H^{2}}{100}}}
#'
#'\ifelse{html}{\out{<p style="text-align: center;"><em>GAM = <big>[</big>
#'<sup>GA</sup> &frasl; <sub><span
#'style="text-decoration:overline">x</span></sub> <big>]</big> &times;
#'100</em></p>}}{\deqn{GAM = \frac{GA}{\overline{x}} \times 100}}
#'
#'\tabular{ll}{ \emph{\strong{GAM}} \tab \strong{Category} \cr x
#'\ifelse{html}{\out{<}}{\eqn{<}} 10 \tab Low \cr 10
#'\ifelse{html}{\out{&le;}}{\eqn{\le}} x \ifelse{html}{\out{<}}{\eqn{<}} 20 \tab
#'Medium \cr \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 20 \tab High }
#'
#'@param aug An object of class \code{augmentedRCBD}.
#'
#'@return A list with the following descriptive statistics:  \item{Count}{The
#'  number of treatments/genotypes.} \item{Mean}{The mean value.}
#'  \item{Std.Error}{The standard error.} \item{Std.Deviation}{The standard
#'  deviation.} \item{Min}{The minimum value} \item{Max}{The maximum value}
#'  \item{Skewness(statistic)}{The skewness estimator.}
#'  \item{Skewness(p.value)}{The p-value from D'Agostino test of skewness.}
#'  \item{Kurtosis(statistic)}{The kurtosis estimator.}
#'  \item{Kurtosis(p.value)}{The p-value from Anscombe-Glynn test of kurtosis.}
#'
#'@seealso \code{\link[augmentedRCBD]{augmentedRCBD}}
#'@references
#'
#'\insertRef{lush_intra-sire_1940}{augmentedRCBD}
#'
#'\insertRef{burton_quantitative_1951}{augmentedRCBD}
#'
#'\insertRef{burton_qualitative_1952}{augmentedRCBD}
#'
#'\insertRef{johnson_estimates_1955}{augmentedRCBD}
#'
#'\insertRef{robinson_quantitative_1966}{augmentedRCBD}
#'
#'\insertRef{sivasubramanian_genotypic_1973}{augmentedRCBD}
#'
#'@export
#'
#' @examples


#'
#' gva not necessary if "Treatment: Test" non significant
#'
#' values



# https://juniperpublishers.com/artoaj/ARTOAJ.MS.ID.555744.php

gva.augmentedRCBD <- function(aug, k = 2.06) {

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class augmentedRCBD')
  }

  PV <- aug$`ANOVA, Block Adjusted`[[1]]$`Sum Sq`["Test"]
  EV <- aug$`ANOVA, Block Adjusted`[[1]]["Residuals", "Sum Sq"]
  GV <- PV - EV
  Mean <- mean(aug$Means$`Adjusted Means`)
  GCV <- (sqrt(GV)/Mean)*100 # Burton 1951 1952
  GCV_category <- ifelse(GCV >= 20, "High", ifelse(GCV >= 10, "Medium", "Low"))
  PCV <- (sqrt(PV)/Mean)*100 # Burton 1951 1952
  PCV_category <- ifelse(PCV >= 20, "High", ifelse(PCV >= 10, "Medium", "Low"))
  ECV <- (sqrt(EV)/Mean)*100 # Burton 1951 1952
  hBS <- (GV/PV)*100 # Lush 1940
  hBS <- ifelse(hBS < 0, NA, hBS) # for negative hbs
  hBS_categroy <- ifelse(hBS >= 60, "High", ifelse(hBS >= 30, "Medium", "Low")) # Robinson 1966
  GA <- k * sqrt(PV) * (hBS/100) # Johnson et al. 1955
  GAM <- (GA/Mean)*100
  GAM_category <-  ifelse(GAM >= 20, "High", ifelse(GAM >= 10, "Medium", "Low"))

  out <- list(Mean = Mean, PV = PV, GV = GV, EV = EV,
              GCV = GCV, `GCV category` = GCV_category,
              PCV = PCV, `PCV category` = PCV_category,
              ECV = ECV, hBS = hBS, `hBS category` = hBS_categroy,
              GA = GA, GAM = GAM, `GAM category` = GAM_category)
  return(out)

}

