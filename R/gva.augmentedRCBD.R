 ### This file is part of 'augmentedRCBD' package for R.

### Copyright (C) 2015-2020, ICAR-NBPGR.
#
# augmentedRCBD is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# augmentedRCBD is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

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
#') are obtained from the ANOVA tables according to the expected value of mean
#'square described by Federer and Searle (1976) as follows:
#'
#'\ifelse{html}{\out{<p style="text-align:
#'center;"><em>&sigma;<sup>2</sup><sub>p</sub></em> = Mean sum of squares of
#'test treatments</p>}}{\deqn{\sigma^{2}_{p} = \textrm{Mean sum of squares of
#'test treatments}}} \ifelse{html}{\out{<p style="text-align:
#'center;"><em>&sigma;<sup>2</sup><sub>e</sub></em> = Mean sum of squares of
#'residuals</p>}}{\deqn{\sigma^{2}_{e} = \textrm{Mean sum of squares of
#'residuals}}} \ifelse{html}{\out{<p style="text-align:
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
#'(\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}}) are categorised
#'according to Robinson (1966) as follows:
#'
#'\tabular{ll}{ \strong{\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}}}
#'\tab \strong{Category} \cr x \ifelse{html}{\out{<}}{\eqn{<}} 30 \tab Low \cr
#'30 \ifelse{html}{\out{&le;}}{\eqn{\le}} x \ifelse{html}{\out{<}}{\eqn{<}} 60
#'\tab Medium \cr \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 60 \tab High }
#'
#'Genetic advance (\ifelse{html}{\out{<i>GA</i>}}{\eqn{GA}}) is estimated
#'and categorised according to Johnson et al., (1955) as follows:
#'
#'\ifelse{html}{\out{<p style="text-align: center;"><em>GA = k &times;
#'&sigma;<sub>g</sub> &times; <big>[</big><sup>H<sup>2</sup></sup> &frasl;
#'</sub>100</sub><big>]</big></em></p>}}{\deqn{GA = k \times \sigma_{g} \times
#'\frac{H^{2}}{100}}}
#'
#'Where the constant \ifelse{html}{\out{<i>k</i>}}{\eqn{k}} is the standardized
#'selection differential or selection intensity. The value of
#'\ifelse{html}{\out{<i>k</i>}}{\eqn{k}} at 5\% proportion selected is 2.063.
#'Values of \ifelse{html}{\out{<i>k</i>}}{\eqn{k}} at other selected proportions
#'are available in Appendix Table A of Falconer and Mackay (1996).
#'
#'Selection intensity (\ifelse{html}{\out{<i>k</i>}}{\eqn{k}}) can also be
#'computed in R as below:
#'
#'If \code{p} is the proportion of selected individuals, then deviation of
#'truncation point from mean (\code{x}) and selection intensity (\code{k}) are
#'as follows:
#'
#'\code{ x = qnorm(1-p) }
#'
#'\code{ k = dnorm(qnorm(1 - p))/p }
#'
#'Using the same the Appendix Table A of Falconer and Mackay (1996) can be
#'recreated as follows.
#'
#'\preformatted{
#'TableA <- data.frame(p = c(seq(0.01, 0.10, 0.01), NA,
#'                            seq(0.10, 0.50, 0.02), NA,
#'                            seq(1, 5, 0.2), NA,
#'                            seq(5, 10, 0.5), NA,
#'                            seq(10, 50, 1)))
#'TableA$x <- qnorm(1-(TableA$p/100))
#'TableA$i <- dnorm(qnorm(1 - (TableA$p/100)))/(TableA$p/100)
#'}
#'
#'\strong{Appendix Table A} (Falconer and Mackay, 1996)
#'
#' \tabular{rrr}{
#' \strong{p\%} \tab \strong{x} \tab \strong{i}\cr
#'  0.01 \tab 3.71901649 \tab 3.9584797\cr
#'  0.02 \tab 3.54008380 \tab 3.7892117\cr
#'  0.03 \tab 3.43161440 \tab 3.6869547\cr
#'  0.04 \tab 3.35279478 \tab 3.6128288\cr
#'  0.05 \tab 3.29052673 \tab 3.5543807\cr
#'  0.06 \tab 3.23888012 \tab 3.5059803\cr
#'  0.07 \tab 3.19465105 \tab 3.4645890\cr
#'  0.08 \tab 3.15590676 \tab 3.4283756\cr
#'  0.09 \tab 3.12138915 \tab 3.3961490\cr
#'  0.10 \tab 3.09023231 \tab 3.3670901\cr
#'    <> \tab         <> \tab        <>\cr
#'  0.10 \tab 3.09023231 \tab 3.3670901\cr
#'  0.12 \tab 3.03567237 \tab 3.3162739\cr
#'  0.14 \tab 2.98888227 \tab 3.2727673\cr
#'  0.16 \tab 2.94784255 \tab 3.2346647\cr
#'  0.18 \tab 2.91123773 \tab 3.2007256\cr
#'  0.20 \tab 2.87816174 \tab 3.1700966\cr
#'  0.22 \tab 2.84796329 \tab 3.1421647\cr
#'  0.24 \tab 2.82015806 \tab 3.1164741\cr
#'  0.26 \tab 2.79437587 \tab 3.0926770\cr
#'  0.28 \tab 2.77032723 \tab 3.0705013\cr
#'  0.30 \tab 2.74778139 \tab 3.0497304\cr
#'  0.32 \tab 2.72655132 \tab 3.0301887\cr
#'  0.34 \tab 2.70648331 \tab 3.0117321\cr
#'  0.36 \tab 2.68744945 \tab 2.9942406\cr
#'  0.38 \tab 2.66934209 \tab 2.9776133\cr
#'  0.40 \tab 2.65206981 \tab 2.9617646\cr
#'  0.42 \tab 2.63555424 \tab 2.9466212\cr
#'  0.44 \tab 2.61972771 \tab 2.9321196\cr
#'  0.46 \tab 2.60453136 \tab 2.9182048\cr
#'  0.48 \tab 2.58991368 \tab 2.9048286\cr
#'  0.50 \tab 2.57582930 \tab 2.8919486\cr
#'    <> \tab         <> \tab        <>\cr
#'  1.00 \tab 2.32634787 \tab 2.6652142\cr
#'  1.20 \tab 2.25712924 \tab 2.6028159\cr
#'  1.40 \tab 2.19728638 \tab 2.5490627\cr
#'  1.60 \tab 2.14441062 \tab 2.5017227\cr
#'  1.80 \tab 2.09692743 \tab 2.4593391\cr
#'  2.00 \tab 2.05374891 \tab 2.4209068\cr
#'  2.20 \tab 2.01409081 \tab 2.3857019\cr
#'  2.40 \tab 1.97736843 \tab 2.3531856\cr
#'  2.60 \tab 1.94313375 \tab 2.3229451\cr
#'  2.80 \tab 1.91103565 \tab 2.2946575\cr
#'  3.00 \tab 1.88079361 \tab 2.2680650\cr
#'  3.20 \tab 1.85217986 \tab 2.2429584\cr
#'  3.40 \tab 1.82500682 \tab 2.2191656\cr
#'  3.60 \tab 1.79911811 \tab 2.1965431\cr
#'  3.80 \tab 1.77438191 \tab 2.1749703\cr
#'  4.00 \tab 1.75068607 \tab 2.1543444\cr
#'  4.20 \tab 1.72793432 \tab 2.1345772\cr
#'  4.40 \tab 1.70604340 \tab 2.1155928\cr
#'  4.60 \tab 1.68494077 \tab 2.0973249\cr
#'  4.80 \tab 1.66456286 \tab 2.0797152\cr
#'  5.00 \tab 1.64485363 \tab 2.0627128\cr
#'    <> \tab         <> \tab        <>\cr
#'  5.00 \tab 1.64485363 \tab 2.0627128\cr
#'  5.50 \tab 1.59819314 \tab 2.0225779\cr
#'  6.00 \tab 1.55477359 \tab 1.9853828\cr
#'  6.50 \tab 1.51410189 \tab 1.9506784\cr
#'  7.00 \tab 1.47579103 \tab 1.9181131\cr
#'  7.50 \tab 1.43953147 \tab 1.8874056\cr
#'  8.00 \tab 1.40507156 \tab 1.8583278\cr
#'  8.50 \tab 1.37220381 \tab 1.8306916\cr
#'  9.00 \tab 1.34075503 \tab 1.8043403\cr
#'  9.50 \tab 1.31057911 \tab 1.7791417\cr
#' 10.00 \tab 1.28155157 \tab 1.7549833\cr
#'    <> \tab         <> \tab        <>\cr
#' 10.00 \tab 1.28155157 \tab 1.7549833\cr
#' 11.00 \tab 1.22652812 \tab 1.7094142\cr
#' 12.00 \tab 1.17498679 \tab 1.6670040\cr
#' 13.00 \tab 1.12639113 \tab 1.6272701\cr
#' 14.00 \tab 1.08031934 \tab 1.5898336\cr
#' 15.00 \tab 1.03643339 \tab 1.5543918\cr
#' 16.00 \tab 0.99445788 \tab 1.5206984\cr
#' 17.00 \tab 0.95416525 \tab 1.4885502\cr
#' 18.00 \tab 0.91536509 \tab 1.4577779\cr
#' 19.00 \tab 0.87789630 \tab 1.4282383\cr
#' 20.00 \tab 0.84162123 \tab 1.3998096\cr
#' 21.00 \tab 0.80642125 \tab 1.3723871\cr
#' 22.00 \tab 0.77219321 \tab 1.3458799\cr
#' 23.00 \tab 0.73884685 \tab 1.3202091\cr
#' 24.00 \tab 0.70630256 \tab 1.2953050\cr
#' 25.00 \tab 0.67448975 \tab 1.2711063\cr
#' 26.00 \tab 0.64334541 \tab 1.2475585\cr
#' 27.00 \tab 0.61281299 \tab 1.2246130\cr
#' 28.00 \tab 0.58284151 \tab 1.2022262\cr
#' 29.00 \tab 0.55338472 \tab 1.1803588\cr
#' 30.00 \tab 0.52440051 \tab 1.1589754\cr
#' 31.00 \tab 0.49585035 \tab 1.1380436\cr
#' 32.00 \tab 0.46769880 \tab 1.1175342\cr
#' 33.00 \tab 0.43991317 \tab 1.0974204\cr
#' 34.00 \tab 0.41246313 \tab 1.0776774\cr
#' 35.00 \tab 0.38532047 \tab 1.0582829\cr
#' 36.00 \tab 0.35845879 \tab 1.0392158\cr
#' 37.00 \tab 0.33185335 \tab 1.0204568\cr
#' 38.00 \tab 0.30548079 \tab 1.0019882\cr
#' 39.00 \tab 0.27931903 \tab 0.9837932\cr
#' 40.00 \tab 0.25334710 \tab 0.9658563\cr
#' 41.00 \tab 0.22754498 \tab 0.9481631\cr
#' 42.00 \tab 0.20189348 \tab 0.9306998\cr
#' 43.00 \tab 0.17637416 \tab 0.9134539\cr
#' 44.00 \tab 0.15096922 \tab 0.8964132\cr
#' 45.00 \tab 0.12566135 \tab 0.8795664\cr
#' 46.00 \tab 0.10043372 \tab 0.8629028\cr
#' 47.00 \tab 0.07526986 \tab 0.8464123\cr
#' 48.00 \tab 0.05015358 \tab 0.8300851\cr
#' 49.00 \tab 0.02506891 \tab 0.8139121\cr
#' 50.00 \tab 0.00000000 \tab 0.7978846
#'}
#'
#' Where \strong{p\%} is the selected percentage of individuals from a
#' population, \strong{x} is the deviation of the point of truncation of
#' selected individuals from population mean and \strong{i} is the selection
#' intensity.
#'
#'Genetic advance as per cent of mean
#'(\ifelse{html}{\out{<i>GAM</i>}}{\eqn{GAM}}) are estimated and categorised
#'according to Johnson et al., (1955) as follows:
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
#'@inheritParams describe.augmentedRCBD
#'@param k The standardized selection differential or selection intensity.
#'  Default is 2.063 for 5\% selection proportion (see \strong{Details}).
#'
#'@return A list with the following descriptive statistics:  \item{Mean}{The
#'  mean value.} \item{PV}{Phenotyic variance.} \item{GV}{Genotyipc variance.}
#'  \item{EV}{Environmental variance.} \item{GCV}{Genotypic coefficient of
#'  variation} \item{GCV category}{The GCV category according to
#'  \insertCite{sivasubramaniam_genotypic_1973;textual}{augmentedRCBD}.}
#'  \item{PCV}{Phenotypic coefficient of variation} \item{PCV category}{The PCV
#'  category according to
#'  \insertCite{sivasubramaniam_genotypic_1973;textual}{augmentedRCBD}.}
#'  \item{ECV}{Environmental coefficient of variation} \item{hBS}{The
#'  broad-sense heritability
#'  (\ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}})
#'  \insertCite{lush_intra-sire_1940}{augmentedRCBD}.} \item{hBS category}{The
#'  \ifelse{html}{\out{<i>H<sup>2</sup></i>}}{\eqn{H^{2}}} category according to
#'  \insertCite{robinson_quantitative_1966;textual}{augmentedRCBD}.}
#'  \item{GA}{Genetic advance
#'  \insertCite{johnson_estimates_1955}{augmentedRCBD}.} \item{GAM}{Genetic
#'  advance as per cent of mean
#'  \insertCite{johnson_estimates_1955}{augmentedRCBD}.} \item{GAM category}{The
#'  GAM category according to
#'  \insertCite{johnson_estimates_1955;textual}{augmentedRCBD}.}
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
#'\insertRef{dudley_interpretation_1969}{augmentedRCBD}
#'
#'\insertRef{sivasubramaniam_genotypic_1973}{augmentedRCBD}
#'
#'\insertRef{federerModelConsiderationsVariance1976}{augmentedRCBD}
#'
#'\insertRef{falconer_introduction_1996}{augmentedRCBD}
#'
#'@note Genetic variability analysis needs to be performed only if the sum of
#'  squares of "Treatment: Test" are significant.
#'
#'  Negative estimates of variance components if computed are not abnormal. For
#'  information on how to deal with these, refer Dudley and Moll (1969).
#'
#'@importFrom methods is
#'@importFrom grDevices col2rgb
#'@importFrom Rdpack reprompt
#'@export
#'
#'@examples
#' # Example data
#' blk <- c(rep(1,7),rep(2,6),rep(3,7))
#' trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
#'         70, 75, 74)
#' y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
#'         240, 268, 287, 226, 395, 450)
#' data <- data.frame(blk, trt, y1, y2)
#' # Convert block and treatment to factors
#' data$blk <- as.factor(data$blk)
#' data$trt <- as.factor(data$trt)
#' # Results for variable y1
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' # Results for variable y2
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y2, method.comp = "lsd",
#'                      alpha = 0.05, group = TRUE, console = TRUE)
#'
#' # Genetic variability analysis
#' gva.augmentedRCBD(out1)
#' gva.augmentedRCBD(out2)
gva.augmentedRCBD <- function(aug, k = 2.063) {

  if (!is(aug, "augmentedRCBD")) {
    stop('"aug" is not of class "augmentedRCBD"')
  }

  if (is.data.frame(aug$`ANOVA, Block Adjusted`)){
    PV <- aug$`ANOVA, Block Adjusted`[aug$`ANOVA, Block Adjusted`$Source == "Treatment: Test", "Mean.Sq"]
    PV <- unname(PV)
    EV <- aug$`ANOVA, Block Adjusted`[aug$`ANOVA, Block Adjusted`$Source == "Residuals", "Mean.Sq"]
    EV <- unname(EV)

  } else {
    PV <- aug$`ANOVA, Block Adjusted`[[1]]$`Mean Sq`["Test"]
    PV <- unname(PV)
    EV <- aug$`ANOVA, Block Adjusted`[[1]]["Residuals", "Mean Sq"]
    EV <- unname(EV)
  }

  GV <- PV - EV
  Mean <- mean(aug$Means$`Adjusted Means`)
  if (GV > 0) {
    GCV <- (sqrt(GV) / Mean) * 100 # Burton 1951 1952
    GCV_category <- ifelse(GCV >= 20, "High", ifelse(GCV >= 10, "Medium", "Low"))
    PCV <- (sqrt(PV) / Mean) * 100 # Burton 1951 1952
    PCV_category <- ifelse(PCV >= 20, "High", ifelse(PCV >= 10, "Medium", "Low"))
    ECV <- (sqrt(EV) / Mean) * 100 # Burton 1951 1952
    hBS <- (GV / PV) * 100 # Lush 1940
    if (hBS < 0) {
      hBS <- ifelse(hBS < 0, NA, hBS) # for negative hbs
      warning('"hBS" computed was negative. Truncated to zero.')
    }
    hBS_categroy <- ifelse(hBS >= 60, "High", ifelse(hBS >= 30, "Medium", "Low")) # Robinson 1966
    GA <- k * sqrt(PV) * (hBS / 100) # Johnson et al. 1955
    GAM <- (GA / Mean) * 100
    GAM_category <-  ifelse(GAM >= 20, "High", ifelse(GAM >= 10, "Medium", "Low"))

  } else {
    GV <- NA
    GCV <- NA
    GCV_category <- NA
    PCV <- (sqrt(PV) / Mean) * 100 # Burton 1951 1952
    PCV_category <- ifelse(PCV >= 20, "High", ifelse(PCV >= 10, "Medium", "Low"))
    ECV <- (sqrt(EV) / Mean) * 100 # Burton 1951 1952
    hBS <- NA
    hBS_categroy <- NA
    GA <- NA
    GAM <- NA
    GAM_category <- NA

    warning(paste('Negative GV detected.\n',
                  'GCV, GCV category, hBS, hBS category, GA, GAM and\n',
                  'GAM category could not be computed'))

  }

  out <- list(Mean = Mean, PV = PV, GV = GV, EV = EV,
              GCV = GCV, `GCV category` = GCV_category,
              PCV = PCV, `PCV category` = PCV_category,
              ECV = ECV, hBS = hBS, `hBS category` = hBS_categroy,
              GA = GA, GAM = GAM, `GAM category` = GAM_category)
  return(out)

}



# http://www.ihh.kvl.dk/htm/kc/popgen/genetics/8/1.htm
# http://agtr.ilri.cgiar.org/Documents/compendia/Comp%20Selection%20Appendix.pdf
# https://jvanderw.une.edu.au/Day1cChangeofVariance.pdf
# https://wiki.groenkennisnet.nl/display/TAB/Chapter+9.5%3A+Selected+proportion+and+selection+intensity
selection.intensity <- function(p, pop.size = 1000) {

  selection.proportion <- p
  threshold <- -qnorm(selection.proportion)
  height.at.threshold <- exp(-0.5 * (threshold ^ 2)) / sqrt(2 * pi)
  # infinite pop size
  selection.intensity <- height.at.threshold / selection.proportion

  # corrected for finite pop size
  selection.intensity.corr <- selection.intensity - (pop.size - (pop.size * selection.proportion)) / (2 * selection.proportion * pop.size * ((pop.size + 1) * selection.intensity))

  return(list(`Selection intensity` = selection.intensity,
              `Corrected selection intensity` = selection.intensity.corr))
}
