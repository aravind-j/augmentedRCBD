# Analysis of Augmented Randomised Complete Block Design

`augmentedRCBD` is a function for analysis of variance of an augmented
randomised block design (Federer, 1956; Federer, 1961; Searle, 1965) and
the generation as well as comparison of the adjusted means of the
treatments/genotypes.

## Usage

``` r
augmentedRCBD(
  block,
  treatment,
  y,
  checks = NULL,
  method.comp = c("lsd", "tukey", "none"),
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

A list of class `augmentedRCBD` containing the following components:

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

## Details

This function borrows code from `DAU.test` function of `agricolae`
package (de Mendiburu et al., 2016) as well as from Appendix VIII of
Mathur et al., (2008).

## Note

- Data should preferably be balanced i.e. all the check genotypes should
  be present in all the blocks. If not, a warning is issued.

- There should not be any missing values.

- The number of test genotypes can vary within a block.

In case the large number of treatments or genotypes, it is advisable to
avoid comparisons with the `group = FALSE` argument as it will be memory
and processor intensive. Further it is advised to simplify output with
`simplify = TRUE` in order to reduce output object size.

## References

Federer WT (1956). “Augmented (or Hoonuiaku) designs.” *The Hawaiian
Planters' Record*, **LV(2)**, 191–208.

Federer WT (1956). “Augmented (or Hoonuiaku) Designs.” Technical Report
BU-74-M, Cornell University, New York.

Federer WT (1961). “Augmented designs with one-way elimination of
heterogeneity.” *Biometrics*, **17**(3), 447–473.

Searle SR (1965). “Computing Formulae for Analyzing Augmented Randomized
Complete Block Designs.” Technical Report BU-207-M, Cornell University,
New York.

Mathur PN, Muralidharan K, Parthasarathy VA, Batugal P, Bonnot F (2008).
*Data Analysis Manual for Coconut Researchers-Bioversity Technical
Bulletin No. 14*. Bioversity International. ISBN 978-92-9043-736-9.

de Mendiburu F (2015). *agricolae: Statistical Procedures for
Agricultural Research*. R package version 1.2-8.

@examples \# Example data blk \<- c(rep(1,7),rep(2,6),rep(3,7)) trt \<-
c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10) y1 \<-
c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
70, 75, 74) y2 \<- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237,
227, 281, 311, 250, 240, 268, 287, 226, 395, 450) data \<-
data.frame(blk, trt, y1, y2)

\# Convert block and treatment to factors data\$blk \<-
as.factor(data\$blk) data\$trt \<- as.factor(data\$trt)

\# Results for variable y1 (checks inferred) out1 \<-
augmentedRCBD(block = data\$blk, treatment = data\$trt, y = data\$y1,
method.comp = "lsd", alpha = 0.05, group = TRUE, console = TRUE) \#
Results for variable y2 (checks inferred) out2 \<- augmentedRCBD(block =
data\$blk, treatment = data\$trt, y = data\$y2, method.comp = "lsd",
alpha = 0.05, group = TRUE, console = TRUE)

\# Results for variable y1 (checks specified) out1 \<-
augmentedRCBD(block = data\$blk, treatment = data\$trt, y = data\$y1,
method.comp = "lsd", alpha = 0.05, group = TRUE, console = TRUE, checks
= c("1", "2", "3", "4")) \# Results for variable y2 (checks specified)
out2 \<- augmentedRCBD(block = data\$blk, treatment = data\$trt, y =
data\$y2, method.comp = "lsd", alpha = 0.05, group = TRUE, console =
TRUE, checks = c("1", "2", "3", "4"))

\# Warning in case test treatments are replicated out1 \<-
augmentedRCBD(block = data\$blk, treatment = data\$trt, y = data\$y1,
method.comp = "lsd", alpha = 0.05, group = TRUE, console = TRUE) out1
\<- augmentedRCBD(block = data\$blk, treatment = data\$trt, y =
data\$y1, method.comp = "lsd", alpha = 0.05, group = TRUE, console =
TRUE, checks = c("2", "3"))

## See also

[`DAU.test`](https://rdrr.io/pkg/agricolae/man/DAU.test.html),
[`ea1`](https://www.rdocumentation.org/packages/easyanova/versions/5.0/topics/ea1),
[`emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
`cld.emmGrid`,
[`aug.rcb`](https://rdrr.io/rforge/plantbreeding/man/aug.rcb.html)
