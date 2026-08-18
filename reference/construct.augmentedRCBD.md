# Construct an Augmented Randomised Complete Block Design Layout

Generates an augmented block design layout (Federer, 1956; Federer,
1961) in which test treatments are divided into blocks of a specified
size. A set of check treatments can be randomly positioned among the
test treatments, while another set of check treatments can be placed at
fixed positions at the end of each block.

## Usage

``` r
construct.augmentedRCBD(
  block.size,
  test.treatments,
  check.treatments,
  randomize.test = FALSE,
  random.checks = check.treatments,
  fixed.checks = NULL
)
```

## Arguments

- block.size:

  Integer specifying the number of test treatments assigned to each
  block. Check treatments are added in addition to this number.

- test.treatments:

  A vector containing the test treatment identifiers.

- check.treatments:

  A vector containing all available check treatment identifiers.

- randomize.test:

  Logical indicating whether the test treatments should be randomized
  before being divided into blocks. Defaults to `FALSE`.

- random.checks:

  A vector containing the check treatments to be randomly positioned
  within each block. Defaults to all `check.treatments`.

- fixed.checks:

  A vector containing check treatments to be placed at the end of each
  block. Defaults to `NULL`.

## Value

A data frame with three columns:

- Block:

  The block number.

- Plot:

  The plot position within the block.

- Treatment:

  The treatment assigned to the plot.

## Details

By default, test treatments retain their supplied order. When
`randomize.test = TRUE`, the test treatments are randomized before being
divided into blocks. The relative order of test treatments within each
block is otherwise preserved; only the positions of the `random.checks`
are randomized.

The number of blocks is determined by dividing the number of test
treatments by `block.size` and rounding up. The final block may
therefore contain fewer test treatments than the specified `block.size`.

Within each block, the test treatments occupy all positions not assigned
to `random.checks`. The order of the test treatments is preserved. The
positions of the random checks are selected randomly from the combined
set of test-treatment and random-check positions. The `fixed.checks`,
when supplied, are appended to the end of the block in their supplied
order.

The elements of `random.checks` and `fixed.checks` must be distinct, and
all must be present in `check.treatments`.

## References

Federer WT (1956). “Augmented (or Hoonuiaku) designs.” *The Hawaiian
Planters' Record*, **LV(2)**, 191–208.

Federer WT (1956). “Augmented (or Hoonuiaku) Designs.” Technical Report
BU-74-M, Cornell University, New York.

Federer WT (1961). “Augmented designs with one-way elimination of
heterogeneity.” *Biometrics*, **17**(3), 447–473.

## Examples

``` r
checks <- paste0("C", 1:6)
tests <- paste0("T", sprintf("%03d", 1:100))

design <-
  construct.augmentedRCBD(block.size = 20,
                          test.treatments = tests,
                          check.treatments = checks,
                          random.checks = checks[1:4],
                          fixed.checks = checks[5:6])

head(design)
#>   Block Plot Treatment
#> 1     1    1      T001
#> 2     1    2      T002
#> 3     1    3      T003
#> 4     1    4      T004
#> 5     1    5        C4
#> 6     1    6      T005
```
