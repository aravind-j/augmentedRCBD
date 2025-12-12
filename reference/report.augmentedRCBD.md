# Generate MS Word or Excel Report from `augmentedRCBD` Output

`report.augmentedRCBD` generates a tidy report from an object of class
`augmentedRCBD` as docx MS word file using the
[`officer`](https://davidgohel.github.io/officer/reference/officer.html)
package or xlsx MS excel file using the
[`openxlsx`](https://rdrr.io/pkg/openxlsx/man/openxlsx.html) package.

## Usage

``` r
report.augmentedRCBD(
  aug,
  target,
  file.type = c("word", "excel"),
  k = 2.063,
  check.col = "red"
)
```

## Arguments

- aug:

  An object of class `augmentedRCBD`.

- target:

  The path to the report file to be created.

- file.type:

  The file type of the report. Either `"word"` for MS Word report file
  or `"excel"` for MS Excel report file.

- k:

  The standardized selection differential or selection intensity
  required for computation of Genetic advance. Default is 2.063 for 5%
  selection proportion (see **Details** in
  [`gva.augmentedRCBD`](https://aravind-j.github.io/augmentedRCBD/reference/gva.augmentedRCBD.md)).
  Ignored if `gva = FALSE`.

- check.col:

  The colour(s) to be used to highlight check values in the plot as a
  character vector. Must be valid colour values in R (named colours,
  hexadecimal representation, index of colours \[`1:8`\] in default R
  [`palette()`](https://rdrr.io/r/grDevices/palette.html) etc.).

## Note

The raw values in the `augmentedRCBD` object are rounded off to 2 digits
in the word and excel reports. However, in case of excel report, the raw
values are present in the cell and are formatted to display only 2
digits.

So, if values such as adjusted means are being used of downstream
analysis, export the raw values from within R or use the excel report.

This default rounding can be changed by setting the global options
`augmentedRCBD.round.digits`. For example
`setOption(augmentedRCBD.round.digits = 3)` sets the number of decimal
places for rounding to 3.

Values will not be rounded to zero, instead will be rounded to the
nearest decimal place. F value, t ratio and p values are not rounded to
less than 3 decimal places.

## See also

[`officer`](https://davidgohel.github.io/officer/reference/officer.html),
[`flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)

## Examples

``` r
# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
data <- data.frame(blk, trt, y1, y2)
# Convert block and treatment to factors
data$blk <- as.factor(data$blk)
data$trt <- as.factor(data$trt)
# Results for variable y1 (checks inferred)
out <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = FALSE)

# \donttest{
report.augmentedRCBD(aug = out,
                     target = file.path(tempdir(),
                                        "augmentedRCBD output.docx"),
                     file.type = "word",
                     check.col = c("brown", "darkcyan",
                                   "forestgreen", "purple"))
#> Warning: P-value for "Treatment: Test" is > 0.05. Genetic variability analysis may not be appropriate for this trait.
#> File created at /var/folders/bp/kmfmhnl95kx1c8x321z7twbw0000gn/T//RtmpYgg6iT/augmentedRCBD output.docx
report.augmentedRCBD(aug = out,
                     target = file.path(tempdir(),
                                        "augmentedRCBD output.xlsx"),
                     file.type = "excel",
                     check.col = c("brown", "darkcyan",
                                   "forestgreen", "purple"))
#> File created at /var/folders/bp/kmfmhnl95kx1c8x321z7twbw0000gn/T//RtmpYgg6iT/augmentedRCBD output.xlsx
# }
```
