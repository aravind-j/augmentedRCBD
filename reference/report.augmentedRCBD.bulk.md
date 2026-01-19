# Generate MS Word or Excel Report from `augmentedRCBD.bulk` Output

`report.augmentedRCBD.bulk` generates a tidy report from an object of
class `augmentedRCBD.bulk` as docx MS word file using the
[`officer`](https://davidgohel.github.io/officer/reference/officer.html)
package or xlsx MS excel file using the
[`openxlsx`](https://rdrr.io/pkg/openxlsx/man/openxlsx.html) package.

## Usage

``` r
report.augmentedRCBD.bulk(aug.bulk, target, file.type = c("word", "excel"))
```

## Arguments

- aug.bulk:

  An object of class `augmentedRCBD.bulk`.

- target:

  The path to the docx file to be created.

- file.type:

  The file type of the report. Either `"word"` for MS Word report file
  or `"excel"` for MS Excel report file.

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
nearest decimal place.

## See also

[`officer`](https://davidgohel.github.io/officer/reference/officer.html),
[`flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)

[`augmentedRCBD.bulk`](https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.bulk.md)

## Examples

``` r
# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)

y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
dataf <- data.frame(blk, trt, y1, y2)

bout <- augmentedRCBD.bulk(data = dataf, block = "blk",
                           treatment = "trt", traits = c("y1", "y2"),
                           checks = NULL, alpha = 0.05, describe = TRUE,
                           freqdist = TRUE, gva = TRUE,
                           check.col = c("brown", "darkcyan",
                                         "forestgreen", "purple"),
                           console = FALSE)
#> 
#> ANOVA for y1 computed (1/2)
#> 
#> ANOVA for y2 computed (2/2)

# \donttest{
report.augmentedRCBD.bulk(
  aug.bulk = bout,
  target = file.path(tempdir(),
                     "augmentedRCBD bulk output.docx"),
  file.type = "word")
#> File created at /var/folders/kg/7q73ww8s3llgyl61c9z_j5g40000gn/T//RtmpCaS79L/augmentedRCBD bulk output.docx
report.augmentedRCBD.bulk(
  aug.bulk = bout,
  target = file.path(tempdir(),
                     "augmentedRCBD bulk output.xlsx"),
  file.type = "excel")
#> Error in insertImage(wb = wb, sheet = sheet, file = fileName, width = width,     height = height, startRow = startRow, startCol = startCol,     units = units, dpi = dpi): File does not exist.
# }
```
