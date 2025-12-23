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
data <- data.frame(blk, trt, y1)

# Convert block and treatment to factors
data$blk <- as.factor(data$blk)
data$trt <- as.factor(data$trt)

# Results for variable y1 (checks inferred)
out <- augmentedRCBD(block = data$blk, treatment = data$trt, y = data$y1,
                     method.comp = "lsd", alpha = 0.05, group = TRUE,
                     console = TRUE)
#> 
#> Augmented Design Details
#> ========================
#>                                        
#> Number of blocks           "3"         
#> Number of treatments       "12"        
#> Number of check treatments "4"         
#> Number of test treatments  "8"         
#> Check treatments           "1, 2, 3, 4"
#> 
#> 
#> ANOVA, Treatment Adjusted
#> =========================
#>                                      Df Sum Sq Mean Sq F value Pr(>F)  
#> Block (ignoring Treatments)           2  360.1  180.04   6.675 0.0298 *
#> Treatment (eliminating Blocks)       11  285.1   25.92   0.961 0.5499  
#>   Treatment: Check                    3   52.9   17.64   0.654 0.6092  
#>   Treatment: Test and Test vs. Check  8  232.2   29.02   1.076 0.4779  
#> Residuals                             6  161.8   26.97                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> ANOVA, Block Adjusted
#> =====================
#>                                Df Sum Sq Mean Sq F value Pr(>F)
#> Treatment (ignoring Blocks)    11  575.7   52.33   1.940  0.215
#>   Treatment: Check              3   52.9   17.64   0.654  0.609
#>   Treatment: Test               7  505.9   72.27   2.679  0.125
#>   Treatment: Test vs. Check     1   16.9   16.87   0.626  0.459
#> Block (eliminating Treatments)  2   69.5   34.75   1.288  0.342
#> Residuals                       6  161.8   26.97               
#> 
#> Coefficient of Variation
#> ========================
#> 6.372367
#> 
#> Overall Adjusted Mean
#> =====================
#> 81.0625
#> 
#> Standard Errors
#> ===============
#>                                          Std. Error of Diff.  CD (5%)
#> Control Treatment Means                             4.240458 10.37603
#> Two Test Treatments (Same Block)                    7.344688 17.97180
#> Two Test Treatments (Different Blocks)              8.211611 20.09309
#> A Test Treatment and a Control Treatment            6.704752 16.40594
#> 
#> Treatment Means
#> ===============
#>  Treatment Block Means   SE r   Min   Max Adjusted Means
#>          1       84.67 3.84 3 79.00 92.00          84.67
#>         10     3 74.00 <NA> 1 74.00 74.00          77.25
#>         11     1 89.00 <NA> 1 89.00 89.00          86.50
#>         12     1 82.00 <NA> 1 82.00 82.00          79.50
#>          2       79.00 1.15 3 77.00 81.00          79.00
#>          3       82.00 2.65 3 78.00 87.00          82.00
#>          4       83.33 3.93 3 78.00 91.00          83.33
#>          5     2 79.00 <NA> 1 79.00 79.00          78.25
#>          6     3 75.00 <NA> 1 75.00 75.00          78.25
#>          7     1 96.00 <NA> 1 96.00 96.00          93.50
#>          8     3 70.00 <NA> 1 70.00 70.00          73.25
#>          9     2 78.00 <NA> 1 78.00 78.00          77.25
#> 
#> 
#> Comparisons
#> ===========
#> 
#> Method : lsd
#> 
#>                   contrast estimate   SE df t.ratio p.value sig
#>    treatment1 - treatment2     5.67 4.24  6   1.336   0.230    
#>    treatment1 - treatment3     2.67 4.24  6   0.629   0.553    
#>    treatment1 - treatment4     1.33 4.24  6   0.314   0.764    
#>    treatment1 - treatment5     6.42 6.36  6   1.009   0.352    
#>    treatment1 - treatment6     6.42 6.36  6   1.009   0.352    
#>    treatment1 - treatment7    -8.83 6.36  6  -1.389   0.214    
#>    treatment1 - treatment8    11.42 6.36  6   1.795   0.123    
#>    treatment1 - treatment9     7.42 6.36  6   1.166   0.288    
#>   treatment1 - treatment10     7.42 6.36  6   1.166   0.288    
#>   treatment1 - treatment11    -1.83 6.36  6  -0.288   0.783    
#>   treatment1 - treatment12     5.17 6.36  6   0.812   0.448    
#>    treatment2 - treatment3    -3.00 4.24  6  -0.707   0.506    
#>    treatment2 - treatment4    -4.33 4.24  6  -1.022   0.346    
#>    treatment2 - treatment5     0.75 6.36  6   0.118   0.910    
#>    treatment2 - treatment6     0.75 6.36  6   0.118   0.910    
#>    treatment2 - treatment7   -14.50 6.36  6  -2.280   0.063    
#>    treatment2 - treatment8     5.75 6.36  6   0.904   0.401    
#>    treatment2 - treatment9     1.75 6.36  6   0.275   0.792    
#>   treatment2 - treatment10     1.75 6.36  6   0.275   0.792    
#>   treatment2 - treatment11    -7.50 6.36  6  -1.179   0.283    
#>   treatment2 - treatment12    -0.50 6.36  6  -0.079   0.940    
#>    treatment3 - treatment4    -1.33 4.24  6  -0.314   0.764    
#>    treatment3 - treatment5     3.75 6.36  6   0.590   0.577    
#>    treatment3 - treatment6     3.75 6.36  6   0.590   0.577    
#>    treatment3 - treatment7   -11.50 6.36  6  -1.808   0.121    
#>    treatment3 - treatment8     8.75 6.36  6   1.376   0.218    
#>    treatment3 - treatment9     4.75 6.36  6   0.747   0.483    
#>   treatment3 - treatment10     4.75 6.36  6   0.747   0.483    
#>   treatment3 - treatment11    -4.50 6.36  6  -0.707   0.506    
#>   treatment3 - treatment12     2.50 6.36  6   0.393   0.708    
#>    treatment4 - treatment5     5.08 6.36  6   0.799   0.455    
#>    treatment4 - treatment6     5.08 6.36  6   0.799   0.455    
#>    treatment4 - treatment7   -10.17 6.36  6  -1.598   0.161    
#>    treatment4 - treatment8    10.08 6.36  6   1.585   0.164    
#>    treatment4 - treatment9     6.08 6.36  6   0.956   0.376    
#>   treatment4 - treatment10     6.08 6.36  6   0.956   0.376    
#>   treatment4 - treatment11    -3.17 6.36  6  -0.498   0.636    
#>   treatment4 - treatment12     3.83 6.36  6   0.603   0.569    
#>    treatment5 - treatment6     0.00 8.21  6   0.000   1.000    
#>    treatment5 - treatment7   -15.25 8.21  6  -1.857   0.113    
#>    treatment5 - treatment8     5.00 8.21  6   0.609   0.565    
#>    treatment5 - treatment9     1.00 7.34  6   0.136   0.896    
#>   treatment5 - treatment10     1.00 8.21  6   0.122   0.907    
#>   treatment5 - treatment11    -8.25 8.21  6  -1.005   0.354    
#>   treatment5 - treatment12    -1.25 8.21  6  -0.152   0.884    
#>    treatment6 - treatment7   -15.25 8.21  6  -1.857   0.113    
#>    treatment6 - treatment8     5.00 7.34  6   0.681   0.521    
#>    treatment6 - treatment9     1.00 8.21  6   0.122   0.907    
#>   treatment6 - treatment10     1.00 7.34  6   0.136   0.896    
#>   treatment6 - treatment11    -8.25 8.21  6  -1.005   0.354    
#>   treatment6 - treatment12    -1.25 8.21  6  -0.152   0.884    
#>    treatment7 - treatment8    20.25 8.21  6   2.466   0.049   *
#>    treatment7 - treatment9    16.25 8.21  6   1.979   0.095    
#>   treatment7 - treatment10    16.25 8.21  6   1.979   0.095    
#>   treatment7 - treatment11     7.00 7.34  6   0.953   0.377    
#>   treatment7 - treatment12    14.00 7.34  6   1.906   0.105    
#>    treatment8 - treatment9    -4.00 8.21  6  -0.487   0.643    
#>   treatment8 - treatment10    -4.00 7.34  6  -0.545   0.606    
#>   treatment8 - treatment11   -13.25 8.21  6  -1.614   0.158    
#>   treatment8 - treatment12    -6.25 8.21  6  -0.761   0.475    
#>   treatment9 - treatment10     0.00 8.21  6   0.000   1.000    
#>   treatment9 - treatment11    -9.25 8.21  6  -1.126   0.303    
#>   treatment9 - treatment12    -2.25 8.21  6  -0.274   0.793    
#>  treatment10 - treatment11    -9.25 8.21  6  -1.126   0.303    
#>  treatment10 - treatment12    -2.25 8.21  6  -0.274   0.793    
#>  treatment11 - treatment12     7.00 7.34  6   0.953   0.377    
#> 
#> Treatment Groups
#> ================
#> 
#> Method : lsd
#> 
#>  Treatment Adjusted Means   SE df lower.CL upper.CL Group
#>          8          73.25 5.61  6    59.52    86.98    1 
#>          9          77.25 5.61  6    63.52    90.98    12
#>         10          77.25 5.61  6    63.52    90.98    12
#>          5          78.25 5.61  6    64.52    91.98    12
#>          6          78.25 5.61  6    64.52    91.98    12
#>          2          79.00 3.00  6    71.66    86.34    12
#>         12          79.50 5.61  6    65.77    93.23    12
#>          3          82.00 3.00  6    74.66    89.34    12
#>          4          83.33 3.00  6    76.00    90.67    12
#>          1          84.67 3.00  6    77.33    92.00    12
#>         11          86.50 5.61  6    72.77   100.23    12
#>          7          93.50 5.61  6    79.77   107.23     2

# Generate reports
# \donttest{
report.augmentedRCBD(aug = out,
                     target = file.path(tempdir(),
                                        "augmentedRCBD output.docx"),
                     file.type = "word",
                     check.col = c("brown", "darkcyan",
                                   "forestgreen", "purple"))
#> Warning: P-value for "Treatment: Test" is > 0.05. Genetic variability analysis may not be appropriate for this trait.
#> File created at /var/folders/bp/kmfmhnl95kx1c8x321z7twbw0000gn/T//RtmpfdS0kK/augmentedRCBD output.docx
report.augmentedRCBD(aug = out,
                     target = file.path(tempdir(),
                                        "augmentedRCBD output.xlsx"),
                     file.type = "excel",
                     check.col = c("brown", "darkcyan",
                                   "forestgreen", "purple"))
#> File created at /var/folders/bp/kmfmhnl95kx1c8x321z7twbw0000gn/T//RtmpfdS0kK/augmentedRCBD output.xlsx
# }
```
