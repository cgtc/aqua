
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aqua - <ins>a</ins>ssay <ins>qua</ins>lification and validation tools

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dmarginean/aqua?branch=master&svg=true)](https://ci.appveyor.com/project/dmarginean/aqua)
<!-- badges: end -->

The (future, long term, eventual) goal of aqua is to provide tools that
help with different assay qualification and validation project analysis
and reporting.

## Installation

You can install aqua. I hope. You’re on your own.

``` r
devtools::install_github("cgtc/aqua")    # fingers crossed
```

## Examples

``` r
library(aqua)
#> Loading required package: magrittr

sd(c(2,3,2))   # boring
#> [1] 0.5773503
sd.u(c(2,3,2)) # amazing
#> [1] 0.65147
```

`sd.u` produces a very unbiased standard deviation estimate. Makes every
small sample size data look horrible. The sad but true reality. Uses a
chi-distributed correction factor.

``` r
library(lme4)
#> Loading required package: Matrix

funky_model <- lmer(total.fruits ~ 1 + (1|amd) + (1|status), data = Arabidopsis)

get_intermed_levels(funky_model) %>%
  knitr::kable(escape = F) %>%
  kable_style()
```

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;text-align: center;vertical-align: middle;">

Error term

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;text-align: center;vertical-align: middle;">

CV %

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

status

</td>

<td style="text-align:right;">

18.836883

</td>

</tr>

<tr>

<td style="text-align:left;">

amd

</td>

<td style="text-align:right;">

5.834408

</td>

</tr>

</tbody>

</table>

Boom. Clearly the contribution of `amd` and `status` to variations in
`fruit.growth` (in a vacuum) in this model I just invented that makes no
sense is what it says there.

You can get repeatability and overall intermediate precision /
reproducibility of an assay if you turned it into an lme4 random model
with the only fixed term being an intercept. Just describe your model
like `1 + (1|Lab/Operator/Day) + (1|SampleType)` or whatever your assay
design structure is and fire it up.

You can also do the whole TCID50 assay calculations. There’s a lot of
functions for that that tend to even work\! Does anyone use TCID50
anymore? Hello?

## Version

Currently 0.1 MEGA ALPHA
