# svmkR: Full-stack survey research with SurveyMonkey in R

[![R](https://img.shields.io/badge/R-4.0+-blue)](https://img.shields.io/badge/R-4.0+-blue)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Unit Tests](https://github.com/soubhikbarari/svmkR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/soubhikbarari/svmkR/actions/workflows/test-coverage.yaml/badge.svg)
[![R CMD Check](https://github.com/soubhikbarari/svmkR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/soubhikbarari/svmkR/actions/workflows/check-standard.yaml/badge.svg)


This package provides a suite of tools to work with SurveyMonkey surveys.

You can:

* Create and upload a survey from a **questionnaire** document.
* Browse and **download** surveys in your account.
* Conduct basic **analysis** (e.g. margin of error) on your surveys.
* Create statistical **weights** adjust your survey to a target population.
* Create presentable SurveyMonkey-style **banners** for polls.

## Installation

To install the production version of `svmkR` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("soubhikbarari/svmkR")
```

## Authors

* [Soubhik Barari](https://github.com/soubhikbarari) (creator/maintainer)

* [Zoe Padgett](https://github.com/znpadgett) (contributor)

* [Justine Orgel](https://github.com/jorgelsurveys) (contributor)

* [Christopher Remmel](https://github.com/calremmel) (contributor)

Authors of the `surveymonkey` package, of which `svmkR` was forked from and re-factored include:

* [Thomas Leeper](https://github.com/leeper) (creator/maintainer)

* [Sam Firke](https://github.com/sfirke) (creator/maintainer)

* [Matt Roumaya](https://github.com/mattroumaya) (creator/maintainer)
