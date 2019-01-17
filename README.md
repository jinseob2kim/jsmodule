# jsmodule

[![Build Status](https://travis-ci.org/jinseob2kim/jsmodule.svg?branch=master)](https://travis-ci.org/jinseob2kim/jsmodule)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jinseob2kim/jsmodule?branch=master&svg=true)](https://ci.appveyor.com/project/jinseob2kim/jsmodule)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jsmodule)](http://cran.r-project.org/package=jsmodule)
[![codecov](https://codecov.io/github/jinseob2kim/jsmodule/branch/master/graphs/badge.svg)](https://codecov.io/github/jinseob2kim/jsmodule)
[![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/issues)
[![GitHub forks](https://img.shields.io/github/forks/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/network)
[![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/stargazers)
[![GitHub license](https://img.shields.io/github/license/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/blob/master/LICENSE)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](https://github.com/jinseob2kim/jsmodule)
[![GitHub contributors](https://img.shields.io/github/contributors/jinseob2kim/jsmodule.svg?maxAge=2592000)](https://github.com/jinseob2kim/jsmodule/graphs/contributors)

Rstudio Addins and Shiny Modules for Medical Research

## Install

```r
devtools::install_github(c('jinseob2kim/jstable', 'jinseob2kim/jsmodule'))
```

## Gadget

### Basic statistics 

```r
jsBasicGadget(mtcars)
```
![](https://blog.anpanman.co.kr/posts/2018-11-24-basic-biostatistics/addin.gif)

### Propensity score analysis

```r
jsPropensityGadget(mtcars)
```

![](http://app.anpanman.co.kr/img/ps.png)


### Repeated measure analysis

```r
jsRepeatedGadget(mtcars)
```


### Survey data analysis

```r
library(survey)
data(api)
jsRepeatedGadget(apistrat)
```

