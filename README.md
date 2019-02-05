# jsmodule

[![Build Status](https://travis-ci.org/jinseob2kim/jsmodule.svg?branch=master)](https://travis-ci.org/jinseob2kim/jsmodule)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jinseob2kim/jsmodule?branch=master&svg=true)](https://ci.appveyor.com/project/jinseob2kim/jsmodule)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/jsmodule)](https://cran.r-project.org/package=jsmodule)
[![codecov](https://codecov.io/github/jinseob2kim/jsmodule/branch/master/graphs/badge.svg)](https://codecov.io/github/jinseob2kim/jsmodule)
[![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/issues)
[![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/stargazers)
[![GitHub license](https://img.shields.io/github/license/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/blob/master/LICENSE)


RStudio Addins and Shiny Modules for Medical Research

## Install

```r
remotes::install_github('jinseob2kim/jsmodule')
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

![](https://community.rstudio.com/uploads/default/original/2X/8/886b35e85e9ee2e9d03d02788f6b09f4e93b29cc.png)

### Survey data analysis

```r
library(survey)
data(api)
jsRepeatedGadget(apistrat)
```

![](https://community.rstudio.com/uploads/default/original/2X/6/66f7190f409df1b0c5d4c41bbf0b63237c4cfd9e.png)
