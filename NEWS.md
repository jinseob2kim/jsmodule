# jsmodude 0.4.9

* Bug fixes : IPTW table in `tb1simple` and `tb1simple2` modules.

# jsmodude 0.4.8

* `tb1module` and `tb1module2`  can control the maximum factor levels to include.

# jsmodule 0.4.6

* Bug fixes : Modules for regression exclude `NA` when selecting binary variables.

# jsmodule 0.4.5

* Update : `csvFile` module can remove empty columns.

# jsmodule 0.4.4

* Change regression table function to `glmshow.display` in **jstable** package.


# jsmodule 0.4.3

## New gadget

* `jsPropensityGadget` : Propensity score analysis

## Update

* Excluded non-normal variable selection feature from `tb1simple` module.

* Apply original variable names to label information : `csvFileInput` & `FilePsInput` functions. 

# jsmodule 0.4.0

## New module

* `tb1simpleUI`, `tb1simple`, `tb1simple2` for propensity score analysis

# jsmodule 0.3.7

## Bug fixes

* factor variable criteria in `csvInput`, `tb1module` and `tb1module2`

# jsmodule 0.3.6

## New module

* `FilePsInput`, `FilePs` are modules of data input for propensity score calculation.

## Minor update

* Change default `data.table` & `fread` **check.names** option to `TRUE` 


# jsmodule 0.3.5

## New gadget

* `jsBasicGadget` : Basic statistics

## Minor update

* `csvFile` changes variable name including `/` to `_` and etc.

## Bug fixes

* Dependency issue with **jstable** package

# jsmodule 0.3.1

## Minor update

* Support `sav/sas7bdat` formats

## Bug fixes:

* `tbmoduleUI`, `tbmoduleUI2` : some error


# jsmodule 0.3.0

## Bug fixes: 

* `tbmoduleUI`, `tbmoduleUI2` : Apply `shapiro.test`

## New module

* `regressModuleUI`, `regressModule`, `regressModule2`

* `logisticModuleUI`, `logisticModule`, `logisticModule2`

* `ggpairsModuleUI1`, `ggpairsModuleUI2`, `ggpairsModule`

## New function 

* `regress.display2`, `logistic.display2` : Regression table for reactive data.



# jsmodule 0.2.5

## Update

* `csvFile` : Add *n_* to column names that begin with a number.

## New module

* `tb1moduleUI2` can deal **reactive data**

# jsmodule 0.2.0

* New module: `tb1moduleUI`, `tb1module`

# jsmodule 0.1.0

* Update: `csvFile` can deal label information

# jsmodule 0.0.1

* File upload: `csvFileInput`, `csvFile`
