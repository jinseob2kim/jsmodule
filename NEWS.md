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
