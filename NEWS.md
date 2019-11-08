# jsmodule 1.0.3

* Bug fixes: Incorrect p-value when applying cluster options.


# jsmodule 1.0.2

* Update: Stop running ShinyApps by closing the browser window.


# jsmodule 1.0.1

## Update

* **Change reference** of categorical variables

* Add **line type** option to **kaplan-meier** plot 

## Bug fixes

* **Reactive error** in propensity score matching.


# jsmodule 1.0.0

## Update

* Change UI: ROC/time-dependent ROC analysis.

* Choose cut-off value in **kaplan-meier analysis with continuous independent variable**.

* Add some icons.

* Choose **multiple conditions when subsetting data**.

* Add **caliper** option when propensity score matching. 

* **Make binary variables** from contiuous variables.

* Non-normal variables can be summarized with [min,max]. 

* Add **line color**, **x-axis label** option to **kaplan-meier** plot

## Bug fixes

* Encoding error when `csv` file upload.

# jsmodule 0.9.9

## New addins

* Basic statistics with external file: `jsBasicExtAddin`

* Propensity score analysis with external file: `jsPropensityExtAddin`

* Repeated measure analysis with external file: `jsRepeatedExtAddin`

* Survey data analysis with external file: `jsSurveyExtAddin`

## Bug fixes

* Regression/Logistic model in `jsSurveyAddin`.

* **Id** issue in repeated measure analysis.

# jsmodule 0.9.3

## Update: Propensity score analysis

* Add scatterplot, ROC/time-dependent ROC analysis.

## Bug fixes: repeated measure analysis 

* Error if repeated measure variable is **id**.

* Error if there are missing values.

## Bug fixes: survey data analysis

* Error in **ROC** analysis

## Bug fixes: Table 1

* `data.frame` vs `data.table` issue.


# jsmodule 0.9.2

* Bug fixes: Apply nested survey design to `jsSurveyGadget` and `FileSurveyInput`

# jsmodule 0.9.1

## Bug fixes

* Encoding issues when reading `sav/sas7bdat/dta` formats.

* Default independent variables when running **regression/logistic/cox** analysis

## Update

* Add `limit.unires` option to **regression/logistic/cox** analysis to prevent computation burden.


# jsmodule 0.9.0

## New module

* Shiny module forROC analysis : `rocUI` `rocModule`.

* Add this module to `jsBasicGadget` and `jsRepeatedGadjet`.

## Update

* Apply time-dependent ROC to survey data analysis(`jsSurveyGadget`)


# jsmodule 0.8.6

## Update
* Change **candidate time variables** in survival analysis  : allow only > 0 

* Add option to **change legend, p-value position in kaplan-meier plot**

## Bug fixes

* Time-dependent ROC analysis can allow 1 model.

# jsmodule 0.8.5

* **Keep original variable's name** in tables/figures


# jsmodule 0.8.4

* Change default **step of x-axis range**.

# jsmodule 0.8.3

* Bug fix: time-dependent ROC analysis.

# jsmodule 0.8.2.1

* Bug fix: **emf** format can recognize figure's line.

# jsmodule 0.8.2

* Add **emf** format to the plot download modules. You can customize the plot in **MS powerpoint**.

# jsmodule 0.8.1

* **Change default time** when time-dependent ROC analysis. 

# jsmodule 0.8.0

## New module

* Shiny module for time-dependent ROC analysis : `timerocUI` `timerocModule`.

* Add this module to `jsBasicGadget` and `jsRepeatedGadjet`.


# jsmodule 0.7.10

* Add Kaplan-meier plot options: **timeby, x/y-axis ranges**.

# jsmodule 0.7.9

## Update

* Add **progress bar** to download handlers of scatterplot and kaplan-meier plot. 

* Kaplan-meier plot in `Basic statistics` can use **continuous variables as group variables**. The module suggest best 5 cuts based on the p-values of logrank test. 

* Add **number at risk** table option to weighted kalan-meier plot.

## Bug fixes

* Add namespace **survival::cluster** for marginal cox model.

# jsmodule 0.7.8

## Update

* Add sub-group analysis based on the range of **continuous variable**.

* Add **multiple factor choice** option when doing sub-group analysis based on **categorical variable**. 


# jsmodule 0.7.7

* Fix **NOTE** and examples for cran upload

# jsmodule 0.7.6

* You can get **Subset data** based on the range of continuous variables.  

# jsmodule 0.7.5

## Update

* Add **Subset data** feature to all Addins and input modules. You can select subset data before analysis.


## Bug fixes

* Fix `warn` variable error in `regressModule`, `logisticModule`.

* Fix list of except variables in `ggpairsModule`.


# jsmodule 0.6.9

## Bug fixes

* Fix some spell for cran release.

## Update

* Update **travis-ci**.

* Add **appveyor** CI to test **window** environment. 


# jsmodule 0.6.8

## Update

* Warning if there are stongly correlated independent variables in `regressModule`, `jsBasicGadget`.

# jsmodule 0.6.7

## Update

* Show **unweighted & weighted table 1** at same tab in `jsSurveyGadget`

## Bug fixes

* Initial cluster variable state in `jsSurveyGadget`

# jsmodule 0.6.6

## Update

* Add **unweighted table 1** to `jsSurveyGadget`

## Bug fixes

* Apply list tye independent variables to `regressModule`, `logisticModule`, `GEEModuleLinear` and `GEEModuleLogistic`

* Change **sub-group analysis's reactive methods**: `regressModule`, `logisticModule`, `GEEModuleLinear` and `GEEModuleLogistic` 

# jsmodule 0.6.5

## Update

* Add **marginal cox model** option to `coxModule`, `kaplanModule` and `jsRepeatedGadget`

* Add **Overall kaplan-meier plot** to `kaplanModule`

* Set default **cluster, strata and weights** to **None** in `jsSurveyGadget`, `FileSurveyInput`

## Bug fixes

* Some error comments in `coxModule` and `kaplanModule`

* Change default candidate dependent variables to **categorical variables with 0, 1** in `GEEModuleLogistic`, `jsRepeatedGadget`

# jsmodule 0.6.4

## New module

* Module for **Survival analysis**: `coxUI`, `coxModule`, `kaplanUI` and `kaplanModule`

* Module for **ggplot download UI**: `ggplotdownUI`

## Update 

* Add survival analysis to `jsBasicGadget`, `jsSurveyGadget`

## Bug fixes

* `jsPropensityGadget`



# jsmodule 0.6.2

* Bug fixes : Label information in `regressModule`, `logisticModule`

* Update: **default.unires** option to `regressModule`, `logisticModule`

# jsmodule 0.6.1

* Update : Apply **complex survey design** to `FileSurvey`, `regressModule`, `logisticModule` and `jsSurveyGadget`


# jsmodule 0.6.0

## New Gadget

* Shiny Gadget for survey data analysis: `jsSurveyGadget`

## New module

* Module for **survey data analysis**: `FileSurvey` 

## Update

* Survey analysis option to `regressModule`, `logisticModule` and `tb1`

# jsmodule 0.5.7

* Bug fixes: **sub-group option** in `regressModule`, `logisticModule`, `jsPropensityGadget`, `GEEModuleLinear` and `GEEModuleLogistic`

# jsmodule 0.5.6

* Add **testthat**.

# jsmodule 0.5.5

## New module

* Modules for repeated measure analysis: `FileRepeated`, `GEEModuleUI`, `GEEModuleLinear` and `GEEModuleLogistic`

## New Gadget

* Shiny Gadget for repeated measure analysis: `jsRepeatedGadget`

# jsmodule 0.5.3

## Update

* New option: `nfactor.limit` in `regressModule`, `logisticModule`, `jsBasicGadget`, `csvFileInput` and `FilePsInput`

## Bug fixes

* `regressModule`, `logisticModule`, `jsPropensityGadget` can deal missing data issue.

* `csvFileInput`, `FilePSInput` change variable class **integer64** to **double**.

# jsmodule 0.5.2

* Update: Upload **Stata** `.dta` format.

# jsmodule 0.5.1

* Modify `tb1module`, `tb1module2`, `tb1simple` ,`tb1simple2`: compatible with updated `jstable`

# jsmodule 0.5.0

## Update

* Add sub-group analysis to `regressModule`, `logisticModule` and `jsPropensityGadget`.

* Apply the updated `cox2.display` function in **jstable** package to `jsPropensityGadget`.

## Typo

* Strata variable in `ggpairsModule`, `ggpairsModule2`.

# jsmodule 0.4.9

## Bug fixes 

* IPTW table in `tb1simple` and `tb1simple2` modules.

* Kaplan-meier UI in `jsPropensityGadget`

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
