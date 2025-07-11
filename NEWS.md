# jsmodule 1.6.12
## Update
- refactor ROC time dependent table function (remove for statement)
- add new column in ROC time dependent table (CI & p-value difference of AUC & Brier)
## Bugfix
- "survive"" library dependent issue in ROC time dependent feat
- remove error message when add new model in ROC time dependent feat
- fix remove model button that remain removed label issue in ROC time dependent feat


# jsmodule 1.6.11
- Update: simplified file input functions.
- Update: Modified the Fine-Gray module to use the `id` argument.

# jsmodule 1.6.10
- Fix: default cut-off value in `rocModule` and `rocModule2` when there is only one independent variable.
- Fix: Kaplan-Meier plots now work when "Time by" is below 1.

# jsmodule 1.6.9
## Update
- Added Brier and AUC calculation to timeroc functionality; fixed bugs.

# jsmodule 1.6.8
## Update
- New addin: templateGenerator, make your shiny in ease.

# jsmodule 1.6.7
## Update
- Independant variable can be selected continuously when there is vec.event and vec.time

# jsmodule 1.6.6
## Update
- Modified the jsmodule theme to enhance visual consistency and readability.

# jsmodule 1.6.5
## Update
- Added a slider to adjust pcut.univariate, allowing multivariate analysis to be performed only with significant variables.
- Select first variable when vec.event and vec.time is NULL in coxph.R.


# jsmodule 1.6.4
## Update
- Added an option to adjust the line thickness of the Kaplan-Meier plot.


# jsmodule 1.6.3
## Update
- Added an option to the Kaplan-Meier, Cox, and ForestCox modules that allows the outcome (event) and time variables to be linked when the user specifies vec.event and vec.time. When this option is enabled, selecting an event variable automatically updates the corresponding time variable, and vice versa.


# jsmodule 1.6.2
## Update
- Add options to customize x axis ticks in forest plot

# jsmodule 1.6.1
## Update
- Add function to allow adjusting cutoff for a single independent variable and observing model's metrics in `rocModule`, `rocModule2`.
- Add option to turn pairwise p value option on in case level of stratified group >= 3, in `tb1moduleUI`.
- Add subgroup analysis for propensity score matching/repeated/survey.

# jsmodule 1.6.0 
## Update
- Add competing risk analysis in subgroup, Kaplan-meier, regression tab.

# jsmodule 1.5.10
## Update:
- Change Boxplot fill type


# jsmodule 1.5.9
## Bugfix:
- `sav` file load in `FilePSInput.R`, `FileRepeatedInput.R`, `FileSurveyInput.R`

# jsmodule 1.5.8

## Update
- Add label option on lineplot

# jsmodule 1.5.7

## Bugfix:
- Fix 2 group factor not working problem in `forestcoxServer`

# jsmodule 1.5.6

## Bugfix:
- Fix var_label displaying incorrectly when loading .sav file


# jsmodule 1.5.5

## Update
- Download table as word document (Thanks for [Jinhwan Kim](https://github.com/jhk0530))


# jsmodule 1.5.4

## Update
- Apply SPSS label info (Thanks for [Jinhwan Kim](https://github.com/jhk0530))

# jsmodule 1.5.3

## Bugfix: 
- dropdown button & p value options: barplot, boxplot and lineplot

# jsmodule 1.5.2

## Update: 
- Add P value option to barplot, boxplot and lineplot
- Add dropdown button for graphical option to barplot, boxplot and lineplot

# jsmodule 1.5.1

## Update: 
- Add Subgroupanalysis to jsBasicGadget
- Add figure in subgroup analysis with forestplot
- Add family 'poisson' in forestglm
- Add "line size" "point size" to lineplot

# jsmodule 1.5.0

## New module: 
- Subgroup analysis with forestplot (Thanks for [Yoonkyoung Jeon](https://github.com/cyk0315))

* forestcox (`forestcoxUI`, `forestcoxServer`)
* forestglm (`forestglmUI`, `forestglmServer`)

# jsmodule 1.4.4

## Update: 
- Add "reverse y-axis" option to lineplot

# jsmodule 1.4.3

## Update 
- `rocModule`: Add accucary/ppv/npv with threshold

# jsmodule 1.4.2

## Update: 
- Change default non-normal 2 group comparison test to `wilcox.test`

# jsmodule 1.4.1

## Error fix: `jsBasicExtAddin`, `jsBasicGatget`


# jsmodule 1.4.0

## Fix :

* update deprecated icon (`Plot` panel)

## Update : 

> feature applies in `jsBasicExtAddin` (shiny app)

* Add description with callout
  * `Data` , `Table 1` panel

* modified some UI (with `style.css`)
  * header color changed
  * add zarathu & github page in header
  * table 1's significant data changed
  * `Data`'s table column has color (when vairable is category)

* update DT function (`Data`)
  * `Column visiblity` button added
  * `Column Reorder` feature added
  * may browse table with keyboard (`arrow` keys)

## Refactor: 

* `csvfileInput.R` Not use file's extension not name. (`csv.txt` will nor work)
* `ggpairs.R` apply theme with less `if-else`

# jsmodule 1.3.6

## Fix: 

* `kaplanModule` PPT download


# jsmodule 1.3.5

## Fix: 

* `rocModule`

* Change `jsRepeatedGadjet` to `jsRepeatedGadget`


# jsmodule 1.3.4

## Fix and Update: `rocModule`

* cutoff fix: `rocModule`

* Add **"Show 1-specificity"** option

# jsmodule 1.3.3

## New module (Thanks for [Changwoo Lim](https://github.com/leevenstar))

* Histogram(`histogramUI`, `histogramServer`)

# jsmodule 1.3.2

## Update 

* Add cutoff information to `timerocModule`, `rocModule`   when 1 model, 1 indpendent variable.

## Fix

* `timerocModule`: Invert plot when auc < 0.5

# jsmodule 1.3.1

## Fix

* Fix cutoff label of Kaplan-meier plot when continuous independent variable

# jsmodule 1.3.0

## New module (Thanks for [Hyunki Lee](https://github.com/leevenstar))

* Boxplot(`boxUI`, `boxServer`), Lineplot(`LineUI`, `LineServer`), Barplot(`BarUI`, `BarServer`)

# jsmodule 1.2.0

## Update

* Add `coxph` ties option to `coxModule`: **ties.coxph**

## Bugfix

* `kaplanModule` runs even if **timeby = 1**

# jsmodule 1.1.9

## Update

* Faster `timerocModule`: Change AUC/AUC difference 95%CI calculation method to `survival::concordance`

# jsmodule 1.1.8

## Update

* Change **emf** to **pptx**(editable) in figure download.

# jsmodule 1.1.7

## Bug fix

* Label info in subgroup analysis: regression, cox

# jsmodule 1.1.6

## Fix & update

* New module `scatterplotUI` & `scatterplotServer`: scatterplot using **[ggpubr](https://rpkgs.datanovia.com/ggpubr/)**: apply to **Basic statistics**

* `timerocModule`, `rocModule`: Compare ROC curve even if there are different sample size(na omit) 

* Change **timeby unit** from 1 to 0.5 (when <365) 

* Add **surv.scale** option to kaplan-meier plot

* Fix PS matching error with missing values  

# jsmodule 1.1.5

## Fix 

* Fix according to **[MatchIt](https://github.com/kosukeimai/MatchIt) 4.0** update.

* Erratum: Cumulative hazard -> Cumulative incidence 

# jsmodule 1.1.4

## Update

* **Show survival probabilities(%)** at kaplan-meier plot. This requires **[jskm](https://github.com/jinseob2kim/jskm) 0.4.2** or higher.

# jsmodule 1.1.3

## Update

* Choose time ranges: `coxUI`, `coxModule`.


# jsmodule 1.1.2

## Update

* Add **No** initial covariate option when ps calculation


# jsmodule 1.1.1

## Update

* Add landmark analysis to kaplan-meier analysis. Pease update **jskm to 0.4.0** version.

* Add option **hide 95%CI of AUC, NRIIDI** to `timerocModule`. It reduces computation time.


# jsmodule 1.1.0

* Apply **AIC based stepwise selection** to linear/logistic regression, cox model.

* Deprecated fucntion: `regressModule` & `logisticModule`, please use `regressModule2` & `logisticModule2`

# jsmodule 1.0.8

* Add option to `tb1module1` ,`tb1module2`: get simulated p value in Fisher exact test when larger than 2 by 2 tables.

# jsmodule 1.0.7

## Dependency

* Add dependency: **Cairo** package

# jsmodule 1.0.6

## Bug fix

* PS matching: **Duplicated independent variables UI** in ROC/timeROC analysis.  

## Update

* Change **minimum timeby** in kaplan-meier plot: 1 to 0


# jsmodule 1.0.5

## Update

* PS matching: Change font size & color of the message with missing value information. 


# jsmodule 1.0.4

## Bug fixes

* **Cox model** of Basic statistics module.

# jsmodule 1.0.3

## Bug fixes

* Incorrect p-value when applying cluster options.

* PS matching: allow continuous variables as Independent variables

## Update

* PS matching: allow 1:N matching.


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
