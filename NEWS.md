# ClinicoPath 0.0.3.66

## Enhanced Calibration Assessment with Flexible Spline Methods - Complete Implementation

### Advanced Spline-Based Calibration Analysis
- **Flexible spline calibration curves** using Restricted Cubic Splines (RCS) for non-linear calibration assessment
- **Multi-method calibration framework** combining traditional Hosmer-Lemeshow with advanced spline-based methods
- **RCS calibration implementation** providing flexible non-linear calibration curve fitting with proper confidence intervals
- **Lowess-based calibration** as robust alternative method for smooth calibration assessment
- **rms package integration** framework for enhanced calibration methodologies and validation
- **Comprehensive calibration metrics** including spline-based slopes, intercepts, and uncertainty quantification

### Enhanced Calibration Visualization
- **Dual-curve calibration plots** showing both traditional Loess and advanced spline calibration methods
- **GAM-based spline smoothing** with confidence bands for visual calibration assessment
- **Multi-layer visualization** with distinct styling for different calibration approaches
- **Enhanced plot aesthetics** including color-coded curves, line type differentiation, and informative subtitles
- **Automatic spline integration** in calibration plots when spline data is available
- **Publication-ready graphics** with professional styling and comprehensive legends

### Comprehensive Calibration Table Integration
- **Extended calibration analysis table** including spline calibration results alongside traditional metrics
- **Separate spline calibration rows** with dedicated interpretation and statistical measures
- **Informative table annotations** explaining spline methodology and applicability
- **Statistical completeness** with confidence intervals, slopes, and intercepts for all methods
- **Method-specific interpretations** providing clinical context for different calibration approaches
- **Error handling integration** with graceful fallbacks for missing or incomplete spline data

### Technical Infrastructure Enhancements
- **Package import additions** including mgcv for GAM functionality and rms for spline methods
- **Robust spline data generation** with comprehensive error handling and validation
- **Method integration framework** allowing seamless addition of new calibration methods
- **Performance optimization** with efficient spline calculation and minimal computational overhead
- **Backward compatibility** maintaining all existing calibration functionality while adding new features
- **Documentation completeness** with method explanations and usage guidance

# ClinicoPath 0.0.3.64

## Visualization and Dashboard Enhancements - Complete Implementation

### Stage Migration Flow Visualization
- **Sankey diagram visualization** showing patient flow between original and new staging systems
- **Multiple visualization backends** with networkD3 for interactive diagrams, ggalluvial for alluvial plots, and basic ggplot2 fallback
- **Automatic flow quantification** displaying patient counts for each migration pattern (unchanged, upstaged, downstaged)
- **Visual flow analysis** helping identify dominant migration patterns and assess reclassification magnitude
- **Robust error handling** with graceful fallbacks when specialized packages are unavailable

### Comparative Analysis Dashboard
- **Comprehensive summary table** aggregating results from all advanced migration analyses
- **Evidence synthesis framework** combining discrimination metrics, model fit criteria, and clinical assessments
- **Automated recommendation engine** generating overall evidence-based recommendations for staging system adoption
- **Multi-dimensional assessment** covering discrimination (C-index), model fit (AIC), validation (monotonicity), bias (Will Rogers), and assumptions (proportional hazards)
- **Clinical relevance classification** with clear interpretation of statistical significance and practical importance
- **Research-ready summary** providing publication-quality evidence synthesis for staging validation studies

### Advanced Dashboard Features
- **Evidence scoring system** quantifying positive indicators across multiple analysis categories
- **Proportional recommendation logic** generating strong/moderate/limited evidence classifications
- **Migration rate assessment** with clinical relevance thresholds (high >30%, moderate >10%)
- **Statistical significance integration** combining multiple statistical tests and criteria
- **Clinical decision support** providing actionable recommendations for staging system adoption
- **Error-resilient design** with comprehensive fallback mechanisms for incomplete analyses

### Visualization Integration
- **Seamless workflow integration** with advanced migration analysis framework
- **Automatic activation** when `advancedMigrationAnalysis` option is enabled
- **Performance optimized** with efficient data processing and minimal computational overhead
- **Cross-platform compatibility** supporting multiple visualization packages and graceful degradation

## Time-dependent AUC Enhancement with Integrated Measures - Complete Implementation

### Enhanced Time-dependent ROC Analysis
- **Integrated AUC calculation** using trapezoidal rule across multiple time points for comprehensive discrimination assessment
- **Enhanced time-dependent ROC methodology** with improved statistical methods and robust confidence intervals
- **Brier score integration** for combined calibration/discrimination assessment providing unified model performance metrics
- **AUC comparison testing** with DeLong statistical tests for differences between staging systems
- **Temporal AUC trends analysis** showing discrimination changes over time with linear regression modeling
- **Bootstrap confidence intervals** for integrated AUC differences with 500-iteration validation
- **Multiple discrimination metrics** including mean time-dependent AUC and integrated measures
- **Clinical interpretation framework** with magnitude classification (Substantial/Moderate/Small/Minimal) and clinical relevance thresholds

### Advanced Statistical Methods
- **Time-dependent AUC calculation** using timeROC package with pROC fallback for robust analysis
- **Trapezoidal integration** for computing area under time-dependent AUC curves
- **DeLong test implementation** for statistical comparison of AUC values between staging systems
- **Temporal trend detection** using linear regression to identify discrimination changes over follow-up
- **Combined discrimination/calibration** assessment through Brier score analysis
- **Bootstrap validation** with automated confidence interval calculation for integrated measures

### Integration with Advanced Migration Analysis
- **Seamless integration** with existing advanced migration analysis framework
- **Uses NRI time points** for consistency across all time-dependent analyses
- **Non-breaking implementation** as part of `advancedMigrationAnalysis` option
- **Comprehensive error handling** with graceful degradation and informative error messages
- **Performance optimization** with bootstrap iteration limits for computational efficiency

### Clinical Research Applications
- **Publication-ready metrics** with proper statistical rigor and confidence intervals
- **Evidence-based thresholds** using clinically meaningful AUC improvement criteria (≥0.02 for clinical relevance)
- **Temporal discrimination assessment** for understanding staging performance across different follow-up periods
- **Combined performance evaluation** through Brier scores integrating calibration and discrimination
- **Comparative staging validation** with statistical tests for superiority assessment

# ClinicoPath 0.0.3.50

## Advanced Migration Analysis Framework - Complete Implementation

### Advanced Migration Analysis Suite
- **Complete implementation** of all high-priority advanced migration analyses from staging literature
- **Non-breaking integration** with new `advancedMigrationAnalysis` option
- **Comprehensive validation tools** for staging system comparisons beyond basic C-index metrics
- **Clinical research-grade outputs** with detailed interpretations and recommendations

### Monotonicity Assessment Engine
- **Automated detection** of staging violations where higher stages have better survival than lower stages
- **Quantitative monotonicity scoring** (0-1 scale) measuring consistency across all stage comparisons
- **Detailed violation reporting** with specific stage pairs and survival differences
- **Clinical interpretation** guidance for both original and new staging systems

### Will Rogers Phenomenon Detection
- **Comprehensive migration pattern analysis** detecting artificial survival improvements
- **Classic Will Rogers identification** where patient reclassification improves both stage survivals without individual outcome changes
- **Evidence strength classification** (Strong/Possible/None) with statistical validation
- **Migration rate assessment** with bias risk evaluation and clinical impact guidance
- **Per-pattern analysis** for each migration type (e.g., Stage II → Stage III)

### Stage-Specific Validation Framework
- **Subgroup C-index analysis** ensuring new staging maintains prognostic value within each original stage category
- **Confidence interval estimation** for discrimination metrics in each subgroup
- **Insufficient sample size detection** with appropriate handling for sparse data
- **Clinical interpretation** of discrimination quality (Good/Moderate/Poor/None) with significance testing

### Enhanced Pseudo R-squared Suite
- **Multiple R² variants**: Nagelkerke, Cox & Snell, McFadden, and Royston & Sauerbrei measures
- **Comprehensive model comparison** with absolute and relative improvement quantification
- **Variance explanation analysis** showing percentage of survival variation captured by each system
- **Improvement magnitude classification** (Substantial/Moderate/Small/Negligible) with clinical relevance thresholds

### Enhanced Reclassification Metrics Suite
- **Category-free NRI**: Rank-based Net Reclassification Improvement without predefined risk categories
- **Clinical NRI**: Risk threshold-based NRI using clinically meaningful cut-points (tertiles for high-risk identification)
- **Relative IDI**: Integrated Discrimination Improvement expressed as percentage of baseline discrimination
- **Continuous NRI**: Linear predictor-based continuous reclassification assessment
- **Event/Non-event Discrimination Improvement**: Separate discrimination metrics for events and non-events
- **Kaplan-Meier based NRI**: Survival curve-derived reclassification using stage-specific survival probabilities
- **Bootstrap confidence intervals**: Robust statistical inference with optional bootstrap validation for all metrics
- **Comprehensive clinical interpretation**: Magnitude classification (Substantial/Moderate/Small/Minimal) with direction assessment

### Proportional Hazards Assumption Testing
- **Schoenfeld residuals testing**: Automated validation of Cox model assumptions using survival::cox.zph()
- **Global test statistics**: Chi-square test results with degrees of freedom and p-values for both staging systems
- **Assumption status classification**: Clear indication of whether proportional hazards assumption is met or violated
- **Clinical interpretation guidance**: Specific recommendations for handling violations (stratified models, time-varying coefficients)
- **Violation severity assessment**: Graded interpretation (Strong/Moderate/Weak violation) with appropriate recommendations
- **Automated integration**: Runs automatically as part of advanced migration analysis without user configuration

### Enhanced Decision Curve Analysis
- **Net benefit calculation**: Implements clinical decision curve analysis across multiple threshold probabilities (5%-50%)
- **Time-specific analysis**: Uses same time points as NRI analysis for consistent temporal assessment
- **Clinical utility comparison**: Compares net benefit between staging systems at clinically relevant decision thresholds
- **Impact classification**: Graded assessment (Substantial/Moderate/Small Benefit/Harm) with clinical interpretations
- **Survival probability integration**: Uses Cox model baseline hazard for accurate time-specific mortality risk calculation
- **Evidence-based thresholds**: Covers clinically meaningful probability ranges for staging-based treatment decisions

### Enhanced Calibration Assessment
- **Perfect calibration detection**: Enhanced focus on calibration slope = 1.0 as ideal target (per document recommendations)
- **Robust statistical methods**: Quantile-based risk grouping and profile likelihood confidence intervals for calibration slopes
- **Additional calibration metrics**: Calibration-in-the-large, Expected/Observed ratios, and Brier scores for comprehensive assessment
- **Enhanced Hosmer-Lemeshow testing**: Improved handling of sparse data with continuity correction and minimum group size requirements
- **Baseline hazard integration**: More accurate survival probability calculation using Cox model baseline hazard functions
- **Comprehensive interpretation**: Multi-component interpretation covering H-L test, calibration slope quality, and systematic bias detection

### Technical Architecture Improvements
- **Modular analysis framework** with `.performAdvancedMigrationAnalysis()` main dispatcher
- **Individual analysis functions** for each advanced method with independent error handling
- **Helper function library** for pseudo R² calculations and survival metrics
- **Robust statistical calculations** with comprehensive fallback mechanisms for edge cases

### User Experience Enhancements
- **Detailed explanatory documentation** for each advanced analysis with clinical context
- **Progress tracking** with comprehensive error reporting and debugging information
- **Clinical interpretation** guidance for all metrics with actionable recommendations
- **Research-ready outputs** suitable for publication with proper statistical rigor

### Integration and Compatibility
- **Seamless integration** with existing stage migration workflow without breaking changes
- **Backward compatibility** with all existing functionality and options
- **Configurable activation** via single `advancedMigrationAnalysis` boolean option
- **Error isolation** ensuring advanced analyses failures don't affect core functionality

# ClinicoPath 0.0.3.47

## Advanced TNM Stage Migration Analysis - Major Updates

### Stage Migration Module Enhancements
- **Complete effect sizes implementation**: Added comprehensive effect size calculations for staging system comparisons
- **Enhanced statistical summary**: Comprehensive statistical summary table with all key metrics including C-index improvements, AIC/BIC differences, and overall recommendations  
- **Robust error handling**: Fixed multiple Turkish locale errors ("fonksiyon olmayana uygulama denemesi" and "TRUE/FALSE gereken yerde eksik değer")
- **Clinical interpretation improvements**: Enhanced practical significance assessments for effect sizes and statistical measures

### Effect Sizes Analysis
- **Cohen's d calculation**: Standardized effect size for C-index differences between staging systems
- **R² equivalents**: Variance explained calculations for both original and new staging systems  
- **Improvement metrics**: Quantified discrimination improvements with clinical significance thresholds
- **Multiple effect size perspectives**: Comprehensive view including raw differences, standardized measures, and practical significance

### Statistical Summary Enhancements
- **Comprehensive metrics display**: C-index values with confidence intervals for both systems
- **Model comparison statistics**: AIC/BIC differences with evidence strength assessments
- **Relative improvement calculations**: Percentage improvements with magnitude classifications
- **Overall recommendations**: Evidence-based recommendations using multiple criteria (3/4 criteria met framework)
- **Advanced metrics integration**: NRI and IDI results when available

### Error Resolution and Stability
- **Turkish locale compatibility**: Fixed "fonksiyon olmayana uygulama denemesi" errors in effect sizes calculation
- **Logical condition validation**: Fixed "TRUE/FALSE gereken yerde eksik değer" errors in statistical summary
- **Robust data extraction**: Enhanced data validation and fallback mechanisms for missing or malformed results
- **Graceful error handling**: Comprehensive error handling with informative messages and fallback calculations

### Clinical Research Features
- **Practical significance thresholds**: Evidence-based thresholds for clinical relevance (0.02 C-index improvement)
- **Multiple evidence synthesis**: Integration of discrimination, model fit, and clinical significance criteria
- **Research-ready outputs**: Publication-quality tables with confidence intervals and interpretation guidelines
- **Effect size interpretations**: Standard magnitude classifications (negligible, small, medium, large) with clinical context

### Technical Improvements
- **Simplified calculations**: Streamlined effect size calculations to avoid complex data structure navigation
- **Fallback strategies**: Multiple data source attempts with known values as safety nets
- **Enhanced validation**: Comprehensive input validation and data structure checking
- **Performance optimization**: Reduced computational complexity while maintaining accuracy

# ClinicoPath 0.0.3.46

## Major Infrastructure Improvements

### Enhanced Module Update System
- **Completely rewritten _updateModules.R** with enterprise-grade features
- **Smart asset copying**: Configuration-based vs legacy mode selection via `use_legacy_copying` option
- **Enhanced error handling**: Graceful degradation with comprehensive validation and recovery
- **Better logging**: Visual progress indicators with emojis and structured reporting
- **Module validation**: Pre-flight checks for all module directories and dependencies
- **Improved Git integration**: Enhanced commit handling with dry-run support and better error messages

### New Configuration Options
- **File copying control**: Granular control via `copy_vignettes`, `copy_data_files`, `copy_test_files`, `copy_r_files`
- **Smart path resolution**: Automatic fallback for missing dependencies and utility files
- **Enhanced backup system**: Automatic cleanup of old backups with configurable retention
- **Performance optimization**: Incremental updates and parallel processing support

### Function Enhancements

#### kappasizeci Function - Complete Overhaul
- **Enhanced implementation**: Added comprehensive parameter validation and error handling
- **Performance optimization**: Implemented caching system for repeated calculations
- **Comprehensive test suite**: 40+ test cases covering functionality, validation, edge cases, and real-world scenarios
- **Complete documentation**: 1,297-line comprehensive vignette with clinical examples and best practices
- **Test data generation**: 776-line script creating realistic datasets for 7 research domains (medical, radiological, psychological, etc.)

#### outlierdetection Function - Complete Enhancement
- **Advanced detection methods**: Comprehensive outlier detection using easystats performance package
- **Multiple method categories**: Univariate (Z-scores, IQR, confidence intervals), multivariate (Mahalanobis, MCD, OPTICS, LOF), composite, and comprehensive analysis
- **Robust input validation**: Extensive data quality checks with user-friendly feedback and actionable error messages
- **Comprehensive test suite**: 9 test datasets covering 3,400 observations across clinical, psychological, temporal, and international scenarios
- **Professional documentation**: Complete roxygen2 documentation with clinical examples, method guidelines, threshold recommendations, and scientific references
- **Enhanced error handling**: Context-aware error messages with troubleshooting guidance and specific solutions
- **Clinical focus**: Optimized for medical research data quality control and preprocessing applications
- **Performance validation**: Tested with large datasets and high-dimensional data for robust performance

#### eurostatmap Function - Turkey Data Integration
- **Moved test files**: Relocated Turkey eurostatmap tests to proper testing infrastructure
- **Enhanced test structure**: Converted to proper testthat framework with comprehensive validation
- **Improved coverage**: Added data structure validation and statistical bounds checking

### Testing Infrastructure
- **Comprehensive test coverage**: Enhanced test suites for multiple functions
- **Proper test organization**: Moved standalone test files to appropriate testing directories
- **Better test structure**: Converted legacy tests to modern testthat framework
- **Real-world scenarios**: Added tests for clinical, medical, and research applications

### Developer Experience
- **Enhanced error messages**: Clear, actionable error messages with context
- **Better progress reporting**: Real-time feedback with completion summaries
- **Improved documentation**: Updated configuration files with comprehensive comments
- **Module status awareness**: Only processes enabled modules, skips disabled ones

### Code Quality
- **Consistent error handling**: Unified error handling patterns throughout
- **Enhanced validation**: Comprehensive input validation with helpful error messages
- **Better logging**: Structured logging with visual indicators and progress tracking
- **Robust operations**: Graceful handling of missing files and directories


# ClinicoPath 0.0.2.0072


calculated cut-off groups in continuous survival can be added to data


# ClinicoPath 0.0.2.0069

optional padjustement to pairwise survival fixes: https://github.com/sbalci/ClinicoPathJamoviModule/issues/38

saving calculated variables to Data


# ClinicoPath 0.0.2.0064

- added ggvenn function
fixed https://github.com/yanlinlin82/ggvenn/issues/16

- deleted some functions, will add as updated

# ClinicoPath 0.0.2.0048

- started adding arguments of ggstatsplot
fixed: https://github.com/sbalci/jjstatsplot/issues/3


# ClinicoPath 0.0.2.0046

-  use `x` and `y` instead of `main` and `condition` arguments for `ggpiestats` and `ggbarstats`  
partially fixed: https://github.com/sbalci/jjstatsplot/issues/1

-  add point.path argument in grouped_ggwithinstats
partially fixed: https://github.com/sbalci/jjstatsplot/issues/2

- waiting for update in jamovi mran version. current ggstatsplot dependencies are not up to date in jamovi's library



# ClinicoPath 0.0.2.0044

- fixed multivariate survival to work without continuous explanatory 


# ClinicoPath 0.0.2.0043

- added jsurvival to linux
- added some functions of descriptives to linux


# ClinicoPath 0.0.2.0041

- updated jsurvival
- added more controls under collapse boxes
- advanced outcome: users can select more than one outcome level depending on their analysis (event free or overall survival). Competing risk survival will also be added in the future.
- advanced survival: users can use dates to calculate survival time. the date type should be defined. many variations for date types are given.
- Cumulative events, cumulative survival and KMunicate style Kaplan-Meier curves are added.
- A separate function for continuous explanatory variable is added. The optimal cut-off based on survival outcome is defined and after cut-off definition other univariate survival analysis are performed.
- under multivariate analysis users can now generate Kaplan-Meier curves upto two explanatory variables.
- an adjusted survival curve is also added, though it requires further management.


# ClinicoPath 0.0.2.0040

- separating univariate continuous survival from categorical


# ClinicoPath 0.0.2.0039

- added options to vartree

fixes: https://github.com/sbalci/ClinicoPathJamoviModule/issues/28


# ClinicoPath 0.0.2.0038

- fixes https://github.com/sbalci/ClinicoPathJamoviModule/issues/20

# ClinicoPath 0.0.2.0037

- added Survival Analysis for Continuous Explanatory

- added vtree package functions to vartree function

- fixes https://github.com/sbalci/ClinicoPathJamoviModule/issues/9


# ClinicoPath 0.0.2.0036

- added Benford Analysis

- added interactive size to jjbarstats


```
.init = function() {
            deplen <- length(self$options$dep)
            self$results$plot$setSize(400, deplen*300)
        }
```


# ClinicoPath 0.0.2.0035

- meddecide has been added to jamovi library

https://github.com/sbalci/meddecide/

https://github.com/sbalci/meddecide/releases/

https://library.jamovi.org/win64/R3.6.3/meddecide-0.0.1.0005.jmo

https://library.jamovi.org/linux/R3.6.3/meddecide-0.0.1.0005.jmo

https://library.jamovi.org/macos/R3.6.3/meddecide-0.0.1.0005.jmo

# ClinicoPath 0.0.2.0034

- added metrics to survival functions
- updating survival options (continuous explanatory, cut-off, two categorical explanatory, multiple outcome options, elapsed time calculation from dates)
- added KMunicate type survival curve
- added Fagan's nomogram

# ClinicoPath 0.0.2.0027

- temporarily added many functions from various packages to update functions and arguments


# ClinicoPath 0.0.2.0026

- jsurvival has been added to jamovi library

https://github.com/sbalci/jsurvival

https://github.com/sbalci/jsurvival/releases/

https://library.jamovi.org/macos/R3.6.3/jsurvival-0.0.2.0026.jmo

https://library.jamovi.org/win64/R3.6.3/jsurvival-0.0.2.0026.jmo


# ClinicoPath 0.0.2.0025

- ClinicoPath as a combined module has been taken down from jamovi library.
- Users are adviced to install submodules.

- ClinicoPathDescriptives, jsurvival, and meddecide tables have been updated to look more jamovian :)





# ClinicoPath 0.0.2.0024

- ClinicoPathDescriptives functions are separately added to jamovi library under Exploration menu

ClinicoPathDescriptives module can be downloaded inside jamovi (click Modules and jamovi library)

https://library.jamovi.org/win64/R3.6.3/ClinicoPathDescriptives-0.0.2.0019.jmo

https://library.jamovi.org/linux/R3.6.3/ClinicoPathDescriptives-0.0.2.0019.jmo

https://library.jamovi.org/macos/R3.6.3/ClinicoPathDescriptives-0.0.2.0019.jmo



# ClinicoPath 0.0.2.0023

- added Age Pyramid

- survival status can be selected from levels

- WIP decisioncalculator



# ClinicoPath 0.0.2.0022

- GGStatsPlot functions are separately added to jamovi library under jjstatsplot menu

JJStastPlot module can be downloaded inside jamovi (click Modules and jamovi library)

https://library.jamovi.org/macos/R3.6.3/jjstatsplot-0.0.1.0001.jmo

https://library.jamovi.org/win64/R3.6.3/jjstatsplot-0.0.1.0001.jmo

https://library.jamovi.org/linux/R3.6.3/jjstatsplot-0.0.1.0001.jmo



# ClinicoPath 0.0.2.0021

- updating jjstatsplot for release



# ClinicoPath 0.0.2.0020

- made submodules:


- ClinicoPathDescriptives


https://github.com/sbalci/ClinicoPathDescriptives/

https://github.com/sbalci/ClinicoPathDescriptives/releases/


- JJStatsPlot: 


https://github.com/sbalci/jjstatsplot

https://github.com/sbalci/jjstatsplot/releases/


- jsurvival:

https://github.com/sbalci/jsurvival

https://github.com/sbalci/jsurvival/releases/


- meddecide

https://github.com/sbalci/meddecide/


https://github.com/sbalci/meddecide/releases/



# ClinicoPath 0.0.2.0019

- rewrote summary of categorical values to decrease dependencies




# ClinicoPath 0.0.2.0018

-   added cumulative events and cumulative hazard plots to survival

<https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf>

-   added cox adjusted survival to multivariate survival


# ClinicoPath 0.0.2.0017

-   added alluvial diagrams using easyalluvial package to Descriptives (under
    Explore menu)\
    <https://github.com/sbalci/ClinicoPathJamoviModule/issues/19>\
    <https://github.com/erblast/easyalluvial/issues/19>

-   added easyalluvial as an option to Graphs and Plots (under JJStatsPlot menu)
    for repeated categorical measurements.

# ClinicoPath 0.0.2.0016

-   See: <https://github.com/sbalci/jjstatsplot/releases/>

-   See: <https://github.com/sbalci/ClinicoPath/releases/>

-   crosstable function partially resolves
    <https://github.com/jamovi/jamovi/issues/443>

-   survival function resolves
    <https://github.com/jonathon-love/deathwatch/issues/2>

-   added export html to crosstables to bypass
    <https://github.com/jamovi/jamovi/issues/892>

# ClinicoPath 0.0.2.0015

-   Added tangram statistical results

-   Added options to finalfit crosstables fixes:
    <https://github.com/sbalci/ClinicoPathJamoviModule/issues/24> Partially
    fixes: <https://github.com/jamovi/jamovi/issues/901> fixes:
    <https://github.com/ewenharrison/finalfit/issues/52>

-   Added experimental biblometrics functions

-   includes experimental changes

# ClinicoPath 0.0.2.0014

-   Added footnote to arsenal crosstable

# ClinicoPath 0.0.2.0012

-   jjstatsplot is the wrapper functions for using ggstatsplot in jamovi.
-   I thought it might be a nice separate module too. I have prepared it to ask
    opinions.
-   Use attached `.jmo`files to install via side load in jamovi.
-   Requires latest jamovi \>=1.2.22 <https://www.jamovi.org/download.html>
-   tangram error is fixed, but it does not reveal statistical test results.
-   arsenal's footnote is not visible in Html output

# ClinicoPath 0.0.2.0004

Using `spgarbet/tangram@0.3.2` until the bug is fixed.

# ClinicoPath 0.0.2.0003

Added arsenal, finalfit, and gtsummary to crosstable function. gtsummary gives
different results, due to nonparametric tests. Should add options and
documentation. tangram still not functioning.

# ClinicoPath 0.0.2

-   version 0.0.2 is released
-   works with jamovi latest release (\>=1.2.18)
    <https://www.jamovi.org/download.html>

![](man/figures/jamovi-ClinicoPath-0.0.2-released.gif){align="center"
width="75%"}

-   The new version of \#ClinicoPath [@jamovistats] module is on the jamovi
    library. Requires \#jamovi 1.2.18 \#rstats \#biostatistics \#pathology
    \#pathologists

<https://twitter.com/serdarbalci/status/1261256107919642629>

-   \#ClinicoPath \#jamovi module comes with example datasets as with other
    \#jamovi modules. Use them as example to prepare your data.

<https://twitter.com/serdarbalci/status/1261639212664840192>

-   You can easily make 'Table One' for reports/manuscripts via \#ClinicoPath
    [@jamovistats] module. Uses \#tableone, \#arsenal, \#gtsummary, and
    \#janitor packages. \#rstats \#biostatistics \#pathology \#pathologists

<https://twitter.com/serdarbalci/status/1262083972328230912>

-   \#jamovi has very nice tables. Sometimes I prefer to read the tables
    automatically via \#ClinicoPath [@jamovistats] module. Using \#easystats
    \#report package. \#naturallanguage \#data \#summary \#rstats
    \#biostatistics \#pathology \#pathologists

<https://twitter.com/serdarbalci/status/1262354990787694599>

-   With \#ClinicoPath [@jamovistats] module it is easy to make crosstables.
    uses \#tangram package \#rstats \#biostatistics \#pathology \#pathologists

<https://twitter.com/serdarbalci/status/1262691784574017536>

-   You can make different plots based on variable type via \#jamovi
    \#ClinicoPath module. Using \#rstats [@jamovistats] \#ggstatsplot
    \#ggalluvial \#easyalluvial packages \#pathology \#pathologists
    \#datavisualisation

<https://www.youtube.com/watch?v=m3uInetiC8w>

<https://twitter.com/serdarbalci/status/1263191858454413312>

-   Some examples of survival analysis via [@jamovistats] \#ClinicoPath module.
    Using \#rstats \#finalfit by [@ewenharrison] \#survival \#survminer
    \#ggstatsplot in \#jamovi \#biostatistics \#pathology \#pathologists
    <https://www.youtube.com/watch?v=gIPf4xIKAOU>

<https://www.linkedin.com/pulse/survival-analysis-via-jamovi-clinicopath-module-serdar-balc%25C4%25B1>
\#datavisualisation \#datascience \#patoloji \#analysis \#datascientist \#data
\#clinicaltrials \#clinicalstudies \#clinicaltrial \#clinicalresearch

<https://twitter.com/serdarbalci/status/1264153665386004480>

It is generating natural language summaries to make easy to read the tables:
"Median Survival: When LVI is Absent, median survival is 26 [20.1 - 32.3,"95%
CI] months. When LVI is Present, median survival is 9.3 [8.8 - 10.6, 95% CI]
months."

<https://twitter.com/serdarbalci/status/1264153686508478465>

"Hazard: When LVI is Present, there is 2.55 (1.85-3.51, p\<0.001) times risk
than when LVI is Absent."

<https://twitter.com/serdarbalci/status/1264153695715053568>

"1, 3, 5-yr Survival: When LVI Absent, 12 month survival is 70.9% [63.36%-79.3%,
95% CI]. When LVI Absent, 24 month survival is 54.2% [45.85%-64.1%, When LVI
Present, 12 month survival is 28.4% [20.03%-40.3%, 95% CI]. When LVI Present, 24
month survival is 14.4% ..."

<https://twitter.com/serdarbalci/status/1264153698764312577>

"pairwise comparison of Grade: The comparison between Grade 2 and Grade 1 has a
p-value of 0.87." Note for myself: The wording should be better.

<https://twitter.com/serdarbalci/status/1264153700114862080>

You can do multivariate survival analysis

<https://twitter.com/serdarbalci/status/1264153711087140864>

And also make Odds Ratio Tables and Plots. When you change the order of
variables in jamovi data, the analysis also changes.

<https://twitter.com/serdarbalci/status/1264153752015122432>

<https://github.com/sbalci/ClinicoPathJamoviModule>

<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLxRBOaoEoP4JfAMi7aIbkRXPXGUEwzTNv" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>

</iframe>

# ClinicoPath 0.0.1.0001

Added multivariate survival, and comparison plots.

# ClinicoPath v0.0.1

A jamovi module that contains main analysis used in clinicopathological
research. ClinicoPath help researchers to generate natural language summaries of
their dataset, generate cross tables with statistical tests, and survival
analysis with survival tables, survival curves, and natural language summaries.

You may install using side load: windows:
<https://library.jamovi.org/win64/R3.6.1/ClinicoPath-0.0.1.jmo> macOS:
<https://library.jamovi.org/macos/R3.6.1/ClinicoPath-0.0.1.jmo>

<https://github.com/sbalci/ClinicoPathJamoviModule>

<https://github.com/sbalci/ClinicoPathJamoviModule/releases/tag/v0.0.1>

# ClinicoPath 0.0.1.1001

-   removed 'frequencies'
-   Documentations are being added.
-   CI are being added.
-   Badges, README are updated.

# ClinicoPath 0.0.1.1000

## Functions work as defaults

-   Divided module into 2 windows: ClinicoPath1 and ClinicoPath2
-   Removed unnecessary outputs.
-   Added ToDo section and a warning that still in development
-   Updated Readme file
-   Currently functions are working. But only in defaults.

### For descriptive analysis:

-   TableOne
-   WriteSummary
-   Report General Features
-   Frequencies

### For comparing variables:

-   CrossTable
-   GGStatsPlot2

### For survival analysis

-   FinalFit
-   FinalFit Multivariate Survival

### For medical decision tests:

-   Medical Decision
-   Decision Calculator

### For correlation analysis:

-   Correlation

### For inter and intra observer agreement

-   Interrater Intrarater Reliability

### Decision tree and cluster analysis sections.

-   Not active yet.

# ClinicoPath 0.0.1-beta

-   First Pre-release

-   <https://github.com/sbalci/ClinicoPathJamoviModule/releases/tag/0.0.1-beta>
