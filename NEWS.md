# ClinicoPath 0.0.3.72

## PHASE 3 CUTTING-EDGE FEATURES - Optimal Cut-point Determination Implementation

### Optimal Cut-point Determination for Continuous Variables
- **Complete Implementation**: State-of-the-art optimal cut-point determination for developing new staging criteria from continuous biomarkers and measurements
- **Multiple Statistical Methods**: Maximal selected rank statistics, minimum p-value approach, survminer optimal separation, and comprehensive multi-method comparison
- **Rigorous Multiple Testing Correction**: Bonferroni, Benjamini-Hochberg (FDR), Holm, and customizable correction methods to control false discovery rates
- **Advanced Validation Framework**: Bootstrap validation with stability assessment and cross-validation for cut-point reliability testing
- **Automated Staging System Generation**: Creates new categorical staging variables (2-6 levels) from optimal cut-points with comprehensive survival statistics
- **Clinical Decision Support**: Automated interpretation with significance testing, hazard ratio assessment, and group size validation

### Comprehensive Cut-point Analysis Suite
- **Range-Based Search**: Configurable cut-point search range (default 10%-90%) to exclude extreme values and maintain statistical power
- **Statistical Rigor**: Log-rank testing, Cox regression analysis, and hazard ratio calculation with confidence intervals for each potential cut-point
- **Method Comparison**: Side-by-side comparison of different cut-point determination approaches with automated best-method selection
- **Stability Assessment**: Bootstrap coefficient of variation and cross-validation consistency metrics for cut-point robustness evaluation
- **Group Balance Validation**: Automatic detection and handling of unbalanced groups to ensure valid statistical comparisons

### New Staging System Development Tools
- **Multi-level Staging Creation**: Supports 2-6 staging levels with intelligent cut-point distribution (binary, tertile, quartile, quintile, sextile)
- **Stage-Specific Statistics**: Complete survival analysis for each generated stage including median survival, hazard ratios, and confidence intervals
- **Prognostic Validation**: Automated assessment of stage ordering, monotonicity, and discrimination performance
- **Clinical Interpretation**: Comprehensive interpretation framework with significance levels and risk stratification guidance

### Technical Architecture Excellence
- **Robust Error Handling**: Comprehensive fallback mechanisms and informative error messages for challenging datasets
- **Package Integration**: Seamless integration with survival, survminer, and maxstat packages with graceful degradation
- **Performance Optimization**: Efficient algorithms for large-scale cut-point testing with configurable bootstrap iterations
- **Non-breaking Enhancement**: Full backward compatibility with existing stagemigration functionality

### Research Applications
- **Biomarker Threshold Development**: Optimal thresholds for continuous biomarkers (tumor markers, inflammatory indices, molecular scores)
- **Staging Criteria Optimization**: Data-driven development of new staging criteria from morphometric measurements
- **Clinical Decision Thresholds**: Evidence-based cut-points for treatment decisions and risk stratification
- **Multi-institutional Validation**: Robust methods suitable for external validation and cross-population studies

---

# ClinicoPath 0.0.3.71

## STAGEMIGRATION MODULE COMPLETION - All Core Features Implemented

### Complete Implementation Status
The stagemigration module has reached full implementation with all core functionality and advanced features completed through **Phase 4-5**. All foundational features, advanced validation methods, clinical integration tools, and enhanced statistical analysis suite are now fully operational and production-ready.

### Major Implementation Milestones Completed
- **Phase 1**: Foundational Features - Migration matrix analysis, C-index comparison, NRI/IDI suite, DCA, Will Rogers detection
- **Phase 2**: Advanced Validation & Clinical Utility - Enhanced calibration, comprehensive model diagnostics, bootstrap frameworks
- **Phase 3**: Clinical Integration Features - Patient risk stratification, clinical alerts, implementation guidance, quality assurance
- **Phase 4-5**: Enhanced Statistical Analysis Suite - SME quantification, RMST discrimination, advanced testing frameworks

### Next Development Focus
With stagemigration module completion, development focus shifts to remaining TODO items from Phases 3-6 for specialized research applications and cutting-edge methodological enhancements.

---

# ClinicoPath 0.0.3.70

## PHASE 4-5 ADVANCED FEATURES - Enhanced Statistical Analysis Suite

### Advanced Migration Effect Quantification
- **Stage Migration Effect Formula (SME)**: Mathematical quantification of cumulative survival differences between staging systems using SME = Σ(S_new_i - S_old_i) formula
- **Multi-timepoint SME Analysis**: Comprehensive assessment at 1, 2, 3, and 5-year survival with stage-specific contributions and overall effect magnitude
- **Clinical Significance Thresholds**: Evidence-based interpretation with >0.10 (clinically significant), >0.05 (moderate), ≤0.05 (minimal) migration effect classifications
- **Will Rogers Phenomenon Detection**: Automated identification of artificial survival improvements through SME pattern analysis

### Robust Survival Discrimination Metrics
- **Restricted Mean Survival Time (RMST)**: Model-free survival discrimination independent of proportional hazards assumptions
- **RMST-based Discrimination Assessment**: Stage-specific RMST calculation with automatic tau selection (75th percentile of observed times)
- **Comprehensive RMST Analysis**: Range-based discrimination evaluation (>6 months = good, 3-6 months = moderate, <3 months = poor)
- **Clinical Interpretability**: Direct comparison of absolute survival benefits with median survival comparison and confidence assessment

### Advanced Statistical Testing Framework
- **Linear Trend Chi-square Tests**: Ordinal trend assessment across staging systems with Wald statistics and coefficient analysis
- **Simulation-Based Will Rogers Validation**: Synthetic data generation to demonstrate and validate Will Rogers effects with controlled scenarios
- **Martingale Residual Analysis**: Comprehensive Cox model assumption testing with outlier detection and systematic pattern identification
- **Enhanced Model Diagnostics**: Runs test for systematic patterns, heteroscedasticity testing, and autocorrelation assessment

### Comprehensive Clinical Integration
- **Evidence-Based Recommendations**: Automated clinical guidance based on SME magnitude and RMST discrimination patterns
- **Migration Effect Interpretation**: Clear clinical context for positive (Will Rogers phenomenon) vs negative (understaging) migration effects
- **Publication-Ready Outputs**: Statistical tables with confidence intervals, p-values, and clinical interpretation suitable for manuscript preparation
- **Robust Error Handling**: Comprehensive fallback mechanisms ensuring analysis completion even with challenging data structures

### Technical Architecture Excellence
- **Modular Implementation**: Independent calculation functions for SME, RMST, linear trends, simulation, and residual analysis
- **Seamless Integration**: Optional activation via boolean options (calculateSME, calculateRMST) without breaking existing workflows
- **Comprehensive Validation**: Extensive input checking, sample size requirements, and data structure verification
- **Performance Optimization**: Efficient bootstrap operations and memory management for large datasets

### Enhanced User Experience
- **Interactive Explanations**: Comprehensive HTML explanations for each advanced method with clinical context and interpretation guidance
- **Statistical Education**: Built-in methodology descriptions covering advantages, limitations, and appropriate use cases
- **Visual Integration**: Enhanced table presentations with notes, interpretations, and methodological context
- **Configuration Flexibility**: User-controlled activation allowing selective use of advanced features based on research needs

### Research Applications
- **Staging System Validation**: Complete framework for evaluating new staging systems against established standards
- **Will Rogers Phenomenon Research**: Tools for detecting, quantifying, and validating migration artifacts in clinical data
- **Regulatory Compliance**: Statistical rigor meeting current oncology and biostatistics publication standards
- **Multi-institutional Studies**: Robust methods suitable for complex real-world clinical research scenarios

---

# ClinicoPath 0.0.3.69

## PHASE 3 CLINICAL INTEGRATION FEATURES - Complete Implementation

### Clinical Decision Support System
- **Patient Risk Stratification**: Automated risk category reclassification analysis with Low/Moderate/High risk groupings based on quantile-based thresholds
- **Clinical Alert System**: Intelligent alerts for Will Rogers phenomenon detection, sample size adequacy warnings, and clinical significance thresholds
- **Implementation Guidance**: Evidence-based recommendations with priority levels (High/Medium/Low/None) and detailed implementation steps
- **Quality Assurance Framework**: Structured audit guidelines and performance monitoring protocols for staging system transitions

### Advanced Analytics Suite (Phase 2 Complete)
- **Time-Dependent Calibration Assessment**: Multi-timepoint calibration analysis with enhanced Hosmer-Lemeshow testing and calibration slope evaluation
- **Comprehensive Homogeneity Testing**: Within-stage and between-stage homogeneity validation with Jonckheere-Terpstra trend testing
- **Time-Varying Coefficient Analysis**: Advanced Cox model extension testing for proportional hazards violations and time-dependent effects
- **Enhanced Model Diagnostics**: Comprehensive residual analysis, influence detection, and goodness-of-fit assessment comparing staging systems

### Publication-Ready Reporting System
- **Executive Summary Generator**: Automated publication-quality summaries with key findings extraction and clinical interpretation
- **Methods Documentation**: Standardized methodology descriptions for manuscript preparation
- **Key Findings Extraction**: Automatic identification and formatting of primary outcomes for scientific reporting
- **Implementation Integration**: Seamless integration with advanced migration analysis workflow

### Backward Compatibility and Integration
- All new features are fully backward compatible with existing analyses
- Phase 3 features activate automatically when `advancedMigrationAnalysis = TRUE`
- Enhanced error handling and graceful degradation for incomplete data scenarios
- Comprehensive debug logging for clinical validation and troubleshooting

---

# ClinicoPath 0.0.3.68

## PHASE 1 ADVANCED ENHANCEMENTS - Evidence-Based Assessment Framework

### Revolutionary Will Rogers Phenomenon Evidence Assessment
- **Multi-criteria evaluation framework** with comprehensive evidence assessment across migration patterns, survival similarity, biological consistency, and prognostic discrimination
- **Traffic light assessment system** providing clear PASS/BORDERLINE/CONCERN/FAIL evidence grading for clinical decision-making
- **Automated clinical recommendations** with implementation guidance based on evidence strength and confidence levels
- **Advanced migration pattern analysis** with stability assessment, upstaging/downstaging balance evaluation, and staging criteria validation
- **Survival pattern validation** ensuring upstaged patients show appropriate survival similarity to target stages

### Enhanced Migration Analytics with Advanced Statistics  
- **Major migration flow identification** automatically detecting significant reclassification patterns (>10% threshold)
- **Stage retention rate analysis** with clinical interpretations for staging system stability assessment
- **Net migration flow calculations** showing patient gains/losses per stage with impact assessment
- **Flow intensity mapping** for enhanced visualization and publication-quality migration heatmaps
- **Advanced heatmap statistics** providing comprehensive migration pattern documentation

### Landmark Analysis Integration with Cancer-Type Optimization
- **Time-based discrimination analysis** with cancer-specific landmark cutoffs for optimal clinical relevance
- **Cancer-type specific landmark times**: Lung (3,6,12,24), Breast (6,12,24,60), Colorectal (6,12,24,36), Prostate (12,24,60,120) months
- **Post-landmark survival discrimination** measuring staging system performance across different time horizons  
- **Landmark-specific C-index comparisons** providing time-dependent validation of staging improvements
- **Clinical interpretation framework** for landmark analysis results with actionable guidance

### Evidence-Based Clinical Decision Support System
- **Comprehensive recommendation engine** synthesizing multiple lines of evidence for staging system adoption decisions
- **Implementation guidance framework** with specific steps for different recommendation levels (Strong/Moderate/Weak confidence)
- **Quality control integration** ensuring robust evidence evaluation before clinical implementation
- **Risk assessment protocols** identifying potential Will Rogers phenomenon with specific mitigation strategies
- **Clinical validation checklists** providing structured approach to staging system evaluation

### Technical Excellence and Integration
- **Seamless backward compatibility** preserving all existing functionality while adding advanced capabilities
- **Comprehensive error handling** with graceful failure modes and informative diagnostic messages
- **Extensive debug instrumentation** for troubleshooting and performance monitoring
- **Modular architecture** allowing selective activation of advanced features based on analysis requirements
- **Publication-ready outputs** with detailed statistical tables and clinical interpretation guidance

## Complete Documentation System & Configuration Guidance - Enhanced User Experience

### Comprehensive Analysis Configuration Guide
- **Complete decision framework** for selecting optimal analysis configurations based on research context
- **Evidence-based selection matrices** covering analysis scope, cancer type, multifactorial comparison, and baseline models
- **Resource-based optimization** with computational time estimates and memory requirements for different configurations
- **Publication target guidance** with specific configurations for high-impact (IF > 10), specialty (IF 3-10), and general journals
- **Cancer-specific optimization** with tailored time points and thresholds for lung, breast, colorectal, prostate, and other cancers

### Enhanced User Experience and Documentation
- **Real-time configuration guidance** integrated directly into the analysis interface with resource estimation
- **Comprehensive glossary expansion** including all new statistical terms and clinical interpretation thresholds  
- **Interactive decision support** with quick decision matrices for pilot studies, standard research, and high-impact publications
- **Sample size guidelines** with specific recommendations for different dataset sizes and computational constraints
- **Performance optimization strategies** for large datasets and resource-limited environments

### Professional Documentation Suite
- **Complete analysis guide** (stagemigration_analysis_guide.md) with 400+ lines of detailed configuration guidance
- **Decision matrices and examples** covering research type, resource availability, and publication targets
- **Quality control guidelines** with minimum sample sizes, follow-up requirements, and validation standards
- **Troubleshooting guidance** for common configuration issues and performance optimization
- **Real-world examples** with complete code for different research scenarios and study designs

### Advanced Configuration Framework
- **Analysis scope optimization** with clear guidance on Basic vs Standard vs Comprehensive vs Publication-ready approaches
- **Multifactorial comparison strategies** explaining when to use Adjusted C-index vs Nested models vs Stepwise vs Comprehensive
- **Baseline model selection** guidance for Covariates-only vs Original+covariates vs New+covariates approaches
- **Cancer type-specific recommendations** with literature-based time points and clinical significance thresholds
- **Multi-center study support** with institution variable guidance and internal-external cross-validation setup

### In-Function Guidance Integration
- **Enhanced explanatory outputs** with resource estimation and configuration recommendations directly in the interface
- **Quick reference links** to detailed documentation files for comprehensive guidance
- **Performance estimates** showing computational time and memory requirements for different configurations
- **Sample size recommendations** with specific thresholds for optimal analysis performance
- **Clinical significance thresholds** clearly explained with evidence-based cutoffs for different metrics

## Major Multivariable Analysis Enhancement - Complete State-of-the-Art Implementation

### Revolutionary Multivariable Stage Migration Analysis
- **Complete multivariable analysis overhaul** with state-of-the-art statistical methods for staging system validation
- **Bootstrap model selection stability** using 500 bootstrap samples with comprehensive variable selection assessment
- **Advanced interaction detection** with statistical tests for stage-covariate interactions and clinical significance evaluation
- **Comprehensive model diagnostics** including validation metrics, residual analysis, and assumption testing
- **Research-grade statistical rigor** matching current oncology and biostatistics literature standards

### Advanced Net Reclassification Improvement (NRI)
- **Adjusted NRI calculation** accounting for baseline covariates in multifactorial analysis
- **Multiple model comparisons** including baseline (covariates only), old staging + covariates, and new staging + covariates
- **Time-specific analysis** at customizable time points (default: 12, 24, 60 months)
- **Comprehensive NRI components** with event-specific and overall metrics, confidence intervals, and p-values
- **Clinical interpretation framework** with automatic significance assessment and practical relevance evaluation

### Multivariable Decision Curve Analysis
- **Clinical utility assessment** comparing multiple model combinations across probability thresholds (0.01-0.99)
- **Net benefit calculations** for baseline, old staging + covariates, new staging + covariates, and combined models
- **Optimal threshold identification** showing where each model provides maximum clinical utility
- **Model superiority ranges** identifying threshold ranges where each model outperforms others
- **Standardized comparisons** relative to treat-all and treat-none strategies for clinical decision support

### Personalized Risk Prediction System
- **Individual patient assessments** with personalized risk predictions at multiple time points
- **Risk categorization** using clinical thresholds (Low/Moderate/High/Very High) with automated reclassification analysis
- **Clinical recommendations** providing automated treatment intensity guidance based on risk changes
- **Representative patient profiles** including young/low risk, older/high risk, and average patient archetypes
- **Population-level insights** with comprehensive reclassification statistics and summary metrics

### Bootstrap Model Selection Stability
- **Variable selection stability** using 500 bootstrap samples with stepwise selection
- **AIC impact assessment** measuring average model improvement when variables are included vs excluded
- **Selection frequency analysis** identifying variables selected in >80% of samples as highly stable
- **Confidence intervals** for AIC impact with bootstrap-derived uncertainty quantification
- **Clinical relevance thresholds** for interpreting variable importance and model stability

### Advanced Interaction Detection Framework
- **Formal statistical testing** for stage-covariate interactions using likelihood ratio tests
- **Clinical significance assessment** with automated interpretation of interaction effects
- **Subgroup identification** revealing patient populations with differential staging system benefit
- **Comprehensive interaction analysis** including chi-square statistics, degrees of freedom, and p-values
- **Evidence-based interpretation** with clinical recommendations for subgroup-specific approaches

### Enhanced User Experience and Documentation
- **Comprehensive explanatory outputs** with state-of-the-art method descriptions and clinical interpretations
- **Advanced glossary system** including all new multivariable analysis terms and statistical concepts
- **Clinical significance thresholds** with evidence-based cutoffs (C-index ≥ 0.02, NRI ≥ 20%, selection frequency > 80%)
- **Complete documentation** with comprehensive stagemigration_documentation.md covering all new features
- **Professional visualization support** with enhanced explanations for all advanced analysis components

### Technical Architecture Improvements
- **Modular analysis framework** with independent error handling for each advanced method
- **Robust statistical calculations** with comprehensive fallback mechanisms and edge case handling
- **Performance optimization** with efficient bootstrap operations and memory management
- **Error isolation** ensuring advanced analysis failures don't affect core functionality
- **Comprehensive validation** with extensive input checking and data structure verification

### Clinical Research Applications
- **Publication-ready outputs** meeting current standards for staging system validation studies
- **Evidence-based recommendations** using multiple criteria synthesis for adoption decisions
- **Real-world performance assessment** accounting for confounding by other prognostic factors
- **Personalized medicine support** with individual risk assessments and clinical decision guidance
- **Multi-center validation** compatibility with internal-external cross-validation methodologies

### Integration and Compatibility
- **Seamless integration** with existing stage migration workflow without breaking changes
- **Backward compatibility** maintaining all existing functionality while adding advanced features
- **Configurable activation** via multifactorialComparisonType option (comprehensive mode recommended)
- **Non-breaking enhancement** ensuring existing analyses continue to work as before
- **Progressive enhancement** allowing users to access advanced features when needed

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
