# Overall, Cause Specific, and Competing Survival Documentation

## Feature Summary

The `competingsurvival` function provides clinicians and pathologists with a single interface for three distinct survival analysis strategies that handle multi-outcome follow-up data. In clinical datasets, patients may experience death from the disease under study, death from unrelated causes, or remain alive (with or without disease). The choice of how to handle these different outcomes profoundly affects survival estimates and clinical conclusions.

The function supports **overall survival** (all deaths are events), **cause-specific survival** (only disease deaths are events, competing deaths are censored), and **competing risks analysis** (cumulative incidence functions that properly account for the impossibility of experiencing the event of interest after experiencing a competing event). The competing risks mode offers Gray's test for group comparisons, Fine-Gray subdistribution hazard regression, cumulative incidence estimation at user-specified timepoints, and three visualization types: CIF curves, stacked probability plots, and 1-KM vs CIF bias comparison plots.

All analyses are built on established R packages -- `survival` and `finalfit` for standard survival, `cmprsk` for competing risks -- and present results through jamovi's structured table, plot, and HTML output system. The function resides in the SurvivalD > Drafts menu pending validation against published reference datasets.

## Feature Details

### Core Variables

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Follow-up time | `overalltime` | Time Variable | All analyses | `survival::Surv()` time component; `cmprsk::cuminc(ftime=)` |
| Multi-level outcome | `outcome` | Outcome Variable | All analyses | Source of event coding via `dod`/`dooc`/`awd`/`awod` levels |
| Group comparison variable | `explanatory` | Group Variable (Optional) | `survivalTable`, `fineGrayTable`, `summary`, all plots | `finalfit::finalfit()` explanatory; `cmprsk::cuminc(group=)`; `cmprsk::crr(cov1=)` |

### Outcome Levels

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Primary event (disease death) | `dod` | Primary Event (e.g., Death from Disease) | All analyses; coded as status=1 | `dplyr::case_when()` in event coding; `cmprsk::crr(failcode=1)` |
| Competing event (other death) | `dooc` | Competing Event (e.g., Death from Other Causes) | `compete`: status=2; `overall`: status=1; `cause`: status=0 | `dplyr::case_when()` in event coding |
| Censored with disease present | `awd` | Censored with Event (e.g., Alive with Disease) | All analyses; coded as status=0 (censored) | `dplyr::case_when()` in event coding |
| Censored disease-free | `awod` | Censored without Event (e.g., Alive without Disease) | All analyses; coded as status=0 (censored) | `dplyr::case_when()` in event coding |

### Analysis Configuration

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Analysis type selector | `analysistype` | Analysis Type | Controls all output visibility and analysis branch | `.performAnalysis()` dispatch: `overall` -> `.overallSurvival()`, `cause` -> `.causeSpecificSurvival()`, `compete` -> `.competingRisksSurvival()` |
| Gray's test for CIF comparison | `graystest` | Gray's Test for Competing Risks | `summary` (HTML table with chi-squared, df, p-value) | `.performGraysTest()` extracts from `cmprsk::cuminc()$Tests` matrix |
| Fine-Gray subdistribution model | `subdistribution` | Subdistribution Hazard Model (Fine-Gray) | `survivalTable` (subdistribution HR row), `fineGrayTable` (full coefficient table) | `.performSubdistributionAnalysis()` calls `cmprsk::crr(failcode=1, cencode=0)` |

### Statistical Options

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Cumulative incidence timepoints | `timepoints` | Time Points for Risk Estimates | `cuminc` table rows, `summary` HTML timepoint table | `.calculateCumIncAtTimepoints()` parses comma-separated string; `.formatCumulativeIncidence()` populates table |
| Confidence level for intervals | `confidencelevel` | Confidence Level (0-1) | `survivalTable` CI columns, `fineGrayTable` CI columns, `summary` CI labels | `qnorm(1 - (1 - conf_level)/2)` for z-critical in Fine-Gray and CIF CI computation |

### Visualization

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Number at risk display | `showrisksets` | Show Number at Risk | `summary` (appended HTML with summary counts) | Appends total/event/competing/censored counts to summary HTML (full risk table is TODO) |
| Stacked probability plot | `showStackedPlot` | Show Stacked Probability Plot | `stackedPlot` (700x500 Image) | `.stackedPlot()` computes CIF1 + CIF2 + S(t) = 1; `ggplot2::geom_area()` |
| 1-KM vs CIF bias comparison | `showKMvsCIF` | Show 1-KM vs CIF Comparison | `kmvscifPlot` (700x500 Image) | `.kmvscifPlot()` fits `survival::survfit()` for naive 1-KM, overlays proper CIF; `ggplot2::geom_step()` |
| CIF color palette | `cifColors` | CIF Color Scheme | `comprisksPlot`, `stackedPlot`, `kmvscifPlot` | `switch()` in each plot render: `default` (Red/Blue), `colorblind` (Okabe-Ito), `grayscale` (monochrome) |

### Display Options

| Feature | YAML Argument | UI Label | Results Section | R Function |
|---|---|---|---|---|
| Welcome / instructions panel | (auto) | -- | `todo` (Html) | `.run()` sets welcome HTML when `outcome` or `overalltime` is NULL |
| Analysis summary | (auto) | -- | `summary` (Html) | `.formatSurvivalResults()` or `.formatEnhancedCompetingRisksResults()` builds HTML |
| Survival / HR table | (auto) | -- | `survivalTable` (Table) | `.formatSurvivalResults()` parses finalfit HR text; `.formatEnhancedCompetingRisksResults()` adds CR and Fine-Gray rows |
| Cumulative incidence table | (auto) | -- | `cuminc` (Table, visible when compete) | `.formatCumulativeIncidence()` populates from cuminc result at specified timepoints |
| CIF curves plot | (auto) | -- | `comprisksPlot` (700x400 Image, visible when compete) | `.plotCompetingRisks()` converts serialized cuminc to long-format df; `ggplot2::geom_step()` |
| Fine-Gray coefficient table | (auto) | -- | `fineGrayTable` (Table, visible when subdistribution) | `.formatEnhancedCompetingRisksResults()` extracts `summary(crr_model)$coef` |
| Clinical interpretation | (auto) | -- | `interpretation` (Html) | `.generateInterpretation()` switch on analysis_type |
| Assumptions & caveats | (auto) | -- | `assumptions` (Html) | `.generateInterpretation()` sets fixed competing risks assumptions HTML |

## Complete Options-to-Results Matrix

The table below shows which options affect which result items. An "X" means the result's `clearWith` list includes that option (i.e., the result is recomputed when that option changes).

| Option | todo | summary | survivalTable | cuminc | comprisksPlot | stackedPlot | kmvscifPlot | interpretation | assumptions | fineGrayTable |
|---|---|---|---|---|---|---|---|---|---|---|
| `overalltime` | X | X | X | X | X | X | X | X | X | X |
| `outcome` | X | X | X | X | X | X | X | X | X | X |
| `explanatory` | X | X | X | X | X | X | X | X | X | X |
| `dod` | | X | X | X | X | X | X | X | | X |
| `dooc` | | X | X | X | X | X | X | X | | X |
| `awd` | | X | X | X | X | X | X | X | | |
| `awod` | | X | X | X | X | X | X | X | | |
| `analysistype` | | X | X | X | X | X | X | X | X | X |
| `graystest` | | X | X | | | | | X | | |
| `subdistribution` | | X | X | | | | | X | | X |
| `timepoints` | | X | X | X | | | | X | | |
| `confidencelevel` | | X | X | | | | | X | | X |
| `showrisksets` | | | | | X | | | | | |
| `showStackedPlot` | | | | | | X | | | | |
| `showKMvsCIF` | | | | | | | X | | | |
| `cifColors` | | | | | X | X | X | | | |

## Analysis Type Behavior Summary

| Aspect | `overall` | `cause` | `compete` |
|---|---|---|---|
| Event definition | dod=1, dooc=1 | dod=1, dooc=0 | dod=1, dooc=2 |
| Required levels | dod or dooc | dod | dod and dooc |
| R engine | `finalfit::finalfit()` or `survfit()~1` | `finalfit::finalfit()` or `survfit()~1` | `cmprsk::cuminc()` + `finalfit::crrmulti()` |
| Optional models | -- | -- | Gray's test, Fine-Gray (`cmprsk::crr()`) |
| survivalTable content | Cox HR from finalfit | Cox HR from finalfit | Standard CR HR + optional Fine-Gray HR |
| cuminc table | hidden | hidden | CIF estimates at timepoints |
| Plots | hidden | hidden | CIF curves + optional stacked + optional KM vs CIF |
| Clinical interpretation | All-cause mortality | Disease-specific mortality | Competing cause accounting |
