# Interobserver reliability beyond a single kappa

## When to use this

A reliability study almost never ends with one number. You report an
overall Fleiss’ kappa, a reviewer asks *“which two readers actually
disagreed?”* and *“which diagnostic category is dragging the agreement
down?”*, and suddenly a single summary statistic is not enough.
Interobserver studies of structured reporting systems - the Yokohama
System for Reporting Breast cytopathology (YSRB), the Milan system, the
Bethesda system, ISUP grading - routinely pair the overall kappa with
two supplementary tables:

- **Table I - every rater pair.** Cohen’s kappa for each of the
  $`\binom{k}{2}`$ reader pairs, with a confidence interval and a
  significance test. This is how you spot an outlier reader whose
  pairwise kappas are systematically lower than everyone else’s.
- **Table II - every category.** A per-category agreement rate that
  answers *“when readers land on category c, how often do they all
  agree?”* This is how you find the diagnostic bottleneck - typically
  the “atypical / suspicious” middle category.

The `agreement` analysis produces both, alongside the overall kappa,
from the same set of rater columns.

## A synthetic YSRB-like dataset

The real study data are not redistributable, so we simulate a dataset
with the same shape: 99 cases, four readers, and five ordinal YSRB
categories (I = insufficient, II = benign, III = atypical, IV =
suspicious, V = malignant) with the characteristic heavily-benign
marginal distribution.

``` r

library(ClinicoPath)

set.seed(2024)
n     <- 99
cats  <- c("I", "II", "III", "IV", "V")
prob  <- c(0.05, 0.75, 0.05, 0.04, 0.11)   # mostly benign, as in real cytology
truth <- sample(cats, n, replace = TRUE, prob = prob)

# Each reader agrees with the latent truth most of the time, otherwise slips to
# an adjacent category. Reader 4 is a little noisier than the rest.
slip <- function(x) {
  i <- match(x, cats)
  cats[pmin(length(cats), pmax(1, i + sample(c(-1, 1), 1)))]
}
reader <- function(p_agree) {
  vapply(truth, function(t) if (runif(1) < p_agree) t else slip(t), character(1))
}

ysrb <- data.frame(
  Reader1 = reader(0.92),
  Reader2 = reader(0.90),
  Reader3 = reader(0.91),
  Reader4 = reader(0.84),
  stringsAsFactors = FALSE
)
ysrb[] <- lapply(ysrb, factor, levels = cats, ordered = TRUE)

knitr::kable(head(ysrb), caption = "First few cases (4 readers, YSRB category)")
```

| Reader1 | Reader2 | Reader3 | Reader4 |
|:--------|:--------|:--------|:--------|
| V       | V       | V       | V       |
| II      | II      | II      | II      |
| II      | II      | II      | III     |
| II      | II      | II      | II      |
| II      | II      | III     | II      |
| II      | II      | II      | III     |

First few cases (4 readers, YSRB category) {.table}

## Run the analysis

We turn on the all-pairs table, the per-category item-modal table, and a
Bonferroni correction for the $`\binom{4}{2} = 6`$ pairwise tests.

``` r

res <- agreement(
  data                   = ysrb,
  vars                   = c("Reader1", "Reader2", "Reader3", "Reader4"),
  allPairsKappa          = TRUE,
  itemModalCategoryAgreement = TRUE,
  multipleTestCorrection = "bonferroni"
)
```

### Overall agreement

``` r

knitr::kable(res$irrtable$asDF, digits = 3,
             caption = "Overall agreement (Fleiss' kappa across all four readers)")
```

|     | method                     | subjects | raters | peragree | kappa |    z |   p |
|:----|:---------------------------|---------:|-------:|---------:|------:|-----:|----:|
| “1” | Fleiss’ Kappa for m Raters |       99 |      4 |   60.606 | 0.571 | 22.8 |   0 |

Overall agreement (Fleiss’ kappa across all four readers) {.table
style="width:100%;"}

The overall kappa tells you the readers agree appreciably better than
chance, but it hides *where* the agreement comes from and *where* it
breaks down.

## Table I - All-pairs Cohen’s kappa

``` r

allpairs <- res$allPairsKappaTable$asDF
knitr::kable(
  allpairs[, c("rater_a", "rater_b", "n", "peragree", "kappa",
               "ci_lower", "ci_upper", "p_adj")],
  digits = 3,
  col.names = c("Reader A", "Reader B", "n", "Obs. agree", "Kappa",
                "CI lower", "CI upper", "p (Bonf.)"),
  caption = "Cohen's kappa for every reader pair, with 95% CI."
)
```

|  | Reader A | Reader B | n | Obs. agree | Kappa | CI lower | CI upper | p (Bonf.) |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| “Reader1\_\_Reader2” | Reader1 | Reader2 | 99 | 0.848 | 0.674 | 0.528 | 0.819 | 0 |
| “Reader1\_\_Reader3” | Reader1 | Reader3 | 99 | 0.848 | 0.669 | 0.524 | 0.814 | 0 |
| “Reader1\_\_Reader4” | Reader1 | Reader4 | 99 | 0.747 | 0.500 | 0.345 | 0.656 | 0 |
| “Reader2\_\_Reader3” | Reader2 | Reader3 | 99 | 0.818 | 0.613 | 0.464 | 0.762 | 0 |
| “Reader2\_\_Reader4” | Reader2 | Reader4 | 99 | 0.747 | 0.510 | 0.355 | 0.665 | 0 |
| “Reader3\_\_Reader4” | Reader3 | Reader4 | 99 | 0.737 | 0.486 | 0.330 | 0.642 | 0 |

Cohen’s kappa for every reader pair, with 95% CI. {.table
style="width:100%;"}

Two points worth stressing about this table:

- **The confidence intervals are computed from the non-null asymptotic
  standard error** (via
  [`vcd::Kappa`](https://rdrr.io/pkg/vcd/man/Kappa.html)), so they agree
  with
  [`psych::cohen.kappa()`](https://rdrr.io/pkg/psych/man/kappa.html) and
  are *wider* - i.e. honest - compared with intervals built from the
  kappa/z test statistic, which uses the standard error under the null
  hypothesis of no agreement. A too-narrow CI would overstate precision.
- **Pairs involving the noisier Reader 4** sit at the bottom of the
  kappa ranking. In a real study this is exactly the signal that prompts
  targeted re-training rather than a blanket “agreement was moderate”
  conclusion.

## Table II - Agreement by modal category

For each case we take the modal (most common) reading across the four
readers, then, within each category, average the case-level agreement
rate. A 4/4 case scores 1.00, a 3/1 split scores 0.75, and a 2/2 split
has no unique mode and is excluded.

``` r

modal <- res$itemModalAgreementTable$asDF
knitr::kable(
  modal,
  digits = 3,
  col.names = c("Modal category", "Cases", "Mean agreement",
                "CI lower", "CI upper"),
  caption = "Within-case agreement, by the case's modal category."
)
```

|       | Modal category | Cases | Mean agreement | CI lower | CI upper |
|:------|:---------------|------:|---------------:|---------:|---------:|
| “I”   | I              |     2 |          1.000 |    1.000 |    1.000 |
| “II”  | II             |    76 |          0.898 |    0.867 |    0.929 |
| “III” | III            |     5 |          0.850 |    0.730 |    0.970 |
| “IV”  | IV             |     4 |          0.938 |    0.815 |    1.000 |
| “V”   | V              |     9 |          0.917 |    0.835 |    0.998 |

Within-case agreement, by the case’s modal category. {.table}

The benign category (II) carries most of the cases and shows high,
tightly estimated agreement. The rarer categories (I, III, IV) are
represented by only a handful of modal cases, so their agreement
estimates are unstable and their confidence intervals are wide - a
reminder that a per-category mean is only as trustworthy as its cell
count. In a real, larger YSRB or Milan series this same table is where
the atypical/suspicious middle categories typically reveal themselves as
the agreement bottleneck; here it mainly illustrates that you should
read sparse-category rows with their CIs, not their point estimates.

## Two cautions the analysis flags for you

**The kappa paradox.** When a category is rare, kappa can be
paradoxically low even when observed agreement is high. If any category
is sparse, the analysis attaches a note to the overall table suggesting
the prevalence-robust alternatives it also offers - Gwet’s AC1/AC2 and
PABAK - as sensitivity analyses. Enable those (and `bootstrapCI`) when
your marginal distribution is as lopsided as a typical benign-dominated
cytology series.

**Multiplicity.** With $`k`$ readers you run $`\binom{k}{2}`$ pairwise
tests - 6 for four readers, 15 for six. The
`Multiple Testing Correction` option (Bonferroni, Benjamini-Hochberg, or
Holm) adds an adjusted-p column to Table I so you do not read six raw
p-values as if they were one.

## Reproducibility

``` r

sessionInfo()
```

    ## R version 4.6.0 (2026-04-24)
    ## Platform: aarch64-apple-darwin23
    ## Running under: macOS Tahoe 26.5.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8
    ## 
    ## time zone: Europe/Istanbul
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] ClinicoPath_0.0.47
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] MatrixGenerics_1.24.0     httr_1.4.8               
    ##   [3] tidyr_1.3.2               jsonlite_2.0.0           
    ##   [5] gsDesign_3.10.0           RcppParallel_5.1.11-2    
    ##   [7] Formula_1.2-5             expm_1.0-0               
    ##   [9] blob_1.3.0                coefplot_1.2.9           
    ##  [11] pec_2025.06.24            RSQLite_3.53.1           
    ##  [13] factoextra_2.1.0          pillar_1.11.1            
    ##  [15] quantreg_6.1              ggsignif_0.6.4           
    ##  [17] DataExplorer_0.9.0        XVector_0.52.0           
    ##  [19] promises_1.5.0            polyclip_1.10-7          
    ##  [21] recipes_1.3.3             plyr_1.8.9               
    ##  [23] MLEcens_0.1-7.1           bibtex_0.5.2             
    ##  [25] logger_0.4.2              naivebayes_1.0.0         
    ##  [27] tweenr_2.0.3              ggalluvial_0.12.6        
    ##  [29] marqLevAlg_2.0.8          terra_1.9-27             
    ##  [31] maptiles_0.11.0           MatrixModels_0.5-4       
    ##  [33] BiocParallel_1.46.0       openssl_2.4.2            
    ##  [35] timechange_0.4.0          mets_1.3.11              
    ##  [37] cachem_1.1.0              UpSetR_1.4.1             
    ##  [39] cli_3.6.6                 forecast_9.0.2           
    ##  [41] ggtext_0.1.2              memoise_2.0.1            
    ##  [43] evaluate_1.0.5            parallelly_1.48.0        
    ##  [45] rstatix_1.0.0             foreign_0.8-91           
    ##  [47] httr2_1.2.2               tools_4.6.0              
    ##  [49] multcomp_1.4-30           locfit_1.5-9.12          
    ##  [51] cluster_2.1.8.2           tinytable_0.16.0         
    ##  [53] irlba_2.3.7               DoE.base_1.2-5           
    ##  [55] compiler_4.6.0            multcompView_0.1-11      
    ##  [57] grid_4.6.0                tmaptools_3.3            
    ##  [59] ggridges_0.5.7            irr_0.85                 
    ##  [61] igraph_2.3.3              tcltk_4.6.0              
    ##  [63] gtable_0.3.6              deSolve_1.42             
    ##  [65] knitr_1.51                eurostat_4.0.0           
    ##  [67] zeallot_0.2.0             fastmap_1.2.0            
    ##  [69] optimx_2025-4.9           future.apply_1.20.2      
    ##  [71] tmap_4.3                  DiagrammeRsvg_0.1        
    ##  [73] coro_1.1.0                rcrossref_1.2.1          
    ##  [75] riskRegression_2026.03.11 KernSmooth_2.23-26       
    ##  [77] readr_2.2.0               otel_0.2.0               
    ##  [79] crul_1.6.0                leaflet_2.2.3            
    ##  [81] lmtest_0.9-40             sass_0.4.10              
    ##  [83] edgeR_4.10.1              foreach_1.5.2            
    ##  [85] splines_4.6.0             carData_3.0-6            
    ##  [87] prodlim_2026.03.11        pseudo_1.4.3             
    ##  [89] Rdpack_2.6.6              paletteer_1.7.0          
    ##  [91] showtextdb_3.0            cols4all_0.10            
    ##  [93] bayestestR_0.18.1         ggrepel_0.9.8            
    ##  [95] backports_1.5.1           lattice_0.22-9           
    ##  [97] visdat_0.6.0              jtools_2.3.1             
    ##  [99] checkmate_2.3.4           minqa_1.2.8              
    ## [101] snakecase_0.11.1          muhaz_1.2.6.4            
    ## [103] colorspace_2.1-2          survey_4.5               
    ## [105] cmprsk_2.2-12             psych_2.6.5              
    ## [107] timeDate_4052.112         lpSolve_5.6.23           
    ## [109] flexsurv_2.3.2            ComplexUpset_1.3.3       
    ## [111] stars_0.7-2               caret_7.0-1              
    ## [113] DiagrammeR_1.0.12         genefilter_1.94.0        
    ## [115] sysfonts_0.8.9            scales_1.4.0             
    ## [117] fontawesome_0.5.3         mlr3_1.7.1               
    ## [119] gower_1.0.2               rootSolve_1.8.2.4        
    ## [121] lme4_2.0-1                combinat_0.0-8           
    ## [123] tibble_3.3.1              IRanges_2.46.0           
    ## [125] geepack_1.3.13            Epi_2.65                 
    ## [127] nnet_7.3-20               TMB_1.9.21               
    ## [129] paradox_1.0.1             mime_0.13                
    ## [131] easyalluvial_0.4.0        future_1.70.0            
    ## [133] fs_2.1.0                  nlme_3.1-169             
    ## [135] MASS_7.3-65               gtExtras_0.6.2           
    ## [137] class_7.3-23              progressr_1.0.0          
    ## [139] palmerpenguins_0.1.1      directlabels_2026.4.23   
    ## [141] hms_1.1.4                 matrixStats_1.5.0        
    ## [143] stringr_1.6.0             perm_1.0-0.4             
    ## [145] plotly_4.12.0             glmnet_5.0               
    ## [147] statsExpressions_2.0.0    survminer_0.5.2          
    ## [149] leaflegend_1.2.8          kableExtra_1.4.0         
    ## [151] rlang_1.2.0               units_1.0-1              
    ## [153] iterators_1.0.14          gt_1.3.0                 
    ## [155] base64enc_0.1-6           abind_1.4-8              
    ## [157] pivottabler_1.5.6         reshape2_1.4.5           
    ## [159] R6_2.6.1                  epitools_0.5-10.1        
    ## [161] lubridate_1.9.5           Icens_1.84.0             
    ## [163] bit_4.6.0                 rprojroot_2.1.1          
    ## [165] Hmisc_5.2-6               ggfortify_0.4.19         
    ## [167] exactRankTests_0.8-37     ggVennDiagram_1.5.7      
    ## [169] stringi_1.8.7             png_0.1-9                
    ## [171] viridis_0.6.5             Seqinfo_1.2.0            
    ## [173] kknn_1.4.1                here_1.0.2               
    ## [175] ellipse_0.5.0             microbenchmark_1.5.0     
    ## [177] S7_0.2.2                  statmod_1.5.2            
    ## [179] askpass_1.2.1             fontLiberation_0.1.0     
    ## [181] doBy_4.7.2                robustbase_0.99-7        
    ## [183] gdtools_0.5.1             Rttf2pt1_1.3.14          
    ## [185] quadprog_1.5-8            reticulate_1.46.0        
    ## [187] extrafontdb_1.1           gridtext_0.1.6           
    ## [189] randomForest_4.7-1.2      tidyheatmaps_0.2.1       
    ## [191] fontBitstreamVera_0.1.1   pkgconfig_2.0.3          
    ## [193] httpcode_0.3.0            SparseM_1.84-2           
    ## [195] xtable_1.8-8              patchwork_1.3.2          
    ## [197] svglite_2.2.2             lava_1.9.2               
    ## [199] tzdb_0.5.0                metadat_1.6-0            
    ## [201] KMsurv_0.1-6              nleqslv_3.3.7            
    ## [203] annotate_1.90.0           KEGGREST_1.52.0          
    ## [205] dbscan_1.2.5              furrr_0.4.0              
    ## [207] urca_1.3-4                BiocGenerics_0.58.1      
    ## [209] mnormt_2.1.2              survival_3.8-6           
    ## [211] ggpp_0.6.1                readxl_1.5.0             
    ## [213] polspline_1.1.25          data.tree_1.2.0          
    ## [215] doParallel_1.0.17         forcats_1.0.1            
    ## [217] rapportools_1.2           data.table_1.18.4        
    ## [219] mitools_2.4               Exact_3.3                
    ## [221] S4Vectors_0.50.1          V8_8.2.0                 
    ## [223] spacesXYZ_1.6-0           zip_3.0.0                
    ## [225] Biostrings_2.80.0         flashClust_1.1-4         
    ## [227] Deriv_4.2.0               networkD3_0.4.1          
    ## [229] torchvision_0.9.0         numbers_0.9-2            
    ## [231] finalfit_1.1.0            visNetwork_2.1.4         
    ## [233] miniUI_0.1.2              partitions_1.10-9        
    ## [235] leafem_0.2.5              DescTools_0.99.60        
    ## [237] fracdiff_1.5-4            conf.design_2.0.0        
    ## [239] polynom_1.4-1             rematch2_2.1.2           
    ## [241] rstantools_2.6.0          bdsmatrix_1.3-7          
    ## [243] ragg_1.5.2                SQUAREM_2026.1           
    ## [245] Rcpp_1.1.1-1.1            sva_3.60.0               
    ## [247] coda_0.19-4.1             Biobase_2.72.0           
    ## [249] mixmeta_1.2.2             bit64_4.8.2              
    ## [251] maxstat_0.7-26            viridisLite_0.4.3        
    ## [253] showtext_0.9-8            epiR_2.0.94              
    ## [255] lgr_0.5.2                 stats4_4.6.0             
    ## [257] gld_2.6.8                 ggdist_3.3.3             
    ## [259] cellranger_1.1.0          yaml_2.3.12              
    ## [261] mlr3misc_0.22.0           globals_0.19.1           
    ## [263] numDeriv_2016.8-1.1       limma_3.68.3             
    ## [265] torch_0.17.0              uuid_1.2-2               
    ## [267] RefManageR_1.4.0          sandwich_3.1-1           
    ## [269] timeROC_0.4.1             dfoptim_2023.1.0         
    ## [271] httpuv_1.6.17             rms_8.1-1                
    ## [273] mstate_0.3.3              raster_3.6-32            
    ## [275] TidyDensity_1.5.2         listenv_1.0.0            
    ## [277] s2_1.1.11                 purrr_1.2.2              
    ## [279] officer_0.7.5             fontquiver_0.2.1         
    ## [281] htmlTable_2.5.0           proxy_0.4-29             
    ## [283] gmp_0.7-5.1               splines2_0.5.4           
    ## [285] mgcv_1.9-4                generics_0.1.4           
    ## [287] car_3.1-5                 etm_1.1.2                
    ## [289] TH.data_1.1-5             janitor_2.2.1            
    ## [291] leafsync_0.1.0            ggstatsplot_1.0.0        
    ## [293] ISOweek_0.6-2             lwgeom_0.2-16            
    ## [295] reReg_1.4.7               tableone_0.13.2          
    ## [297] textshaping_1.0.5         survC1_1.0-3             
    ## [299] crosstable_0.9.0          lifecycle_1.0.5          
    ## [301] rpart.plot_3.1.4          scam_1.2-22              
    ## [303] callr_3.7.6               AnnotationDbi_1.74.0     
    ## [305] r2rtf_1.3.1               vcd_1.4-13               
    ## [307] pkgdown_2.2.0             ggrain_0.1.2             
    ## [309] e1071_1.7-17              benford.analysis_0.1.5   
    ## [311] systemfonts_1.3.2         rstudioapi_0.19.0        
    ## [313] htmlwidgets_1.6.4         leaps_3.2                
    ## [315] ggswim_0.1.0.9002         lmerTest_3.2-1           
    ## [317] BB_2026.1.0               mlr3learners_0.15.0      
    ## [319] DBI_1.3.0                 zoo_1.8-15               
    ## [321] performance_0.17.1        useful_1.2.7             
    ## [323] dplyr_1.2.1               pwr_1.3-0                
    ## [325] grafify_5.1.0             extrafont_0.20           
    ## [327] utf8_1.2.6                mathjaxr_2.0-0           
    ## [329] jomo_2.7-6                shape_1.4.6.1            
    ## [331] BiasedUrn_2.0.12          sf_1.1-1                 
    ## [333] icenReg_2.0.16            vctrs_0.7.3              
    ## [335] arsenal_3.7.1             modelr_0.1.11            
    ## [337] bslib_0.11.0              moments_0.14.1           
    ## [339] PRROC_1.4                 shiny_1.14.0             
    ## [341] assertthat_0.2.1          datawizard_1.3.1         
    ## [343] distributional_0.8.1      countrycode_1.8.0        
    ## [345] sp_2.2-1                  lmom_3.3                 
    ## [347] metafor_5.0-1             broom.mixed_0.2.9.7      
    ## [349] pander_0.6.6              frailtypack_3.8.0        
    ## [351] mvmeta_1.0.3              dcurves_0.5.1            
    ## [353] corrplot_0.95             ipred_0.9-15             
    ## [355] timereg_2.0.7             glue_1.8.1               
    ## [357] reformulas_0.4.4          digest_0.6.39            
    ## [359] pROC_1.19.0.1             glmmTMB_1.1.14           
    ## [361] RColorBrewer_1.1-3        rbibutils_2.4.1          
    ## [363] hardhat_1.4.3             GPArotation_2026.6-1     
    ## [365] Matrix_1.7-5              tidyplots_0.4.0.9000     
    ## [367] cowplot_1.2.0             htmltools_0.5.9          
    ## [369] XML_3.99-0.23             FactoMineR_2.16          
    ## [371] mlr3measures_1.3.0        labelled_2.16.0          
    ## [373] broom_1.0.13              magick_2.9.1             
    ## [375] haven_2.5.5               mvtnorm_1.4-1            
    ## [377] mlr3viz_0.11.0            jmvcore_2.7.35           
    ## [379] rsm_2.10.6                processx_3.9.0           
    ## [381] later_1.4.8               mlr3pipelines_0.11.0     
    ## [383] powerSurvEpi_0.1.5        ggforce_0.5.0            
    ## [385] mada_0.5.12               pracma_2.4.6             
    ## [387] emmeans_2.0.3             farver_2.1.2             
    ## [389] ggplot2_4.0.3             DT_0.34.0                
    ## [391] ggpubr_0.6.3              gtsummary_2.5.1          
    ## [393] withr_3.0.3               pan_2.0                  
    ## [395] lazyeval_0.2.3            mitml_0.4-5              
    ## [397] magrittr_2.0.5            crayon_1.5.3             
    ## [399] effectsize_1.0.2          estimability_2.0.0       
    ## [401] mice_3.19.0               vtree_5.7.0              
    ## [403] RcppArmadillo_15.4.0-1    xml2_1.6.0               
    ## [405] ggvenn_0.1.19             giscoR_1.1.0             
    ## [407] coxme_2.2-22              flextable_0.9.12         
    ## [409] jquerylib_0.1.4           clintools_0.9.10.1       
    ## [411] summarytools_1.1.5        nloptr_2.2.1             
    ## [413] classInt_0.4-11           parameters_0.29.2        
    ## [415] rappdirs_0.3.4            regions_0.1.8            
    ## [417] crosstalk_1.2.2           waffle_1.0.2             
    ## [419] rmarkdown_2.31            wk_0.9.5                 
    ## [421] boot_1.3-32               ModelMetrics_1.2.2.2     
    ## [423] codetools_0.2-20          curl_7.1.0               
    ## [425] correlation_0.8.8         ggmice_0.1.1             
    ## [427] gridExtra_2.3.1           interval_1.1-1.0         
    ## [429] reda_0.5.6                insight_1.5.2            
    ## [431] desc_1.4.3                ggeconodist_0.1.0        
    ## [433] matrixcalc_1.0-6          parallel_4.6.0           
    ## [435] cutpointr_1.2.1           rpart_4.1.27             
    ## [437] xfun_0.59                 DEoptimR_1.2-0           
    ## [439] tidyselect_1.2.1          scatterplot3d_0.3-45
