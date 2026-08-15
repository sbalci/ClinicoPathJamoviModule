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

|  | method | subjects | raters | peragree | kappa | ci_lower | ci_upper | z | p |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| “1” | Fleiss’ Kappa for m Raters | 99 | 4 | 60.606 | 0.571 | NA | NA | 22.8 | 0 |

Overall agreement (Fleiss’ kappa across all four readers) {.table}

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
    ## Running under: macOS Tahoe 26.5.2
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/Istanbul
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] ClinicoPath_1.0.6
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] ggeconodist_0.1.0         DBI_1.3.0                
    ##   [3] bslib_0.12.0              epitools_0.5-10.1        
    ##   [5] lpSolve_5.6.23            powerSurvEpi_0.1.5       
    ##   [7] httr_1.4.8                pan_2.0                  
    ##   [9] BiocParallel_1.46.0       pillar_1.11.1            
    ##  [11] kableExtra_1.4.1          Epi_2.66                 
    ##  [13] pivottabler_1.5.6         R6_2.6.1                 
    ##  [15] boot_1.3-32               mime_0.13                
    ##  [17] correlation_0.8.8         lmom_3.3                 
    ##  [19] visdat_0.6.0              sysfonts_0.8.9           
    ##  [21] reticulate_1.46.0         edgeR_4.10.1             
    ##  [23] gridtext_0.1.6            pwr_1.3-0                
    ##  [25] viridis_0.6.5             genefilter_1.94.0        
    ##  [27] Rttf2pt1_1.3.14           survC1_1.0-3             
    ##  [29] leafem_0.2.5              polspline_1.1.25         
    ##  [31] tidyheatmaps_0.2.1        Hmisc_5.2-6              
    ##  [33] ggpubr_1.0.0              rprojroot_2.1.1          
    ##  [35] KMsurv_0.1-6              limma_3.68.4             
    ##  [37] S7_0.2.2                  parallelly_1.48.0        
    ##  [39] rbibutils_2.4.1           mgcv_1.9-4               
    ##  [41] polyclip_1.10-7           rms_8.1-1                
    ##  [43] htmltools_0.5.9           torch_0.17.0             
    ##  [45] caret_7.0-1               showtext_0.9-8           
    ##  [47] muhaz_1.2.6.5             e1071_1.7-17             
    ##  [49] factoextra_2.2.0          datawizard_1.3.1         
    ##  [51] ggrepel_0.9.8             classInt_0.4-11          
    ##  [53] car_3.1-5                 snakecase_0.11.1         
    ##  [55] forcats_1.0.1             countrycode_1.8.0        
    ##  [57] lwgeom_0.2-17             ComplexUpset_1.3.3       
    ##  [59] leaflegend_1.2.8          pec_2025.06.24           
    ##  [61] scatterplot3d_0.3-45      survminer_0.5.2          
    ##  [63] mlr3learners_0.15.1       rpart_4.1.27             
    ##  [65] coxme_2.2-22              metadat_1.6-0            
    ##  [67] tidyselect_1.2.1          utf8_1.2.6               
    ##  [69] RSQLite_3.53.3            jomo_2.7-6               
    ##  [71] cowplot_1.2.0             psych_2.6.5              
    ##  [73] gridExtra_2.3.1           fs_2.1.0                 
    ##  [75] timereg_2.0.7             coefplot_1.2.9           
    ##  [77] RColorBrewer_1.1-3        future.apply_1.20.2      
    ##  [79] ggVennDiagram_1.5.7       glmmTMB_1.1.14           
    ##  [81] ipred_0.9-15              frailtypack_3.8.0        
    ##  [83] rapportools_1.2           mathjaxr_2.0-0           
    ##  [85] uuid_1.2-2                riskRegression_2026.03.11
    ##  [87] gsDesign_3.10.1           flextable_0.10.0         
    ##  [89] furrr_0.4.0               sass_0.4.10              
    ##  [91] scales_1.4.0              carData_3.0-6            
    ##  [93] gt_1.3.0                  ellipse_0.5.0            
    ##  [95] lava_1.9.2                doBy_4.7.2               
    ##  [97] sva_3.60.0                pracma_2.4.6             
    ##  [99] V8_8.2.0                  stars_0.7-3              
    ## [101] KEGGREST_1.52.2           promises_1.5.0           
    ## [103] ISOweek_0.6-2             shape_1.4.6.1            
    ## [105] terra_1.9-34              data.tree_1.2.0          
    ## [107] lgr_0.5.2                 zoo_1.9-0                
    ## [109] BiasedUrn_2.0.12          locfit_1.5-9.12          
    ## [111] dplyr_1.2.1               effectsize_1.0.3         
    ## [113] networkD3_0.4.1           multcomp_1.4-31          
    ## [115] assertthat_0.2.1          paletteer_1.7.0          
    ## [117] tools_4.6.0               processx_3.9.0           
    ## [119] insight_1.5.2             shiny_1.14.0             
    ## [121] modelr_0.1.11             mvmeta_1.0.3             
    ## [123] mixmeta_1.2.2             rlang_1.3.0              
    ## [125] generics_0.1.4            ggridges_0.5.7           
    ## [127] extrafont_0.20            corrplot_0.95            
    ## [129] evaluate_1.0.5            coro_1.1.0               
    ## [131] httr2_1.3.0               kknn_1.4.1               
    ## [133] giscoR_1.1.1              spacesXYZ_1.6-0          
    ## [135] DiagrammeRsvg_0.1         bayestestR_0.18.1        
    ## [137] otel_0.2.0                reshape2_1.4.5           
    ## [139] fracdiff_1.5-4            maptiles_0.12.0          
    ## [141] expm_1.0-0                colorspace_2.1-3         
    ## [143] data.table_1.18.4         withr_3.0.3              
    ## [145] marqLevAlg_2.0.8          mets_1.3.12              
    ## [147] cutpointr_1.2.1           tibble_3.3.1             
    ## [149] ggswim_0.1.0.9002         PRROC_1.4                
    ## [151] Deriv_4.3.0               statsExpressions_2.0.0   
    ## [153] xtable_1.8-8              plyr_1.8.9               
    ## [155] cmprsk_2.2-12             paradox_1.0.1            
    ## [157] lme4_2.0-6                MatrixModels_0.5-4       
    ## [159] UpSetR_1.4.1              systemfonts_1.3.2        
    ## [161] grafify_5.1.0             mlr3_1.7.1               
    ## [163] ggvenn_0.1.19             httpuv_1.6.17            
    ## [165] tableone_0.13.2           BiocGenerics_0.58.1      
    ## [167] rmarkdown_2.31            robustbase_0.99-7        
    ## [169] geepack_1.3.13            units_1.0-1              
    ## [171] officer_0.7.6             MASS_7.3-66              
    ## [173] flexsurv_2.3.2            clintools_0.9.10.1       
    ## [175] stringr_1.6.0             broom_1.0.13             
    ## [177] FactoMineR_2.16           sandwich_3.1-3           
    ## [179] jtools_2.3.1              logger_0.4.2             
    ## [181] vctrs_0.7.3               lifecycle_1.0.5          
    ## [183] readxl_1.5.0              eurostat_4.0.0           
    ## [185] ragg_1.5.2                proxy_0.4-29             
    ## [187] codetools_0.2-20          RefManageR_1.4.0         
    ## [189] DT_0.34.0                 mnormt_2.1.2             
    ## [191] recipes_1.3.3             jmvcore_2.7.38           
    ## [193] here_1.0.2                haven_2.5.5              
    ## [195] nlme_3.1-170              Seqinfo_1.2.0            
    ## [197] future_1.75.0             Biobase_2.72.0           
    ## [199] labelled_2.16.0           cellranger_1.1.0         
    ## [201] jquerylib_0.1.4           Rcpp_1.1.2               
    ## [203] rstudioapi_0.19.0         irr_0.85                 
    ## [205] patchwork_1.3.2           stringi_1.8.9            
    ## [207] hms_1.1.4                 minqa_1.2.8              
    ## [209] cachem_1.1.0              pROC_1.19.0.1            
    ## [211] ggmice_0.1.1              tcltk_4.6.0              
    ## [213] XVector_0.52.0            listenv_1.0.0            
    ## [215] ggrain_0.1.2              torchvision_0.9.0        
    ## [217] useful_1.2.7              pkgdown_2.2.1            
    ## [219] plotly_4.12.1             leafsync_0.1.0           
    ## [221] TidyDensity_1.5.2         mada_0.5.12              
    ## [223] palmerpenguins_0.1.1      etm_1.1.2                
    ## [225] htmlwidgets_1.6.4         Formula_1.2-6            
    ## [227] leaps_3.2                 matrixcalc_1.0-6         
    ## [229] DescTools_0.99.60         class_7.3-24             
    ## [231] memoise_2.0.1             r2rtf_1.3.1              
    ## [233] crayon_1.5.3              mlr3pipelines_0.11.0     
    ## [235] mice_3.19.0               gtsummary_2.5.1          
    ## [237] naivebayes_1.0.0          xml2_1.6.0               
    ## [239] Exact_3.3                 rpart.plot_3.1.4         
    ## [241] s2_1.1.11                 ggtext_0.1.2             
    ## [243] crosstable_0.9.0          png_0.1-9                
    ## [245] progressr_1.0.0           tzdb_0.5.0               
    ## [247] emmeans_2.0.4             pseudo_1.4.3             
    ## [249] fastmap_1.2.0             coda_0.19-4.1            
    ## [251] vcd_1.4-14                tidyr_1.3.2              
    ## [253] flashClust_1.1-4          deSolve_1.42             
    ## [255] tmap_4.4-1                urca_1.3-4               
    ## [257] pkgconfig_2.0.3           raster_3.6-32            
    ## [259] cli_3.6.6                 ggforce_0.5.0            
    ## [261] prodlim_2026.03.11        TMB_1.9.23               
    ## [263] httpcode_0.3.0            gld_2.6.8                
    ## [265] DataExplorer_0.9.0        ggsignif_0.6.4           
    ## [267] nnet_7.3-21               icenReg_2.0.16           
    ## [269] forecast_9.0.2            easyalluvial_0.4.1       
    ## [271] performance_0.17.1        lubridate_1.9.5          
    ## [273] zeallot_0.2.0             ggplot2_4.0.3            
    ## [275] ggalluvial_0.12.6         lmtest_0.9-40            
    ## [277] RcppArmadillo_15.4.2-1    textshaping_1.0.5        
    ## [279] multcompView_0.1-12       lmerTest_3.2-1           
    ## [281] gdtools_0.5.1             DEoptimR_1.2-0           
    ## [283] pander_0.6.6              MLEcens_0.1-7.1          
    ## [285] timechange_0.4.0          viridisLite_0.4.3        
    ## [287] tmaptools_3.3             blob_1.3.0               
    ## [289] foreign_0.8-91            timeDate_4052.112        
    ## [291] splines_4.6.0             mlr3viz_0.11.1           
    ## [293] askpass_1.2.1             timeROC_0.4.1            
    ## [295] annotate_1.90.0           XML_3.99-0.23            
    ## [297] numDeriv_2016.8-1.1       moments_0.14.1           
    ## [299] globals_0.19.1            perm_1.0-0.4             
    ## [301] knitr_1.51                crul_1.6.0               
    ## [303] stats4_4.6.0              broom.mixed_0.2.9.7      
    ## [305] compiler_4.6.0            sf_1.1-2                 
    ## [307] reformulas_0.4.4          janitor_2.2.1            
    ## [309] bibtex_0.5.2              RcppParallel_6.2.0       
    ## [311] bit_4.6.0                 extrafontdb_1.1          
    ## [313] grid_4.6.0                metafor_5.0-1            
    ## [315] ggpp_0.6.1                glue_1.8.1               
    ## [317] Icens_1.84.0              sp_2.2-3                 
    ## [319] estimability_2.0.0        ggstatsplot_1.0.0        
    ## [321] parameters_0.29.2         digest_0.6.39            
    ## [323] quadprog_1.5-8            irlba_2.3.7              
    ## [325] readr_2.2.0               showtextdb_3.0           
    ## [327] waffle_1.0.2              bdsmatrix_1.3-7          
    ## [329] tidyplots_0.4.0.9000      glmnet_5.0               
    ## [331] summarytools_1.1.5        magick_2.9.1             
    ## [333] benford.analysis_0.1.5    fontLiberation_0.1.0     
    ## [335] foreach_1.5.2             mlr3misc_0.22.0          
    ## [337] fontBitstreamVera_0.1.1   SparseM_1.84-2           
    ## [339] tweenr_2.0.3              lattice_0.22-9           
    ## [341] ModelMetrics_1.2.2.2      microbenchmark_1.5.0     
    ## [343] fontawesome_0.5.3         statmod_1.5.2            
    ## [345] openssl_2.4.2             igraph_2.3.3             
    ## [347] nloptr_2.2.1              mvtnorm_1.4-2            
    ## [349] yaml_2.3.12               later_1.4.8              
    ## [351] rstantools_2.7.0          backports_1.5.1          
    ## [353] rstatix_1.1.0             AnnotationDbi_1.74.0     
    ## [355] gtExtras_0.6.2            dcurves_0.5.1            
    ## [357] parallel_4.6.0            rematch2_2.1.2           
    ## [359] quantreg_6.1              fontquiver_0.2.1         
    ## [361] interval_1.1-1.0          miniUI_0.1.2             
    ## [363] gtable_0.3.6              abind_1.4-8              
    ## [365] xfun_0.60                 crosstalk_1.2.2          
    ## [367] Biostrings_2.80.1         curl_7.1.0               
    ## [369] callr_3.8.0               rootSolve_1.8.2.4        
    ## [371] epiR_2.0.96               doParallel_1.0.17        
    ## [373] arsenal_3.7.1             KernSmooth_2.23-26       
    ## [375] survival_3.8-9            leaflet_2.2.3            
    ## [377] survey_4.5                jsonlite_2.0.0           
    ## [379] magrittr_2.0.5            desc_1.4.3               
    ## [381] svglite_2.2.2             base64enc_0.1-6          
    ## [383] purrr_1.2.2               vtree_5.7.0              
    ## [385] iterators_1.0.14          TH.data_1.1-5            
    ## [387] mitml_0.4-5               exactRankTests_0.8-37    
    ## [389] matrixStats_1.5.0         Matrix_1.7-6             
    ## [391] regions_0.1.8             mitools_2.4              
    ## [393] ggfortify_0.4.19          cols4all_0.10            
    ## [395] distributional_0.8.1      ggdist_3.3.3             
    ## [397] gower_1.0.2               checkmate_2.3.4          
    ## [399] hardhat_1.4.3             MatrixGenerics_1.24.0    
    ## [401] htmlTable_2.5.0           randomForest_4.7-1.2     
    ## [403] wk_0.9.5                  S4Vectors_0.50.1         
    ## [405] mstate_0.3.3              IRanges_2.46.0           
    ## [407] dbscan_1.2.5              tinytable_0.17.0         
    ## [409] maxstat_0.7-26            rcrossref_1.2.1          
    ## [411] polynom_1.4-1             bit64_4.8.2              
    ## [413] cluster_2.1.8.3           Rdpack_2.6.6             
    ## [415] farver_2.1.2              zip_3.0.2
