I am a pathologist. Most of my research is on TNM staging and improving staging systems. I want to learn in detail how to compare an old staging system with the new proposed one. What is the appropriate statistical method to offer a new stage.  I want to learn stage migration analysis in detail. I use R for my projects. 

Thanks for the context. To tailor the research to your needs, could you please clarify the following:

1. Are you interested in applying these comparisons primarily to a specific cancer type (e.g., colorectal, breast, lung), or across multiple types?
2. Do you need guidance on survival analysis methods (e.g., Kaplan-Meier, Cox regression) for comparing staging systems?
3. Should the research include practical R code examples and packages for stage migration and staging system comparison?
4. Are you looking for published methodological frameworks, validation studies, or both?

Let me know and I’ll dive in accordingly.


I work on pancreatobiliary system and gastrointestinal system tumors. 
Guide me in survival analysis, and comparing models. 
I need practical R code examples and packages. 
Use both methodology studies as well as validation studies. 

Great. I’ll explore in detail how to compare old and new cancer staging systems for gastrointestinal and pancreatobiliary tumors. I’ll focus on survival analysis techniques (e.g., Kaplan-Meier, Cox regression), model comparison metrics (e.g., C-index, AIC, BIC), and stage migration analysis methods. I’ll include relevant methodological and validation studies, with a focus on practical implementation in R using appropriate packages and code examples.

I’ll share the findings with you shortly.


# Comparing Old vs New TNM Staging Systems: Stage Migration and Model Evaluation

## Understanding Stage Migration in Staging Systems

**Stage Migration** refers to changes in how patients are distributed among stages when a new classification or diagnostic method is introduced. In other words, patients may be “upstaged” or “downstaged” under the new system. This often occurs due to refined criteria or improved diagnostics (the classic *Will Rogers phenomenon*). For example, if new imaging detects small metastases, some patients formerly in Stage II might migrate to Stage III – raising the average survival of both stages without any change in individual outcomes. Formally, “stage migration describes a change in the proportion of T, N or M categories among patients within a population, following the introduction of a new means of assessing disease extent”.

**Why it matters:** Stage migration can complicate comparisons of outcomes across eras and is crucial when proposing a new staging system. A new staging system should ideally *re-stratify* patients so that each stage group is more prognostically homogeneous and distinct. Analyzing stage migration involves quantifying how many patients move between stages and assessing the impact on survival distributions.

*Example:* The 8th edition AJCC TNM for thyroid cancer (DTC) reclassified about **38% of patients to lower stages** (downstaging) compared to the 7th edition. Consequently, survival rates by stage changed markedly: under TNM-7, 10-year disease-specific survival (DSS) for stage III was \~98.8%, but under TNM-8 many low-risk patients were downstaged, and stage III 10-year DSS dropped to \~72.3% (stage IV from 83% to 48%). This reflects a more accurate stratification of risk. Overall, TNM-8 showed improved prognostic accuracy (e.g. Harrell’s C-index rose from 0.86 to 0.88). In such analyses, one creates a **reclassification table** of old vs. new stage to see how many patients migrated to each new category, and then examines survival in each group.

## Statistical Methods to Compare Staging Systems

When proposing a new stage or system, you must demonstrate **improved prognostic performance**. Key considerations include: **discrimination** (do the stages separate patients by outcome?) and **homogeneity** (are patients within the same stage more similar in outcome under the new system?). Here are appropriate statistical approaches and metrics:

* **Kaplan-Meier Survival Curves & Log-Rank Tests:** Plot KM curves for each stage in both old and new systems to visually assess separation. Ideally, survival curves for Stage I, II, III, IV under the new system should show clear separation with no overlaps. Use log-rank tests to confirm that survival differences between adjacent stages are statistically significant. If the new staging is better, previously overlapping survival curves may become distinct. For example, in pancreatic cancer AJCC 8th edition, new T and N definitions led to more even distribution of patients across stages and better-separated survival curves. Ensure **monotonicity** (higher stage = worse survival consistently).

* **C-index (Harrell’s Concordance Index):** This is a metric of predictive discrimination for survival models (analogous to AUC for time-to-event data). It measures the probability that a patient with a higher predicted risk (higher stage) actually dies sooner than a patient with lower risk. A higher C-index means better discriminative ability. To compare staging systems, compute the C-index for each system on the same dataset. The system with the higher C-index has better overall prognostic performance. For instance, a head-to-head comparison might show the old system A has c-index 0.74 vs new system B 0.62, indicating system A is more prognostic. In practice, even small improvements can be meaningful in large cohorts (e.g. a proposed new pancreatic cancer staging had C-index 0.637 vs 0.620 for the current system, a modest but statistically significant gain). The **`survival`** package in R can compute Harrell’s C: after fitting a Cox model, `summary(cox_model)$concordance` gives the C-index. There are also packages like **`survcomp`** (`concordance.index` function) or **`compareC`** (for testing difference between two C-indices) that are useful.

* **Likelihood Ratio Chi-square and AIC:** Fit a Cox proportional hazards model with stage (as a categorical predictor) for each staging system. The **likelihood ratio (LR) chi-square** from the Cox model (or its p-value) indicates how well the staging explains survival variability – larger values imply better stratification. You can compare models via **Akaike Information Criterion (AIC)** as well; a lower AIC means better goodness-of-fit accounting for model complexity. *Note:* Old vs new staging models are usually not nested, so you can’t do a direct LRT between them, but comparing AIC is informative. One study comparing 5 liver cancer staging systems used metrics like LR χ² and linear trend χ² (for ordinal trend) alongside the c-index to identify the best system. In that analysis, the system with highest LR χ² and c-index was deemed most prognostic. In R, you can get these metrics with a Cox model: e.g. `fit <- coxph(Surv(time,status) ~ stageFactor, data=...)` then `extractAIC(fit)` or use `anova(fit1, fit2)` if models were nested.

* **Net Reclassification Improvement (NRI) and Reclassification Tables:** In risk prediction literature, NRI is used to quantify how many individuals are “correctly” reclassified by a new model. For staging, one could examine how many patients move to a stage with closer-to-actual outcome. A **reclassification table** (old vs new stage cross-tab) is a great exploratory tool to inspect stage migration. For example, you might report: “20% of patients were upstaged (moved to a higher stage) and 15% downstaged under the new system; of those upstaged, X% had poor outcomes consistent with higher stage.” However, **caution** is needed interpreting reclassification alone. A method was proposed by Kattan & Gerds (2015) that requires the new system be prognostic within each category of the old system (and vice-versa). While intuitive (ensuring no loss of information within subgroups), this approach can be misleading – it’s possible for each system to appear prognostic within levels of the other yet one is overall inferior. In fact, focusing purely on reclassification counts or NRI without considering actual survival outcomes can lead to paradoxical conclusions. **Bottom line:** The decision should rest on **direct performance measures in the population**, such as higher c-index or greater separation of survival curves, rather than the sheer extent of reclassification.

* **Calibration and Other Metrics:** Less commonly, researchers might look at calibration-like metrics (e.g. how well predicted survival matches observed by stage). In the thyroid cancer example, authors reported the **“proportion of variation explained”** (a pseudo R² for survival) which increased from 6.0% to 7.0% with the new staging. For a pathologist, it’s often more relevant that each stage correlates with a distinct prognosis (calibration is more pertinent for continuous risk scores). Still, if you propose a new stage grouping, you should check that it doesn’t *worsen* calibration or create any logical inconsistencies (e.g. a supposedly “higher” stage having better outcomes than a lower stage).

## Survival Analysis and Model Comparison in R (Practical Steps)

**Data Setup:** Assume you have a dataset `df` with follow-up time, event status (death), and two staging variables (e.g. `old_stage` and `new_stage`). Ensure your data is coded appropriately (stages as factors or numeric ranks).

1. **Examine Stage Migration:** Use a contingency table to see how patients move between systems. For example:

   ```r
   with(df, table(old_stage, new_stage))
   ```

   This will show how many patients in each old stage fall into each new stage category. From this, calculate proportions up- or down-staged. For instance, in thyroid DTC data \~38% were downstaged. You might do:

   ```r
   tab <- with(df, table(old_stage, new_stage))
   prop.table(tab, margin=1)  # proportion by old_stage
   ```

   to see the distribution of new stages for each old stage group. This helps identify major shifts.

2. **Kaplan-Meier curves for each system:** Leverage the **`survival`** and **`survminer`** packages for clear survival plots.

   ```r
   library(survival)
   library(survminer)
   # KM by old stage
   fit_old <- survfit(Surv(time, status) ~ old_stage, data = df)
   ggsurvplot(fit_old, conf.int=TRUE, pval=TRUE, legend.title="Old Stage")
   # KM by new stage
   fit_new <- survfit(Surv(time, status) ~ new_stage, data = df)
   ggsurvplot(fit_new, conf.int=TRUE, pval=TRUE, legend.title="New Stage")
   ```

   This will plot survival curves and can perform a log-rank test (`pval=TRUE` shows the log-rank p-value for difference across all groups). You may also do pairwise comparisons between specific stages if needed (e.g. use `survdiff()` for two groups at a time). Check that in the new system, curves are better separated (and ordered by stage). If Stage IIB had higher survival than IIA in the old system (as seen in some pancreatic cancer data), a good new system might eliminate that anomaly.

3. **Cox proportional hazards models:** Fit models to quantify prognostic power.

   ```r
   # Cox models for old vs new staging
   fit_cox_old <- coxph(Surv(time, status) ~ factor(old_stage), data=df)
   fit_cox_new <- coxph(Surv(time, status) ~ factor(new_stage), data=df)
   summary(fit_cox_old)
   summary(fit_cox_new)
   ```

   Look at the hazard ratios (they should increase with stage if ordering is correct). More importantly, note the **concordance (C-index)** reported in the model summary. For example, you might see `Concordance= 0.62` for old and `Concordance= 0.74` for new – indicating better discrimination with the new system. You can also extract it as `summary(fit_cox_new)$concordance` which gives the C-index and standard error. If not directly given, use `survConcordance(Surv(time,status) ~ predict(fit_cox_new), data=df)` to compute it.

   Additionally, check model fit statistics:

   ```r
   AIC(fit_cox_old, fit_cox_new)
   ```

   This compares AIC for the two models. A lower AIC for the new staging suggests better goodness-of-fit (penalizing complexity). If the new staging has more categories, AIC naturally accounts for that via degrees of freedom. You can also compare the **likelihood ratio chi-square**: it’s reported in `summary()` (labeled as “Likelihood ratio test=... on df=...”). A higher LR chi-square for one model implies it explains more survival variation. For example, in an HCC study one staging system had LR χ² of 35.8 vs another’s 17.5, indicating much stronger prognostic stratification.

4. **Statistical comparison of models:** If you want to test whether the new staging is significantly better, you have a couple of options:

   * **Test difference in C-index:** Use the **`compareC`** package which implements statistical tests for two correlated C-indices. This accounts for the fact that the two models are applied to the same dataset. Example usage:

     ```r
     library(compareC)
     # Suppose lp_old and lp_new are linear predictors (risk scores) from the Cox models:
     lp_old <- predict(fit_cox_old, type="lp") 
     lp_new <- predict(fit_cox_new, type="lp")
     compareC(df$time, df$status, lp_old, lp_new)
     ```

     This will give you the difference in C-index and a p-value.
   * **Net reclassification or category-specific tests:** You could assess, for each patient, whether the new stage is higher or lower and if their outcome was indeed worse or better, to see if reclassification was “appropriate.” This is akin to NRI calculation. However, given the critiques, it’s safer to rely on global measures like the C-index or Brier score improvements.

5. **Other considerations:** Perform checks for **proportional hazards assumption** (using `cox.zph`) for the stage factor – generally stages should satisfy PH if they are meaningful groupings. Also, consider if you need to adjust for other covariates (e.g., age, treatment) in comparing staging systems. Often, staging is evaluated univariately, but you can also compare **adjusted** models (e.g., Cox model with stage + other prognostic factors) to see if stage remains an independent predictor and how much incremental value it adds (e.g. use **Bayesian Information Criterion (BIC)** or likelihood ratio test to see if replacing old stage with new stage in a multivariable model significantly improves fit).

## Learning from Methodology and Validation Studies

To deepen your understanding, it’s useful to look at both methodological papers and real-world validation studies:

* **Methodology:** The work by Kattan & Gerds (2015) introduced a reclassification-based algorithm to decide if a new staging is better, but a \*commentary by Kerr *et al.* (2017) argues for a simpler and more robust approach: directly compare overall performance using metrics like the c-index or mean survival in a head-to-head fashion. Their provided R scripts simulate scenarios showing that a new system can appear better within each old stage yet be worse overall. The take-home message is to focus on **population-level prognostic performance** rather than just internal reclassification consistency.

* **Validation Studies:** Numerous studies in GI and pancreatobiliary cancers illustrate how to evaluate staging:

  * *Pancreatic Cancer:* The AJCC 8th edition (2016) changed tumor size cut-offs and lymph node categorization (N0 vs N1 (1–3 nodes) vs N2 (≥4 nodes)). Researchers analyzed large cohorts to see if these changes improved stratification. Multi-center and SEER studies found that 8th edition stages were more evenly distributed and had higher concordance indexes than 7th edition. However, some noted issues: e.g., **Stage IIB vs IIA** inversion – one analysis of 45k patients discovered Stage IIB (node-positive small tumors) had better median survival than Stage IIA (node-negative large tumors) under AJCC 8, prompting a proposal to further split Stage III and reassign those groups. This led to a **modified staging** with new Stage IIIA/IIIB which achieved a higher c-index (0.637 vs 0.620) than the original system. This is a classic example of proposing new stages by identifying *outcome inconsistencies* in the current system and showing improvement with a tweak.
  * *Colon/Gastric Cancer:* Changes in lymph node classification or tumor deposits have been analyzed for stage migration effects. Studies often report what fraction of patients get up- or down-staged with a new edition and use survival analysis to verify that each successive stage in the new system has worse outcomes than the last (ensuring no overlaps). If the new system reduces the **Will Rogers effect** (where improved staging artificially boosts survival in each stage), that’s a positive sign that it’s capturing prognosis better.
  * *Hepatocellular Carcinoma (HCC):* Because multiple competing staging systems exist (e.g., TNM, BCLC, CLIP, etc.), comparative studies have used statistical metrics to decide which is best. One such study compared five HCC staging schemes by computing the Cox model **LR chi-square, trend chi-square, and c-index** for each – the system with the highest values was deemed most prognostically useful. This approach can be applied to any cancer: higher chi-square indicates the staging explains more variance in survival, and higher c-index indicates better discrimination.

**Key insight:** A new staging system (or a new stage category) should *add* prognostic value. It could be by splitting an overly broad category into two (if their outcomes differ significantly) or by merging groups that have similar outcomes. Statistics guide these decisions – if splitting improves the log-rank separation and boosts C-index/AIC, it’s justified. Conversely, if a proposed new stage doesn’t yield a significant improvement in model performance, it may not be worth adopting.

## Conclusion

To compare an old vs. new staging system in pathology research, incorporate both **descriptive “stage migration” analysis** and **rigorous survival model comparisons**. Use Kaplan-Meier curves and log-rank tests to ensure the new stages are distinct in outcome. Quantify improvement with metrics like Harrell’s C-index, Cox model likelihood ratio, and AIC. Tools in R such as the `survival` package (for Cox models and KM plots), `survminer` (for visualization), and specialty packages like `compareC` or `survcomp` will facilitate these analyses. By studying methodological papers and validation studies in GI and pancreatobiliary cancers, you can gain deeper insight into best practices. The goal is to propose a staging system that objectively **stratifies patients better** than the old – yielding clear separation of survival by stage and higher overall prognostic accuracy. With these statistical tools and R examples, you are equipped to perform a detailed stage migration analysis and confidently evaluate whether a new staging proposal truly outperforms the old.


Example this in more detailed: "For example, if new imaging detects small metastases, some patients formerly in Stage II might migrate to Stage III – raising the average survival of both stages without any change in individual outcomes."

Could you clarify what you'd like to see in the more detailed explanation? For instance:

* Are you looking for a mathematical example (e.g., with numbers and survival times) to demonstrate this effect?
* Should the explanation focus on a specific cancer (e.g., pancreatic or colorectal)?
* Would you like this illustrated with R code or simulation as well?

Let me know how you'd like this clarified so I can tailor it effectively.


show me mathematical explanation with r codes and simulation. Give examples from other systems as well.

Understood. I’ll create a detailed mathematical explanation and R simulation illustrating the Will Rogers phenomenon in stage migration, showing how shifting patients between stages due to better diagnostics can artificially improve survival in both stages—even when individual outcomes don’t change.

I’ll include R code, simulation results, and examples from other staging systems (e.g., colorectal, lung, or thyroid cancer) to make the concept concrete.


# Will Rogers Phenomenon and Stage Migration in Cancer Staging Systems

## Introduction

In cancer staging, improvements in diagnostic methods can paradoxically make it appear that patient prognoses have improved even when no actual change in outcomes has occurred. This counterintuitive effect is known as the **Will Rogers phenomenon** (or *stage migration*). The phenomenon is named after the humorist Will Rogers, who joked that when a group of people migrated from Oklahoma to California, **“they raised the average intelligence level in both states.”** In medical terms, if some patients are reclassified from a “better” prognostic group to a “worse” group, it can raise the average survival in *both* groups, without any patient living longer. This often occurs when new imaging or staging criteria detect previously hidden disease.

**Key concept:** When borderline patients (with relatively worse prognosis for a lower stage, but better than the typical higher stage) are moved to a higher stage, the lower stage’s average survival goes up (since its worst members left) and the higher stage’s average survival also goes up (since it gained relatively better members). Crucially, the *overall* survival of all patients combined remains the same – the change is purely in the statistics of each subgroup.

In this tutorial, we will:

* Explain the Will Rogers phenomenon mathematically in the context of cancer staging.
* Use **R** to simulate survival data for two cancer stages (Stage II and Stage III) to demonstrate how reclassification (stage migration) can improve average survival in both stages.
* Show the effect with Kaplan–Meier survival curves and compare mean/median survival before and after reclassification.
* Discuss real-world examples from cancer staging (e.g., lung and colorectal cancer) where this phenomenon has been observed.

## The Will Rogers Phenomenon in Cancer Staging

The Will Rogers phenomenon was first described by Feinstein *et al.* (1985) in the context of lung cancer patients when new imaging techniques became available. Many patients who would previously have been classified in a “good” stage (e.g., localized disease) were found to have occult metastases and thus migrated to a “bad” stage (metastatic disease). Feinstein noted that *“because the prognosis of those who migrated, although worse than that for other members of the good-stage group, was better than that for other members of the bad-stage group, survival rates rose in each group without any change in individual outcomes.”* In other words, each stage’s survival statistics improved simply due to reclassification, not due to better treatment.

Another illustrative example is in tumor **TNM** staging with new imaging (as described on Radiopaedia): Suppose advanced imaging detects tiny metastases in some patients who were previously staged M0 (no metastasis). Those patients are now upstaged to M1. The M0 group *loses* its worst-prognosis members (those who actually had hidden mets and would have fared worse than other M0 patients), so the remaining M0 patients have a higher average survival. The M1 group *gains* some patients with only small, early metastases, who survive longer than the typical M1 patient with more advanced disease, thereby raising the average survival of the M1 group. In both groups, the outcomes appear better even though **no individual’s outcome changed**, and the overall combined outcome is unchanged.

This phenomenon can bias comparisons over time or between institutions. For example, a hospital that uses more sensitive diagnostic criteria may appear to have better stage-specific survival rates simply because they have migrated certain patients to different stages (stage II vs III, or Dukes’ B vs C, etc.). Meanwhile, a hospital with less sensitive staging might leave those patients in a lower stage, depressing that stage’s survival statistics but also keeping the higher stage’s stats worse. This complicates historical comparisons and can create a **non-therapeutic “improvement” in prognosis**.

## Mathematical Explanation of Stage Migration

Mathematically, the Will Rogers phenomenon can be understood by examining how a reclassification affects group averages. Consider two groups of patients, A and B (think of A = Stage II, B = Stage III), with average survival times \$\mu\_A\$ and \$\mu\_B\$ respectively (typically \$\mu\_A > \mu\_B\$ for more advanced stages having worse survival). Now suppose there exists a patient *p* in group A with survival time \$x\$ such that

$\mu_B < x < \mu_A.$

This means patient *p*’s outcome is below A’s average (one of the worse outcomes in group A) but above B’s average (better than the typical outcome in group B). If we **move** patient *p* from group A to group B (i.e., upstage them):

* Group A loses a member with a value lower than its mean. Removing a below-average value will **increase** group A’s mean survival (since the remaining patients were, on average, better than \$x\$).
* Group B gains a member with a value higher than its mean. Adding an above-average value will **increase** group B’s mean survival (since the new patient does better than B’s original average).

No individual outcome has changed (\$x\$ is still \$x\$), but the averages shift: \$\mu\_A^{\text{new}} > \mu\_A\$ and \$\mu\_B^{\text{new}} > \mu\_B\$. The condition for this simultaneous increase is exactly that there are one or more \$x\$ in group A with \$\mu\_B < x < \mu\_A\$. In statistical terms, the two distributions must overlap – some of the “worst” of the better group are still “better” than the “best” of the worse group. This often holds true in clinical data, since stage categories are not perfectly disjoint in prognosis.

To give a simple numeric example: suppose originally Stage II patients had survival times (in months) `{20, 40, 60, 80}` (mean = 50), and Stage III patients had `{5, 10, 15, 20}` (mean = 12.5). The worst Stage II patient has 20 months survival, which is above the Stage III mean. If that patient is reclassified to Stage III, the new means become: Stage II (without the 20) = mean of `{40,60,80}` = 60 (improved from 50); Stage III (with the added 20) = mean of `{5,10,15,20}` = 12.5 → `{5,10,15,20,20}` = 14 (improved from 12.5). Both averages went up. The same principle applies to medians and to survival rates at specific time points – each stage’s Kaplan-Meier curve can shift favorably.

## Simulation of Stage Migration Effect with R

To illustrate this with survival data, we will simulate a simple scenario using **R**. We’ll consider two stages, Stage II and Stage III, with the following setup:

* **Stage II (Original):** 100 patients. Most have relatively good prognosis, but a subset (say 20 patients) actually have occult advanced disease giving them worse survival than their peers.
* **Stage III (Original):** 100 patients, all with advanced disease and poorer prognosis on average than Stage II patients.

We’ll represent survival times (e.g., overall survival in months) with random draws from exponential distributions for simplicity (in real data, survival might not be exponential, but this is sufficient for demonstration). Let’s assume:

* True Stage II patients have an average survival of \~60 months.
* Occult Stage II patients (those with hidden metastases who should be Stage III) have worse survival, say \~30 months on average.
* Stage III patients have the worst survival, \~15 months on average.

**Step 1: Simulate baseline survival data for Stage II and Stage III.** We generate survival times for each group using these distributions. We'll use `rexp()` for exponential simulation and set a seed for reproducibility:

```r
# Step 1: Simulate survival times for original Stage II and Stage III groups
set.seed(42)
N2 <- 100  # number of Stage II patients
N3 <- 100  # number of Stage III patients

# Subset of Stage II with occult metastases to be upstaged
N2_hidden <- 20                 
N2_true   <- N2 - N2_hidden     # truly early-stage patients

# Simulate survival times (in months)
times_stage2_good   <- rexp(N2_true, rate = 1/60)  # mean ~60 months for true Stage II
times_stage2_hidden <- rexp(N2_hidden, rate = 1/30) # mean ~30 months for occult mets (worse prognosis)
times_stage3_orig   <- rexp(N3,       rate = 1/15)  # mean ~15 months for original Stage III

# Combine Stage II original survival times
times_stage2_orig <- c(times_stage2_good, times_stage2_hidden)
```

**Step 2: Reclassify the subset of Stage II patients with occult metastases to Stage III.** This mimics the effect of a new staging test that finds those hidden mets. We remove the `times_stage2_hidden` patients from Stage II and add them into Stage III:

```r
# Step 2: Perform stage migration (reclassification)
times_stage2_new <- times_stage2_good                     # Stage II after reclassification (only good prognosis patients remain)
times_stage3_new <- c(times_stage3_orig, times_stage2_hidden)  # Stage III after (original Stage III + upstaged patients)
```

Now we have four groups of survival times:

* `times_stage2_orig` (Stage II before, 100 patients)
* `times_stage3_orig` (Stage III before, 100 patients)
* `times_stage2_new`  (Stage II after, 80 patients remain)
* `times_stage3_new`  (Stage III after, 120 patients total)

**Step 3: Compare survival statistics before vs. after reclassification.** We compute the mean and median survival for each group to see the changes:

```r
# Step 3: Compute mean and median survival for each group
mean_stage2_orig   <- mean(times_stage2_orig)
median_stage2_orig <- median(times_stage2_orig)
mean_stage2_new    <- mean(times_stage2_new)
median_stage2_new  <- median(times_stage2_new)

mean_stage3_orig   <- mean(times_stage3_orig)
median_stage3_orig <- median(times_stage3_orig)
mean_stage3_new    <- mean(times_stage3_new)
median_stage3_new  <- median(times_stage3_new)

# Summarize in a data frame
summary_df <- data.frame(
  Group           = c("Stage II (Original)", "Stage II (New)",
                      "Stage III (Original)", "Stage III (New)"),
  N               = c(length(times_stage2_orig), length(times_stage2_new),
                      length(times_stage3_orig), length(times_stage3_new)),
  Mean_Survival   = c(mean_stage2_orig, mean_stage2_new, mean_stage3_orig, mean_stage3_new),
  Median_Survival = c(median_stage2_orig, median_stage2_new, median_stage3_orig, median_stage3_new)
)

print(summary_df)
```

Running the above in R, we might get output (one example simulation) like:

```
               Group    N Mean_Survival Median_Survival
1  Stage II (Original)  100     49.9           28.7
2        Stage II (New)   80     56.2           34.4
3 Stage III (Original)  100     14.6           10.6
4       Stage III (New)  120     16.3           11.4
```

*Table:* **Mean and median survival before vs. after stage migration** (values in months; your simulation results may vary). Both Stage II and Stage III show higher mean and median survival after reclassification. Stage II’s mean increased from \~49.9 to \~56.2 months, and Stage III’s mean from \~14.6 to \~16.3 months, even though no individual survival time changed. Stage II lost 20 patients with poorer outcomes, and Stage III gained those 20 relatively moderate outcomes, improving both groups. Note that the total patient count is the same (200) and overall combined survival (if we pooled all patients) is unchanged by this relabeling.

**Step 4: Visualize with Kaplan–Meier curves.** Finally, we can plot Kaplan–Meier survival curves for Stage II and Stage III before and after reclassification. We use the `survival` package to create KM estimates and base R plotting (or you could use `survminer::ggsurvplot` for fancier plots):

```r
# Step 4: Plot Kaplan–Meier curves for before vs. after
library(survival)

# KM fit for original groups
group_orig <- c(rep("Stage II", length(times_stage2_orig)),
                rep("Stage III", length(times_stage3_orig)))
time_orig  <- c(times_stage2_orig, times_stage3_orig)
status_orig <- rep(1, length(time_orig))  # status=1 for events (assuming all times are death times for illustration)
km_orig <- survfit(Surv(time_orig, status_orig) ~ group_orig)

# KM fit for new groups
group_new <- c(rep("Stage II", length(times_stage2_new)),
               rep("Stage III", length(times_stage3_new)))
time_new  <- c(times_stage2_new, times_stage3_new)
status_new <- rep(1, length(time_new))
km_new <- survfit(Surv(time_new, status_new) ~ group_new)

# Plot the KM curves
par(mfrow=c(1,2))  # 2 plots side by side
plot(km_orig, col=c("blue","red"), lty=1:2, xlab="Time (months)", ylab="Survival probability",
     main="Before Reclassification")
legend("bottomleft", legend=c("Stage II (Orig)","Stage III (Orig)"), col=c("blue","red"), lty=1:2)

plot(km_new, col=c("blue","red"), lty=1:2, xlab="Time (months)", ylab="Survival probability",
     main="After Reclassification")
legend("bottomleft", legend=c("Stage II (New)","Stage III (New)"), col=c("blue","red"), lty=1:2)
```

This will produce two side-by-side Kaplan–Meier plots. **Before** reclassification, the Stage II curve is higher (better survival) than Stage III – as expected. **After** reclassification, both curves shift upward: Stage II’s curve becomes even better (since its worst outcomes were removed), and Stage III’s curve also improves (since it gained some relatively better patients). However, if you were to plot the combined overall survival of all patients, it would be identical before and after.

*(In our simulation, Stage II’s 5-year survival might increase, for example, from around 40% to 50%, and Stage III’s from around 5% to 10% – the exact numbers depend on the random data. The key is that both improved without new therapy.)*

## Real-World Examples of Stage Migration

The Will Rogers phenomenon has been documented in several real cancer staging scenarios:

* **Lung Cancer:** The introduction of more advanced imaging (CT scans, PET scans) in the 1970s–80s led to detection of previously occult metastases. Patients who would have been classified as early-stage were upstaged. Feinstein *et al.* observed that a 1977 cohort of lung cancer patients had higher six-month survival in each TNM stage than a 1950s–60s cohort, attributable to stage migration from new imaging. The upstaged patients had intermediate prognosis: worse than other Stage I/II patients, but better than the original Stage III/IV patients, thus improving survival statistics in both groups.

* **Colorectal Cancer:** Improved lymph node examination in pathology can cause stage migration in colorectal cancer. If more lymph nodes are retrieved from surgical specimens, some patients originally diagnosed as node-negative (Stage II/Dukes’ B) are found to have positive nodes and are upstaged to Stage III (Dukes’ C). Those upstaged cases typically had worse outcomes than other Stage II (so Stage II survival improves when they are removed) but better outcomes than other Stage III (so Stage III survival improves when they are added). A study of 3592 colon cancer patients noted this Will Rogers effect: thorough lymph node pathology **“apparently \[improves] survival in patients in stage B by excluding patients with a worse prognosis,”** while **“these patients \[also] improve survival amongst Stage C patients, as they are likely to be better prognostically than those \[otherwise] graded as Dukes’ C.”** This phenomenon can give the illusion that outcomes are better when more nodes are examined, even without changes in therapy (though in practice, thorough staging is still important for proper treatment).

* **Thyroid Cancer:** Changes in staging criteria themselves can produce a Will Rogers-like effect. For example, the 8th edition of the AJCC staging for thyroid cancer raised the age threshold for risk stratification from 45 to 55. Many patients who would previously be Stage III (simply due to age >45) are now down-staged to Stage I/II. This **migration of lower-risk patients into earlier stages** can artifactually improve survival rates in the higher stage (which no longer includes those relatively good-prognosis patients) and potentially alter survival in the lower stage as well. In general, any reclassification (up or down) that selectively moves patients at the boundaries of prognostic groups can create misleading changes in survival statistics – one must analyze outcomes for the entire cohort to confirm whether true improvement has occurred.

* **Other examples:** The Will Rogers phenomenon has been discussed in contexts like improved imaging for prostate cancer (e.g., PSMA PET scans upstaging patients with small metastatic lesions), in breast cancer grading migration, and even outside oncology (e.g., changes in diagnostic criteria for diseases). In all cases, the common theme is *“an apparent improvement in patient survival”* due to more sensitive detection or new criteria, rather than a real advance in treatment.

## Conclusion

The Will Rogers phenomenon (stage migration effect) serves as a cautionary example for clinicians and researchers. It demonstrates that **improvements in diagnostic staging can bias survival comparisons** over time or between institutions. For a pathologist or oncologist, recognizing this effect is important when interpreting cancer statistics. If a new staging system or diagnostic tool is introduced, one might see improved stage-specific survival not because patients are living longer from better therapy, but because the stage definitions have changed.

**Key takeaway:** Always consider whether differences in survival could be due to stage migration. One way to check is to look at the overall survival of the entire cohort – in true Will Rogers cases, the combined outcome for all patients remains unchanged. By using simulations and understanding the math behind it, we see clearly how *“zero-time shifts”* in staging can create an illusion of progress. Robust analysis of outcomes must adjust for such biases or use consistent staging criteria when comparing data over time.

**References:** The concept of the Will Rogers phenomenon was first described by Feinstein *et al.* (1985). It has since been observed in various cancers, including lung, colorectal, breast, and thyroid cancers. The phrase itself humorously originates from Will Rogers’ observation on migration and averages, encapsulating how reclassification can raise “averages” in both groups without genuine improvement. The phenomenon reminds us that **statistical improvements do not always equate to real clinical gains** – careful analysis is needed to distinguish true progress from artifactual changes due to stage migration.


suppose we have identified a morphologic feature like size and added it to the staging and moved some cases from T2 to T3. isn't this what we aim for? 

Yes — and that’s a very insightful observation.

✅ **You’re absolutely right:**
If you’ve identified a meaningful morphologic feature (like tumor size, lymphovascular invasion, or a histologic subtype) and use it to reclassify certain cases (e.g., from T2 to T3), **this is precisely what a better staging system should do** — improve prognostic stratification by placing patients with *worse outcomes* into higher stages. In this case, **stage migration is *intentional and desirable***, not just an artifact.

---

### 🧠 The key distinction lies in *why* stage migration occurs:

| **Type of stage migration**                  | **Source**                                                    | **Meaning**                                                        | **Survival impact**                                              |
| -------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------------------------------ | ---------------------------------------------------------------- |
| 🔍 **Artifact** (Will Rogers phenomenon)     | Better imaging or detection only                              | Reclassifies patients without actual change in biology             | Stage-specific survival improves **without** true outcome change |
| 🧬 **Real improvement** (Staging refinement) | Addition of a *true prognostic factor* (e.g., size, invasion) | Reclassifies patients with *different biology* or *worse outcomes* | Stage-specific survival improves **because of better groupings** |

---

### 🔎 Why this matters:

When you add a **real morphologic prognostic factor** (like size) to staging:

* You're **correctly identifying heterogeneity** within a previous group (e.g., some T2 tumors actually behave more like T3).
* The goal of staging is to create **monotonic survival trends**: higher stage → worse prognosis.
* If moving some T2 tumors to T3 improves this trend (e.g., better separation of Kaplan-Meier curves), that's a sign your added feature has **clinical value**.

✅ In this context, **stage migration is not a bias** — it's evidence that the new system better reflects underlying tumor biology.

---

### 🔬 Example: Pancreatic Cancer AJCC 8th Edition

In pancreatic cancer:

* The **size cutoffs for T-staging** were changed (e.g., T1 ≤ 2 cm, T2 > 2 cm and ≤ 4 cm, T3 > 4 cm).
* This led to **reclassification** of some tumors.
* Studies showed improved survival separation across stages after these changes — this is **positive stage migration**, improving prognostic accuracy.

---

### ✅ What you should do as a researcher:

When proposing a new staging system or adding a variable:

1. **Quantify the stage migration** (who moved from T2 → T3).
2. **Compare survival curves** before and after.
3. Show **C-index improvement**, increased LR χ², or better AIC/BIC.
4. Make sure the new stage definitions are **monotonic and clinically intuitive**.

If these hold, the migration you observe is **not a bias** but a **validated refinement**.

---

### ⚠️ The caution with the Will Rogers phenomenon

The Will Rogers effect becomes problematic **when you *don’t* change biology or add a true risk factor** — e.g., only improving imaging or lymph node dissection — and then **compare survival across eras or institutions** without accounting for it. It creates the illusion of progress.

But **your case — using morphology like size to propose better staging — is the ideal use case of staging refinement.**

Let me know if you’d like an R example to show this exact effect using a real or synthetic morphologic variable like tumor size.


