#' @importFrom jmvcore .
#' @importFrom R6 R6Class
NULL

agreementintroClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "agreementintroClass",
  inherit = agreementintroBase,
  private = list(
    .init = function() {},

    .escape = function(x) {
      x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x <- gsub('"', "&quot;", x, fixed = TRUE)
      gsub("'", "&apos;", x, fixed = TRUE)
    },

    .panel = function(text) {
      # An empty string is treated as absent HTML by the native client, which
      # can leave its previous iframe content visible after invalidation.
      if (!length(text)) return("<div></div>")
      paste0('<div style="max-width: 850px; line-height: 1.5; color: inherit;">',
        paste0("<p>", private$.escape(text), "</p>", collapse = ""), "</div>")
    },

    .run = function() {
      words <- private$.messages()
      tr <- function(key) {
        value <- words[[key]]
        if (is.null(value)) stop(paste("Missing agreement guide message:", key))
        value
      }
      o <- lapply(names(.agreementIntroDefaults()), function(n) self$options[[n]])
      names(o) <- names(.agreementIntroDefaults())
      profile <- .agreementIntroProfile(if (o$inspectData) self$data else NULL, o)
      decision <- .agreementIntroRecommend(o, profile)
      catalogue <- .agreementIntroCatalog()
      for (name in c("recommendations", "settings", "dataSummary", "columnSummary", "catalogue")) {
        table <- self$results[[name]]
        table$deleteRows()
        # jmvcore 2.7 keeps dynamic row names after deleteRows(). Clear that cache
        # as well, including when a previously recommended route becomes unknown.
        if (length(table$names)) table$.__enclos_env__$private$.rowNames <- character()
      }
      for (name in c("overview", "status", "preparation", "inspectionNotes", "examples",
                     "pitfalls", "glossary", "catalogueNotes"))
        self$results[[name]]$setContent(private$.panel(character()))
      for (name in c("recommendations", "settings")) self$results[[name]]$setVisible(
        self$options$showRecommendation && length(decision$recommendations) > 0L)
      self$results$dataSummary$setVisible(o$inspectData && length(profile$facts) > 0L)
      self$results$columnSummary$setVisible(o$inspectData && length(profile$columns) > 0L)
      if (self$options$showOverview)
        self$results$overview$setContent(private$.panel(c(tr("overview"), tr("workflow"))))
      if (self$options$showRecommendation) {
        status <- c(tr(decision$status), paste(tr("decision_path"), decision$path),
          vapply(decision$messages, tr, character(1)))
        self$results$status$setContent(private$.panel(status))
        preparation <- vapply(unique(decision$notes), tr, character(1))
        for (i in seq_along(decision$recommendations)) {
          r <- decision$recommendations[[i]]; c <- catalogue[[r$target]]
          self$results$recommendations$addRow(rowKey = paste0("route_", i), values = list(
            priority = tr(if (i == 1L) "primary" else "alternative"),
            analysis = r$target, method = tr(r$method)))
          for (name in names(r$settings)) {
            value <- r$settings[[name]]
            # Quote strings as R literals; this is displayed text, never evaluated.
            printed <- if (is.character(value)) encodeString(value, quote = '"') else
              as.character(value)
            self$results$settings$addRow(rowKey = paste0("setting_", i, "_", name),
              values = list(analysis = r$target, option = name,
                value = paste(printed, collapse = ", ")))
          }
          preparation <- c(preparation,
            paste(r$target, tr(r$reason), sep = ": "),
            paste(r$target, paste(c$group, c$subgroup, c$title, sep = " > "), sep = ": "),
            paste(r$target, tr(c$state), sep = ": "), tr(r$preparation))
        }
        if (length(decision$recommendations))
          preparation <- c(preparation, tr("settings_scope"), tr("catalogue_scope"))
        self$results$preparation$setContent(private$.panel(unique(preparation)))
      }
      if (o$inspectData) {
        fact_names <- c(rows = "fact_rows", raters = "fact_raters", distinctRaters = "fact_distinct_raters",
          cases = "fact_cases",
          patients = "fact_patients", missing = "fact_missing", complete = "fact_complete",
          overlap = "fact_overlap", duplicateKeys = "fact_duplicates", conditions = "fact_conditions")
        for (n in intersect(names(fact_names), names(profile$facts)))
          self$results$dataSummary$addRow(rowKey = n,
            values = list(feature = tr(fact_names[[n]]), value = as.character(profile$facts[[n]])))
        declared_levels <- character()
        for (i in seq_along(profile$columns)) {
          x <- profile$columns[[i]]
          levels <- paste(utils::head(x$declared, 30L), collapse = "; ")
          if (length(x$declared) > 30L) levels <- paste(levels, tr("levels_truncated"))
          if (nzchar(levels)) declared_levels <- c(declared_levels,
            paste(names(profile$columns)[i], tr("declared_levels"), levels, sep = ": "))
          self$results$columnSummary$addRow(rowKey = paste0("column_", i), values = list(
            variable = names(profile$columns)[i], type = tr(x$type), observed = x$observed,
            missing = x$missing, categories = x$categories))
        }
        suggestions <- character()
        if (length(profile$facts$raters)) suggestions <- c(suggestions,
          jmvcore::format(tr("suggest_raters"), list(n = profile$facts$raters)))
        if (isTRUE(profile$facts$incomplete)) suggestions <- c(suggestions, tr("suggest_missing"))
        if (isTRUE(profile$facts$clustered)) suggestions <- c(suggestions, tr("suggest_cluster"))
        self$results$inspectionNotes$setContent(private$.panel(c(tr("inspection_scope"), suggestions,
          declared_levels,
          vapply(unique(c(profile$issues, profile$notes)), tr, character(1)))))
      }
      for (section in c("examples", "pitfalls", "glossary")) {
        option <- switch(section, examples = "showExamples", pitfalls = "showPitfalls",
          glossary = "showGlossary")
        if (self$options[[option]]) self$results[[section]]$setContent(private$.panel(
          vapply(paste0(section, seq_len(3L)), tr, character(1))))
      }
      if (self$options$showCatalogue) {
        menu_locations <- character()
        for (name in names(catalogue)) {
          c <- catalogue[[name]]
          self$results$catalogue$addRow(rowKey = name, values = list(analysis = name,
            version = c$version, development = tr(paste0(c$state, "_short"))))
          menu_locations <- c(menu_locations,
            paste(name, paste(c$group, c$subgroup, c$title, sep = " > "), sep = ": "))
        }
        self$results$catalogueNotes$setContent(private$.panel(c(tr("catalogue_scope"), menu_locations)))
      }
    },

    # Keep complete, extractable sentences inside the R6 translation context.
    .messages = function() list(
      overview = .("Choose an agreement method from the question you want to answer, the meaning of the scores, and the study design. This guide describes analyses; it does not calculate agreement statistics or open another analysis."),
      workflow = .("Start with Study Question, complete the relevant Design Details, and optionally inspect selected columns. Read the recommendation together with its preparation steps and development status. No dataset is needed for questionnaire-only guidance."),
      needs_information = .("More information is needed before recommending a configuration."),
      unsupported = .("This design needs a workflow that this guide cannot currently recommend with verified settings."),
      data_conflict = .("Resolve the data check or the conflicting design answers before using a recommendation."),
      recommended = .("A method configuration matches the declared study design."),
      recommendation_scope = .("The suggestion is conditional on your confirmed answers. It is not evidence that your study or the suggested software has been clinically validated."),
      decision_path = .("Decision path:"),
      declared_levels = .("Declared category order"),
      testing_short = .("Testing"),
      draft_short = .("Draft"),
      pending_short = .("Pending"),
      production_short = .("Production menu"),
      primary = .("Primary"), alternative = .("Alternative"),
      testing = .("Testing route; not a production release"),
      pending = .("Pending implementation; not a production release"),
      draft = .("Draft; not a production release"),
      production = .("Production menu route; this guide does not certify release status"),
      settings_scope = .("The settings table contains option names and values for the named analysis. Assign the required data columns in that analysis as described below. Unlisted options retain their defaults; inspect its notices and denominators before interpreting results."),
      catalogue_scope = .("Menu paths and versions describe this source catalogue, not modules installed on another computer. Draft and pending analyses may be absent from distributed modules. No analysis is launched automatically."),
      need_purpose = .("Select your study question."),
      need_scale = .("Confirm whether the ratings are binary, nominal, ordinal, continuous, or ranks."),
      need_nRaters = .("Enter at least two raters, methods, or foci. Zero means not specified."),
      need_panel = .("Specify whether the same identified raters rate every case, raters are exchangeable, or the panel is incomplete."),
      need_missingRatings = .("Specify whether any ratings are missing."),
      need_repeatedReadings = .("Specify whether the same rater reads the same case more than once."),
      need_analysisUnit = .("Specify independent cases or multiple specimens or visits per patient."),
      need_categoryOrder = .("Establish one complete category order, including unused categories, and declare it identically for every rater before using ordinal methods."),
      need_weights = .("Choose unweighted, linear, or quadratic disagreement weights for ordered categories."),
      need_raterSelection = .("Specify whether reliability should generalize to comparable raters or only to this fixed panel."),
      need_iccDefinition = .("Choose absolute agreement or consistency; consistency permits systematic shifts between raters."),
      need_reportingUnit = .("Specify reliability of one measurement or of the average of the selected measurements."),
      need_referenceType = .("Identify the reference as a disease standard, expert consensus, or comparison method. Agreement-only cutoff studies may select no reference."),
      need_comparisonGoal = .("Specify overall panel agreement, fixed-reader modality comparison, or the specialist two-modality workflow."),
      need_planningGoal = .("Choose interval precision, hypothesis-test power, or a lower bound with fixed sample size."),
      need_nCategories = .("Enter the number of categories for study planning."),
      need_reader_labels = .("Enter distinct, exact positive and negative labels for the reader calls."),
      need_reference_labels = .("Enter distinct, exact positive and negative reference labels."),
      need_cutoffPrespecified = .("Choose the clinical cutoff before inspecting performance, then confirm that it was prespecified."),
      need_cutoff = .("Enter the prespecified numeric cutoff or ordinal boundary category."),
      need_cutoffDirection = .("Select which side of the numeric cutoff is a positive call, including whether equality is positive."),
      need_numeric_cutoff = .("The numeric cutoff must be one finite number."),
      planning_limits = .("The kappaSize routes cover unweighted categorical kappa with 2 to 5 categories and 2 to 6 raters on independent cases without repeated readings. Binary planning requires exactly two categories. Weighted-kappa, ICC, clustered, and repeated-reading planning need a separate design."),
      planning_model = .("The calculators assume an intraclass, common-correlation kappa model with common category probabilities. Their sample sizes are not automatically valid for unrestricted Cohen or fixed-panel Conger designs; confirm the model and anticipated margins in the selected calculator."),
      unsupported_cluster = .("Ordinary kappa, ICC, and Bland-Altman settings do not adjust uncertainty for multiple specimens or visits per patient. Use a supported reader cluster workflow or obtain a design-specific analysis; do not treat rows as independent patients."),
      unsupported_repeated = .("Do not pool repeated readings as independent raters. Select the paired-condition workflow when appropriate, or obtain a design-specific repeatability analysis."),
      categorical_scale = .("Categorical kappa needs categories or grades. Use measurement agreement or reliability for continuous measurements."),
      measurement_pair = .("This measurement-agreement route requires exactly two continuous methods measured on the same cases."),
      icc_continuous = .("This ICC route is for continuous measurements. Numerical category codes alone do not establish a continuous measurement scale."),
      icc_incomplete = .("The recommended ICC implementations use complete rectangular ratings. Incomplete panels need an appropriate model; simply deleting missing cases may change the target population."),
      icc_oneway = .("A one-way ICC requires different exchangeable random raters across cases and equal numbers of ratings. A fixed-panel or consistency-specific interpretation is not supported by this one-way route."),
      icc_label_limit = .("Fixed-rater absolute agreement and random-rater consistency are valid ICC designs, but the current target labels do not unambiguously express these combinations. No exact configuration is recommended until that interface is verified."),
      icc_oneway_definition = .("One-way ICC does not separate rater effects. The software uses its one-way calculation even though the API type value is consistency; report one-way random absolute agreement and the single or average measurement unit."),
      icc_report = .("Report the ICC model, absolute-agreement or consistency definition, single or average measurement unit, confidence interval, and intended rater population. A fixed-rater consistency estimate must not be described as absolute agreement."),
      reader_scale = .("Reader workflows here support binary calls, an ordered boundary, or a prespecified numeric cutoff."),
      reader_repeated = .("Repeated reads within the same reader and condition must not be treated as independent cases. Select one prespecified occasion or use an explicitly matched modality design."),
      reader_fixed = .("This reader workflow conditions on identified readers. Different exchangeable readers across cases need a design-specific analysis rather than fixed-reader shared-case comparisons."),
      reader_id_limit = .("The guide preserves these original IDs, but the target would merge distinct numeric IDs when formatting them as text. Prepare lossless text identifiers from the source before proceeding; ordinary numeric-to-text conversion is not sufficient."),
      reader_cluster = .("Assign Patient / cluster ID to readerCluster in pathagreement and retain paired readings within each resampled cluster. These bootstrap intervals are exploratory; the analysis unit is the case or specimen, with clustered uncertainty."),
      reader_modality = .("Use long data with caseID, readerID, modality and rating. Choose baselineModality and comparisonModality explicitly; each case-reader-modality combination must be unique. This comparison conditions on the selected readers and does not generalize to a population of readers."),
      reference_not_truth = .("Agreement with expert consensus or another method is not disease sensitivity or specificity. Keep the comparator description and classification agreement labels in the report."),
      modality_pair = .("The specialist modality route requires exactly two categorical or ordinal modalities."),
      modality_order = .("The modalitycomparison draft sorts custom categories alphabetically and has no explicit order setting. Its ordinal direction and weighted outputs are not a verified route. Use the categorical agreement route with a confirmed order, or review a supported specialist preset separately."),
      paired_complete = .("The paired agreement configuration requires the same panel and complete matched cases in both conditions, with a confirmed common ordinal scale when applicable."),
      paired_scale = .("The current paired agreement tool compares categorical kappa and exact matches. It does not compare continuous ICC or CCC estimates. A paired continuous agreement comparison needs a separate verified workflow."),
      paired_multi = .("For three or more raters, the current paired tool compares unweighted Fleiss coefficients even for a fixed panel. It cannot carry the recommended Conger or weighted ordinal estimand through the comparison."),
      paired_occasion = .("Multiple reading occasions within a condition require an explicit selection or a repeated-measures model. The recommended paired workflow accepts one reading per case, reader and condition."),
      rank_complete = .("Kendall's W here requires complete comparable rankings or ordered ratings of the same cases."),
      bias_pair = .("The marginal-homogeneity route requires two categorical raters on the same cases."),
      foci_categorical = .("The multifocal route requires categorical biomarker or mutation calls and explicit patient and focus identities."),
      foci_not_clonality = .("Concordant biomarker calls alone do not establish tumour clonality. The candidate settings leave the case-level clonality summary disabled."),
      unsupported_design = .("No supported configuration matches this combination of answers."),
      category_specific = .("For binary or imbalanced categories, inspect category frequencies and consider category-specific agreement and Gwet's AC as complementary summaries. They answer different questions and do not automatically replace kappa."),
      cohen = .("Cohen's kappa"), weighted_cohen = .("Weighted Cohen's kappa"),
      cohen_specialist = .("Specialist Cohen's kappa interface"),
      alpha = .("Krippendorff's alpha"), conger = .("Conger's exact kappa"),
      fleiss = .("Fleiss' kappa"), gwet = .("Gwet's AC2"),
      bland_ccc = .("Bland-Altman limits and Lin's CCC"), platform = .("Continuous platform comparison"),
      icc = .("Intraclass correlation coefficient"), icc_specialist = .("Specialist ICC interface"),
      reader = .("Reader accuracy and cutoff disagreement"), paired = .("Paired agreement comparison"),
      modality = .("Specialist categorical modality comparison"), kendall = .("Kendall's W"),
      marginal = .("Stuart-Maxwell marginal homogeneity"), foci = .("Multifocal categorical concordance"),
      plan_precision = .("Kappa interval precision planning"), plan_power = .("Kappa hypothesis-test power planning"),
      plan_fixed = .("Kappa lower bound at fixed sample size"),
      why_cohen = .("Two identified raters score the same independent cases. Ordinal weighting requires a common ordered scale and a prespecified disagreement cost."),
      why_alpha = .("Alpha can use incomplete rating panels. Cases with fewer than two observed ratings provide no pairwise agreement information; missingness can still bias the study."),
      why_ordinal_multi = .("Ordinal alpha respects the ordered scale across several raters. It is not a weighted version of the displayed unweighted Fleiss or Conger coefficient."),
      why_gwet = .("AC2 is a complementary weighted agreement coefficient with a different chance-agreement model."),
      why_conger = .("The same three or more identified raters assess every case; Conger's exact formulation retains rater-specific marginal distributions."),
      why_fleiss = .("The design has equal numbers of exchangeable ratings per case. Fleiss' coefficient uses pooled category frequencies rather than fixed rater-specific margins."),
      why_measurement = .("Limits of agreement describe differences in measurement units; CCC summarizes concordance. Correlation alone does not demonstrate agreement."),
      why_icc = .("The declared rater design, agreement definition, and measurement unit match this ICC configuration."),
      why_reader = .("This workflow separates reproducibility from reference-based accuracy and makes class labels, cutoffs, case populations, and clustering explicit."),
      why_paired = .("The same cases and matched raters are observed in both conditions; the paired resampling compares their agreement coefficients."),
      why_modality = .("This specialist interface describes categorical modality discordance. Its draft status requires separate verification before use."),
      why_rank = .("Kendall's W summarizes similarity of rankings, not equality of measurement values."),
      why_bias = .("Marginal homogeneity asks whether raters use categories with the same distribution; it is not an agreement coefficient."),
      why_foci = .("Patient and focus identifiers define within-patient biomarker concordance. This draft recipe uses descriptive concordance. Kappa and gain/loss interpretations need consistent focus roles across cases, which the questionnaire has not established."),
      why_plan_precision = .("The objective is an interval around anticipated kappa with a specified precision, not hypothesis-test power."),
      why_plan_power = .("The objective is power to reject a specified null kappa in favour of an anticipated alternative."),
      why_plan_fixed = .("The number of independent cases is fixed; this route describes the attainable lower confidence bound."),
      why_alternative = .("An alternative interface is available for this method. Its development status and assumptions need separate review."),
      prep_categorical = .("Use one row per independent case and one column per identified rater. For ordinal methods, declare the same full ordered category scale on every column. Report observed agreement, category frequencies, the named chance-corrected coefficient and its interval."),
      prep_alpha = .("Select rater columns in agreement, set the recommended alpha type, and preserve missing ratings. Report alpha separately from the default table: two-rater kappa uses complete pairs; multirater kappa uses all observed ratings, including singleton cases in its chance term, while exact agreement uses complete cases. Alpha requires at least two ratings per usable case."),
      prep_exchangeable = .("Supply a complete equal-width case-by-rating-slot matrix with exchangeable slots. Slot columns are not fixed rater identities. Do not encode absent ratings as a category or use Cohen's kappa for exchangeable slots."),
      prep_measurement = .("Select the two numeric measurements in the same units on matched independent cases. Prespecify clinically acceptable differences; inspect the difference distribution and changing variability before interpreting ordinary limits of agreement. Missing pairs are excluded."),
      prep_icc = .("Select a complete numeric case-by-measurement matrix. The same columns identify the same raters for a two-way design; for a one-way design they identify exchangeable rating slots. Interpret the interval in the intended population, not only a verbal quality band."),
      prep_reader = .("In pathagreement assign vars for categorical calls or readerScores for numeric cutoffs, caseID for case identity, and referenceStandard when used. Select exact labels and a prespecified cutoff, keep shared-case populations, and supply readerCluster for clustered specimens. For an ordinal boundary, calls at or above the ordered category are positive."),
      prep_paired = .("Use wide data: assign condition A columns to vars and matched condition B columns to conditionBVars in the same rater order. Match cases by identity, retain independent cases, and report the two coefficients and their paired difference."),
      prep_modality = .("Assign modality1_var and modality2_var and optionally case_id. Use one matched pair per independent case. This custom-category recipe reports overall agreement and unweighted kappa on complete pairs. Directional, weighted, discordance, contingency, plot and confidence-interval outputs are disabled because their custom-category behavior is not verified. The target remains a draft."),
      prep_rank = .("Use one column per rater ranking the same cases. Preserve ties and a common direction. Kendall's W does not quantify absolute measurement agreement."),
      prep_foci = .("Use long data: assign caseId to patient identity, focusId to sample identity, and markers to categorical biomarker columns. Preserve all foci within a patient. This draft provides descriptive concordance and does not certify clinical interchangeability."),
      prep_planning = .("Supply defensible anticipated kappa and category proportions in the selected calculator. Precision, power and fixed-sample lower-bound calculations are different objectives. These settings concern unweighted kappa and independent subjects, not weighted ordinal kappa or ICC."),
      inspection_scope = .("These are observed data features, not confirmed study-design answers. The guide never recodes columns, changes your answers, or infers a disease reference. Counts respect the data and filters supplied by jamovi."),
      suggest_raters = .("Suggested rater count from the selected columns or distinct reader IDs: {n}. Confirm that this represents the intended study panel; exchangeable rating slots may require a different count."),
      suggest_missing = .("Suggested answer for missing ratings: yes. At least one selected rating is missing or long-format coverage is incomplete. Confirm how this arose in the study."),
      suggest_cluster = .("Suggested analysis unit: multiple specimens or visits per patient. Confirm patient identities and the intended unit of inference."),
      inspect_empty = .("No rows are available for inspection. Select data or turn off inspection to use the questionnaire alone."),
      inspect_roles = .("Wide inspection needs rater columns. Long inspection needs case ID, reader ID and rating columns."),
      inspect_columns = .("A selected column is no longer available. Update the optional data selections."),
      inspect_role_overlap = .("Rating columns and identifier/reference roles must be distinct. Case ID and patient ID may intentionally be the same column."),
      inspect_paired_columns = .("Condition A and B must contain the same number of matched rater columns."),
      inspect_missing_id = .("A selected identifier contains missing or blank values. Complete the identifiers before checking matched designs."),
      inspect_duplicates = .("Duplicate reading keys were found. Wide keys are case-condition-occasion; long keys additionally include reader. Identify distinct specimens or occasions rather than silently averaging duplicate readings."),
      inspect_case_patient = .("A case ID maps to more than one patient ID. Resolve the case-to-patient mapping."),
      inspect_reference = .("A case has conflicting reference values. Resolve the reference definition before using reader accuracy."),
      inspect_no_overlap = .("No case has at least two observed ratings in the same condition and occasion."),
      inspect_order = .("Selected ordered columns do not declare exactly the same full category order. Harmonize the declared scale, including unused levels, before an ordinal analysis."),
      inspect_numeric = .("Numeric storage does not establish a continuous measurement: values may be category codes. Confirm the measurement scale yourself."),
      inspect_constant = .("At least one selected column has fewer than two observed distinct values. A chance-corrected coefficient may be undefined; constant ratings do not establish reliability."),
      inspect_missing = .("Missing or blank ratings were observed. Review each recommended method's usable-case population; missingness is not automatically ignorable."),
      inspect_long = .("Long-data coverage is counted within case-condition-occasion groups. General agreement analyses require a case-by-rater matrix; reshape a copy without combining duplicate readings. Reader modality workflows can use long data directly."),
      inspect_cluster = .("A patient contributes more than one distinct case. These cases are clustered, even when case IDs are unique."),
      conflict_raters = .("The declared number of raters differs from the selected columns or observed reader IDs. Confirm that you selected the intended panel."),
      conflict_missing = .("The answer says no ratings are missing, but the selected data have missing readings or incomplete long-format coverage."),
      conflict_binary = .("Binary ratings were declared, but the selected rating columns together contain more than two observed values. Confirm a common binary coding."),
      conflict_continuous = .("Continuous measurements were declared, but at least one selected rating column is categorical or text. Confirm and prepare the scale explicitly."),
      conflict_cluster = .("Independent cases were declared, but a patient contributes multiple cases in the selected data."),
      conflict_repeated = .("No repeated readings were declared, but multiple occasions are present. Confirm how occasions enter the study design."),
      conflict_panel = .("The same complete panel was declared, but long-format reader coverage differs between cases. Confirm an incomplete panel or prepare the intended matched population."),
      conflict_reader_labels = .("Observed reader calls include values outside the declared positive and negative labels. Correct the labels or explicitly handle nondiagnostic calls in the target workflow."),
      conflict_reference_labels = .("Observed reference values include labels outside the declared positive and negative reference classes. Confirm the comparator coding."),
      conflict_ordinal = .("The ordinal boundary does not occur in every selected column's declared category order. Confirm the complete scale and boundary before recommending a cutoff."),
      inspect_ordinal_declaration = .("Ordinal ratings were confirmed, but the selected columns do not all declare ordered levels. Prepare a copy with one explicit full ordered scale before applying ordinal settings; the guide does not recode your data."),
      inspect_multiple_conditions = .("Several conditions are present. Select one condition for a single-panel analysis or choose a matched paired-condition workflow; do not pool conditions as independent cases."),
      fact_rows = .("Input rows"), fact_raters = .("Raters / columns / maximum foci"),
      fact_distinct_raters = .("Distinct reader / focus IDs in long input"),
      fact_cases = .("Cases"), fact_patients = .("Patients / clusters"),
      fact_missing = .("Missing rating cells"), fact_complete = .("Complete case-condition-occasion groups"),
      fact_overlap = .("Groups with at least two observed ratings"), fact_duplicates = .("Duplicate reading keys"),
      fact_conditions = .("Observed conditions"), levels_truncated = .("(first 30 declared levels shown)"),
      ordered = .("Ordered factor"), nominal = .("Nominal factor"), numeric = .("Numeric"), text = .("Text"),
      examples1 = .("Two pathologists classify independent biopsies as benign or malignant: choose categorical agreement, binary scores, two fixed raters, and describe missingness. Start with Cohen's kappa and observed/category-specific agreement."),
      examples2 = .("Two platforms measure Ki67 percentages: choose continuous measurement agreement. Inspect differences and limits in percentage points and specify acceptable differences before interpreting concordance."),
      examples3 = .("Several readers score multiple slides per patient with and without AI assistance: choose a fixed-reader modality workflow, identify patient clusters, state the comparator, and keep paired case-reader observations together. An expert consensus comparator is not automatically disease truth."),
      pitfalls1 = .("Agreement, reliability, correlation, and diagnostic accuracy are different questions. A large correlation, a non-significant marginal test, or a verbal kappa band does not establish clinical interchangeability."),
      pitfalls2 = .("Do not use numerical category codes as continuous measurements without justification. Weighted methods need a common ordinal scale; repeated readings and clustered specimens need an appropriate analysis unit and uncertainty method."),
      pitfalls3 = .("Development menu labels describe implementation routing, not statistical or clinical validation. Prespecify cutoffs, disagreement weights and acceptable differences, and report uncertainty and the analysed population."),
      glossary1 = .("Observed agreement: the fraction of matching calls. Kappa and alpha: agreement coefficients with different definitions of chance disagreement. Gwet's AC uses another chance model, so its value need not match kappa."),
      glossary2 = .("ICC: reliability of continuous measurements for a stated rater design and reporting unit. Absolute agreement includes systematic rater shifts; consistency permits them. CCC summarizes agreement between continuous measurements; Bland-Altman limits describe differences in their original units."),
      glossary3 = .("Analysis unit: the independent entity relevant to the question. A patient may contribute several dependent specimens. A reference standard, expert consensus, and comparison method have different interpretations and must be labelled explicitly.")
    )
  )
)
