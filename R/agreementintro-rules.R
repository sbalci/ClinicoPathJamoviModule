# Pure study-design rules. Message keys are translated by the analysis backend.
# No agreement statistic is calculated here and no selected data are modified.

.agreementIntroDefaults <- function() list(
  purpose = "unspecified", scale = "unspecified", nRaters = 0L,
  panel = "unspecified", missingRatings = "unspecified",
  repeatedReadings = "unspecified", analysisUnit = "unspecified",
  categoryOrder = "unspecified", weights = "unspecified",
  raterSelection = "unspecified", iccDefinition = "unspecified",
  reportingUnit = "unspecified", referenceType = "unspecified",
  comparisonGoal = "unspecified", planningGoal = "unspecified", nCategories = 0L,
  positiveLabel = "", negativeLabel = "", referencePositive = "",
  referenceNegative = "", cutoff = "", cutoffDirection = "unspecified",
  cutoffPrespecified = "unspecified", inspectData = FALSE, dataLayout = "wide",
  ratings = NULL, conditionBRatings = NULL, caseID = NULL, patientID = NULL,
  readerID = NULL, rating = NULL, condition = NULL, occasion = NULL,
  reference = NULL
)

.agreementIntroCatalog <- function() {
  entry <- function(title, group, subgroup, version, state) {
    list(title = title, group = group, subgroup = subgroup, version = version, state = state)
  }
  list(
    agreement = entry("Interrater Reliability", "meddecideT", "Agreement", "1.0.83", "testing"),
    cohenskappa = entry("Cohen's Kappa Agreement", "meddecideExtraD", "Diagnostic Statistics",
      "1.0.83", "draft"),
    icccoeff = entry("Intraclass Correlation Coefficient", "meddecideExtraD", "Agreement",
      "0.0.31", "draft"),
    pathagreement = entry("Pathology Interrater Reliability", "OncoPathT", "Agreement",
      "1.0.83", "testing"),
    pathologyagreement = entry("Pathology Agreement Analysis", "OncoPathP", "Agreement",
      "1.0.0", "pending"),
    modalitycomparison = entry("Modality Comparison Analysis", "meddecideExtraD", "Agreement",
      "0.0.31", "draft"),
    multifocalconcordance = entry("Multifocal / Primary-Metastasis Concordance", "OncoPathD",
      "ClinicoPath Biomarker Analysis", "0.0.1", "draft"),
    kappaSizeCI = entry("Confidence Interval Approach for the Number of Subjects Required",
      "Power", "Power Analysis by meddecide", "1.0.83", "production"),
    kappaSizePower = entry("Power Approach for the Number of Subjects Required",
      "PowerT", "Power Analysis by meddecide", "1.0.83", "testing"),
    kappaSizeFixedN = entry("Lowest Expected Value for a fixed sample size", "Power",
      "Power Analysis by meddecide", "1.0.83", "production")
  )
}

.agreementIntroMissing <- function(x) {
  is.na(x) | if (is.character(x) || is.factor(x)) trimws(as.character(x)) == "" else FALSE
}

# Match original ID values before making a composite key. Formatting floating-point
# identifiers as strings first can merge distinct patients or cases.
.agreementIntroKey <- function(data, columns) {
  if (!length(columns)) return(seq_len(nrow(data)))
  parts <- lapply(data[columns], function(x) match(x, unique(x)))
  do.call(paste, c(parts, sep = ":"))
}

.agreementIntroProfile <- function(data, options) {
  o <- utils::modifyList(.agreementIntroDefaults(), options, keep.null = TRUE)
  out <- list(active = isTRUE(o$inspectData), issues = character(), notes = character(),
    facts = list(), columns = list())
  if (!out$active) return(out)
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    out$issues <- "inspect_empty"
    return(out)
  }
  if (o$dataLayout == "wide") o$readerID <- NULL
  selected <- function(x) !is.null(x) && length(x) > 0L && all(nzchar(x))
  roles <- c(o$caseID, o$patientID, o$readerID, o$condition, o$occasion, o$reference)
  values <- if (o$dataLayout == "wide") c(o$ratings, o$conditionBRatings) else o$rating
  required <- if (o$dataLayout == "wide") o$ratings else c(o$caseID, o$readerID, o$rating)
  incomplete <- if (o$dataLayout == "wide") !selected(o$ratings) else
    !all(vapply(list(o$caseID, o$readerID, o$rating), selected, logical(1)))
  if (incomplete) out$issues <- c(out$issues, "inspect_roles")
  if (any(!c(required, roles, values) %in% names(data)))
    out$issues <- c(out$issues, "inspect_columns")
  # Case ID and patient ID may intentionally be the same variable.
  distinct_roles <- c(o$readerID, o$condition, o$occasion, o$reference)
  if (anyDuplicated(values) || length(intersect(values, roles)) ||
      anyDuplicated(distinct_roles) || length(intersect(c(o$caseID, o$patientID), distinct_roles)))
    out$issues <- c(out$issues, "inspect_role_overlap")
  if (length(out$issues)) return(out)
  missing <- .agreementIntroMissing
  for (name in unique(values)) {
    x <- data[[name]]; present <- !missing(x)
    out$columns[[name]] <- list(
      type = if (is.ordered(x)) "ordered" else if (is.factor(x)) "nominal" else
        if (is.numeric(x)) "numeric" else "text",
      observed = sum(present), missing = sum(!present),
      categories = length(unique(x[present])),
      declared = if (is.factor(x)) levels(x) else character(),
      constant = length(unique(x[present])) < 2L)
  }
  out$facts$rows <- nrow(data)
  out$facts$missing <- sum(vapply(out$columns, function(x) x$missing, numeric(1)))
  out$facts$constant <- any(vapply(out$columns, function(x) x$constant, logical(1)))
  out$facts$numeric <- all(vapply(out$columns, function(x) x$type == "numeric", logical(1)))
  out$facts$nonNumeric <- any(vapply(out$columns, function(x)
    x$type %in% c("nominal", "ordered", "text"), logical(1)))
  out$facts$maxCategories <- max(vapply(out$columns, function(x) x$categories, integer(1)))
  out$facts$ratingValues <- unique(unlist(lapply(data[values], function(x)
    as.character(x[!missing(x)])), use.names = FALSE))
  out$facts$allOrdered <- all(vapply(data[values], is.ordered, logical(1)))
  out$facts$referenceValues <- if (selected(o$reference))
    unique(as.character(data[[o$reference]][!missing(data[[o$reference]])])) else character()
  out$facts$idFormatCollision <- any(vapply(data[unique(c(o$caseID, o$readerID,
    o$patientID, o$condition))], function(x)
      length(unique(x)) != length(unique(as.character(x))), logical(1)))
  ordered <- lapply(data[values], function(x) if (is.ordered(x)) levels(x) else NULL)
  declared <- Filter(Negate(is.null), ordered)
  out$facts$orderConflict <- length(declared) > 1L &&
    !all(vapply(declared, identical, logical(1), declared[[1]]))
  if (out$facts$orderConflict) out$notes <- c(out$notes, "inspect_order")
  if (out$facts$numeric) out$notes <- c(out$notes, "inspect_numeric")
  if (out$facts$constant) out$notes <- c(out$notes, "inspect_constant")
  if (out$facts$missing > 0) out$notes <- c(out$notes, "inspect_missing")
  if (o$dataLayout == "wide") {
    present <- vapply(data[o$ratings], function(x) !missing(x), logical(nrow(data)))
    if (is.null(dim(present))) present <- matrix(present, ncol = length(o$ratings))
    out$facts$raters <- length(o$ratings)
    out$facts$cases <- nrow(data)
    out$facts$complete <- sum(rowSums(present) == ncol(present))
    out$facts$overlap <- sum(rowSums(present) >= 2L)
    out$facts$incomplete <- any(!present) ||
      any(vapply(data[o$conditionBRatings], function(x) any(missing(x)), logical(1)))
    if (length(o$conditionBRatings) && length(o$conditionBRatings) != length(o$ratings))
      out$issues <- c(out$issues, "inspect_paired_columns")
    if (selected(o$caseID)) {
      if (any(missing(data[[o$caseID]]))) out$issues <- c(out$issues, "inspect_missing_id")
      out$facts$duplicateKeys <- sum(duplicated(.agreementIntroKey(data,
        c(o$caseID, o$condition, o$occasion))))
    } else out$facts$duplicateKeys <- 0L
  } else {
    keys <- c(o$caseID, o$readerID, o$condition, o$occasion)
    if (any(vapply(data[keys], function(x) any(missing(x)), logical(1))))
      out$issues <- c(out$issues, "inspect_missing_id")
    out$facts$duplicateKeys <- sum(duplicated(.agreementIntroKey(data, keys)))
    out$facts$raters <- length(unique(data[[o$readerID]][!missing(data[[o$readerID]])]))
    out$facts$cases <- length(unique(data[[o$caseID]][!missing(data[[o$caseID]])]))
    units <- .agreementIntroKey(data, c(o$caseID, o$condition, o$occasion))
    groups <- split(seq_len(nrow(data)), units)
    coverage <- vapply(groups, function(i)
      length(unique(data[[o$readerID]][i][!missing(data[[o$rating]][i])])), integer(1))
    out$facts$distinctRaters <- out$facts$raters
    if (o$panel == "different") out$facts$raters <- max(vapply(groups, function(i)
      length(unique(data[[o$readerID]][i])), integer(1)))
    # Focus labels may be unique to a patient; an absent label in another patient
    # is not a missing rater. Varying numbers of observed foci are allowed.
    if (o$purpose == "multifocal") out$facts$raters <- max(coverage)
    out$facts$complete <- sum(coverage == out$facts$raters)
    out$facts$overlap <- sum(coverage >= 2L)
    out$facts$incomplete <- if (o$purpose == "multifocal") any(missing(data[[o$rating]])) else
      any(coverage != out$facts$raters)
    out$notes <- c(out$notes, "inspect_long")
  }
  if (out$facts$duplicateKeys > 0) out$issues <- c(out$issues, "inspect_duplicates")
  out$facts$repeatedOccasions <- selected(o$occasion) &&
    length(unique(data[[o$occasion]][!missing(data[[o$occasion]])])) > 1L
  out$facts$conditions <- if (selected(o$condition))
    length(unique(data[[o$condition]][!missing(data[[o$condition]])])) else 0L
  out$facts$clustered <- FALSE
  if (selected(o$patientID)) {
    pid <- data[[o$patientID]]
    if (any(missing(pid))) out$issues <- c(out$issues, "inspect_missing_id")
    out$facts$patients <- length(unique(pid[!missing(pid)]))
    case_key <- if (selected(o$caseID)) match(data[[o$caseID]], unique(data[[o$caseID]])) else
      seq_len(nrow(data))
    patient_key <- match(pid, unique(pid))
    pairs <- unique(data.frame(patient = patient_key, case = case_key))
    out$facts$clustered <- anyDuplicated(pairs$patient) > 0L
    if (selected(o$caseID) && anyDuplicated(pairs$case))
      out$issues <- c(out$issues, "inspect_case_patient")
    if (out$facts$clustered) out$notes <- c(out$notes, "inspect_cluster")
  }
  if (selected(o$reference) && selected(o$caseID)) {
    groups <- split(seq_len(nrow(data)), match(data[[o$caseID]], unique(data[[o$caseID]])))
    if (any(vapply(groups, function(i)
      length(unique(data[[o$reference]][i][!missing(data[[o$reference]][i])])) > 1L,
      logical(1)))) out$issues <- c(out$issues, "inspect_reference")
  }
  if (out$facts$overlap == 0L) out$issues <- c(out$issues, "inspect_no_overlap")
  out$issues <- unique(out$issues)
  out
}

.agreementIntroRecommend <- function(options, profile = NULL) {
  o <- utils::modifyList(.agreementIntroDefaults(), options, keep.null = TRUE)
  out <- list(status = "needs_information", path = "START", messages = character(),
    recommendations = list(), notes = character())
  stop_with <- function(status, path, messages) {
    out$status <- status; out$path <- path; out$messages <- unique(messages); out
  }
  unspecified <- function(x) is.null(x) || !length(x) || identical(x, "unspecified")
  need <- function(fields) fields[vapply(fields, function(x) unspecified(o[[x]]), logical(1))]
  require_fields <- function(fields) {
    absent <- need(fields)
    if (length(absent)) paste0("need_", absent) else character()
  }
  add <- function(target, method, settings, reason, preparation, refs) {
    out$recommendations[[length(out$recommendations) + 1L]] <<- list(target = target,
      method = method, settings = settings, reason = reason, preparation = preparation, refs = refs)
  }
  done <- function(path) {
    out$status <- "recommended"; out$path <- path
    out$recommendations <- lapply(out$recommendations, .agreementIntroBind, options = o)
    out$messages <- "recommendation_scope"; out
  }
  if (!is.null(profile) && profile$active) {
    if (length(profile$issues)) return(stop_with("data_conflict", "DATA_CHECK", profile$issues))
    f <- profile$facts
    conflicts <- character()
    if (o$purpose != "multifocal" && o$nRaters > 0 && o$nRaters != f$raters)
      conflicts <- c(conflicts, "conflict_raters")
    if (o$missingRatings == "no" && isTRUE(f$incomplete))
      conflicts <- c(conflicts, "conflict_missing")
    if (o$scale == "binary" && length(f$ratingValues) > 2L)
      conflicts <- c(conflicts, "conflict_binary")
    if (o$scale == "continuous" && isTRUE(f$nonNumeric))
      conflicts <- c(conflicts, "conflict_continuous")
    if (o$scale %in% c("ordinal", "rankings") && isTRUE(f$orderConflict))
      conflicts <- c(conflicts, "inspect_order")
    if (o$analysisUnit == "independent" && isTRUE(f$clustered))
      conflicts <- c(conflicts, "conflict_cluster")
    if (o$repeatedReadings == "no" && isTRUE(f$repeatedOccasions))
      conflicts <- c(conflicts, "conflict_repeated")
    if (o$dataLayout == "long" && o$panel == "same" && isTRUE(f$incomplete) &&
        o$missingRatings == "no") conflicts <- c(conflicts, "conflict_panel")
    if (o$purpose != "paired" && isTRUE(f$conditions > 1L))
      conflicts <- c(conflicts, "inspect_multiple_conditions")
    if (o$purpose == "paired" && isTRUE(f$repeatedOccasions))
      conflicts <- c(conflicts, "paired_occasion")
    reader <- o$purpose %in% c("readerAccuracy", "cutoff") ||
      (o$purpose == "paired" && o$comparisonGoal == "readerModality")
    if (reader && o$scale == "binary" && nzchar(o$positiveLabel) && nzchar(o$negativeLabel) &&
        any(!f$ratingValues %in% c(o$positiveLabel, o$negativeLabel)))
      conflicts <- c(conflicts, "conflict_reader_labels")
    if (reader && o$referenceType %in% c("disease", "consensus", "method") &&
        nzchar(o$referencePositive) && nzchar(o$referenceNegative) &&
        any(!f$referenceValues %in% c(o$referencePositive, o$referenceNegative)))
      conflicts <- c(conflicts, "conflict_reference_labels")
    if (reader && o$scale == "ordinal" && nzchar(o$cutoff) && isTRUE(f$allOrdered) &&
        any(vapply(profile$columns, function(x) !o$cutoff %in% x$declared, logical(1))))
      conflicts <- c(conflicts, "conflict_ordinal")
    if (length(conflicts)) return(stop_with("data_conflict", "DATA_CONFLICT", conflicts))
    out$notes <- profile$notes
    if (o$scale == "ordinal" && !isTRUE(f$allOrdered))
      out$notes <- c(out$notes, "inspect_ordinal_declaration")
  }
  if (unspecified(o$purpose)) return(stop_with("needs_information", "START", "need_purpose"))
  if (o$purpose == "planning") {
    missing <- require_fields(c("planningGoal", "scale", "analysisUnit", "repeatedReadings"))
    if (o$nRaters == 0L) missing <- c(missing, "need_nRaters")
    if (o$nCategories == 0L) missing <- c(missing, "need_nCategories")
    if (length(missing)) return(stop_with("needs_information", "PLAN_INPUT", missing))
    if (!o$scale %in% c("binary", "nominal", "ordinal") || o$nRaters < 2 || o$nRaters > 6 ||
        o$nCategories < 2 || o$nCategories > 5 || o$analysisUnit != "independent" ||
        o$repeatedReadings != "no" || (o$scale == "binary" && o$nCategories != 2))
      return(stop_with("unsupported", "PLAN_UNSUPPORTED", "planning_limits"))
    target <- switch(o$planningGoal, precision = "kappaSizeCI", power = "kappaSizePower",
      fixed = "kappaSizeFixedN")
    add(target, paste0("plan_", o$planningGoal),
      list(outcome = as.character(o$nCategories), raters = as.character(o$nRaters)),
      paste0("why_plan_", o$planningGoal), "prep_planning", "rotondiDonnerKappaCI")
    out$notes <- c(out$notes, "planning_model")
    return(done(paste0("PLAN_", toupper(o$planningGoal))))
  }
  missing <- require_fields(c("scale", "analysisUnit", "repeatedReadings", "missingRatings"))
  if (o$purpose == "paired") missing <- c(missing, require_fields("comparisonGoal"))
  if (o$nRaters < 2L) missing <- c(missing, "need_nRaters")
  if (length(missing)) return(stop_with("needs_information", "DESIGN_INPUT", missing))
  reader_route <- o$purpose %in% c("readerAccuracy", "cutoff") ||
    (o$purpose == "paired" && o$comparisonGoal == "readerModality")
  if (o$analysisUnit == "clustered" && !reader_route && o$purpose != "multifocal")
    return(stop_with("unsupported", "CLUSTER_REVIEW", "unsupported_cluster"))
  if (o$repeatedReadings == "yes" && !o$purpose %in% c("paired", "readerAccuracy", "cutoff"))
    return(stop_with("unsupported", "REPEAT_REVIEW", "unsupported_repeated"))
  if (o$purpose == "categorical") {
    if (!o$scale %in% c("binary", "nominal", "ordinal"))
      return(stop_with("unsupported", "CAT_SCALE", "categorical_scale"))
    missing <- require_fields("panel")
    if (o$scale == "ordinal") missing <- c(missing, require_fields(c("categoryOrder", "weights")))
    if (length(missing)) return(stop_with("needs_information", "CAT_INPUT", missing))
    if (o$scale == "ordinal" && o$categoryOrder != "confirmed")
      return(stop_with("needs_information", "CAT_ORDER", "need_categoryOrder"))
    w <- if (o$scale == "ordinal") switch(o$weights, linear = "equal", quadratic = "squared",
      unweighted = "unweighted") else "unweighted"
    if (o$missingRatings == "yes" || o$panel == "incomplete" ||
        (o$nRaters == 2L && o$panel == "different")) {
      method <- if (o$scale == "ordinal" && w != "unweighted") "ordinal" else "nominal"
      add("agreement", "alpha", list(kripp = TRUE, krippMethod = method),
        "why_alpha", "prep_alpha", "Krippendorff2004")
      return(done("CAT_INCOMPLETE_ALPHA"))
    }
    if (o$nRaters == 2L && o$panel == "same") {
      add("agreement", if (w == "unweighted") "cohen" else "weighted_cohen", list(wght = w),
        "why_cohen", "prep_categorical", "Cohen1960")
      add("cohenskappa", "cohen_specialist", list(kappa_type = switch(w,
        equal = "linear", squared = "quadratic", "cohen")), "why_alternative",
        "prep_categorical", "Cohen1960")
      out$notes <- c(out$notes, "category_specific")
      return(done(if (o$scale == "ordinal") "CAT_TWO_ORDINAL" else "CAT_TWO_NOMINAL"))
    }
    if (o$scale == "ordinal" && w != "unweighted") {
      add("agreement", "alpha", list(kripp = TRUE, krippMethod = "ordinal"),
        "why_ordinal_multi", "prep_alpha", "Krippendorff2004")
      add("agreement", "gwet", list(gwet = TRUE, gwetWeights = o$weights),
        "why_gwet", "prep_categorical", "Gwet2008")
      return(done("CAT_MULTI_ORDINAL"))
    }
    if (o$panel == "same" && o$nRaters >= 3L) {
      add("agreement", "conger", list(exct = TRUE, wght = "unweighted"),
        "why_conger", "prep_categorical", "Conger1980")
      out$notes <- c(out$notes, "category_specific")
      return(done("CAT_PANEL_CONGER"))
    }
    # A complete count matrix with exchangeable rater slots is required.
    add("agreement", "fleiss", list(exct = FALSE, wght = "unweighted"),
      "why_fleiss", "prep_exchangeable", "Fleiss1971")
    return(done("CAT_EXCHANGE_FLEISS"))
  }
  if (o$purpose == "measurement") {
    if (o$scale != "continuous" || o$nRaters != 2L)
      return(stop_with("unsupported", "MEASURE_UNSUPPORTED", "measurement_pair"))
    add("agreement", "bland_ccc", list(blandAltmanPlot = TRUE, linCCC = TRUE),
      "why_measurement", "prep_measurement", "BlandAltman1986")
    add("pathologyagreement", "platform", list(icc_type = "absolute", clinical_preset = "general"),
      "why_alternative", "prep_measurement", "BlandAltman1986")
    return(done("MEASUREMENT_PAIR"))
  }
  if (o$purpose == "reliability") {
    if (o$scale != "continuous")
      return(stop_with("unsupported", "ICC_SCALE", "icc_continuous"))
    missing <- require_fields(c("panel", "raterSelection", "iccDefinition", "reportingUnit"))
    if (length(missing)) return(stop_with("needs_information", "ICC_INPUT", missing))
    if (o$panel == "incomplete" || o$missingRatings == "yes")
      return(stop_with("unsupported", "ICC_INCOMPLETE", "icc_incomplete"))
    if (o$panel == "different" && (o$raterSelection != "random" ||
        o$iccDefinition != "absolute"))
      return(stop_with("unsupported", "ICC_ONEWAY_REVIEW", "icc_oneway"))
    if (o$panel == "same" && ((o$raterSelection == "fixed" && o$iccDefinition == "absolute") ||
        (o$raterSelection == "random" && o$iccDefinition == "consistency")))
      return(stop_with("unsupported", "ICC_LABEL_REVIEW", "icc_label_limit"))
    base <- if (o$panel == "different") "1" else if (o$raterSelection == "random") "2" else "3"
    unit <- if (o$reportingUnit == "single") "1" else "k"
    setting <- paste0("icc", base, unit)
    add("agreement", "icc", list(icc = TRUE, iccType = setting),
      "why_icc", "prep_icc", c("Koo2016", "Koo2017Erratum"))
    add("icccoeff", "icc_specialist", list(icc_type = paste0("icc", base, "_", unit),
      agreement_type = if (base == "2") "agreement" else "consistency"),
      "why_alternative", "prep_icc", "Koo2016")
    out$notes <- c(out$notes, if (base == "1") "icc_oneway_definition" else "icc_report")
    return(done(paste0("ICC_", base, "_", toupper(unit))))
  }
  if (reader_route) {
    if (!o$scale %in% c("binary", "ordinal", "continuous"))
      return(stop_with("unsupported", "READER_SCALE", "reader_scale"))
    missing <- require_fields(c("referenceType", "panel"))
    accuracy <- o$purpose %in% c("readerAccuracy", "paired") || o$referenceType != "none"
    if (o$purpose %in% c("readerAccuracy", "paired") && o$referenceType == "none")
      missing <- c(missing, "need_referenceType")
    if (o$scale == "binary" && (!nzchar(o$positiveLabel) || !nzchar(o$negativeLabel) ||
        o$positiveLabel == o$negativeLabel)) missing <- c(missing, "need_reader_labels")
    if (accuracy && (!nzchar(o$referencePositive) || !nzchar(o$referenceNegative) ||
        o$referencePositive == o$referenceNegative)) missing <- c(missing, "need_reference_labels")
    if (o$scale != "binary") {
      if (o$cutoffPrespecified != "yes") missing <- c(missing, "need_cutoffPrespecified")
      if (!nzchar(trimws(o$cutoff))) missing <- c(missing, "need_cutoff")
      if (o$scale == "continuous") {
        missing <- c(missing, require_fields("cutoffDirection"))
        if (nzchar(o$cutoff) && !is.finite(suppressWarnings(as.numeric(o$cutoff))))
          missing <- c(missing, "need_numeric_cutoff")
      } else if (o$categoryOrder != "confirmed") missing <- c(missing, "need_categoryOrder")
    }
    if (length(missing)) return(stop_with("needs_information", "READER_INPUT", missing))
    if (o$panel == "different")
      return(stop_with("unsupported", "READER_PANEL_REVIEW", "reader_fixed"))
    if (!is.null(profile) && profile$active &&
        isTRUE(profile$facts$idFormatCollision))
      return(stop_with("unsupported", "READER_ID_REVIEW", "reader_id_limit"))
    if (o$repeatedReadings == "yes" && !(o$purpose == "paired" &&
        o$comparisonGoal == "readerModality"))
      return(stop_with("unsupported", "READER_REPEAT_REVIEW", "reader_repeated"))
    settings <- list(readerWorkflow = switch(o$scale, binary = "binary",
      ordinal = "ordinal_cutoff", continuous = "numeric_cutoff"),
      referenceType = if (o$referenceType == "none") "legacy" else o$referenceType,
      referenceAccuracy = accuracy, readerPopulation = "shared")
    if (o$scale == "binary") settings <- c(settings,
      list(readerPositive = o$positiveLabel, readerNegative = o$negativeLabel))
    if (accuracy) settings <- c(settings,
      list(referencePositive = o$referencePositive, referenceNegative = o$referenceNegative))
    if (o$scale == "continuous") settings <- c(settings,
      list(clinicalCutoff = o$cutoff, positiveRule = o$cutoffDirection))
    if (o$scale == "ordinal") settings <- c(settings, list(ordinalCutoff = o$cutoff))
    if (o$analysisUnit == "clustered") {
      settings$readerBootstrap <- TRUE
      out$notes <- c(out$notes, "reader_cluster")
    }
    if (o$purpose == "paired") {
      settings$readerFormat <- "long"; settings$readerModalityComparison <- TRUE
      out$notes <- c(out$notes, "reader_modality")
    }
    if (o$referenceType %in% c("consensus", "method")) out$notes <- c(out$notes, "reference_not_truth")
    add("pathagreement", "reader", settings, "why_reader", "prep_reader", "ClinicoPathJamoviModule")
    return(done(if (o$purpose == "paired") "READER_MODALITY" else
      paste0("READER_", toupper(o$scale))))
  }
  if (o$purpose == "paired") {
    if (!o$scale %in% c("binary", "nominal", "ordinal"))
      return(stop_with("unsupported", "PAIRED_SCALE", "paired_scale"))
    missing <- require_fields("comparisonGoal")
    if (length(missing)) return(stop_with("needs_information", "PAIRED_INPUT", missing))
    if (o$comparisonGoal == "specialized") {
      if (!o$scale %in% c("binary", "nominal", "ordinal") || o$nRaters != 2L)
        return(stop_with("unsupported", "MODALITY_SCALE", "modality_pair"))
      if (o$scale == "ordinal")
        return(stop_with("unsupported", "MODALITY_ORDER", "modality_order"))
      add("modalitycomparison", "modality", list(score_categories = "custom",
        calculate_weighted_kappa = FALSE, directional_analysis = FALSE,
        show_discordance_analysis = FALSE, show_contingency_table = FALSE,
        show_plots = FALSE, confidence_intervals = FALSE),
        "why_modality", "prep_modality", "ClinicoPathJamoviModule")
      return(done("MODALITY_SPECIALIST"))
    }
    missing <- require_fields("panel")
    if (o$scale == "ordinal") missing <- c(missing, require_fields(c("categoryOrder", "weights")))
    if (length(missing)) return(stop_with("needs_information", "PAIRED_INPUT", missing))
    if (o$panel != "same" || o$missingRatings != "no" ||
        (o$scale == "ordinal" && o$categoryOrder != "confirmed"))
      return(stop_with("unsupported", "PAIRED_REVIEW", "paired_complete"))
    if (o$nRaters > 2L)
      return(stop_with("unsupported", "PAIRED_MULTI_REVIEW", "paired_multi"))
    add("agreement", "paired", list(pairedAgreementTest = TRUE,
      wght = if (o$scale == "ordinal") switch(o$weights, linear = "equal",
        quadratic = "squared", "unweighted") else "unweighted"),
      "why_paired", "prep_paired", "ClinicoPathJamoviModule")
    return(done("PAIRED_AGREEMENT"))
  }
  if (o$purpose == "ranking") {
    if (unspecified(o$panel)) return(stop_with("needs_information", "RANK_INPUT", "need_panel"))
    if (!o$scale %in% c("rankings", "ordinal") || o$missingRatings != "no")
      return(stop_with("unsupported", "RANK_REVIEW", "rank_complete"))
    if (o$panel != "same") return(stop_with("unsupported", "RANK_REVIEW", "rank_complete"))
    if (o$scale == "ordinal" && o$categoryOrder != "confirmed")
      return(stop_with("needs_information", "RANK_INPUT", "need_categoryOrder"))
    add("agreement", "kendall", list(kendallW = TRUE), "why_rank", "prep_rank", "Kendall1939")
    return(done("RANK_KENDALL"))
  }
  if (o$purpose == "bias") {
    if (unspecified(o$panel)) return(stop_with("needs_information", "BIAS_INPUT", "need_panel"))
    if (!o$scale %in% c("binary", "nominal", "ordinal") || o$nRaters != 2L)
      return(stop_with("unsupported", "BIAS_REVIEW", "bias_pair"))
    if (o$panel != "same") return(stop_with("unsupported", "BIAS_REVIEW", "bias_pair"))
    add("agreement", "marginal", list(stuartMaxwell = TRUE), "why_bias",
      "prep_categorical", "ClinicoPathJamoviModule")
    return(done("BIAS_MARGINAL"))
  }
  if (o$purpose == "multifocal") {
    if (!o$scale %in% c("binary", "nominal", "ordinal"))
      return(stop_with("unsupported", "FOCI_SCALE", "foci_categorical"))
    if (!is.null(profile) && profile$active && isTRUE(profile$facts$idFormatCollision))
      return(stop_with("unsupported", "FOCI_ID_REVIEW", "reader_id_limit"))
    add("multifocalconcordance", "foci", list(showKappa = FALSE, showCaseLevel = FALSE,
      showDiscordance = FALSE, showSummary = FALSE),
      "why_foci", "prep_foci", "ClinicoPathJamoviModule")
    out$notes <- c(out$notes, "foci_not_clonality")
    return(done("FOCI_CONCORDANCE"))
  }
  stop_with("unsupported", "UNSUPPORTED", "unsupported_design")
}

# Bind only columns explicitly selected for inspection. Long-to-wide restructuring,
# choosing a modality contrast and declaring category order remain user decisions.
.agreementIntroBind <- function(recommendation, options) {
  o <- options; r <- recommendation
  if (!isTRUE(o$inspectData)) return(r)
  settings <- list()
  if (o$dataLayout == "wide") {
    settings <- switch(r$target,
      agreement = list(vars = o$ratings, conditionBVars =
        if (o$purpose == "paired") o$conditionBRatings else NULL),
      cohenskappa = list(rater1 = o$ratings[1], rater2 = o$ratings[2]),
      icccoeff = list(vars = o$ratings),
      pathologyagreement = list(dep1 = o$ratings[1], dep2 = o$ratings[2]),
      modalitycomparison = list(modality1_var = o$ratings[1], modality2_var = o$ratings[2],
        case_id = o$caseID),
      pathagreement = if (o$purpose != "paired") {
        cols <- if (o$scale == "continuous") list(readerScores = o$ratings) else
          list(vars = o$ratings)
        c(cols, list(caseID = o$caseID, readerCluster = o$patientID,
          referenceStandard = o$reference))
      } else list(), list())
  } else if (r$target == "pathagreement" && o$purpose == "paired" && length(o$condition)) {
    settings <- list(readerFormat = "long", caseID = o$caseID, readerID = o$readerID,
      rating = o$rating, modality = o$condition, readerCluster = o$patientID,
      referenceStandard = o$reference)
  }
  r$settings <- utils::modifyList(r$settings, Filter(Negate(is.null), settings))
  r
}
