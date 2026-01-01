# Enhanced Variable Name Handling for Crosstable
# This demonstrates the recommended approach

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

.getDisplayName <- function(cleaned_name, name_mapping) {
    # Get original display name from mapping
    original_name <- name_mapping[[cleaned_name]]
    if (is.null(original_name)) {
        return(cleaned_name)  # Fallback to cleaned name
    }
    return(original_name)
}

.enhancedLabelData <- function(self) {
    mydata <- self$data
    original_names <- names(mydata)
    
    # Create mapping: original_name -> original_name for display
    display_labels <- setNames(original_names, original_names)
    
    # Clean variable names for safe processing
    mydata <- mydata %>% janitor::clean_names()
    cleaned_names <- names(mydata)
    
    # Create bidirectional mapping
    original_to_cleaned <- setNames(cleaned_names, original_names)
    cleaned_to_original <- setNames(original_names, cleaned_names)
    
    # Apply labels to cleaned data (for gtsummary/finalfit to display properly)
    mydata <- labelled::set_variable_labels(.data = mydata, .labels = cleaned_to_original)
    
    # Get all labels for matching user selections
    all_labels <- labelled::var_label(mydata)
    
    # Match user-selected variables to cleaned names
    myvars <- self$options$vars
    myvars_cleaned <- names(all_labels)[match(myvars, all_labels)]
    
    # For group variable
    mygroup <- self$options$group
    mygroup_cleaned <- names(all_labels)[all_labels == mygroup]
    
    return(list(
        "mydata" = mydata,
        "myvars" = myvars_cleaned,
        "mygroup" = mygroup_cleaned,
        "original_names_mapping" = cleaned_to_original,  # For display
        "cleaned_names_mapping" = original_to_cleaned    # For processing
    ))
}

# Enhanced formula construction with escaping
.buildSafeFormula <- function(terms, dep, name_mapping = NULL) {
    # Escape variable names for formula safety
    escaped_terms <- .escapeVariableNames(terms)
    escaped_dep <- .escapeVariableNames(dep)
    
    # Build formula with escaped names
    formula <- jmvcore::constructFormula(terms = escaped_terms, dep = escaped_dep)
    return(as.formula(formula))
}

# Restore original names in table outputs (for styles that don't respect labels)
.restoreOriginalNamesInTable <- function(table_data, name_mapping) {
    if (is.null(table_data) || is.null(name_mapping)) {
        return(table_data)
    }
    
    # Replace cleaned names with original names in relevant columns
    if (is.data.frame(table_data) && "variable" %in% names(table_data)) {
        table_data$variable <- ifelse(
            table_data$variable %in% names(name_mapping),
            name_mapping[table_data$variable],
            table_data$variable
        )
    }
    
    return(table_data)
}
