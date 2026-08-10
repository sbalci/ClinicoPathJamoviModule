# Module Utilities for Enhanced _updateModules.R
# This file contains helper functions for module management, validation, security, and performance

# Load required packages with validation
load_required_packages <- function(packages) {
  missing_packages <- c()

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }

  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
         "\nPlease install with: install.packages(c('",
         paste(missing_packages, collapse = "', '"), "'))")
  }

  # Load packages
  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }

  message("✅ All required packages loaded successfully")
}

# Security: Path validation
validate_path <- function(path, base_dir, description = "path") {
  if (is.null(path) || is.na(path) || nchar(path) == 0) {
    stop("Invalid ", description, ": path is null or empty")
  }

  # Check for path traversal attempts
  if (grepl("\\.\\.", path) || grepl("~", path)) {
    warning("Potential path traversal detected in ", description, ": ", path)
  }

  # Normalize paths for comparison
  real_path <- tryCatch({
    normalizePath(path, mustWork = FALSE)
  }, error = function(e) {
    stop("Invalid ", description, ": ", path, " - ", e$message)
  })

  real_base <- tryCatch({
    normalizePath(base_dir, mustWork = TRUE)
  }, error = function(e) {
    stop("Invalid base directory: ", base_dir, " - ", e$message)
  })

  # Check if path is within base directory
  if (!startsWith(real_path, real_base) && !startsWith(real_path, dirname(real_base))) {
    warning("Path ", description, " is outside base directory: ", path)
  }

  return(real_path)
}

# Security: File integrity verification
verify_file_integrity <- function(source_file, dest_file) {
  if (!file.exists(source_file)) {
    warning("Source file does not exist: ", source_file)
    return(FALSE)
  }

  if (!file.exists(dest_file)) {
    return(TRUE)  # Destination doesn't exist, copy is needed
  }

  # Check file sizes first (quick check)
  source_size <- file.size(source_file)
  dest_size <- file.size(dest_file)

  if (source_size != dest_size) {
    return(FALSE)
  }

  # Verify checksums for critical files
  if (requireNamespace("digest", quietly = TRUE)) {
    tryCatch({
      source_hash <- digest::digest(file = source_file, algo = "sha256")
      dest_hash <- digest::digest(file = dest_file, algo = "sha256")
      return(source_hash == dest_hash)
    }, error = function(e) {
      warning("Failed to verify checksums: ", e$message)
      return(FALSE)
    })
  }

  return(TRUE)
}

# Security: File size validation
validate_file_size <- function(file_path, max_size_mb = 100) {
  if (!file.exists(file_path)) {
    return(TRUE)  # File doesn't exist, no size concern
  }

  file_size_mb <- file.size(file_path) / (1024 * 1024)

  if (file_size_mb > max_size_mb) {
    warning("File exceeds maximum size (", max_size_mb, "MB): ", file_path,
            " (", round(file_size_mb, 2), "MB)")
    return(FALSE)
  }

  return(TRUE)
}

# Module validation: Check module integrity
validate_module_integrity <- function(module_dir, module_name, required_dirs = NULL) {
  if (!dir.exists(module_dir)) {
    stop("Module directory does not exist: ", module_dir)
  }

  # Check required files
  required_files <- c("DESCRIPTION")
  missing_files <- c()

  for (file in required_files) {
    file_path <- file.path(module_dir, file)
    if (!file.exists(file_path)) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    stop("Missing required files in ", module_name, ": ", paste(missing_files, collapse = ", "))
  }

  # Check required directories
  if (!is.null(required_dirs)) {
    missing_dirs <- c()

    for (dir in required_dirs) {
      dir_path <- file.path(module_dir, dir)
      if (!dir.exists(dir_path)) {
        missing_dirs <- c(missing_dirs, dir)
      }
    }

    if (length(missing_dirs) > 0) {
      warning("Missing directories in ", module_name, ": ", paste(missing_dirs, collapse = ", "))
    }
  }

  message("✅ Module integrity validated: ", module_name)
  return(TRUE)
}

# Prune DESCRIPTION.backup.<timestamp> files older than `days` days.
# Uses the filename timestamp (not file mtime) so git checkouts / OS-level
# touches don't accidentally extend the lifespan. Silent on no-op; reports
# count + paths on actual removal.
prune_description_backups <- function(module_dir, days = 10) {
  if (!dir.exists(module_dir)) return(invisible(0L))

  pattern <- "^DESCRIPTION\\.backup\\.([0-9]{8}_[0-9]{6})$"
  candidates <- list.files(module_dir, pattern = pattern, full.names = TRUE)
  if (length(candidates) == 0) return(invisible(0L))

  stamps <- regmatches(basename(candidates), regexec(pattern, basename(candidates)))
  parsed <- vapply(stamps, function(m) if (length(m) == 2) m[[2]] else NA_character_, character(1))
  times <- as.POSIXct(parsed, format = "%Y%m%d_%H%M%S", tz = "UTC")

  cutoff <- Sys.time() - as.difftime(days, units = "days")
  stale <- !is.na(times) & times < cutoff
  if (!any(stale)) return(invisible(0L))

  removed <- file.remove(candidates[stale])
  n_removed <- sum(removed)
  if (n_removed > 0) {
    message("🧹 Pruned ", n_removed, " DESCRIPTION backup(s) older than ", days,
            " days in ", basename(module_dir))
  }
  invisible(n_removed)
}

# NAMESPACE-DESCRIPTION Synchronization: Check and update DESCRIPTION based on NAMESPACE
sync_namespace_with_description <- function(module_dir, dry_run = FALSE) {
  namespace_file <- file.path(module_dir, "NAMESPACE")
  desc_file <- file.path(module_dir, "DESCRIPTION")
  
  if (!file.exists(namespace_file)) {
    message("ℹ️ No NAMESPACE file found in ", basename(module_dir), " - skipping sync")
    return(TRUE)
  }
  
  if (!file.exists(desc_file)) {
    warning("❌ DESCRIPTION file not found in ", basename(module_dir), " - cannot sync")
    return(FALSE)
  }
  
  tryCatch({
    # Read NAMESPACE file and extract package imports
    namespace_lines <- readLines(namespace_file, warn = FALSE)
    
    # Extract packages from various import patterns
    imported_packages <- c()
    
    # Parse different import patterns
    for (line in namespace_lines) {
      line <- trimws(line)
      
      # import(package) or import(package, except = c(...)) -> take FIRST arg only.
      # The old `([^)]+)` captured up to the first ')', which for an
      # `import(dplyr, except = c(a, b))` directive grabbed
      # "dplyr, except = c(a, b" and injected it as a bogus package name into
      # DESCRIPTION Imports. Extract just the leading package identifier.
      if (grepl("^import\\(", line)) {
        pkg <- sub("^import\\(\\s*[\"']?([A-Za-z0-9._]+).*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
      
      # importFrom(package, ...)
      if (grepl("^importFrom\\(", line)) {
        pkg <- gsub("^importFrom\\(([^,)]+).*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
      
      # requireNamespace patterns in comments or code
      if (grepl("requireNamespace.*['\"]([^'\"]+)['\"]", line)) {
        pkg <- gsub(".*requireNamespace.*['\"]([^'\"]+)['\"].*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
    }
    
    # Clean up package names
    imported_packages <- unique(trimws(imported_packages))
    imported_packages <- imported_packages[nchar(imported_packages) > 0]
    imported_packages <- imported_packages[!imported_packages %in% c("stats", "utils", "base", "methods", "graphics", "grDevices")]
    
    if (length(imported_packages) == 0) {
      message("ℹ️ No external packages found in NAMESPACE for ", basename(module_dir))
      return(TRUE)
    }
    
    message("📦 Found packages in NAMESPACE: ", paste(imported_packages, collapse = ", "))
    
    # Read current DESCRIPTION via the `desc` package, which preserves the
    # multi-line formatting of fields like `Remotes:` that base R's
    # read.dcf() + write.dcf() round-trip would reflow.
    if (!requireNamespace("desc", quietly = TRUE)) {
      warning("❌ The 'desc' package is required for safe DESCRIPTION editing. ",
              "Install with: install.packages('desc'). Skipping sync for ",
              basename(module_dir))
      return(FALSE)
    }
    d <- desc::desc(file = desc_file)

    # Get current Imports and Suggests as character vectors (version specs included)
    current_imports_raw <- tryCatch(d$get_list("Imports"), error = function(e) character(0))
    current_suggests_raw <- tryCatch(d$get_list("Suggests"), error = function(e) character(0))

    # Clean package names (remove version specifications) for comparison
    strip_versions <- function(x) trimws(gsub("\\s*\\([^)]*\\)", "", x))
    current_imports <- strip_versions(current_imports_raw)
    current_suggests <- strip_versions(current_suggests_raw)
    current_imports <- current_imports[nchar(current_imports) > 0]
    current_suggests <- current_suggests[nchar(current_suggests) > 0]

    # Find missing packages
    all_declared <- c(current_imports, current_suggests)
    missing_packages <- imported_packages[!imported_packages %in% all_declared]

    if (length(missing_packages) == 0) {
      message("✅ All NAMESPACE packages are declared in DESCRIPTION for ", basename(module_dir))
      return(TRUE)
    }

    message("⚠️ Missing packages in DESCRIPTION for ", basename(module_dir), ": ", paste(missing_packages, collapse = ", "))

    if (dry_run) {
      message("🔍 DRY RUN: Would add packages to Imports: ", paste(missing_packages, collapse = ", "))
      return(TRUE)
    }

    # Update the Imports field — preserve any existing version specs on
    # already-declared packages, add bare names for new ones, sort for stable ordering.
    bare_to_raw <- setNames(current_imports_raw, current_imports)
    updated_imports_bare <- sort(unique(c(current_imports, missing_packages)))
    updated_imports_full <- vapply(
      updated_imports_bare,
      function(name) if (name %in% names(bare_to_raw)) bare_to_raw[[name]] else name,
      character(1)
    )
    # Write as a clean multi-line block (`Imports:\n    pkg1,\n    pkg2`).
    # desc::set_list collapses with a header offset that drops the space after
    # the colon; building the value as a single string with leading newline +
    # 4-space indent gives the standard CRAN-style formatting.
    d$set(Imports = paste0("\n    ", paste(updated_imports_full, collapse = ",\n    ")))

    # Prune backups older than 10 days before creating a new one (keeps the
    # working tree tidy without losing recent rollback options).
    prune_description_backups(module_dir, days = 10)

    # Create backup of original DESCRIPTION
    backup_file <- paste0(desc_file, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(desc_file, backup_file)
    message("💾 Created backup: ", basename(backup_file))

    # Write updated DESCRIPTION. `desc` rewrites only fields it touched and
    # preserves the original formatting of Remotes, Authors@R, etc.
    d$write(file = desc_file)
    message("✅ Updated DESCRIPTION for ", basename(module_dir), " - added: ", paste(missing_packages, collapse = ", "))

    return(TRUE)
    
  }, error = function(e) {
    warning("❌ Failed to sync NAMESPACE with DESCRIPTION for ", basename(module_dir), ": ", e$message)
    return(FALSE)
  })
}

# Enhanced function to sync all modules
sync_all_modules_namespace <- function(modules_config, main_repo_dir, dry_run = FALSE) {
  message("\n🔄 Starting NAMESPACE-DESCRIPTION synchronization...")
  
  success_count <- 0
  error_count <- 0
  
  for (module_name in names(modules_config)) {
    module_config <- modules_config[[module_name]]
    
    if (!module_config$enabled) {
      message("⏭️ Skipping disabled module: ", module_name)
      next
    }
    
    module_dir <- module_config$directory %||% file.path(main_repo_dir, module_config$repo_dir)
    
    if (!dir.exists(module_dir)) {
      warning("⚠️ Module directory not found: ", module_dir)
      error_count <- error_count + 1
      next
    }
    
    message("\n📁 Processing module: ", module_name)
    
    if (sync_namespace_with_description(module_dir, dry_run)) {
      success_count <- success_count + 1
    } else {
      error_count <- error_count + 1
    }
  }
  
  message("\n📊 NAMESPACE-DESCRIPTION sync completed:")
  message("   ✅ Success: ", success_count, " modules")
  message("   ❌ Errors: ", error_count, " modules")
  
  return(error_count == 0)
}

# Backup management: Create backup
create_backup <- function(module_dir, backup_base_dir = "backups") {
  if (!dir.exists(module_dir)) {
    warning("Cannot backup non-existent directory: ", module_dir)
    return(NULL)
  }

  # Create backup directory if it doesn't exist
  if (!dir.exists(backup_base_dir)) {
    dir.create(backup_base_dir, recursive = TRUE)
  }

  # Generate backup name with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  module_name <- basename(module_dir)
  backup_dir <- file.path(backup_base_dir, paste0(module_name, "_backup_", timestamp))

  tryCatch({
    fs::dir_copy(module_dir, backup_dir)
    message("✅ Backup created: ", backup_dir)
    return(backup_dir)
  }, error = function(e) {
    warning("Failed to create backup for ", module_name, ": ", e$message)
    return(NULL)
  })
}

# Backup management: Rollback module
rollback_module <- function(backup_dir, module_dir) {
  if (!dir.exists(backup_dir)) {
    stop("Backup directory does not exist: ", backup_dir)
  }

  if (!dir.exists(module_dir)) {
    warning("Target module directory does not exist, creating: ", module_dir)
  } else {
    # Remove existing module directory
    tryCatch({
      fs::dir_delete(module_dir)
    }, error = function(e) {
      stop("Failed to remove existing module directory: ", e$message)
    })
  }

  # Restore from backup
  tryCatch({
    fs::dir_copy(backup_dir, module_dir)
    message("✅ Module restored from backup: ", module_dir)
    return(TRUE)
  }, error = function(e) {
    stop("Failed to restore from backup: ", e$message)
  })
}

# Backup management: Clean old backups
clean_old_backups <- function(backup_base_dir = "backups", retention_days = 30) {
  if (!dir.exists(backup_base_dir)) {
    return(TRUE)
  }

  backup_dirs <- list.dirs(backup_base_dir, recursive = FALSE)
  current_time <- Sys.time()
  retention_seconds <- retention_days * 24 * 60 * 60

  cleaned_count <- 0

  for (backup_dir in backup_dirs) {
    dir_info <- file.info(backup_dir)
    if (!is.na(dir_info$mtime)) {
      age_seconds <- as.numeric(difftime(current_time, dir_info$mtime, units = "secs"))

      if (age_seconds > retention_seconds) {
        tryCatch({
          fs::dir_delete(backup_dir)
          cleaned_count <- cleaned_count + 1
        }, error = function(e) {
          warning("Failed to clean backup: ", backup_dir, " - ", e$message)
        })
      }
    }
  }

  if (cleaned_count > 0) {
    message("🧹 Cleaned ", cleaned_count, " old backup(s)")
  }

  return(TRUE)
}

# Enhanced vignette copying with domain-based logic
copy_vignettes_by_domain <- function(config, main_repo_dir, module_configs) {
  vignette_config <- config$vignette_domains
  copy_settings <- vignette_config$copy_settings

  # Check if domain-based copying is enabled
  if (!copy_settings$use_domain_based) {
    message("ℹ️ Domain-based vignette copying is disabled")
    return(TRUE)
  }

  message("📄 Starting domain-based vignette copying...")

  # Get all vignette files
  vignette_files <- c()
  vignette_dir <- file.path(main_repo_dir, "vignettes")

  if (!dir.exists(vignette_dir)) {
    warning("Vignettes directory does not exist: ", vignette_dir)
    return(FALSE)
  }

  for (ext in vignette_config$extensions) {
    pattern <- paste0("\\", ext, "$")
    files <- list.files(
      path = vignette_dir,
      pattern = pattern,
      full.names = FALSE
    )
    vignette_files <- c(vignette_files, files)
  }

  if (length(vignette_files) == 0) {
    message("ℹ️ No vignette files found")
    return(TRUE)
  }

  message("📊 Found ", length(vignette_files), " vignette files")

  # Track copying statistics
  copy_stats <- list(
    total_files = length(vignette_files),
    copied_files = 0,
    skipped_files = 0,
    error_files = 0,
    excluded_files = 0
  )

  # Process each vignette file
  for (vignette_file in vignette_files) {

    # Check exclusion patterns
    if (is_file_excluded(vignette_file, vignette_config$exclude_patterns)) {
      copy_stats$excluded_files <- copy_stats$excluded_files + 1
      next
    }

    target_modules <- get_target_modules_for_vignette(vignette_file, vignette_config)

    if (length(target_modules) == 0) {
      message("⚠️ No target modules found for: ", vignette_file)
      copy_stats$skipped_files <- copy_stats$skipped_files + 1
      next
    }

    # Copy to target modules
    file_copied <- FALSE
    for (module_name in target_modules) {
      if (module_name %in% names(module_configs)) {
        module_dir <- module_configs[[module_name]]$directory

        if (copy_vignette_to_module(
          vignette_file, vignette_dir, module_dir, copy_settings
        )) {
          file_copied <- TRUE
        } else {
          copy_stats$error_files <- copy_stats$error_files + 1
        }
      }
    }

    if (file_copied) {
      copy_stats$copied_files <- copy_stats$copied_files + 1
    }
  }

  # Report statistics
  message("📈 Vignette copying completed:")
  message("   📄 Total files: ", copy_stats$total_files)
  message("   ✅ Copied: ", copy_stats$copied_files)
  message("   ⏭️ Skipped: ", copy_stats$skipped_files)
  message("   🚫 Excluded: ", copy_stats$excluded_files)
  message("   ❌ Errors: ", copy_stats$error_files)

  return(copy_stats$error_files == 0)
}

# Helper function to check if file should be excluded
is_file_excluded <- function(filename, exclude_patterns) {
  for (pattern in exclude_patterns) {
    # Convert shell pattern to regex
    regex_pattern <- glob2rx(pattern)
    if (grepl(regex_pattern, filename)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Helper function to determine target modules for a vignette
get_target_modules_for_vignette <- function(vignette_file, vignette_config) {
  domain_mapping <- vignette_config$domain_mapping
  special_files <- vignette_config$special_files

  # Check special files first
  if (vignette_file %in% names(special_files)) {
    return(special_files[[vignette_file]])
  }

  # Extract domain prefix (everything before first number)
  domain_match <- regexpr("^[a-zA-Z-]+(?=-[0-9])", vignette_file, perl = TRUE)

  if (domain_match > 0) {
    domain_prefix <- substr(vignette_file, 1, domain_match + attr(domain_match, "match.length") - 1)

    if (domain_prefix %in% names(domain_mapping)) {
      return(domain_mapping[[domain_prefix]])
    }
  }

  # Check for module-specific patterns without numbers
  for (domain in names(domain_mapping)) {
    if (startsWith(vignette_file, paste0(domain, "-")) ||
        startsWith(vignette_file, domain)) {
      return(domain_mapping[[domain]])
    }
  }

  return(character(0))
}

# Helper function to copy a single vignette to a module
copy_vignette_to_module <- function(vignette_file, source_dir, module_dir, copy_settings) {
  if (!dir.exists(module_dir)) {
    warning("Module directory does not exist: ", module_dir)
    return(FALSE)
  }

  # Create vignettes directory if needed
  target_vignette_dir <- file.path(module_dir, "vignettes")
  if (copy_settings$create_directories && !dir.exists(target_vignette_dir)) {
    tryCatch({
      fs::dir_create(target_vignette_dir)
    }, error = function(e) {
      warning("Failed to create vignettes directory: ", e$message)
      return(FALSE)
    })
  }

  # Copy the file
  source_path <- file.path(source_dir, vignette_file)
  target_path <- file.path(target_vignette_dir, vignette_file)

  # Check if target exists and overwrite setting
  if (file.exists(target_path) && !copy_settings$overwrite_existing) {
    return(TRUE)  # Skip but don't treat as error
  }

  tryCatch({
    fs::file_copy(
      path = source_path,
      new_path = target_path,
      overwrite = copy_settings$overwrite_existing
    )
    return(TRUE)
  }, error = function(e) {
    warning("Error copying ", vignette_file, " to ", basename(module_dir), ": ", e$message)
    return(FALSE)
  })
}

# Enhanced vignette copying with both domain-based and manual options
copy_vignettes_enhanced <- function(config, main_repo_dir, module_configs) {
  vignette_config <- config$vignette_domains
  copy_settings <- vignette_config$copy_settings

  success <- TRUE

  # Domain-based copying
  if (copy_settings$use_domain_based) {
    success <- copy_vignettes_by_domain(config, main_repo_dir, module_configs) && success
  }

  # Manual copying (if enabled)
  if (copy_settings$use_manual_lists) {
    success <- copy_vignettes_manual(config, main_repo_dir, module_configs) && success
  }

  return(success)
}

# Legacy manual vignette copying (kept for backward compatibility)
copy_vignettes_manual <- function(config, main_repo_dir, module_configs) {
  message("📄 Starting manual vignette copying...")

  vignette_dir <- file.path(main_repo_dir, "vignettes")
  success <- TRUE

  for (module_name in names(module_configs)) {
    module_config <- module_configs[[module_name]]

    if (length(module_config$vignette_files) == 0) {
      next
    }

    module_dir <- module_config$directory
    target_vignette_dir <- file.path(module_dir, "vignettes")

    # Create directory if needed
    if (!dir.exists(target_vignette_dir)) {
      fs::dir_create(target_vignette_dir)
    }

    # Copy each specified vignette file
    for (vignette_file in module_config$vignette_files) {
      source_path <- file.path(vignette_dir, vignette_file)
      target_path <- file.path(target_vignette_dir, vignette_file)

      if (file.exists(source_path)) {
        tryCatch({
          fs::file_copy(source_path, target_path, overwrite = TRUE)
        }, error = function(e) {
          warning("Error copying ", vignette_file, " to ", module_name, ": ", e$message)
          success <- FALSE
        })
      } else {
        warning("Vignette file not found: ", source_path)
        success <- FALSE
      }
    }
  }

  return(success)
}

# Testing integration: Run module tests
run_module_tests <- function(module_dir, test_level = "basic") {
  if (!dir.exists(module_dir)) {
    warning("Module directory does not exist: ", module_dir)
    return(FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  tryCatch({
    setwd(module_dir)

    # Basic tests: Check if package can be loaded
    if (test_level %in% c("basic", "full")) {
      message("🧪 Running basic tests for ", basename(module_dir))

      # Try to document the package
      devtools::document()

      # Try to prepare jamovi module
      if (dir.exists("jamovi")) {
        jmvtools::prepare()
      }
    }

    # Full tests: Run testthat tests if they exist
    if (test_level == "full" && dir.exists("tests")) {
      message("🧪 Running full test suite for ", basename(module_dir))
      devtools::test()
    }

    message("✅ Tests passed for ", basename(module_dir))
    return(TRUE)

  }, error = function(e) {
    warning("❌ Tests failed for ", basename(module_dir), ": ", e$message)
    return(FALSE)
  })
}

# Performance: Check if file is newer
is_file_newer <- function(source, dest) {
  if (!file.exists(dest)) {
    return(TRUE)
  }

  if (!file.exists(source)) {
    warning("Source file does not exist: ", source)
    return(FALSE)
  }

  source_time <- file.mtime(source)
  dest_time <- file.mtime(dest)

  return(source_time > dest_time)
}

# Performance: Copy file only if newer
copy_if_newer <- function(source, dest, overwrite = TRUE) {
  if (!file.exists(source)) {
    warning("Source file does not exist: ", source)
    return(FALSE)
  }

  # Create destination directory if it doesn't exist
  dest_dir <- dirname(dest)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  if (is_file_newer(source, dest)) {
    tryCatch({
      fs::file_copy(source, dest, overwrite = overwrite)
      return(TRUE)
    }, error = function(e) {
      warning("Failed to copy file ", source, " to ", dest, ": ", e$message)
      return(FALSE)
    })
  }

  return(FALSE)  # File was not copied (not newer)
}

# Enhanced file copying with validation
safe_copy_files <- function(source_files, dest_dir, check_integrity = TRUE, max_size_mb = 100) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  copied_count <- 0
  skipped_count <- 0
  failed_count <- 0

  for (source_file in source_files) {
    if (!file.exists(source_file)) {
      warning("Source file does not exist: ", source_file)
      failed_count <- failed_count + 1
      next
    }

    # Validate file size
    if (!validate_file_size(source_file, max_size_mb)) {
      warning("Skipping large file: ", source_file)
      skipped_count <- skipped_count + 1
      next
    }

    dest_file <- file.path(dest_dir, basename(source_file))

    # Check if copy is needed
    if (check_integrity && verify_file_integrity(source_file, dest_file)) {
      skipped_count <- skipped_count + 1
      next
    }

    # Copy file
    if (copy_if_newer(source_file, dest_file)) {
      copied_count <- copied_count + 1
    } else {
      skipped_count <- skipped_count + 1
    }
  }

  message("📁 File copy summary: ", copied_count, " copied, ",
          skipped_count, " skipped, ", failed_count, " failed")

  return(list(
    copied = copied_count,
    skipped = skipped_count,
    failed = failed_count
  ))
}

# Load configuration from YAML
load_config <- function(config_file = "updateModules_config.yaml") {
  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package is required for configuration loading")
  }

  tryCatch({
    config <- yaml::read_yaml(config_file)
    message("✅ Configuration loaded from: ", config_file)
    return(config)
  }, error = function(e) {
    stop("Failed to load configuration: ", e$message)
  })
}

# Validate configuration
validate_config <- function(config) {
  # Check for either simplified format (top-level) or nested format
  has_top_level_version <- "new_version" %in% names(config)
  has_nested_global <- "global" %in% names(config) && "new_version" %in% names(config$global)
  
  if (!has_top_level_version && !has_nested_global) {
    stop("Missing version configuration - need either top-level 'new_version' or 'global.new_version'")
  }
  
  # Check for modules section (always required)
  if (!"modules" %in% names(config)) {
    stop("Missing required configuration section: modules")
  }

  # Validate version and date (simplified format takes precedence)
  if (has_top_level_version) {
    if (!"new_date" %in% names(config)) {
      stop("Missing required setting: new_date")
    }
  } else if (has_nested_global) {
    global <- config$global
    required_global <- c("new_version", "new_date")
    
    for (setting in required_global) {
      if (!setting %in% names(global)) {
        stop("Missing required global setting: ", setting)
      }
    }
  }

  # Validate base directory exists (get from either format)
  base_repo_dir <- if ("global" %in% names(config) && "base_repo_dir" %in% names(config$global)) {
    config$global$base_repo_dir
  } else {
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule"  # Default fallback
  }
  
  if (!dir.exists(base_repo_dir)) {
    stop("Base repository directory does not exist: ", base_repo_dir)
  }

  # Validate modules
  if (length(config$modules) == 0) {
    stop("No modules defined in configuration")
  }

  for (module_name in names(config$modules)) {
    module <- config$modules[[module_name]]

    if (!"directory" %in% names(module)) {
      stop("Module ", module_name, " missing 'directory' setting")
    }
  }

  message("✅ Configuration validated successfully")
  return(config)
}

# Enhanced error handling wrapper
with_error_handling <- function(expr, description = "operation", continue_on_error = FALSE) {
  tryCatch({
    result <- expr
    return(list(success = TRUE, result = result, error = NULL))
  }, error = function(e) {
    error_msg <- paste("Failed", description, ":", e$message)

    if (continue_on_error) {
      warning("⚠️ ", error_msg)
      return(list(success = FALSE, result = NULL, error = e$message))
    } else {
      stop("❌ ", error_msg)
    }
  })
}

# Parallel processing setup
setup_parallel_processing <- function(enabled = FALSE, max_workers = 4) {
  if (!enabled) {
    return(FALSE)
  }

  if (!requireNamespace("future", quietly = TRUE)) {
    warning("future package not available, parallel processing disabled")
    return(FALSE)
  }

  library(future)

  # Determine number of workers
  available_cores <- future::availableCores()
  workers <- min(max_workers, available_cores - 1, 8)  # Leave one core free, max 8

  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    message("🚀 Parallel processing enabled with ", workers, " workers")
    return(TRUE)
  } else {
    message("ℹ️ Parallel processing not beneficial, using sequential processing")
    return(FALSE)
  }
}

# =============================================================================
# Dependency reconciliation (P0.2) and distribution coverage (P1.6) checks
# -----------------------------------------------------------------------------
# Rationale: the distributed submodule DESCRIPTIONs are hand-maintained and the
# existing NAMESPACE->DESCRIPTION sync (sync_namespace_with_description) is driven
# by the NAMESPACE file, which only records roxygen @import/@importFrom directives.
# It therefore CANNOT see `pkg::fun()`-style namespaced calls. Real defects have
# shipped this way: jsurvival used cmprsk::cuminc() (hard crash) and meddecide used
# vcd::Kappa()/lme4::lmer() (silent statistical degradation / dead feature) while
# those packages were absent from the submodule Imports. jamovi installs only a
# submodule's Imports, so end users crashed even though the umbrella was fine.
#
# check_module_dependencies() closes that gap by walking each parsed R expression,
# so package-like text in comments and strings is ignored. Unguarded namespace or
# package-attachment use must be declared in Imports/Depends. Use that is proven
# optional by lexical requireNamespace() control flow may instead be in Suggests.
# Only Priority: base packages are implicit; Recommended and transitive packages
# still require a direct declaration.
# =============================================================================

# Only Priority: base packages are implicit. Priority: recommended packages (for
# example MASS and boot) still need direct DESCRIPTION declarations when used.
get_base_packages <- function() {
  fallback <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                "methods", "parallel", "splines", "stats", "stats4", "tcltk",
                "tools", "utils")
  out <- tryCatch({
    ip <- utils::installed.packages()
    prio <- ip[, "Priority"]
    base <- rownames(ip)[!is.na(prio) & prio == "base"]
    unique(c(fallback, base))
  }, error = function(e) fallback)
  out
}

# Extract packages referenced through `pkg::` / `pkg:::` and classify each use by
# lexical control flow. A use is optional only inside the true branch of a positive
# requireNamespace("pkg") check, or after a terminal negative guard such as
# `if (!requireNamespace("pkg")) return()` in the same block.
scan_r_package_usage <- function(r_dir) {
  required <- character(0)
  guarded <- character(0)
  parse_errors <- character(0)
  empty <- list(
    required = required,
    guarded = guarded,
    used = character(0),
    parse_errors = parse_errors
  )
  if (!dir.exists(r_dir)) return(empty)

  call_name <- function(expr) {
    if (!is.call(expr)) return(NA_character_)
    head <- expr[[1]]
    if (is.symbol(head)) return(as.character(head))
    if (is.call(head) && identical(head[[1]], as.name("::")) &&
        as.character(head[[2]]) == "base") {
      return(as.character(head[[3]]))
    }
    NA_character_
  }

  literal_package_arg <- function(expr) {
    if (!is.call(expr) || length(expr) < 2) return(character(0))
    args <- as.list(expr[-1])
    arg_names <- names(args)
    package_index <- match("package", arg_names)
    if (is.na(package_index)) package_index <- 1L
    arg <- args[[package_index]]
    if (is.character(arg) && length(arg) == 1) return(arg)
    character_only_index <- match("character.only", arg_names)
    character_only <- !is.na(character_only_index) &&
      isTRUE(args[[character_only_index]])
    if (is.symbol(arg) && !character_only) return(as.character(arg))
    character(0)
  }

  require_namespace_pkg <- function(expr) {
    if (!is.call(expr) || !identical(call_name(expr), "requireNamespace")) {
      return(character(0))
    }
    args <- as.list(expr[-1])
    if (length(args) == 0) return(character(0))
    package_index <- match("package", names(args))
    if (is.na(package_index)) package_index <- 1L
    arg <- args[[package_index]]
    if (is.character(arg) && length(arg) == 1) arg else character(0)
  }

  common <- function(x, y) intersect(unique(x), unique(y))

  available_when_true <- NULL
  available_when_false <- NULL

  available_when_true <- function(expr) {
    if (!is.call(expr)) return(character(0))
    pkg <- require_namespace_pkg(expr)
    if (length(pkg) > 0) return(pkg)

    head <- expr[[1]]
    name <- call_name(expr)
    if (identical(head, as.name("(")) || identical(name, "isTRUE")) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(name, "isFALSE")) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(head, as.name("!"))) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(head, as.name("&&")) || identical(head, as.name("&"))) {
      return(unique(c(
        available_when_true(expr[[2]]),
        available_when_true(expr[[3]])
      )))
    }
    if (identical(head, as.name("||")) || identical(head, as.name("|"))) {
      return(common(
        available_when_true(expr[[2]]),
        available_when_true(expr[[3]])
      ))
    }
    character(0)
  }

  available_when_false <- function(expr) {
    if (!is.call(expr)) return(character(0))

    head <- expr[[1]]
    name <- call_name(expr)
    if (identical(head, as.name("(")) || identical(name, "isTRUE")) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(name, "isFALSE")) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(head, as.name("!"))) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(head, as.name("&&")) || identical(head, as.name("&"))) {
      return(common(
        available_when_false(expr[[2]]),
        available_when_false(expr[[3]])
      ))
    }
    if (identical(head, as.name("||")) || identical(head, as.name("|"))) {
      return(unique(c(
        available_when_false(expr[[2]]),
        available_when_false(expr[[3]])
      )))
    }
    character(0)
  }

  is_terminal <- function(expr) {
    if (!is.call(expr)) return(FALSE)
    head <- expr[[1]]
    if (identical(head, as.name("{"))) {
      return(length(expr) >= 2 && is_terminal(expr[[length(expr)]]))
    }
    if (identical(head, as.name("if"))) {
      return(length(expr) >= 4 &&
             is_terminal(expr[[3]]) && is_terminal(expr[[4]]))
    }
    name <- call_name(expr)
    identical(name, "return") || identical(name, "stop") ||
      (is.call(head) && identical(head[[1]], as.name("::")) &&
       as.character(head[[2]]) == "jmvcore" &&
       as.character(head[[3]]) == "reject")
  }

  continuation_guards <- function(expr) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("if"))) {
      return(character(0))
    }
    true_terminal <- is_terminal(expr[[3]])
    false_terminal <- length(expr) >= 4 && is_terminal(expr[[4]])
    if (true_terminal && !false_terminal) {
      return(available_when_false(expr[[2]]))
    }
    if (false_terminal && !true_terminal) {
      return(available_when_true(expr[[2]]))
    }
    character(0)
  }

  record_package <- function(pkg, active_guards) {
    if (length(pkg) != 1 || is.na(pkg) || !nzchar(pkg)) return(invisible(NULL))
    if (pkg %in% active_guards) guarded <<- c(guarded, pkg)
    else required <<- c(required, pkg)
    invisible(NULL)
  }

  walk <- NULL
  walk_condition <- function(expr, active_guards) {
    if (!is.call(expr)) return(invisible(NULL))
    head <- expr[[1]]
    if (identical(head, as.name("&&"))) {
      walk(expr[[2]], active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_true(expr[[2]])
      )))
      return(invisible(NULL))
    }
    if (identical(head, as.name("||"))) {
      walk(expr[[2]], active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_false(expr[[2]])
      )))
      return(invisible(NULL))
    }
    walk(expr, active_guards)
  }

  walk <- function(expr, active_guards = character(0)) {
    if (!is.call(expr)) return(invisible(NULL))

    head <- expr[[1]]
    if (identical(head, as.name("::")) || identical(head, as.name(":::"))) {
      record_package(as.character(expr[[2]]), active_guards)
      return(invisible(NULL))
    }

    name <- call_name(expr)
    if (name %in% c("library", "require")) {
      record_package(literal_package_arg(expr), active_guards)
    }

    if (identical(head, as.name("if"))) {
      condition <- expr[[2]]
      walk_condition(condition, active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_true(condition)
      )))
      if (length(expr) >= 4) {
        walk(expr[[4]], unique(c(
          active_guards,
          available_when_false(condition)
        )))
      }
      return(invisible(NULL))
    }

    if (identical(head, as.name("{"))) {
      block_guards <- active_guards
      if (length(expr) >= 2) {
        for (i in 2:length(expr)) {
          walk(expr[[i]], block_guards)
          block_guards <- unique(c(
            block_guards,
            continuation_guards(expr[[i]])
          ))
        }
      }
      return(invisible(NULL))
    }

    if (identical(head, as.name("&&")) || identical(head, as.name("||"))) {
      walk_condition(expr, active_guards)
      return(invisible(NULL))
    }

    for (i in seq_along(expr)) walk(expr[[i]], active_guards)
    invisible(NULL)
  }

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in r_files) {
    parsed <- tryCatch(
      parse(f, keep.source = FALSE),
      error = function(e) {
        parse_errors <<- c(
          parse_errors,
          paste0(basename(f), ": ", conditionMessage(e))
        )
        NULL
      }
    )
    if (!is.null(parsed)) for (expr in parsed) walk(expr)
  }

  required <- unique(required)
  guarded <- setdiff(unique(guarded), required)
  list(
    required = required,
    guarded = guarded,
    used = unique(c(required, guarded)),
    parse_errors = unique(parse_errors)
  )
}

# Packages declared in a DESCRIPTION, keeping runtime requirements separate
# from optional Suggests.
get_description_dependencies <- function(desc_file) {
  empty <- list(required = character(0), optional = character(0))
  if (!file.exists(desc_file)) return(empty)
  dcf <- tryCatch(read.dcf(desc_file), error = function(e) NULL)
  if (is.null(dcf)) return(empty)

  parse_fields <- function(fields) {
    fields <- intersect(fields, colnames(dcf))
    if (length(fields) == 0) return(character(0))
    vals <- unlist(lapply(fields, function(field) dcf[1, field]))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(character(0))
    packages <- unlist(strsplit(paste(vals, collapse = ","), ","))
    packages <- trimws(gsub("\\s*\\([^)]*\\)", "", packages))
    unique(packages[nchar(packages) > 0 & packages != "R"])
  }

  list(
    required = parse_fields(c("Imports", "Depends")),
    optional = parse_fields("Suggests")
  )
}

# Check a single module directory. Unguarded usage requires Imports/Depends;
# lexically guarded usage may be declared in Imports/Depends or Suggests.
check_module_dependencies <- function(module_dir, module_name = basename(module_dir),
                                       base_packages = get_base_packages()) {
  r_dir <- file.path(module_dir, "R")
  desc_file <- file.path(module_dir, "DESCRIPTION")

  usage <- scan_r_package_usage(r_dir)
  dependencies <- get_description_dependencies(desc_file)
  package_name <- tryCatch(
    read.dcf(desc_file)[1, "Package"],
    error = function(e) module_name
  )
  if (is.na(package_name) || !nzchar(package_name)) package_name <- module_name

  ignore <- unique(c(base_packages, package_name))
  required_missing <- setdiff(
    usage$required,
    c(dependencies$required, ignore)
  )
  optional_missing <- setdiff(
    usage$guarded,
    c(dependencies$required, dependencies$optional, ignore)
  )

  list(module = module_name,
       errors = sort(required_missing),
       warnings = sort(optional_missing),
       parse_errors = usage$parse_errors,
       used = usage$used)
}

# Aggregate check across modules. `module_specs` is a named list: name -> dir.
# Prints a report and stops for any declaration or source-parse violation.
check_all_modules_dependencies <- function(module_specs, fail_on_error = TRUE) {
  cat("\n🔎 Reconciling submodule dependencies (pkg:: usage vs DESCRIPTION)...\n")
  base <- get_base_packages()
  any_errors <- FALSE

  for (nm in names(module_specs)) {
    dir <- module_specs[[nm]]
    if (is.null(dir) || !dir.exists(dir)) {
      cat("  ⏭️  ", nm, ": directory not found, skipping\n", sep = "")
      next
    }
    res <- check_module_dependencies(dir, nm, base)
    if (length(res$errors) == 0 && length(res$warnings) == 0 &&
        length(res$parse_errors) == 0) {
      cat("  ✅ ", nm, ": all used packages declared\n", sep = "")
    }
    if (length(res$parse_errors) > 0) {
      any_errors <- TRUE
      cat("  ❌ ", nm, ": could not parse R source: ",
          paste(res$parse_errors, collapse = "; "), "\n", sep = "")
    }
    if (length(res$warnings) > 0) {
      any_errors <- TRUE
      cat("  ⚠️  ", nm, ": used behind requireNamespace() but NOT declared (add to Imports; ",
          "jamovi installs Imports first-run, so runtime deps must NOT sit in Suggests): ",
          paste(res$warnings, collapse = ", "), "\n", sep = "")
    }
    if (length(res$errors) > 0) {
      any_errors <- TRUE
      cat("  ❌ ", nm, ": unguarded package use is not in Imports/Depends: ",
          paste(res$errors, collapse = ", "), "\n", sep = "")
    }
  }

  if (any_errors && fail_on_error) {
    stop("❌ Dependency reconciliation failed: one or more submodules use packages ",
         "without a direct DESCRIPTION declaration. Add every runtime dependency ",
         "(unguarded AND requireNamespace-guarded) to Imports/Depends -- jamovi ",
         "installs Imports first-run and cannot install a missing package on demand, ",
         "so Suggests is not a valid home for runtime deps.")
  }
  invisible(!any_errors)
}

# -----------------------------------------------------------------------------
# Distribution coverage (P1.6): assert every production analysis is routed to
# exactly one submodule; surface analyses that route nowhere or to >1 module,
# and analyses parked in dev/test (…T) or undistributed (…D) buckets. This is a
# REPORT by default (warn, not stop) because the …D staging convention legitimately
# leaves many analyses umbrella-only; set fail_on_gap=TRUE to harden a release build.
# -----------------------------------------------------------------------------
check_distribution_coverage <- function(all_analyses, module_modules,
                                        fail_on_gap = FALSE) {
  cat("\n🗺️  Checking distribution coverage (analysis -> submodule routing)...\n")
  all_analyses <- unique(all_analyses)

  # Which module(s) claim each analysis
  claim_count <- setNames(integer(length(all_analyses)), all_analyses)
  duplicates <- list()
  for (nm in names(module_modules)) {
    claimed <- intersect(module_modules[[nm]], all_analyses)
    for (a in claimed) claim_count[[a]] <- claim_count[[a]] + 1L
  }
  distributed <- names(claim_count)[claim_count >= 1L]
  unrouted <- names(claim_count)[claim_count == 0L]
  multi <- names(claim_count)[claim_count >= 2L]

  cat("  📊 ", length(distributed), "/", length(all_analyses),
      " production analyses routed to a submodule\n", sep = "")

  if (length(multi) > 0) {
    for (a in multi) {
      owners <- names(module_modules)[vapply(module_modules,
                                             function(v) a %in% v, logical(1))]
      cat("  ❗ '", a, "' routed to MULTIPLE submodules: ",
          paste(owners, collapse = ", "), "\n", sep = "")
    }
  }
  if (length(unrouted) > 0) {
    cat("  ℹ️  ", length(unrouted), " analyses route to NO submodule (umbrella-only). ",
        "First few: ", paste(utils::head(unrouted, 8), collapse = ", "),
        if (length(unrouted) > 8) ", ..." else "", "\n", sep = "")
  }

  if ((length(multi) > 0 || (fail_on_gap && length(unrouted) > 0))) {
    if (length(multi) > 0)
      stop("❌ Distribution coverage failed: analyses routed to more than one submodule (see above).")
    if (fail_on_gap)
      stop("❌ Distribution coverage failed: analyses routed to no submodule (fail_on_gap=TRUE).")
  }
  invisible(list(distributed = distributed, unrouted = unrouted, multi = multi))
}

# =============================================================================
# Test distribution & infrastructure (P1.4 / P1.5)
# -----------------------------------------------------------------------------
# The umbrella has a rich test suite but none of it was shipped to submodules
# (copy_test_files was off and every module's test_files list was empty), so the
# dependency regressions above shipped with no CI net. These helpers (a) generate
# a tests/testthat.R runner so any distributed tests actually run under
# devtools::test()/R CMD check, (b) install a self-contained dependency-guard test
# that is the runtime twin of check_module_dependencies(), and (c) provide a
# name-keyed copier so the umbrella's `test-<analysis>*.R` files can be distributed.
# =============================================================================

# Write tests/testthat.R (the standard testthat runner) if the module lacks one.
ensure_testthat_runner <- function(module_dir) {
  desc_file <- file.path(module_dir, "DESCRIPTION")
  if (!file.exists(desc_file)) return(invisible(FALSE))
  pkg_name <- tryCatch(read.dcf(desc_file)[1, "Package"], error = function(e) NA_character_)
  if (is.na(pkg_name)) return(invisible(FALSE))

  tests_dir <- file.path(module_dir, "tests")
  if (!dir.exists(tests_dir)) dir.create(tests_dir, recursive = TRUE)
  runner <- file.path(tests_dir, "testthat.R")
  if (!file.exists(runner)) {
    writeLines(c(
      "library(testthat)",
      paste0("library(", pkg_name, ")"),
      "",
      paste0("test_check(\"", pkg_name, "\")")
    ), runner)
    cat("  🧪 Generated tests/testthat.R runner for ", pkg_name, "\n", sep = "")
  }
  invisible(TRUE)
}

# Copy the self-contained dependency-guard test into a submodule (always refreshed).
write_dependency_guard_test <- function(module_dir, template_path) {
  if (!file.exists(template_path)) {
    warning("Dependency-guard test template not found: ", template_path)
    return(invisible(FALSE))
  }
  dest_dir <- file.path(module_dir, "tests", "testthat")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  fs::file_copy(template_path,
                file.path(dest_dir, "test-zzz-dependency-declaration.R"),
                overwrite = TRUE)
  cat("  🛡️  Installed dependency-guard test in ", basename(module_dir), "\n", sep = "")
  invisible(TRUE)
}

# Distribute the umbrella's per-analysis tests (test-<name>.R, test-<name>-*.R) for
# a set of analysis names. Returns the vector of copied file basenames. Anchored so
# 'survival' does not also match 'survivalcont'. When `module_name` is supplied the
# copied tests are namespace-translated (ClinicoPath -> module_name) so they run
# against the submodule package -- self-contained, so it does NOT depend on the
# separately-gated replace_clinicopath_with_module()/webpage step.
copy_module_tests <- function(module_names, source_test_dir, dest_test_dir,
                              module_name = NULL) {
  if (!dir.exists(source_test_dir) || length(module_names) == 0)
    return(character(0))
  if (!dir.exists(dest_test_dir)) dir.create(dest_test_dir, recursive = TRUE)

  # testthat auto-loads helper-*.R from tests/testthat before running any test, so a
  # helper is part of its analysis's suite, not an optional extra. Globbing only
  # "^test-" shipped tests whose shared setup was left behind -- e.g.
  # helper-decisioncompare.R defines call_decisioncompare(), without which every
  # copied test-decisioncompare*.R fails with "could not find function".
  all_tests <- list.files(source_test_dir, pattern = "^(test|helper)-.*\\.R$")
  copied <- character(0)
  for (nm in module_names) {
    pat <- paste0("^(test|helper)-", nm, "(\\.R$|[.-])")
    hits <- all_tests[grepl(pat, all_tests, ignore.case = FALSE)]
    for (h in hits) {
      dest <- file.path(dest_test_dir, h)
      if (is.null(module_name)) {
        fs::file_copy(file.path(source_test_dir, h), dest, overwrite = TRUE)
      } else {
        txt <- readLines(file.path(source_test_dir, h), warn = FALSE)
        txt <- gsub("library(ClinicoPath)", paste0("library(", module_name, ")"), txt, fixed = TRUE)
        txt <- gsub("ClinicoPath::", paste0(module_name, "::"), txt, fixed = TRUE)
        txt <- gsub('package = "ClinicoPath"', paste0('package = "', module_name, '"'), txt, fixed = TRUE)
        txt <- gsub("package = 'ClinicoPath'", paste0("package = '", module_name, "'"), txt, fixed = TRUE)
        writeLines(txt, dest)
      }
      copied <- c(copied, h)
    }
  }
  unique(copied)
}

message("✅ Module utilities loaded successfully")

# ---------------------------------------------------------------------------
# Prune orphaned analyses from a submodule's jamovi/0000.yaml
# ---------------------------------------------------------------------------
# jmvtools::prepare() MERGES into 0000.yaml rather than rebuilding it, so an
# analysis stays listed forever once written -- including after it is re-routed
# out of the submodule (menuGroup gets a T/D suffix and its files stop being
# copied). The jamovi compiler then emits exports for classes that no longer
# exist and the install dies with:
#     undefined exports: clinicalscoreClass, clinicalscoreOptions, ...
# That is exactly what happened to meddecide with 7 T-routed analyses.
#
# Called before prepare(), this drops any analyses: entry that has neither a
# jamovi/<name>.a.yaml nor an R/<name>.b.R in the target module.
prune_orphan_analyses <- function(module_dir) {
  zero <- file.path(module_dir, "jamovi", "0000.yaml")
  if (!file.exists(zero)) return(invisible(0L))

  lines <- readLines(zero, warn = FALSE)
  start <- which(trimws(lines) == "analyses:")
  if (length(start) != 1L) return(invisible(0L))
  after <- which(grepl("^[A-Za-z]", lines))
  end <- after[after > start]
  end <- if (length(end)) end[1] else (length(lines) + 1L)

  avail_yaml <- tolower(list.files(file.path(module_dir, "jamovi"), pattern = "\\.a\\.yaml$"))
  avail_r    <- tolower(list.files(file.path(module_dir, "R"), pattern = "\\.b\\.R$"))

  starts <- which(grepl("^  - ", lines))
  starts <- starts[starts > start & starts < end]
  if (!length(starts)) return(invisible(0L))
  bounds <- c(starts, end)

  keep <- lines[seq_len(start)]
  dropped <- character(0)
  for (i in seq_along(starts)) {
    block <- lines[bounds[i]:(bounds[i + 1L] - 1L)]
    nm <- sub("^\\s*name:\\s*", "", grep("^\\s*name:\\s*\\S+\\s*$", block, value = TRUE)[1])
    nm <- trimws(nm %||% "")
    # Match case-INSENSITIVELY. 0000.yaml carries the analysis name as declared
    # (kappaSizePower, enhancedROC, psychopdaROC) while the files on disk are lower
    # case (kappasizepower.a.yaml). file.exists() happens to succeed on macOS because
    # HFS+/APFS is case-insensitive, but on Linux it would return FALSE and this
    # function would delete perfectly good analyses from the module.
    has_src <- nzchar(nm) &&
      (tolower(paste0(nm, ".a.yaml")) %in% avail_yaml ||
       tolower(paste0(nm, ".b.R")) %in% avail_r)
    if (isTRUE(has_src)) keep <- c(keep, block) else dropped <- c(dropped, nm)
  }
  keep <- c(keep, lines[end:length(lines)])

  if (length(dropped)) {
    writeLines(keep, zero)
    cat(sprintf("  \U0001F9F9 Pruned %d orphaned analysis %s from 0000.yaml: %s\n",
                length(dropped), if (length(dropped) == 1) "entry" else "entries",
                paste(dropped, collapse = ", ")))
  }
  invisible(length(dropped))
}
