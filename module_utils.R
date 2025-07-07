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

# Dependency management: Check module dependencies
check_module_dependencies <- function(module_dir) {
  desc_file <- file.path(module_dir, "DESCRIPTION")

  if (!file.exists(desc_file)) {
    warning("DESCRIPTION file not found: ", desc_file)
    return(FALSE)
  }

  tryCatch({
    desc_content <- read.dcf(desc_file)

    # Check Imports
    if ("Imports" %in% colnames(desc_content)) {
      imports <- desc_content[1, "Imports"]
      if (!is.na(imports)) {
        import_packages <- trimws(strsplit(imports, ",")[[1]])
        import_packages <- gsub("\\(.*\\)", "", import_packages)  # Remove version specs
        import_packages <- trimws(import_packages)  # Trim whitespace again

        missing_imports <- c()
        for (pkg in import_packages) {
          if (nchar(pkg) > 0 && !requireNamespace(pkg, quietly = TRUE)) {
            missing_imports <- c(missing_imports, pkg)
          }
        }

        if (length(missing_imports) > 0) {
          warning("Missing imported packages for ", basename(module_dir), ": ",
                  paste(missing_imports, collapse = ", "))
          return(FALSE)
        }
      }
    }

    # Check Depends
    if ("Depends" %in% colnames(desc_content)) {
      depends <- desc_content[1, "Depends"]
      if (!is.na(depends)) {
        depend_packages <- trimws(strsplit(depends, ",")[[1]])
        depend_packages <- gsub("\\(.*\\)", "", depend_packages)  # Remove version specs
        depend_packages <- trimws(depend_packages)  # Trim whitespace again
        depend_packages <- depend_packages[depend_packages != "R" & nchar(depend_packages) > 0]  # Exclude R itself and empty strings

        missing_depends <- c()
        for (pkg in depend_packages) {
          if (nchar(pkg) > 0 && !requireNamespace(pkg, quietly = TRUE)) {
            missing_depends <- c(missing_depends, pkg)
          }
        }

        if (length(missing_depends) > 0) {
          warning("Missing dependency packages for ", basename(module_dir), ": ",
                  paste(missing_depends, collapse = ", "))
          return(FALSE)
        }
      }
    }

    message("✅ Dependencies validated: ", basename(module_dir))
    return(TRUE)

  }, error = function(e) {
    warning("Failed to check dependencies for ", basename(module_dir), ": ", e$message)
    return(FALSE)
  })
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
  required_sections <- c("global", "modes", "modules")

  for (section in required_sections) {
    if (!section %in% names(config)) {
      stop("Missing required configuration section: ", section)
    }
  }

  # Validate global settings
  global <- config$global
  required_global <- c("new_version", "new_date", "base_repo_dir")

  for (setting in required_global) {
    if (!setting %in% names(global)) {
      stop("Missing required global setting: ", setting)
    }
  }

  # Validate base directory exists
  if (!dir.exists(global$base_repo_dir)) {
    stop("Base repository directory does not exist: ", global$base_repo_dir)
  }

  # Validate modules
  if (length(config$modules) == 0) {
    stop("No modules defined in configuration")
  }

  for (module_name in names(config$modules)) {
    module <- config$modules[[module_name]]

    if (!"enabled" %in% names(module)) {
      warning("Module ", module_name, " missing 'enabled' setting, assuming TRUE")
      config$modules[[module_name]]$enabled <- TRUE
    }

    if (module$enabled && !"directory" %in% names(module)) {
      stop("Enabled module ", module_name, " missing 'directory' setting")
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

message("✅ Module utilities loaded successfully")
