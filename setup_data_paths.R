# setup_data_paths.R
# Automatic, user-friendly data path detection with manual override option

check_and_offer_rebuild <- function() {
  last_path_file <- file.path("_targets", ".last_data_path")
  
  # Get current configured path if it exists
  current_path <- NULL
  if (file.exists("data_config.R")) {
    tryCatch({
      source("data_config.R", local = TRUE)
      if (exists("data_path", envir = environment())) {
        current_path <- get("data_path", envir = environment())
      }
    }, error = function(e) {
      # Config file is broken, continue
    })
  }
  
  # Check if we have a previous path recorded
  if (file.exists(last_path_file) && !is.null(current_path)) {
    last_path <- tryCatch({
      trimws(readLines(last_path_file, warn = FALSE)[1])
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(last_path) && last_path != current_path && last_path != "") {
      cat("Data path changed:\n")
      cat("  Previous:", last_path, "\n")  
      cat("  Current: ", current_path, "\n\n")
      
      # Check if we're in a targets pipeline context
      in_targets <- !is.null(getOption("targets_in_progress")) || 
                   exists(".targets_envir", envir = .GlobalEnv) ||
                   any(grepl("tar_make", sys.calls()))
      
      if (interactive() && !in_targets) {
        response <- tolower(readline("Do you want to force a full rebuild? (y/n): "))
        if (response %in% c("y", "yes")) {
          cat("Forcing full rebuild...\n")
          if (requireNamespace("targets", quietly = TRUE)) {
            targets::tar_destroy()
            cat("✓ All targets cleared. Will rebuild from scratch.\n\n")
          } else {
            cat("Note: Install 'targets' package to enable automatic rebuild.\n")
            cat("You can manually run: tar_destroy() then tar_make()\n\n")
          }
        } else {
          cat("Continuing with current targets state.\n")
          cat("Note: Some targets may not update if data files are identical.\n\n")
        }
      } else {
        if (in_targets) {
          cat("Data path changed during tar_make() - cannot prompt for rebuild.\n")
          cat("To force rebuild: Stop tar_make(), run tar_destroy(), then tar_make() again.\n")
          cat("Continuing with current targets state for now.\n\n")
        } else {
          cat("Running in non-interactive mode - continuing with current targets state.\n")
          cat("To force rebuild, run: tar_destroy() then tar_make()\n\n")
        }
      }
    }
  }
  
  # Save current path for next time (will be updated after path is determined)
  if (!is.null(current_path)) {
    save_last_data_path(current_path)
  }
}

save_last_data_path <- function(data_path) {
  last_path_file <- file.path("_targets", ".last_data_path")
  dir.create("_targets", showWarnings = FALSE, recursive = TRUE)
  writeLines(data_path, last_path_file)
}

find_data_path <- function() {
  # Check if data path has changed and offer rebuild
  check_and_offer_rebuild()
  
  # Check if we already have a saved config
  if (file.exists("data_config.R")) {
    tryCatch({
      source("data_config.R")
      if (exists("data_path") && dir.exists(data_path)) {
        cat("Using configured data path:", data_path, "\n")
        return(data_path)
      }
    }, error = function(e) {
      # Config exists but is broken, continue to auto-detection
      cat("Existing config is invalid, searching for data folder...\n")
    })
  }
  
  cat("Searching for data folder...\n")
  
  # Search locations automatically
  search_locations <- get_search_locations()
  found_paths <- c()
  
  for (location in search_locations) {
    if (dir.exists(location)) {
      found_paths <- c(found_paths, location)
    }
  }
  
  if (length(found_paths) == 0) {
    # No data found - ask user to specify
    return(prompt_for_data_path())
  } else if (length(found_paths) == 1) {
    # One path found - use it automatically
    data_path <- found_paths[1]
    cat("✓ Found data at:", data_path, "\n")
    save_data_config(data_path)
    return(data_path)
  } else {
    # Multiple paths found - let user choose or pick the first one
    cat("Multiple data folders found:\n")
    for (i in seq_along(found_paths)) {
      cat(sprintf("  %d) %s\n", i, found_paths[i]))
    }
    
    # In non-interactive mode, just use the first one
    if (!interactive()) {
      data_path <- found_paths[1]
      cat("Using first option:", data_path, "\n")
      save_data_config(data_path)
      return(data_path)
    }
    
    # Interactive mode - let user choose
    cat(sprintf("  %d) Specify different path\n", length(found_paths) + 1))
    
    while (TRUE) {
      choice <- readline("Select option (or press Enter for option 1): ")
      if (choice == "") choice <- "1"
      
      choice_num <- as.numeric(choice)
      if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(found_paths)) {
        data_path <- found_paths[choice_num]
        cat("Using:", data_path, "\n")
        save_data_config(data_path)
        return(data_path)
      } else if (!is.na(choice_num) && choice_num == length(found_paths) + 1) {
        return(prompt_for_data_path())
      } else {
        cat("Please enter a number between 1 and", length(found_paths) + 1, "\n")
      }
    }
  }
}

get_search_locations <- function() {
  # Get system info
  os_name <- Sys.info()[["sysname"]]
  user_home <- Sys.getenv("HOME")
  user_profile <- Sys.getenv("USERPROFILE")
  
  search_paths <- c()
  
  # 1. Local data folder (for users who download data locally)
  search_paths <- c(search_paths, "data")
  
  # 2. Current directory variations
  search_paths <- c(search_paths, c(
    "ca-refining-data",
    "refining-data", 
    "calepa-data",
    "project-data"
  ))
  
  # 3. OS-specific Google Drive locations
  if (os_name == "Darwin") {  # macOS
    base_paths <- c(
      file.path(user_home, "Library", "CloudStorage"),
      file.path(user_home, "Google Drive"),
      file.path(user_home, "GoogleDrive")
    )
  } else if (os_name == "Windows") {
    base_paths <- c(
      file.path(user_profile, "Google Drive"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Google", "Drive"),
      "G:\\My Drive",
      "H:\\",
      file.path("C:", "Users", Sys.getenv("USERNAME"), "Google Drive")
    )
  } else {  # Linux and others
    base_paths <- c(
      file.path(user_home, "Google Drive"),
      file.path(user_home, "GoogleDrive"),
      file.path("/mnt", "googledrive"),
      file.path("/media", Sys.getenv("USER"), "Google Drive")
    )
  }
  
  # 4. Search Google Drive for project-specific patterns
  for (base_path in base_paths) {
    if (dir.exists(base_path)) {
      patterns <- c(
        # Your team's specific structure
        "*GoogleDrive*/.shortcut-targets-by-id/*/calepa-cn/refining-paper/data",
        "*/.shortcut-targets-by-id/*/calepa-cn/refining-paper/data",
        # Alternative patterns
        "*/calepa-cn/refining-paper/data",
        "*/refining-paper/data",
        "*/ca-refining/data",
        "*/ca-refining-data"
      )
      
      for (pattern in patterns) {
        matches <- Sys.glob(file.path(base_path, pattern))
        search_paths <- c(search_paths, matches)
      }
    }
  }
  
  # Remove duplicates and normalize paths
  search_paths <- unique(normalizePath(search_paths[dir.exists(search_paths)], mustWork = FALSE))
  
  return(search_paths)
}

prompt_for_data_path <- function() {
  cat("\n=== Manual Data Path Setup ===\n")
  cat("Please specify the path to your data folder.\n")
  cat("This should contain the input data files for the refining analysis.\n\n")
  
  if (!interactive()) {
    stop("Data folder not found and running in non-interactive mode.\n",
         "Please create a 'data_config.R' file with: data_path <- \"/path/to/data\"")
  }
  
  while (TRUE) {
    path <- readline("Enter full path to data folder: ")
    
    if (path == "") {
      cat("Please enter a path, or press Ctrl+C to exit.\n")
      next
    }
    
    # Clean up path
    path <- normalizePath(path, mustWork = FALSE)
    
    if (dir.exists(path)) {
      cat("✓ Path exists:", path, "\n")
      save_data_config(path)
      return(path)
    } else {
      cat("✗ Path does not exist:", path, "\n")
      retry <- tolower(readline("Try again? (y/n): "))
      if (retry %in% c("n", "no")) {
        stop("Data setup cancelled.")
      }
    }
  }
}

save_data_config <- function(data_path) {
  # Handle path escaping for different OS
  if (Sys.info()[["sysname"]] == "Windows") {
    escaped_path <- gsub("\\\\", "\\\\\\\\", data_path)
  } else {
    escaped_path <- data_path
  }
  
  config_content <- sprintf('
# Auto-generated data configuration
# This file specifies where to find the input data for the refining analysis
# 
# To reconfigure:
#   1. Delete this file, or
#   2. Edit the data_path below, or  
#   3. Run: source("setup_data_paths.R"); find_data_path()

data_path <- "%s"

# Verify path exists when loaded
if (!dir.exists(data_path)) {
  warning("Data folder not found: ", data_path,
          "\\nDelete data_config.R and re-run to reconfigure.")
}

# Print data path (comment out to make silent)
# cat("Data path:", data_path, "\\n")
', escaped_path)
  
  writeLines(config_content, "data_config.R")
  save_last_data_path(data_path)
  cat("✓ Configuration saved to data_config.R\n")
}

# For manual reconfiguration
reconfigure_data_path <- function() {
  if (file.exists("data_config.R")) {
    file.remove("data_config.R")
    cat("Removed existing configuration.\n")
  }
  find_data_path()
}

# Main execution - automatically find data path
main_path <- find_data_path()