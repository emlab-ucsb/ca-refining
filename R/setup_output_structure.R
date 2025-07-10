# Output Structure Setup Script
# This script helps set up the standardized output directory structure
# and provides guidance on migrating existing files.

#' Create the standardized output directory structure
#' @param version Version identifier (e.g., "v1.0")
#' @param iteration Iteration name within the version (e.g., "run1")
#' @return List of created directories
setup_output_structure <- function(version, iteration) {
    message(
        "Setting up standardized output structure for version ",
        version,
        ", iteration ",
        iteration
    )

    # Define directories to be tracked in git (blue in PDF)
    git_tracked_dirs <- c(
        file.path("outputs", version, iteration),
        file.path("outputs", version, iteration, "figures"),
        file.path("outputs", version, iteration, "figures", "legends"),
        file.path("outputs", version, iteration, "figures", "maps"),
        file.path("outputs", version, iteration, "data"),
        file.path("outputs", version, iteration, "data", "summary-tables")
    )

    # Define directories not to be tracked in git (black in PDF)
    git_ignored_dirs <- c(
        file.path("outputs", version, iteration, "data", "raw-outputs"),
        file.path("outputs", version, iteration, "data", "processed-inputs"),
        file.path("outputs", version, iteration, "health"),
        file.path("outputs", version, iteration, "labor"),
        file.path("outputs", version, iteration, "fig-csv-files")
    )

    # Create all directories
    all_dirs <- c(git_tracked_dirs, git_ignored_dirs)
    for (dir in all_dirs) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
            message("Created directory: ", dir)
        }
    }

    # Create .gitignore files in non-tracked directories
    for (dir in git_ignored_dirs) {
        gitignore_file <- file.path(dir, ".gitignore")
        if (!file.exists(gitignore_file)) {
            writeLines("*\n!.gitignore", gitignore_file)
            message("Created .gitignore in: ", dir)
        }
    }

    # Create README files in main directories to explain their purpose
    readme_main <- file.path("outputs", version, iteration, "README.md")
    if (!file.exists(readme_main)) {
        writeLines(
            paste0(
                "# ",
                version,
                "/",
                iteration,
                " Outputs\n\n",
                "This directory contains outputs from the CA refining model run with the following parameters:\n\n",
                "- **Version:** ",
                version,
                "\n",
                "- **Iteration:** ",
                iteration,
                "\n\n",
                "## Directory Structure\n\n",
                "- `figures/`: Contains main figures and visualizations (tracked in git)\n",
                "  - `legends/`: Figure legends\n",
                "  - `maps/`: Geographic maps and spatial visualizations\n",
                "- `data/`: Contains model data\n",
                "  - `summary-tables/`: Key summary tables (tracked in git)\n",
                "  - `processed-inputs/`: Processed input data (not tracked in git)\n",
                "  - `raw-outputs/`: Raw model outputs (not tracked in git)\n",
                "- `health/`: Health-related outputs (not tracked in git)\n",
                "- `labor/`: Labor-related outputs (not tracked in git)\n",
                "- `fig-csv-files/`: CSV files used to generate figures (not tracked in git)\n"
            ),
            readme_main
        )
        message("Created README at: ", readme_main)
    }

    message("Output directory structure created successfully!")
    return(list(git_tracked = git_tracked_dirs, git_ignored = git_ignored_dirs))
}

#' Migrate existing files to the new structure
#' @param old_dir Directory containing existing files
#' @param version Target version
#' @param iteration Target iteration
#' @param dry_run If TRUE, only print what would be done without moving files
#' @return NULL
migrate_existing_files <- function(
    old_dir,
    version,
    iteration,
    dry_run = TRUE
) {
    if (!dir.exists(old_dir)) {
        stop("Source directory does not exist: ", old_dir)
    }

    message(
        "Preparing to migrate files from ",
        old_dir,
        " to the new structure"
    )
    if (dry_run) {
        message("DRY RUN MODE: No files will be moved")
    }

    # Define migration mapping rules
    # Format: c(pattern = c(new_category, new_subcategory, is_tracked))
    migration_rules <- list(
        "figures" = c("figures", NA, TRUE),
        "legends" = c("figures", "legends", TRUE),
        "maps" = c("figures", "maps", TRUE),
        "health" = c("health", NA, FALSE),
        "labor" = c("labor", NA, FALSE),
        "fig-csv-files" = c("fig-csv-files", NA, FALSE),
        "summary" = c("data", "summary-tables", TRUE),
        ".csv$" = c("data", "raw-outputs", FALSE),
        ".rds$" = c("data", "raw-outputs", FALSE)
    )

    # Find all files in the old directory (recursively)
    all_files <- list.files(old_dir, recursive = TRUE, full.names = TRUE)
    message("Found ", length(all_files), " files to analyze")

    # Process each file
    for (file_path in all_files) {
        rel_path <- sub(paste0("^", old_dir, "/"), "", file_path)
        file_name <- basename(file_path)

        # Determine target location based on rules
        target_category <- "data"
        target_subcategory <- "raw-outputs"
        is_tracked <- FALSE

        for (pattern in names(migration_rules)) {
            if (grepl(pattern, rel_path, ignore.case = TRUE)) {
                rule <- migration_rules[[pattern]]
                target_category <- rule[1]
                target_subcategory <- if (is.na(rule[2])) NULL else rule[2]
                is_tracked <- as.logical(rule[3])
                break
            }
        }

        # Determine target directory
        if (is_tracked) {
            if (!is.null(target_subcategory)) {
                target_dir <- file.path(
                    "outputs",
                    version,
                    iteration,
                    target_category,
                    target_subcategory
                )
            } else {
                target_dir <- file.path(
                    "outputs",
                    version,
                    iteration,
                    target_category
                )
            }
        } else {
            if (
                !is.null(target_subcategory) &&
                    (target_subcategory == "raw-outputs" ||
                        target_subcategory == "processed-inputs")
            ) {
                target_dir <- file.path(
                    "outputs",
                    version,
                    iteration,
                    "data",
                    target_subcategory,
                    target_category
                )
            } else if (!is.null(target_subcategory)) {
                target_dir <- file.path(
                    "outputs",
                    version,
                    iteration,
                    target_category,
                    target_subcategory
                )
            } else {
                target_dir <- file.path(
                    "outputs",
                    version,
                    iteration,
                    target_category
                )
            }
        }

        target_path <- file.path(target_dir, file_name)

        # Report or perform the migration
        message("File: ", rel_path)
        message("  Target: ", target_path)
        message("  Tracked: ", is_tracked)

        if (!dry_run) {
            # Ensure target directory exists
            if (!dir.exists(target_dir)) {
                dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

                # Create gitignore if needed
                if (!is_tracked) {
                    gitignore_file <- file.path(target_dir, ".gitignore")
                    if (!file.exists(gitignore_file)) {
                        writeLines("*\n!.gitignore", gitignore_file)
                    }
                }
            }

            # Copy the file
            file.copy(file_path, target_path, overwrite = FALSE)
            message("  Copied to new location")
        }
    }

    message("Migration analysis complete.")
    if (dry_run) {
        message("Run again with dry_run=FALSE to actually move the files.")
    } else {
        message("Files have been migrated to the new structure.")
    }
}

# Example usage
if (FALSE) {
    # Create new output structure
    setup_output_structure("v1.0", "run1")

    # Migrate existing files (dry run)
    migrate_existing_files("outputs/test", "v1.0", "run1", dry_run = TRUE)

    # Actual migration (only run after confirming the dry run results)
    # migrate_existing_files("outputs/test", "v1.0", "run1", dry_run = FALSE)
}
