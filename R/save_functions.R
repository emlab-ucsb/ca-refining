#' Create save folders in repository based on output_structure.csv
#' @param save_path Base path for outputs (outputs/version/iteration)
#' @param iteration Iteration name (not used anymore, kept for backward compatibility)
create_save_folders_repo <- function(save_path, iteration) {
    # Load output_structure.csv to extract directories and files to track
    structure_file <- file.path("extras/output_structure.csv")
    if (!file.exists(structure_file)) {
        stop("output_structure.csv file not found")
    }

    structure_df <- data.table::fread(structure_file)

    # Get unique directories and analyze their tracking patterns
    dir_analysis <- structure_df[,
        .(
            total_files = .N,
            tracked_yes = sum(tracked == "YES"),
            tracked_no = sum(tracked == "NO")
        ),
        by = relative_path
    ]

    # Classify directories by tracking pattern
    dir_analysis[,
        tracking_pattern := ifelse(
            tracked_no == 0,
            "ALL_TRACKED",
            ifelse(tracked_yes == 0, "NONE_TRACKED", "MIXED")
        )
    ]

    # Create full directory paths
    all_dirs <- unique(file.path(save_path, dir_analysis$relative_path))

    # Get files grouped by directory for mixed tracking
    mixed_dirs <- dir_analysis[tracking_pattern == "MIXED", relative_path]
    git_tracked_files <- list()

    for (dir_path in mixed_dirs) {
        full_dir_path <- file.path(save_path, dir_path)
        tracked_files <- structure_df[
            relative_path == dir_path & tracked == "YES",
            file_name
        ]
        git_tracked_files[[full_dir_path]] <- tracked_files
    }

    # Add base directories if not already in the list
    base_dirs <- c(
        save_path,
        file.path(save_path, "intermediate"),
        file.path(save_path, "results"),
        file.path(save_path, "results", "figures"),
        file.path(save_path, "tables")
    )

    all_dirs <- unique(c(all_dirs, base_dirs))

    # Create all directories
    for (dir in all_dirs) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
            message("Created directory: ", dir)
        }
    }

    # Create .gitignore files based on tracking patterns
    for (i in 1:nrow(dir_analysis)) {
        dir_path <- file.path(save_path, dir_analysis$relative_path[i])
        tracking_pattern <- dir_analysis$tracking_pattern[i]

        gitignore_path <- file.path(dir_path, ".gitignore")

        if (tracking_pattern == "ALL_TRACKED") {
            # For directories where all files are tracked, no .gitignore needed
            # (or create one that allows everything)
            if (!file.exists(gitignore_path)) {
                writeLines(
                    "# All files in this directory are tracked in git",
                    gitignore_path
                )
                message("Created .gitignore for all-tracked dir: ", dir_path)
            }
        } else if (tracking_pattern == "MIXED") {
            # For directories with mixed tracking, create selective .gitignore
            tracked_files <- structure_df[
                relative_path == dir_analysis$relative_path[i] &
                    tracked == "YES",
                file_name
            ]

            gitignore_content <- c(
                "# Only specific files are tracked in this directory",
                "*",
                "!.gitignore",
                "!.gitkeep"
            )

            # Add exceptions for tracked files
            for (tracked_file in tracked_files) {
                gitignore_content <- c(
                    gitignore_content,
                    paste0("!", tracked_file)
                )
            }

            # Write the file if it doesn't exist or needs updating
            if (
                !file.exists(gitignore_path) ||
                    !identical(readLines(gitignore_path), gitignore_content)
            ) {
                writeLines(gitignore_content, gitignore_path)
                message("Created selective .gitignore in: ", dir_path)
            }
        } else if (tracking_pattern == "NONE_TRACKED") {
            # For directories where no files are tracked, create .gitignore to ignore everything
            gitignore_path <- file.path(dir_path, ".gitignore")

            if (!file.exists(gitignore_path)) {
                writeLines("*", gitignore_path)
                message("Created .gitignore to ignore all files in: ", dir_path)
            }
        }
        # Note: NONE_TRACKED directories also get local .gitignore files to ensure exclusion
    }

    return(all_dirs)
}

#' Save data frame to repository location
#' @param data Data frame to save
#' @param folder_path Relative folder path within repository
#' @param filename Filename for the CSV
#' @param save_path Base save path (optional, will override folder_path if provided)
#' @param file_type Optional file type for structured path resolution
#' @param figure_number Optional figure number for figures
#' @param extra_subfolder Optional extra subfolder
simple_fwrite_repo <- function(
    data,
    folder_path,
    filename,
    save_path = NULL,
    file_type = NULL,
    figure_number = NULL,
    extra_subfolder = NULL
) {
    # Load output_structure.csv if it exists
    structure_df <- NULL
    if (file.exists("output_structure.csv")) {
        structure_df <- data.table::fread("output_structure.csv")
    }

    # Check if we have the file in output_structure.csv
    clean_filename <- gsub("\\*$", "", filename) # Remove any existing asterisk
    file_info <- NULL
    if (!is.null(structure_df)) {
        file_info <- structure_df[structure_df$file_name == clean_filename, ]
    }

    # If save_path and file_type are provided, use the structured path
    if (!is.null(save_path) && !is.null(file_type)) {
        folder_path <- get_structured_path(
            save_path,
            file_type,
            clean_filename,
            figure_number,
            extra_subfolder
        )

        # Override with output_structure.csv path if found
        if (!is.null(file_info) && nrow(file_info) > 0) {
            rel_path <- file_info$relative_path[1]
            folder_path <- file.path(save_path, rel_path)
            message("Using path from output_structure.csv: ", folder_path)
        }
    }

    # Create directory if it doesn't exist
    if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Clean filename (remove any asterisk markers)
    clean_filename <- gsub("\\*$", "", filename)

    # Full file path
    file_path <- file.path(folder_path, clean_filename)

    # Save the file with error handling
    tryCatch(
        {
            data.table::fwrite(data, file_path)
            message("Saved file: ", file_path)
        },
        error = function(e) {
            warning("Failed to save file ", file_path, ": ", e$message)
        }
    )

    return(file_path)
}

#' Save ggplot to repository location
#' @param plot ggplot object
#' @param folder_path Relative folder path within repository
#' @param filename Filename (without extension)
#' @param width Plot width
#' @param height Plot height
#' @param dpi Resolution
#' @param save_path Base save path (optional, will override folder_path if provided)
#' @param file_type Optional file type for structured path resolution
#' @param figure_number Optional figure number for figures
#' @param extra_subfolder Optional extra subfolder
simple_ggsave_repo <- function(
    plot,
    folder_path,
    filename,
    width = 10,
    height = 8,
    dpi = 300,
    save_path = NULL,
    file_type = NULL,
    figure_number = NULL,
    extra_subfolder = NULL
) {
    # If save_path and file_type are provided, use the structured path
    if (!is.null(save_path) && !is.null(file_type)) {
        folder_path <- get_structured_path(
            save_path,
            file_type,
            filename,
            figure_number,
            extra_subfolder
        )
    }

    # Create directory if it doesn't exist
    if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Clean filename (remove any asterisk markers)
    filename <- gsub("\\*$", "", filename)

    # Full file paths for both PNG and PDF
    png_path <- file.path(folder_path, paste0(filename, ".png"))
    pdf_path <- file.path(folder_path, paste0(filename, ".pdf"))

    # Save the plot in both formats
    tryCatch(
        {
            ggplot2::ggsave(
                filename = png_path,
                plot = plot,
                width = width,
                height = height,
                dpi = dpi,
                units = "in"
            )
            message("Saved PNG: ", png_path)
        },
        error = function(e) {
            warning("Failed to save PNG: ", e$message)
        }
    )

    tryCatch(
        {
            # For PDF, make sure we're using the cairo_pdf device for best compatibility
            ggplot2::ggsave(
                filename = pdf_path,
                plot = plot,
                width = width,
                height = height,
                device = grDevices::cairo_pdf,
                units = "in"
            )
            message("Saved PDF: ", pdf_path)
        },
        error = function(e) {
            warning("Failed to save PDF: ", e$message)
        }
    )

    # Return only the PNG path for targets compatibility
    # This is because targets requires a single value return, not a list
    return(png_path)
}

#' Get the correct path for a file based on the new directory structure
#' @param save_path Base save path (outputs/version/iteration)
#' @param file_type Type of file (e.g., "figure", "health", "labor", "table")
#' @param file_name Name of the file
#' @param figure_number Optional figure number for figures (e.g., "figure-1")
#' @param extra_subfolder Optional extra subfolder
#' @return Full path where the file should be saved
get_structured_path <- function(
    save_path,
    file_type,
    file_name,
    figure_number = NULL,
    extra_subfolder = NULL
) {
    base_path <- save_path

    if (file_type == "figure") {
        if (is.null(figure_number)) {
            stop("Figure number must be provided for figure files")
        }

        # Check if it's in the extra folder
        if (!is.null(extra_subfolder)) {
            return(file.path(
                base_path,
                "results",
                "figures",
                "extra",
                extra_subfolder
            ))
        } else if (
            figure_number %in%
                c("figure-1", "figure-2", "figure-3", "figure-4", "figure-5")
        ) {
            return(file.path(base_path, "results", "figures", figure_number))
        } else if (figure_number == "figures-si") {
            return(file.path(base_path, "results", "figures", "figures-si"))
        } else {
            return(file.path(base_path, "results", "figures", "extra"))
        }
    } else if (file_type == "health") {
        return(file.path(base_path, "intermediate", "health"))
    } else if (file_type == "labor") {
        return(file.path(base_path, "intermediate", "labor"))
    } else if (file_type == "table") {
        if (
            grepl("health", file_name, ignore.case = TRUE) &&
                grepl("labor", file_name, ignore.case = TRUE)
        ) {
            return(file.path(base_path, "tables", "health-and-labor"))
        } else if (
            grepl("health|mortality|pm25", file_name, ignore.case = TRUE)
        ) {
            return(file.path(base_path, "tables", "health"))
        } else if (
            grepl("labor|jobs|employment", file_name, ignore.case = TRUE)
        ) {
            return(file.path(base_path, "tables", "labor"))
        } else {
            return(file.path(base_path, "tables", "other"))
        }
    } else if (file_type == "fig-csv") {
        # Store CSV files for figures in the appropriate directory
        # Since fig-csv-files doesn't exist in structure.md, we'll place these
        # in the corresponding figure directory or extra directory
        if (
            !is.null(figure_number) &&
                figure_number %in%
                    c(
                        "figure-1",
                        "figure-2",
                        "figure-3",
                        "figure-4",
                        "figure-5"
                    )
        ) {
            return(file.path(base_path, "results", "figures", figure_number))
        } else if (!is.null(extra_subfolder)) {
            return(file.path(
                base_path,
                "results",
                "figures",
                "extra",
                extra_subfolder
            ))
        } else {
            return(file.path(base_path, "results", "figures", "extra"))
        }
    } else {
        # Default to saving in the base path
        return(base_path)
    }
}

#' Safe file write with directory creation
#' @param data Data to write
#' @param main_path Base path for data files
#' @param save_path Relative save path
#' @param subfolder Subfolder within save_path
#' @param filename Filename to save
safe_fwrite_with_dir <- function(
    data,
    main_path,
    save_path,
    subfolder,
    filename
) {
    # Create full path - use save_path directly, not main_path/save_path
    full_dir_path <- file.path(save_path, subfolder)

    # Ensure directory exists
    if (!dir.exists(full_dir_path)) {
        dir.create(full_dir_path, recursive = TRUE, showWarnings = FALSE)
        message("Created directory: ", full_dir_path)
    }

    # Full file path
    file_path <- file.path(full_dir_path, filename)

    # Write file
    data.table::fwrite(data, file_path)
    message("Saved: ", file_path)
    return(file_path)
}

#' Save data frame to repository location using output_structure.csv for path and tracking info
#' @param data Data frame to save
#' @param filename Filename for the CSV
#' @param save_path Base save path (outputs/version/iteration)
validate_and_save_file <- function(
    data,
    filename,
    save_path
) {
    # Load output_structure.csv
    structure_file <- "extrasoutput_structure.csv"
    if (!file.exists(structure_file)) {
        stop("output_structure.csv file not found")
    }

    structure_df <- data.table::fread(structure_file)

    # Find the file in the structure
    file_info <- structure_df[structure_df$file_name == filename, ]

    if (nrow(file_info) == 0) {
        warning(
            "File ",
            filename,
            " not found in output_structure.csv, using default path"
        )
        return(simple_fwrite_repo(data, save_path, filename))
    }

    # Get the relative path and tracked status
    rel_path <- file_info$relative_path[1]
    tracked <- file_info$tracked[1] == "YES"

    # Form the complete folder path
    folder_path <- file.path(save_path, rel_path)

    # Add asterisk to filename if tracked
    save_filename <- ifelse(tracked, paste0(filename, "*"), filename)

    # Save the file
    simple_fwrite_repo(data, folder_path, save_filename)

    return(file.path(folder_path, filename))
}

#' Check if a file should be tracked in git based on output_structure.csv
#' @param filename The filename to check
#' @return TRUE if the file should be tracked, FALSE otherwise
should_be_tracked <- function(filename) {
    if (!file.exists("extras/output_structure.csv")) {
        warning("output_structure.csv not found, using default tracking")
        return(FALSE)
    }

    structure_df <- data.table::fread("output_structure.csv")
    file_info <- structure_df[structure_df$file_name == filename, ]

    if (nrow(file_info) == 0) {
        warning(
            "File ",
            filename,
            " not found in output_structure.csv, using default tracking"
        )
        return(FALSE)
    }

    return(file_info$tracked[1] == "YES")
}

#' Create targets snapshot for version-iteration documentation
#' @param save_path Base save path (outputs/version/iteration)
#' @param version Version identifier
#' @param iteration Iteration identifier (typically cuf=X.X)
#' @param ... Additional parameters (used for dependency tracking, not directly used in function)
#' @return List of created files
create_targets_snapshot <- function(save_path, version, iteration, ...) {
    # Ensure save_path directory exists
    if (!dir.exists(save_path)) {
        dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Read the current _targets.R file
    targets_file <- "_targets.R"
    if (!file.exists(targets_file)) {
        stop("_targets.R file not found")
    }

    targets_content <- readLines(targets_file)

    # Generate timestamp
    timestamp <- Sys.time()

    # Extract key parameters from _targets.R
    params <- extract_targets_parameters(targets_content)

    # Create human-readable summary
    summary_file <- file.path(save_path, "targets_snapshot_summary.md")
    write_targets_summary(summary_file, params, version, iteration, timestamp)

    # Create full script copy
    full_script_file <- file.path(save_path, "targets_snapshot_full.R")
    write_full_targets_script(full_script_file, targets_content, timestamp)

    message("Created targets snapshots:")
    message("  - Summary: ", summary_file)
    message("  - Full script: ", full_script_file)

    return(list(
        summary = summary_file,
        full_script = full_script_file
    ))
}

#' Extract key parameters from _targets.R content
#' @param targets_content Character vector of _targets.R file lines
#' @return List of extracted parameters
extract_targets_parameters <- function(targets_content) {
    params <- list()

    # Extract single-value targets
    extract_single_value <- function(pattern, name) {
        lines <- grep(pattern, targets_content, value = TRUE)
        if (length(lines) > 0) {
            # Find lines that are not commented out
            active_lines <- lines[!grepl("^\\s*#", lines)]
            if (length(active_lines) > 0) {
                line <- active_lines[1] # Take the first non-commented line
                # Extract the value between 'command = ' and '),'
                value_match <- regmatches(
                    line,
                    regexpr("command = [^,)]+", line)
                )
                if (length(value_match) > 0) {
                    value <- gsub("command = ", "", value_match)
                    value <- gsub('"', '', value) # Remove quotes
                    return(value)
                }
            }
        }
        return(NA)
    }

    # Extract vector/array targets
    extract_vector_value <- function(pattern) {
        start_lines <- grep(pattern, targets_content)
        if (length(start_lines) > 0) {
            # Find lines that are not commented out
            for (start_line in start_lines) {
                if (!grepl("^\\s*#", targets_content[start_line])) {
                    # Extract the command value part
                    command_match <- regmatches(
                        targets_content[start_line],
                        regexpr("command = .*", targets_content[start_line])
                    )
                    if (length(command_match) > 0) {
                        command_part <- gsub("command = ", "", command_match)
                        # Clean up the formatting
                        command_part <- gsub("\\s+", " ", command_part)
                        return(command_part)
                    }
                }
            }
        }
        return(NA)
    }

    # Module settings
    params$user <- extract_single_value('tar_target\\(name = user', 'user')
    params$ref_threshold <- extract_single_value(
        'tar_target\\(name = ref_threshold',
        'ref_threshold'
    )
    params$ren_threshold <- extract_single_value(
        'tar_target\\(name = ren_threshold',
        'ren_threshold'
    )
    params$pred_years <- extract_single_value(
        'tar_target\\(name = pred_years',
        'pred_years'
    )
    params$drop_in_perc <- extract_single_value(
        'tar_target\\(name = drop_in_perc',
        'drop_in_perc'
    )
    params$kern_perc <- extract_single_value(
        'tar_target\\(name = kern_perc',
        'kern_perc'
    )
    params$refinery_level_ghg <- extract_single_value(
        'tar_target\\(name = refinery_level_ghg',
        'refinery_level_ghg'
    )

    # Analysis parameters - Health
    params$beta <- extract_single_value('tar_target\\(name = beta', 'beta')
    params$se <- extract_single_value('tar_target\\(name = se', 'se')
    params$vsl_2015 <- extract_single_value(
        'tar_target\\(name = vsl_2015',
        'vsl_2015'
    )
    params$vsl_2019 <- extract_single_value(
        'tar_target\\(name = vsl_2019',
        'vsl_2019'
    )
    params$income_elasticity_mort <- extract_single_value(
        'tar_target\\(name = income_elasticity_mort',
        'income_elasticity_mort'
    )
    params$discount_rate <- extract_single_value(
        'tar_target\\(name = discount_rate',
        'discount_rate'
    )

    # Analysis parameters - Labor
    params$alpha_comp <- extract_single_value(
        'tar_target\\(name = alpha_comp',
        'alpha_comp'
    )
    params$alpha_emp <- extract_single_value(
        'tar_target\\(name = alpha_emp',
        'alpha_emp'
    )
    params$indirect_induced_mult <- extract_single_value(
        'tar_target\\(name = indirect_induced_mult',
        'indirect_induced_mult'
    )

    # Energy intensities
    params$ei_crude <- extract_single_value(
        'tar_target\\(name = ei_crude',
        'ei_crude'
    )
    params$ei_gasoline <- extract_single_value(
        'tar_target\\(name = ei_gasoline',
        'ei_gasoline'
    )
    params$ei_diesel <- extract_single_value(
        'tar_target\\(name = ei_diesel',
        'ei_diesel'
    )
    params$ei_jet <- extract_single_value(
        'tar_target\\(name = ei_jet',
        'ei_jet'
    )

    # CPI values
    params$cpi2020 <- extract_single_value(
        'tar_target\\(name = cpi2020',
        'cpi2020'
    )
    params$cpi2019 <- extract_single_value(
        'tar_target\\(name = cpi2019',
        'cpi2019'
    )

    # Scenarios
    params$dem_scens <- extract_vector_value(
        'tar_target\\(\\s*name = dem_scens'
    )
    params$ref_scens <- extract_vector_value(
        'tar_target\\(\\s*name = ref_scens'
    )
    params$clus <- extract_vector_value('tar_target\\(\\s*name = clus')

    return(params)
}

#' Write human-readable targets summary
#' @param file_path Path to write the summary file
#' @param params List of extracted parameters
#' @param version Version identifier
#' @param iteration Iteration identifier
#' @param timestamp Timestamp of creation
write_targets_summary <- function(
    file_path,
    params,
    version,
    iteration,
    timestamp
) {
    content <- c(
        "# Targets Snapshot Summary",
        "",
        paste("**Generated:** ", format(timestamp, "%Y-%m-%d %H:%M:%S %Z")),
        paste("**Version:** ", version),
        paste("**Iteration:** ", iteration),
        # paste("**User:** ", params$user %||% "Not specified"),
        "",
        "## Module Settings",
        "",
        paste(
            "- **Refinery threshold:** ",
            params$ref_threshold %||% "Not specified"
        ),
        paste(
            "- **Renewable threshold:** ",
            params$ren_threshold %||% "Not specified"
        ),
        paste(
            "- **Prediction years:** ",
            params$pred_years %||% "Not specified"
        ),
        paste(
            "- **Drop-in percentage:** ",
            params$drop_in_perc %||% "Not specified"
        ),
        paste("- **Kern percentage:** ", params$kern_perc %||% "Not specified"),
        paste(
            "- **Refinery-level GHG:** ",
            params$refinery_level_ghg %||% "Not specified"
        ),
        "",
        "## Analysis Parameters",
        "",
        "### Health Analysis",
        paste("- **Beta coefficient:** ", params$beta %||% "Not specified"),
        paste("- **Standard error:** ", params$se %||% "Not specified"),
        paste("- **VSL 2015:** ", params$vsl_2015 %||% "Not specified"),
        paste("- **VSL 2019:** ", params$vsl_2019 %||% "Not specified"),
        paste(
            "- **Income elasticity (mortality):** ",
            params$income_elasticity_mort %||% "Not specified"
        ),
        paste(
            "- **Discount rate:** ",
            params$discount_rate %||% "Not specified"
        ),
        "",
        "### Labor Analysis",
        paste(
            "- **Alpha compensation:** ",
            params$alpha_comp %||% "Not specified"
        ),
        paste(
            "- **Alpha employment:** ",
            params$alpha_emp %||% "Not specified"
        ),
        paste(
            "- **Indirect/induced multiplier:** ",
            params$indirect_induced_mult %||% "Not specified"
        ),
        "",
        "## Energy Intensities (MMBtu/bbl)",
        "",
        paste("- **Crude:** ", params$ei_crude %||% "Not specified"),
        paste("- **Gasoline:** ", params$ei_gasoline %||% "Not specified"),
        paste("- **Diesel:** ", params$ei_diesel %||% "Not specified"),
        paste("- **Jet fuel:** ", params$ei_jet %||% "Not specified"),
        "",
        "## Economic Parameters",
        "",
        paste("- **CPI 2020:** ", params$cpi2020 %||% "Not specified"),
        paste("- **CPI 2019:** ", params$cpi2019 %||% "Not specified"),
        "",
        "## Scenarios",
        "",
        paste(
            "- **Demand scenarios:** ",
            params$dem_scens %||% "Not specified"
        ),
        paste(
            "- **Refining scenarios:** ",
            params$ref_scens %||% "Not specified"
        ),
        paste("- **Clusters:** ", params$clus %||% "Not specified"),
        "",
        "## Notes",
        "",
        "This snapshot captures the key parameters used for this specific analysis run.",
        "For the complete _targets.R configuration, see `targets_snapshot_full.R`.",
        "",
        "---",
        "*Generated automatically by the targets snapshot system*"
    )

    writeLines(content, file_path)
}

#' Write full targets script copy
#' @param file_path Path to write the full script
#' @param targets_content Character vector of _targets.R file lines
#' @param timestamp Timestamp of creation
write_full_targets_script <- function(file_path, targets_content, timestamp) {
    # Add header comment
    header <- c(
        paste(
            "# Targets snapshot created:",
            format(timestamp, "%Y-%m-%d %H:%M:%S %Z")
        ),
        "# This is a complete copy of _targets.R as it was when this run was executed",
        "# Do not modify this file - it serves as a historical record",
        "",
        targets_content
    )

    writeLines(header, file_path)
}

# Helper function for null coalescing
`%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) {
        return(b)
    }
    if (length(a) == 1 && is.na(a)) {
        return(b)
    }
    return(a)
}
