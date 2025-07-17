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
