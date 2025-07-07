#' Create save folders in repository based on the structure.md file
#' @param save_path Base path for outputs (outputs/version/iteration)
#' @param iteration Iteration name (not used anymore, kept for backward compatibility)
create_save_folders_repo <- function(save_path, iteration) {
    # Parse structure.md to extract directories and files to track
    structure_file <- file.path("structure.md")
    if (!file.exists(structure_file)) {
        stop("structure.md file not found")
    }

    structure_content <- readLines(structure_file)

    # Extract directory and file structure from structure.md
    # Only include lines with directory/file paths (those starting with │   │)
    structure_lines <- structure_content[grepl("^│\\s+│", structure_content)]

    # Extract all directories and files to create
    all_dirs <- c()
    git_tracked_dirs <- c()
    git_tracked_files <- list()

    # Base path for output structure
    path_prefix <- save_path

    # Function to clean path from structure.md format
    clean_path <- function(path) {
        # Remove leading whitespace, directory indicators, and trailing symbols
        path <- gsub("^│\\s+│\\s+├─+\\s+", "", path)
        path <- gsub("^│\\s+│\\s+└─+\\s+", "", path)
        path <- gsub("/\\s*$", "", path) # Remove trailing slashes
        return(path)
    }

    # Process each line to build directory structure
    current_path_parts <- c()
    indent_level <- 0

    # First pass: extract all directories and tracked items (files/dirs with asterisks)
    for (line in structure_lines) {
        # Skip empty or non-directory/file lines
        if (!grepl("├─+|└─+", line)) {
            next
        }

        # Calculate indent level by counting leading spaces
        spaces <- nchar(gsub("^(│\\s+│\\s+).*$", "\\1", line))
        new_indent <- spaces / 2 # Approximate indentation level

        # Get path component from this line
        path_component <- clean_path(line)

        # Check if this is a directory or file
        is_dir <- grepl("/$", path_component)
        is_tracked <- grepl("\\*", path_component)

        # Remove asterisk from tracked items
        if (is_tracked) {
            path_component <- gsub("\\*$", "", path_component)
        }

        # Update current path based on indentation
        if (new_indent <= indent_level) {
            # Going back up the tree or at same level
            diff <- indent_level - new_indent + 1
            current_path_parts <- current_path_parts[
                1:(length(current_path_parts) - diff)
            ]
        }

        # Add this component to the current path
        current_path_parts <- c(current_path_parts, path_component)
        indent_level <- new_indent

        # Build full path from path parts
        full_path <- file.path(
            path_prefix,
            paste(current_path_parts, collapse = "/")
        )

        # Add to list of directories to create
        if (is_dir) {
            all_dirs <- c(all_dirs, full_path)
            if (is_tracked) {
                git_tracked_dirs <- c(git_tracked_dirs, full_path)
            }
        } else {
            # Get parent directory
            parent_dir <- dirname(full_path)
            all_dirs <- c(all_dirs, parent_dir)

            # If this is a tracked file, add to the tracked files list
            if (is_tracked) {
                # Make sure the parent directory exists in the list
                if (!parent_dir %in% names(git_tracked_files)) {
                    git_tracked_files[[parent_dir]] <- c()
                }

                # Add the file to the list for its parent directory
                git_tracked_files[[parent_dir]] <- c(
                    git_tracked_files[[parent_dir]],
                    basename(full_path)
                )
            }
        }
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

    # Create .gitkeep files in tracked directories
    # This ensures the directory structure is maintained in git even if empty
    for (dir in git_tracked_dirs) {
        gitkeep_path <- file.path(dir, ".gitkeep")
        if (!file.exists(gitkeep_path)) {
            writeLines("# This directory is tracked in git", gitkeep_path)
            message("Created .gitkeep in: ", dir)
        }
    }

    # Create .gitignore files
    for (dir in all_dirs) {
        gitignore_path <- file.path(dir, ".gitignore")

        # Different handling based on whether directory is tracked
        if (dir %in% git_tracked_dirs) {
            # For tracked directories, we want to allow everything
            if (
                !file.exists(gitignore_path) ||
                    identical(readLines(gitignore_path), c("*", "!.gitignore"))
            ) {
                # For a tracked directory without specific files, track everything
                writeLines("# This directory is tracked in git", gitignore_path)
                message("Created .gitignore for tracked dir: ", dir)
            }
        } else if (dir %in% names(git_tracked_files)) {
            # For directories with specific tracked files
            gitignore_content <- c(
                "# Only specific files are tracked in this directory",
                "*",
                "!.gitignore",
                "!.gitkeep"
            )

            # Add exceptions for tracked files in this directory
            for (tracked_file in git_tracked_files[[dir]]) {
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
                message("Created selective .gitignore in: ", dir)
            }
        } else {
            # For non-tracked directories, exclude everything
            if (!file.exists(gitignore_path)) {
                writeLines(
                    c(
                        "# No files are tracked in this directory",
                        "*",
                        "!.gitignore"
                    ),
                    gitignore_path
                )
                message("Created .gitignore to exclude all in: ", dir)
            }
        }
    }

    # Create README file in main directory
    readme_path <- file.path(save_path, "README.md")
    if (!file.exists(readme_path)) {
        writeLines(
            paste0(
                "# Output Structure for ",
                basename(dirname(save_path)),
                "/",
                basename(save_path),
                "\n\n",
                "This directory follows the standardized output structure for the CA Refining project.\n\n",
                "## Structure\n\n",
                "- `intermediate/` - Intermediate data files\n",
                "  - `health/` - Health-related intermediate outputs\n",
                "  - `labor/` - Labor-related intermediate outputs\n",
                "- `results/` - Final results\n",
                "  - `figures/` - Organized by figure number and supplementary materials\n",
                "- `tables/` - Output tables organized by category\n",
                "  - `other/` - Miscellaneous tables\n",
                "  - `health/` - Health-related tables\n",
                "  - `labor/` - Labor-related tables\n",
                "  - `health-and-labor/` - Combined health and labor tables\n\n",
                "## Git Tracking\n\n",
                "Only files and folders marked with an asterisk (*) in structure.md are tracked in git.\n",
                "All other files and folders are excluded from git tracking via .gitignore files."
            ),
            readme_path
        )
        message("Created README at: ", readme_path)
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

    # Check if the filename includes an asterisk for git tracking
    track_in_git <- FALSE
    clean_filename <- filename

    if (grepl("\\*$", filename)) {
        # Remove the asterisk from the filename for the actual file
        clean_filename <- gsub("\\*$", "", filename)
        track_in_git <- TRUE
    }

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

    # Update .gitignore for tracked files
    if (track_in_git) {
        # Create or update .gitignore to exclude everything except specific files
        gitignore_path <- file.path(folder_path, ".gitignore")

        if (file.exists(gitignore_path)) {
            # Read existing content
            existing <- readLines(gitignore_path)

            # Check if we need to add this file to exceptions
            if (!any(grepl(paste0("!", clean_filename, "$"), existing))) {
                # If it's the default "exclude all" pattern or similar, append the exception
                if (any(grepl("^\\*$", existing))) {
                    new_content <- c(existing, paste0("!", clean_filename))
                    writeLines(new_content, gitignore_path)
                    message("Updated .gitignore to track: ", clean_filename)
                } else {
                    # For other existing .gitignore files, add the exception if needed
                    writeLines(
                        c(existing, paste0("!", clean_filename)),
                        gitignore_path
                    )
                    message("Updated .gitignore to track: ", clean_filename)
                }
            }
        } else {
            # Create new .gitignore that excludes everything except this file
            writeLines(
                c(
                    "# Files tracked in git",
                    "*",
                    "!.gitignore",
                    "!.gitkeep",
                    paste0("!", clean_filename)
                ),
                gitignore_path
            )
            message("Created .gitignore to track: ", clean_filename)
        }

        # Ensure the parent directory has a .gitkeep file
        gitkeep_path <- file.path(folder_path, ".gitkeep")
        if (!file.exists(gitkeep_path)) {
            writeLines(
                "# This directory contains git-tracked files",
                gitkeep_path
            )
            message("Added .gitkeep to: ", folder_path)
        }

        message("Added git tracking for: ", file_path)
    }

    message("Saved: ", file_path)
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

    # Check if the filename includes an asterisk for git tracking
    track_in_git <- FALSE
    if (grepl("\\*$", filename)) {
        filename <- gsub("\\*$", "", filename)
        track_in_git <- TRUE
    }

    # Check if the filename includes figure number information
    is_figure_dir <- grepl("figure-[1-5]", folder_path)

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

    # If this is a figure directory marked with asterisk, all files are tracked automatically
    if (is_figure_dir) {
        # For figure directories marked with asterisks in structure.md,
        # all files are tracked automatically without needing specific .gitignore entries
        message(
            "Saved in tracked figure directory: ",
            png_path,
            " and ",
            pdf_path
        )
    } else if (track_in_git) {
        # Otherwise, if this file should be tracked in git, update the gitignore
        gitignore_path <- file.path(folder_path, ".gitignore")

        if (file.exists(gitignore_path)) {
            # Read existing content
            existing <- readLines(gitignore_path)

            # Add the file exceptions if they don't exist
            new_content <- existing
            png_pattern <- paste0("!", filename, ".png")
            pdf_pattern <- paste0("!", filename, ".pdf")

            if (!any(grepl(png_pattern, existing, fixed = TRUE))) {
                new_content <- c(new_content, png_pattern)
            }
            if (!any(grepl(pdf_pattern, existing, fixed = TRUE))) {
                new_content <- c(new_content, pdf_pattern)
            }

            if (length(new_content) > length(existing)) {
                writeLines(new_content, gitignore_path)
                message("Updated .gitignore for: ", png_path, " and ", pdf_path)
            }
        } else {
            # Create new .gitignore that excludes everything except these files
            writeLines(
                c(
                    "# Files tracked in git",
                    "*",
                    "!.gitignore",
                    "!.gitkeep",
                    paste0("!", filename, ".png"),
                    paste0("!", filename, ".pdf")
                ),
                gitignore_path
            )
            message(
                "Created .gitignore to track: ",
                filename,
                ".png and ",
                filename,
                ".pdf"
            )
        }

        # Ensure the parent directory has a .gitkeep file
        gitkeep_path <- file.path(folder_path, ".gitkeep")
        if (!file.exists(gitkeep_path)) {
            writeLines(
                "# This directory contains git-tracked files",
                gitkeep_path
            )
            message("Added .gitkeep to: ", folder_path)
        }

        message("Added git tracking for: ", png_path, " and ", pdf_path)
    }

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
        return(file.path(
            base_path,
            "results",
            "figures",
            "extra",
            "fig-csv-files"
        ))
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
