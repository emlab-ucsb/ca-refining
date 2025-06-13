#' Create save folders in repository
#' @param save_path Relative path for figures
#' @param iteration Iteration name for creating subfolders
create_save_folders_repo <- function(save_path, iteration) {
    # Create main output directories
    dirs_to_create <- c(
        file.path("outputs", iteration, "health"),
        file.path("outputs", iteration, "figures"),
        save_path
    )

    for (dir in dirs_to_create) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
            message("Created directory: ", dir)
        }
    }

    return(dirs_to_create)
}

#' Save data frame to repository location
#' @param data Data frame to save
#' @param folder_path Relative folder path within repository
#' @param filename Filename for the CSV
simple_fwrite_repo <- function(data, folder_path, filename) {
    # Create directory if it doesn't exist
    if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Full file path
    file_path <- file.path(folder_path, filename)

    # Save the file
    data.table::fwrite(data, file_path)

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
simple_ggsave_repo <- function(
    plot,
    folder_path,
    filename,
    width = 10,
    height = 8,
    dpi = 300
) {
    # Create directory if it doesn't exist
    if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Full file path
    file_path <- file.path(folder_path, paste0(filename, ".png"))

    # Save the plot
    ggplot2::ggsave(
        filename = file_path,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        units = "in"
    )

    message("Saved: ", file_path)
    return(file_path)
}
