# Health and Labor Figure Helper Functions

## Helper functions for file operations
## -----------------------------------------------------------------------------

#' Ensure directory exists before saving file
#' @param dir Directory to create if it doesn't exist
ensure_dir <- function(dir) {
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
        message("Created directory: ", dir)
    }
}

#' Safe write file with directory creation
#' @param data Data to write
#' @param main_path Base path (not used, included for compatibility)
#' @param save_path Save path
#' @param subdir Subdirectory name
#' @param filename Filename
safe_write_file <- function(data, save_path, subdir, filename) {
    full_dir <- file.path(save_path, subdir)
    ensure_dir(full_dir)
    full_path <- file.path(full_dir, filename)
    data.table::fwrite(data, full_path)
    message("Saved: ", full_path)
    return(full_path)
}
