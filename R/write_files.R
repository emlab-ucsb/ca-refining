create_save_folders <- function(main_path, save_path_name) {
  # Save path should already be the full output path, no need to prepend main_path
  main_outputs_dir <- save_path_name

  if (!dir.exists(main_outputs_dir)) {
    dir.create(main_outputs_dir, recursive = TRUE, showWarnings = FALSE)
    print(paste("Created directory:", main_outputs_dir))
  }

  # Create subdirectories
  subdirs <- c("figures", "health", "labor", "data")

  for (subdir in subdirs) {
    subdir_path <- file.path(main_outputs_dir, subdir)
    if (!dir.exists(subdir_path)) {
      dir.create(subdir_path, recursive = TRUE, showWarnings = FALSE)
      print(paste("Created directory:", subdir_path))
    }
  }

  # Create figure subdirectories
  fig_subdirs <- c("fig-csv-files", "legends", "pulse-figs")
  for (fig_subdir in fig_subdirs) {
    fig_subdir_path <- file.path(main_outputs_dir, fig_subdir)
    if (!dir.exists(fig_subdir_path)) {
      dir.create(fig_subdir_path, recursive = TRUE, showWarnings = FALSE)
      print(paste("Created directory:", fig_subdir_path))
    }
  }

  return(main_outputs_dir)
}

simple_fwrite <- function(x, main_path, sub_path, name) {
  fwrite(x, file.path(main_path, sub_path, name), row.names = F)
}

simple_ggsave <- function(
  x,
  sub_path,
  file_name,
  width,
  height,
  dpi
) {
  # Create directory if it doesn't exist
  save_dir <- file.path(sub_path)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", save_dir)
  }

  # Use named parameters to avoid confusion about parameter order
  ggsave(
    filename = file.path(save_dir, paste0(file_name, ".png")),
    plot = x,
    width = width,
    height = height,
    dpi = dpi
  )
}

# New repository-based functions
create_save_folders_repo <- function(save_path, iteration) {
  # Create the main outputs directory structure in repository
  main_outputs_dir <- file.path("outputs", iteration)

  if (!dir.exists(main_outputs_dir)) {
    dir.create(main_outputs_dir, recursive = TRUE, showWarnings = FALSE)
    print(paste("Created directory:", main_outputs_dir))
  }

  # Create subdirectories
  subdirs <- c("figures", "health", "labor", "data")

  for (subdir in subdirs) {
    subdir_path <- file.path(main_outputs_dir, subdir)
    if (!dir.exists(subdir_path)) {
      dir.create(subdir_path, recursive = TRUE, showWarnings = FALSE)
      print(paste("Created directory:", subdir_path))
    }
  }

  # Create figure subdirectories
  fig_subdirs <- c("fig-csv-files", "legends", "pulse-figs")
  for (fig_subdir in fig_subdirs) {
    fig_subdir_path <- file.path(save_path, fig_subdir)
    if (!dir.exists(fig_subdir_path)) {
      dir.create(fig_subdir_path, recursive = TRUE, showWarnings = FALSE)
      print(paste("Created directory:", fig_subdir_path))
    }
  }

  return(save_path)
}

simple_fwrite_repo <- function(x, save_subpath, filename) {
  # Create directory if it doesn't exist
  if (!dir.exists(save_subpath)) {
    dir.create(save_subpath, recursive = TRUE, showWarnings = FALSE)
  }

  fwrite(x, file.path(save_subpath, filename), row.names = FALSE)
  return(file.path(save_subpath, filename))
}

simple_ggsave_repo <- function(
  plot_obj,
  save_path,
  file_name,
  width,
  height,
  dpi
) {
  # Create directory if it doesn't exist
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  }

  fig_path <- file.path(save_path, file_name)

  ggsave(
    plot_obj,
    filename = paste0(fig_path, ".png"),
    width = width,
    height = height,
    dpi = dpi
  )

  ggsave(
    plot_obj,
    filename = paste0(fig_path, ".pdf"),
    width = width,
    height = height
  )

  return(paste0(fig_path, ".pdf"))
}
