

create_save_folders <- function(main_path,
                                save_path_name) {
  
  # Specify the path to the folder
  folder_path <- file.path(main_path, save_path_name)
  csv_folder_path <- file.path(folder_path, "fig-csv-files")
  legend_path <- file.path(folder_path, "legends")
  pulse_path <- file.path(folder_path, "pulse-figs")
  
  # Check if the directory exists
  if (!dir.exists(folder_path)) {
    # If it doesn't exist, create it
    dir.create(folder_path)
    print(paste("Folder created:", folder_path))
  } else {
    print(paste("Folder already exists:", folder_path))
  }
  
  # Check if the directory exists
  if (!dir.exists(csv_folder_path)) {
    # If it doesn't exist, create it
    dir.create(csv_folder_path)
    print(paste("Folder created:", csv_folder_path))
  } else {
    print(paste("Folder already exists:", csv_folder_path))
  }
  
  # Check if the directory exists
  if (!dir.exists(legend_path)) {
    # If it doesn't exist, create it
    dir.create(legend_path)
    print(paste("Folder created:", legend_path))
  } else {
    print(paste("Folder already exists:", legend_path))
  }
  
  # Check if the directory exists
  if (!dir.exists(pulse_path)) {
    # If it doesn't exist, create it
    dir.create(pulse_path)
    print(paste("Folder created:", pulse_path))
  } else {
    print(paste("Folder already exists:", pulse_path))
  }
  
  return(folder_path)
  
}


simple_fwrite <- function(x, main_path, sub_path, name) {
  fwrite(x, file.path(main_path, sub_path, name), row.names = F)
}

simple_ggsave <- function(x, main_path, sub_path, file_name, width, height, dpi) {
  fig_path <- file.path(main_path, sub_path, file_name)

  ggsave(x,
    filename = paste0(fig_path, ".png"),
    width = width,
    height = height,
    dpi = dpi
  )

  ggsave(x,
    filename = paste0(fig_path, ".pdf"),
    width = width,
    height = height
  )

  # embed_fonts(paste0(fig_path, ".pdf"),
  #             outfile = paste0(fig_path, ".pdf"))
}
