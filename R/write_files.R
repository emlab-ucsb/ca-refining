simple_fwrite <- function(x, main_path, sub_path, name) {
  fwrite(x, file.path(main_path, sub_path, name), row.names = F)
}

simple_ggsave <- function(x, main_path, sub_path, file_name, width, height, dpi) {
  
  fig_path = file.path(main_path, sub_path, file_name)
  
  ggsave(x,
         filename = paste0(fig_path, ".png"),
         width = width,
         height = height,
         dpi = dpi)

  ggsave(x,
         filename = paste0(fig_path, ".pdf"),
         width = width,
         height = height)
  
  # embed_fonts(paste0(fig_path, ".pdf"),
  #             outfile = paste0(fig_path, ".pdf"))
  

}
