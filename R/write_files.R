simple_fwrite <- function(x, main_path, sub_path, name) {
  fwrite(x, file.path(main_path, sub_path, name), row.names = F)
}
