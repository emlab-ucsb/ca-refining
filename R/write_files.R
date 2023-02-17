simple_fwrite <- function(x, path) {
  fwrite(x, path, row.names = F)
}
