# track_files_basic <- function(file) {
#   data = fread(file, header = T)
#   output = head(data)
#   orig_name = sub(".csv", "", basename(file))
#   output_name = paste0(orig_name, "_head.csv")
#   fwrite(output, output_name)
#   output_name
# }

simple_fread <- function(file) {
  fread(file, header = T)
}