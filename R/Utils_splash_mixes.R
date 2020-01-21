#'
#'
#'
#' @export
get_ultimate_splash_one <- function() {
  
  file_path <- system.file("avanti_splash/ultimate_splash_one.txt", package = "lipidomicsUtils")
  
  ultimate_splash_one <- as.data.frame(read.table(file_path,
                                                  sep = "\t",
                                                  header = TRUE,
                                                  stringsAsFactors = FALSE))
  
  ultimate_splash_one
  
}