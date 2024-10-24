#' @export
EXAMPLE <- function(FILE = NULL){
  file <- FILE
  load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/', file, ".RData"))
  return(get(paste0(file)))
}
