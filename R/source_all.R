#' @title source_all
#' @description Sources all the .R scripts in a folder. Important to ensure the .R files in a folder are all functions - not running scripts.
#' @param Loc Provided location to source all functions from
#' On a Windows PC, this could e.g. be: 'C:/Temp_Folder/code'
#' Or if called from within a project, simply source_all("code")
#' @return Sources all the .R scripts in a given folder.
#' @import dplyr
#' @examples
#' source_all("C:/Temp/FinMetrics/Practical1/code")
#' source_all("code")
#' @export

source_all <- function(Loc){

  L <- list.files(Loc, full.names = TRUE) %>% .[grepl("\\.R", .)] %>% .[!grepl("\\.Rmd", .)] %>% .[!grepl("\\.Rproject",.)]

  if(length(L) == 0 ) {
    cat(print("\n==========\nNo R functions to source to source\n=========\n"))
  } else {
    for(i in 1:length(L)) {source(L[i])}
  }

}
