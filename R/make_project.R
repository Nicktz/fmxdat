#' @title make_project
#' @description Creates a new template project folder from a copied location. A Rproject environment with the parent folder will be created. If folder is non-empty, this will return an error (so it won't override a non-empty folder accidentally).
#' @param Mac If using a Mac, set to TRUE. FALSE by default
#' @param Open Should the project be opened or not. FALSE by Default
#' @importFrom rmsfuns build_path
#' @examples
#' # Create a folder, copy its location, and run:
#' make_project()
#' @export
make_project <- function (Mac = FALSE, Open = FALSE){

    if(Mac) {

      FilePath <- normalizePath(as.vector(unlist(read.table(pipe("pbpaste"), sep="\t", header=FALSE))))

    } else {
      FilePath <- normalizePath(readClipboard(),winslash = "/", mustWork = F)
    }
    build_in_case <- rmsfuns::build_path(FilePath)
    if(length(list.files(FilePath)) > 0 ) stop(glue::glue("\n\nThis folder is non-empty:\n\n{FilePath}\n\nThis function is specifically used for a new, empty folder.\nPlease create a new environment to create template folder in."))
    rootloc <- paste(strsplit(path.expand(FilePath), "/")[[1]][-length(strsplit(path.expand(FilePath), "/")[[1]])], collapse = "/")

    ProjNam <- paste0(strsplit(FilePath, "/")[[1]][length(strsplit(FilePath, "/")[[1]])], ".Rproj")
    ProjNam_noproj <- gsub( ".Rproj", "", ProjNam)


  cwd <- getwd()
  setwd( rootloc )
  ProjectTemplate::create.project(project.name = ProjNam_noproj,
                                  merge.strategy = "require.empty", rstudio.project = T,
                                  template = "minimal")

  Del <- list.files(FilePath, full.names = T)[!grepl(".Rproj", list.files(FilePath, full.names = T))]
  unlink(Del)

  if(!Mac){
    invisible(sapply(glue::glue("rmdir /s /q \"{Del[!grepl('.md', Del)]}\" "), shell))

  } else {
    invisible(sapply(glue::glue("rmdir /s /q \"{Del[!grepl('.md', Del)]}\" "), system))
  }

  rmsfuns::build_path(FilePath = file.path(rootloc, ProjNam_noproj, "code"))
  rmsfuns::build_path(FilePath = file.path(rootloc, ProjNam_noproj, "bin"))
  rmsfuns::build_path(FilePath = file.path(rootloc, ProjNam_noproj, "settings"))
  rmsfuns::build_path(FilePath = file.path(rootloc, ProjNam_noproj, "data"))

  writeLines("---
output:
  md_document:
    variant: markdown_github
---

# Purpose

Purpose of this work folder.

Ideally store a minimum working example data set in data folder.

Add binary files in bin, and closed R functions in code. Human Readable settings files (e.g. csv) should be placed in settings/


```{r}

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
library(tidyverse)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

", con = paste0( file.path( rootloc, ProjNam_noproj), "/README.rmd"))

  if(Open) rstudioapi::openProject(path = file.path( FilePath, ProjNam), newSession = TRUE)

  message( glue::glue("Folder created in:\n\n{FilePath}/{ProjNam}\n\n" ))

}
