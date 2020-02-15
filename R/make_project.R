#' @title make_project
#' @description make_project builds an R folder with a workable template in a given Root directory.
#' @param FilePath Provide the full location path within which the directory should be created.
#' On a Windows PC, this could e.g. be: 'C:/Temp_Folder/New_Project'
#' @param ProjNam Name of the project you want to create, e.g. 'ProjNam'
#' @return Path address just built.
#' @importFrom rmsfuns build_path
#' @importFrom rstudioapi openProject
#' @importFrom ProjectTemplate create.project
#' @examples
#' make_project("C:/Temp/FinMetrics/Practical1")
#' @export

make_project <- function(FilePath, ProjNam) {

rmsfuns::build_path(FilePath)
cwd <- getwd()
setwd(FilePath)
ProjectTemplate::create.project(project.name = ProjNam, merge.strategy = "require.empty", rstudio.project = T, template = "minimal")
unlink(list.files(FilePath, recursive = T, full.names = T)[!grepl(".Rproj", list.files(FilePath, recursive = T, full.names = T))], recursive = T, force = T )
setwd(cwd)
sapply(as.list(paste0("rm -r ", list.files(file.path( FilePath, ProjNam), full.names = T)[!grepl(".Rproj", list.files(file.path( FilePath, ProjNam), full.names = T))] )), system)

rmsfuns::build_path(FilePath = file.path(FilePath, ProjNam, "code"))
rmsfuns::build_path(FilePath = file.path(FilePath, ProjNam, "bin"))
rmsfuns::build_path(FilePath = file.path(FilePath, ProjNam, "settings"))
rmsfuns::build_path(FilePath = file.path(FilePath, ProjNam, "data"))

writeLines("# Some deletable function:

example_function <- function(x, y){

print('you can delete me please...')

Result = x * y

Result

}
"
, con = paste0( file.path( FilePath, ProjNam), "/code/example_function.R"))

readr::write_rds( datasets::iris, path = paste0( file.path( FilePath, ProjNam), "/data/flowers.rds") )

writeLines("---
output:
  md_document:
    variant: markdown_github
---

# Purpose

The aim of this folder is to.... (Explain yourself here)

Load in coding scripts

```{r}

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
library(pacman)
p_load(tidyverse, rmsfuns)

# Source in all your functions:
list.files('code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))

Result <- example_function(x = 10, y = 20)


```


# Data loading

Loading in some really crucial data here, and then producing a beautiful plot:

```{r}

df_flowers <- read_rds('data/flowers.rds')

ggplot(df_flowers) +
    geom_point(aes(Petal.Length, Petal.Width, color = Species) ) +
               labs(title = 'Amazing plot', y = 'petal widths', x = 'petal lengths')

```

# Lastly

* Notice that the 'bin' folder is used for binary files (pdfs, word docs, etc)

* 'settings' folder should be used only for human readable files that are used as user input settings files.
"
, con = paste0( file.path( FilePath, ProjNam), "/README.rmd"))

rstudioapi::openProject(path = file.path( FilePath, ProjNam), newSession = TRUE)

  }
