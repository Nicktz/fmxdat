fmxdat
======

Introduction
------------

The aim of this package is to make loading data into the practicals for
my course, Financial Econometrics, easier to do. Installing this package
allows the student to load the data for a particular practical simply by
typing:

      data_Use <- fmxdat::DataName

For details on the course, contact me directly.

Some more helper / wrapper functions might be introduced in time.

Examples
========

Wherever in the practicals a dataframe is mentioned, this dataframe can
now be accessed without the need for an internet connection as follows:

    library(tidyverse)
    library(fmxdat)
    df_Use <- fmxdat::BRICSTRI
    df_Use %>% gather(Countries, TRI, -Date)

Creating projects
-----------------

The following creates a project template that I recommend you use when
doing any work.

> The README is you diary as you proceed, and your manual when you
> return to your project in the future.

Use it as (note - the folder must be empty - you cannot have multiple
projects in a single folder.):

    make_project(FilePath = "C:/Lets/Go/Now", ProjNam = "Projeesss")
