fmxdat
======

Introduction
------------

The aim of this package is to make loading data into the practicals for
my course, Financial Econometrics, easier to do. Installing this package
allows the student to load the data for a particular practical simply by
typing:

      data( DataName )

For details on the course, contact me directly.

Some more helper / wrapper functions might be introduced in time.

Examples
========

Wherever in the practicals a dataframe is mentioned, this dataframe can
now be accessed without the need for an internet connection as followsL

    library(tidyverse)
    library(fmxdat)
    data(BRICSTRI)
    BRICSTRI %>% gather(Countries, TRI, -Date)
