library(shiny, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinyjs,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DT, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(plotly, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(readr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(export, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinyhelper, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(keys, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

# Farben und Levels für Kat. 
cols <-c('darkgreen', 'darkgreen',
         'lightgreen', 'lightgreen',
         '#FFA500', '#CD8500', '#FFA500', 
         'orangered', '#CD3700', 'orangered', 'orangered',
         'red', '#CD0000', 'red', 'red')

lvls <- c("1A", "1B", 
          "2A", "2B", 
          "3C",  "3C*", "3D", 
          "4C", "4C*", "4D", "4E", 
          "5C", "5C*", "5D", "5E")

source("functions.R")

