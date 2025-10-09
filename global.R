req <- readLines("req.txt", warn = FALSE)
lapply(req, function(x) {
  library(x, 
          warn.conflicts = FALSE, 
          quietly = TRUE, 
          verbose = FALSE, 
          character.only = TRUE)
})

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

source("functions/functions.R")
source("functions/plot_functions.R")
source("functions/table2doc_.R")
source("functions/table2spreadsheet_.R")
source("functions/fuzzyMatch.R")

