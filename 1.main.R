# General ----
# Questions and Motivation ----

# R version
R.version.string # "R version 4.0.2 (2020-06-22)"
rm(list = ls())

#==============================================================================#

# Script Index ----
# 1.main.R        
# 2.data.manip.R
# 3.analysis.R
# 4.functions.R

#==============================================================================#

# To do ----

#==============================================================================#

# Global Variables ----
# store current user's working directory
wk.dir <- getwd()

#==============================================================================#

# Folder Management ----
# names of folders for output data (figures + data output)
folder.names <- c("a.data.raw","b.data.clean", "c.results","d.figures", 'e.manuscripts', 'f.scripts', 'g.trash')
# if folder with name "i" does not exist, create it.
for(i in 1:length(folder.names)){
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i]) 
  }
}

# paths to the folders. The 'p.' indicates the variable is a path.
# make sure the variable names describe the folder.names
p.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
p.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
p.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
p.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")

#==============================================================================#

# Run Scripts ----
source("4.functions.R")

#==============================================================================#

# Libraries ----
# load libraries needed for our analyses

#==============================================================================#

# END OF MAIN ----
