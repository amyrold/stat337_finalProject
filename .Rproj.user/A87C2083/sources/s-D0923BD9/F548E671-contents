# Table of Contents ----
# 1. Import Data and Housekeeping
# * 1.1 Import
# * 1.2 Housekeeping

#==============================================================================#

# 1. Import Data and Housekeeping ----
# * 1.1 Import Data ----
data.death <- read.csv(paste(p.data.raw, "stmf.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
data.rank <- read.csv(paste(p.data.raw, 'ranking.csv', sep=''),header = TRUE, stringsAsFactors = FALSE)
c.names <- read.csv(paste(p.data.raw, 'country_names.csv',sep=''),header = TRUE, stringsAsFactors = FALSE)
data.obesity <- read.csv(paste(p.data.raw,'ObesityDataSet_raw_and_data_sinthetic.csv',sep=''),header=TRUE,stringAsFactors=FALSE)
#==============================================================================#

# * 1.2 Housekeeping ----
obesity.numerical <- data.matrix(data.obesity) #Categorical --> Numerical predictors
#==============================================================================#
