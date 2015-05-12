setdir <- "20140901"

####set source and output directories
raw.data.dir <-paste0("W:\\Dropbox\\metabolism\\raw_data", setdir)
raw.data.dir <- paste0("/Users/nickbond/Dropbox/metabolism/raw_data/", setdir)

model.input.dir <- paste0("W:\\Dropbox\\metabolism\\input\\", setdir) 
model.input.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_input/", setdir)

model.output.dir <- paste0("W:\\Dropbox\\metabolism\\input\\", setdir) 
model.output.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_output/", setdir)

####


source("W:\\Dropbox\\metabolism\\R Scripts\\Script 1. File import.R")
source("W:\\Dropbox\\metabolism\\September_2014\\script-1_run-openbugs.r") #run bugs
