#mini-program called checkload 
#input: list of packages that will be used in main R program
#purpose: create a function to install package if the package cannot be loaded and load all relevant packages
#significance: my current way of checking to see if need package install, installing new ones, and loading all necessary packages  
#creation date: 06/29/2018

#the following is based on https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
#list <- c("tidyverse","gmodels","rio")
checkload<-function(list.of.packages) {
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = TRUE)
}
#checkload(list)
