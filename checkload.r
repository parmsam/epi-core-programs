#mini-program called checkload 
#input: list of packages that will be used in main R program
#purpose: create a function to install package if the package cannot be loaded and load all relevant packages
#significance: my current way of checking to see if need package install, installing new ones, and loading all necessary packages  
#creation date: 06/29/2018
#update: 
checkload<-function(packages){
  for (i in packages){
    if (!require(i, character.only = TRUE)){
      cat(i, "=not installed, so installing and loading")
      install.packages(i)
      library(i)} 
  }
}
