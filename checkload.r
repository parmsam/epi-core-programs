#mini-program called checkload 
#purpose: to install package if the package cannot be loaded
#my current way of checking to see if need install then installing and loading necessary packages  
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
