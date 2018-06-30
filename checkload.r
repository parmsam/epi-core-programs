#mini-program called checkload 
#to install package if the package cannot be loaded
#my current way of checking to see if need install and then loading necessary packages
#may improve :)
checkload<-function(packages){
  for (i in packages){
    if (!require(i, character.only = TRUE)){
      cat(i, "=not installed, so installing and loading")
      install.packages(i)
      library(i)} 
  }
}
