#mini-program called checkload 
#to check installed packages, install/loading necessary programs  
#my easy way right now of checking to see if need install and loading necessary packages:
#may improve :)
checkload<-function(packages){
  for (i in packages){
    if (!require(i, character.only = TRUE)){
      cat(i, "=not installed, so installing and loading")
      install.packages(i)
      library(i)} 
  }
}