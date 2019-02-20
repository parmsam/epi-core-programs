#maskingProg.R
#load relevant libraries
library(tidyverse)
library(lubridate)

#develop count masking program
#based on bang bang method from https://www.r-bloggers.com/exploring-nse-enquo-quos-and/

#explanation
#map_lgl applies function to each element then returns vector of same length as input
#then selecting numeric column names based on that T/F list 
#mutate_at allows for mutating at multiple columns example: mutate_at(vars(Y,Z), funs)
#!!, called "unquote", and pronounced bang-bang. !! tells a quoting function to drop the implicit quotes:
#!! is a one-to-one replacement. 
#!!! (called "unquote-splice", and pronounced bang-bang-bang) is a one-to-many replacement. 
#It takes a list of expressions and inserts them at the location of the !!!:

#https://adv-r.hadley.nz/quasiquotation.html

maskIF<-function(df,vars_bool=map_lgl(df, is.numeric), cols=colnames(df[,vars_bool])){
  
  mutate_at(df,vars(!!cols),function(x) x=ifelse(x<5,"<5",x))
  
}

#create test dataset
a<-tibble(
  date = lubridate::today()+runif(min=0,max=1,n=20)*30,
  lab_value=runif(min=100,max=20000,n=20),
  population1=runif(min=0,max=1,n=20)*100,
  population2=runif(min=0,max=1,n=20)*100
) %>% mutate(lab_value=as.integer(lab_value),
             population1=as.integer(population1),
             population2=as.integer(population2))

#test the function out: 

#on all numeric columns
maskIF(a)

#only on specific column of interest
maskIF(a,cols="population1")

#see sdcTable for more sophisticed cell suppressions
#https://github.com/sdcTools/sdcTable
#https://cran.r-project.org/web/packages/sdcTable/sdcTable.pdf
#https://sdctools.github.io/sdcTable/articles/sdcTable.html
