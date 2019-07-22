#Purpose: This sub-program is meant to source into other programs to calculate CIs and add an indicator based on our CI boundaries rule
#Inputs: 
# 1)Dataframe called dataset
# 2)Count variable name (count of interest)
# 3)Population Count variable name (population count)
# 4)Specified Rate_Int, Conf_Lev, and Bound (if needed)
#Output: 
# 1)Dataframe with exact binomial confidence intervals added based on Clopper-Pearson Method 

#Updates:
#04/27/2018 modified code so that users can input any count and population_ct column names

#install.packages("PropCIs")
source("S:/EPI/Codebooks, systems, dataset info/R/General Utilities/libraryLoad.R")
libraryLoad("PropCIs", "tidyverse")
library(PropCIs)
library(tidyverse)

CI_func <- function(dataset,
           Count,
           Population_Ct,       
           Rate_Int= 100000,
           Conf_Lev = 0.95,
           Bound  = 0.6)
 {

  # Count<-substitute(Count_nm)
  # Population_Ct<-substitute(Population_Ct_nm)
  
  x <- substitute(Count)
  y <- substitute(Population_Ct)
  #Population_Ct_nm<-as.character(Population_Ct_nm)
  
  #dataset <- dataset %>% rename(Count=y,Population_Ct=x)
  #dataset <- dataset %>% rename_("Count"= Count_nm,"Population_Ct"= Population_Ct_nm)

  
  dataset<-dataset %>% rename_("Count"=x, "Population_Ct"=y) 
  
  dataset <- dataset %>% 
    mutate(Count=as.numeric(Count), Population_Ct=as.numeric(Population_Ct)) %>% 
    mutate(Rate=Rate_Int*as.numeric(Count/Population_Ct))

  dataset$test<-apply(dataset[,c('Count','Population_Ct')],1, function(x) exactci(x[1], x[2],Conf_Lev))
  
  dataset<-separate(dataset, test, into = c("LowCI", "HighCI"), sep = ",")
  
  dataset$LowCI<-gsub(".*c\\DPopulation_Ct = ", "", dataset$LowCI)
  dataset$HighCI<-gsub("))", "", dataset$HighCI,fixed=TRUE)
  dataset$HighCI<-gsub("Population_Ct = ", "", dataset$HighCI,fixed=TRUE)
  
  dataset<- dataset %>% mutate(LowCI=Rate_Int*as.numeric(LowCI))%>% mutate(HighCI=Rate_Int*as.numeric(HighCI))
  
  dataset<- dataset %>% mutate(CI_boundStatus=ifelse((HighCI-LowCI)<(Bound*Rate),"ShowPoint","DoNotShowPoint"))
  
  #dataset<- dataset %>% mutate(RateAndCI=ifelse(CI_boundStatus=="ShowPoint",paste0(round(Rate,2), "[",round(LowCI,2),"-",round(HighCI,2),"]"),"NS"))
  
  dataset<- dataset %>% mutate(RateAndCI=paste0(round(Rate,1), "[",round(LowCI,1),"-",round(HighCI,1),"]"))
  
  #rename those columns back to original
  xchar<-as.character(x)
  ychar<-as.character(y)
  
  #dataset<-dataset %>% rename_(xchar="Count", ychar="Population_Ct") 
  #for some reason that does not work so going with below to return colum names back to original
  
  names(dataset)[names(dataset) == "Count"] <- xchar
  names(dataset)[names(dataset) == "Population_Ct"] <- ychar
  
  return(dataset)
}

#default parameters:
#unit for your rate to be (per 100,000 is default)
#Rate_Int = 100000
#confidence interval percentage (95% is default)
#Conf_Lev = 0.95
#boundary indicator cutoff (0.6 is cutoff based on current policy)
#Bound = 0.6

#Here is an example of how to use this program:
#make sure to source this script in the program you are working on first this way
#source(paste("S:/EPI/Codebooks, systems, dataset info/R/General Utilities/","Calculate_CIs.R",sep=""))

# test is my dataframe with the Count and Population_Ct variables: 
#(these can now be named anything you want as long as they are declared in the function) declared in the df
# test data: 
#test<-data.frame(Count=10, Pop=10000)
# using it this way (below) provides the rates per 100,000, the 95% CI and the boundary indicator for 60% (0.6) cutoff added into the df

#this way:
#test_ci<-CI_func(test,count=Count, Population_Ct=Pop)
# OR this way:
#test_ci<-CI_func(test,Count, Pop)
# OR this way:
#test_ci<-CI_func(test,Count, Pop,Rate_Int=1000,Conf_Lev=0.8)
