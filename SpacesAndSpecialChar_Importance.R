#It is vital to look at data fields of interest for invisible or special characters 
#this is important when matching or looking frequencies by categories

#lets do an example:

animals_data<-data.frame(
  Animal_name=c("Cat","Cat "," Cat", "Dog","Dog ","Lizard","Chimpanzee"),
  Sizes=c(1.2, 1.45 , 1.7 , 2.1 , 2.2, 2.5, 4.5 )
  )

animal_weights<-data.frame(
  Animal_name=c("Cat", "Dog","Lizard","Chimpanzee"),
  Weight=c(6.5, 7.0 , 3.5 ,16.5 )
)


library(dplyr)

#doing a match between the dataframes as is would make my match the following:
animals_data %>% group_by(Animal_name) %>% summarize(Count=sum(Sizes)) %>% left_join(animal_weights,by=c("Animal_name"))

#this is vital to look at when working with large sql databases where data is manually entered

#blank or special characters may pop up in a proportion of responses
#they would bias results

#in order to correct for this particular problem we need to trim the variable before grouping:
animals_data %>% mutate(Animal_name=trimws(Animal_name, which="both")) %>% group_by(Animal_name) %>% summarize(Count=sum(Sizes))
#now lets do  the join again
animals_data %>% mutate(Animal_name=trimws(Animal_name, which="both")) %>% 
  group_by(Animal_name) %>% summarize(Count=sum(Sizes)) %>% 
 left_join(animal_weights,by=c("Animal_name"))

#now we get the match we want

#this is relevant data quality area to explore during data linkages across databases



