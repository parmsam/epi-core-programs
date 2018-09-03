# This R program uses rvest package to pull CDC opioid rx data for state and counties then saves the data
# More information on CDC posted opioid rx rates data: https://www.cdc.gov/drugoverdose/maps/rxrate-maps.html
# There is information user can specify at the start of the script to get what he or she wants (state or county level data and plots for what state or county of interest).
# Created on 2018-03-09
# Purpose: Text mining on CDC pages to pull opioid prescription rates data for past few years 2006-2016 for county and state levels
# Significance: Obtain state or county level data on opioid prescription rates data for further analysis
# Limitations and Warnings: the all county dataset lapply runtime is one to two minutes to pull the relevant data

# Program Flow Description (high level review of the steps of the program)
#  1) Define key parameters, etc.
#  2) Read and parse HTML file
#  3) Save state or county data as a csv
#  4) Filter by State name and County name
#  5) Plot graph for County and/or State Prescription Rate Trends

#  1) Relevant Packages load

packages<-c("rvest", "tidyverse","reshape2","stringr")
lapply(packages,require, character.only = TRUE)

#What State? Use full state name (example="Indiana")
StateName="Indiana"
#What county? Format as county comma then State 2 letter abbreviation here (example="Hamilton, IN")
CountyName="Hamilton, IN"
#Would you like to output a plot for the selected county(ChooseOut='county') or for the selected state(ChooseOut='state')?
ChooseOut='state'
#Would you like to save a Data frame for as a CSV? Declare Save=1 and declare destination path (with forward slashes) in filePath
Save=1

#Where would you like to save the dataframe for the CSV? (declare path here)
filePath="C:/TestPath/"
destPath<-paste(filePath, ChooseOut, '_OpioidRxData.csv', sep='')


# 2) Read and parse HTML file

#from: http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# two examples of method working to obtain 2016 county or state rate data
# webpage1 <- read_html("https://www.cdc.gov/drugoverdose/maps/rxcounty2016.html")
# tbls_ls1 <- webpage %>%
#   html_nodes("table") %>%
#   html_table(fill = TRUE)
# webpage2 <- read_html("https://www.cdc.gov/drugoverdose/maps/rxstate2016.html")
# tbls_ls2 <- webpage %>%
#   html_nodes("table") %>%
#   html_table(fill = TRUE)

#created function that performs above actions
my_webPull <- function(url) {
  webpage <- read_html(url)
  tbls_ls2 <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
}

#tested function and it works
#t2<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxstate2016.html")
#View(t2)

#urls have commmon patterns .../rxstate2016, /rxstate2015, etc
#so get a table of the urls using a the base and generating the pattern for both county and state

state_urls<-c()
for (i in 2006:2016){
  url_text<-paste("https://www.cdc.gov/drugoverdose/maps/rxstate", i,".html", sep="")
  state_urls<-append(state_urls, url_text)
}
state_urls<-data.frame(state_urls)
#make sure that the elements are characters using a paste since i guess it works
state_urls$state_urls <- paste0("", state_urls$state_urls, "")

#change the data frame to just a list using a for loop
for (state_url in state_urls){
}

#use lapply to pipe list of arguments into the function that was created in earlier steps
All_StateData<-lapply(state_url, my_webPull)
All_StateData<-data.frame(All_StateData)


county_urls<-c()
for (i in 2006:2016){
  url_text<-paste("https://www.cdc.gov/drugoverdose/maps/rxcounty", i,".html", sep="")
  county_urls<-append(county_urls, url_text)
}
county_urls<-data.frame(county_urls)
#make sure that the elements are characters using a paste since it works
#as.character conversion might work instead too
county_urls$county_urls <- paste0("", county_urls$county_urls, "")

#change the data frame to just a list using a for loop
for (county_url in county_urls){
}

#use lapply to pipe list of arguments into the function that was created in earlier steps
All_CountyData<-lapply(county_url, my_webPull)
All_CountyData<-data.frame(All_CountyData)

#now have datasets for state data (All_StateData) and county data (All_CountyData). 

#3) Save state or county data as a data frame in specified directory
SaveDf<-function(Save,ChooseOut,destPath) {
  if (Save==1){
    if(ChooseOut=='county'){
      write_csv(All_CountyData,path=destPath) 
    }
    else if (ChooseOut=='state'){
      write_csv(All_StateData,path=destPath)   
    }
  }
}

SaveDf(Save, ChooseOut, destPath)

#4)  Filter by State name and County name

StateData<-All_StateData %>% filter(State==StateName)
StateData <- melt(StateData, id=c("State"))
StateData<- StateData %>% filter(!str_detect(variable, 'State|County')) %>% 
  mutate(variable=substr(variable, 2, 5))


CountyData<-All_CountyData %>% filter(County==CountyName)
CountyData <- melt(CountyData, id=c("County"))
CountyData<- CountyData %>% filter(!str_detect(variable, 'State|County')) %>% 
  mutate(variable=substr(variable, 2, 5))


#5) Use ggplot to plot graph for County or State Prescription Rate Trends

StateData$variable <- factor(StateData$variable, levels =StateData$variable)
StateData$value <- as.numeric(StateData$value)

titleState <- paste("Estimated Rate of Opioid Prescriptions per 100 U.S. Residents by Year,", 
                    StateData[1,1], "(State)")


CountyData$variable <- factor(CountyData$variable, levels =CountyData$variable)
CountyData$value <- as.numeric(CountyData$value)

titleCounty <- paste("Estimated Rate of Opioid Prescriptions per 100 U.S. Residents by Year,", 
                     CountyData[1,1], "(County, State)")


plotFunc<-function(pickGeo) {
  if (pickGeo=='state'){
    dataset<-StateData
    gg_IN<-ggplot(data = dataset,aes(x=variable,y=value))+
      geom_point()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_y_continuous(breaks=seq(75,120,5))+
      #  expand_limits(y=0)+
      geom_line(alpha=1,color='black', group=1) +
      labs(x = "Year",y="Estimated Rate", 
           title=titleState)
    gg_IN
  }
  else if (pickGeo=='county'){
    dataset<-CountyData
    gg_MC<-ggplot(data = dataset,aes(x=variable,y=value))+
      geom_point()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_y_continuous(breaks=seq(70,120,5))+
      #  expand_limits(y=0)+
      geom_line(alpha=1,color='black', group=1) +
      labs(x = "Year",y="Estimated Rate", 
           title=titleCounty)
    gg_MC
  }
}

plotFunc(ChooseOut)


