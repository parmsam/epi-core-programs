# This R program uses pdftools, tidyverse, and stringer package to pull IDOC data on adult releases and admissions 
# Posted Indiana Dept. of Corrections (IDOC) data here: https://www.in.gov/idoc/2376.htm

# Created on 2018-08-09

# Purpose: Pull and assess IDOC for past few years 
# Significance: Obtain state and county admissions and releases over the past few years
# Limitations and Warnings: data based on summary reports that IDOC produces every year (see above for link to this data); 
# this analysis is based on reported IDOC data and will have any undereporting or misclassification inherent in the collection or report development process 

# Program Flow Description (high level review of the steps of the program)
#  1) Define key parameters, etc.
#  2) Read and parse HTML file
#  3) Save state or county data as a csv
#  4) Filter by State name and County name
#  5) Plot graph for County and/or State Prescription Rate Trends
#  X) Clean up. 

#  1) Relevant Packages install/load
library(tidyverse)
library(pdftools)
library(stringr)

#  2) Pull all adult admission and release summary pdfs posted IDOC website:
#commented all loops out after first time use and saved into wd inside folder called IDOC_Data

#here is an an example of hoiw to download a file pdf file in R:
#download.file("https://www.in.gov/idoc/files/Adult%20REL%20CY2017.pdf",destfile = "Adult%20REL%20CY2017.pdf",mode="wb")

# #looping through all the posted adult release data
#  for(i in seq(2016,2017)){
#    url<-paste("https://www.in.gov/idoc/files/Adult%20REL%20CY",i,".pdf",sep="")
#    filename<-paste("Adult%20REL%20CY",i,".pdf")
#    print(paste("downloading...",url,"into file called",filename))
#    download.file(url,destfile = paste("IDOC_Releases/",filename,sep=""),mode="wb")
#  }
# #looping through all the posted adult release data
# for(i in seq(2005,2015)){
#   url<-paste("https://www.in.gov/idoc/files/Adult_REL_CY",i,".pdf",sep="")
#   filename<-paste("Adult%20REL%20CY",i,".pdf")
#   print(paste("downloading...",url,"into file called",filename))
#   download.file(url,destfile = paste("IDOC_Releases/",filename,sep=""),mode="wb")
# }

#doing same thing for adult NEW ADMISSIONS data
#need to run through 2 diff for loops b/c of changed pattern on pdf file names

# for(i in seq(2016,2017)){
#   url<-paste("https://www.in.gov/idoc/files/Adult%20ADMCY",i,"%20nopvs.pdf",sep="")
#   filename<-paste("Adult%20ADMCY",i,".pdf")
#   print(paste("downloading...",url,"into file called",filename))
#   download.file(url,destfile = paste("IDOC_Admissions/",filename,sep=""),mode="wb")
# }
# 
# for(i in seq(2005,2015)){
#   url<-paste("https://www.in.gov/idoc/files/Adult_ADMCY",i,"_nopvs.pdf",sep="")
#   filename<-paste("Adult%20ADMCY",i,".pdf")
#   print(paste("downloading...",url,"into file called",filename))
#   download.file(url,destfile = paste("IDOC_Admissions/",filename,sep=""),mode="wb")
# }

#  3)Initialize relevant counties of interest and pdfs folder path:

County_filt<-c("Total")
# assign pdf data releases directory:
pdf_rel_dir<-paste(getwd(),"IDOC_Releases/",sep="/")
pdf_adm_dir<-paste(getwd(),"IDOC_Admissions/",sep="/")


pdf_rel_list<-list.files(pdf_rel_dir)
pdf_adm_list<-list.files(pdf_adm_dir)

#  4)Extract relevant data from the 2017 adult releases pdf file:


# text<-pdf_text(paste(pdf_dir,"Adult%20REL%20CY 2016 .pdf",sep="")) %>% strsplit(split = "\r","\n", " ")
# 
# #Also possible: text<-pdf_text("https://www.in.gov/idoc/files/Adult%20REL%20CY2017.pdf") %>% strsplit(split = "\r","\n", " ")
# 
# text1 <- data.frame(text[1])
# text2 <- data.frame(text[2])
# 
# colnames(text1) <-"Var1"
# text1$Var1<-as.character(text1$Var1)
# colnames(text2) <-"Var1"
# text2$Var1<-as.character(text2$Var1)
# 
# # regtest <- text1 %>% filter(row_number()==6) %>% select(Var1) %>% mutate(splits = strsplit(Var1, "\\s+")) %>% select(splits)
# grouping=c("","Commit", "M_CTP", "M_PROB", "M_PAROLE", "M_DISCHG", "M_TOTAL", "F_CTP", "F_PROB", "F_PAROLE", "F_DISCHG", "F_TOTAL", "TOTAL")
# 
# count_grp1<-text1 %>% filter(between(row_number(), 7, 61)) %>% separate(Var1,into=grouping,sep="\\s+")
# count_grp2<-text2 %>% filter(between(row_number(), 7, 46)) %>% separate(Var1,into=grouping,sep="\\s+")
# 
# IDOC_RelData<-rbind(count_grp1,count_grp2)
# IDOC_RelData$Commit
# 
# TGA_IDOC<-IDOC_RelData[grep(paste(County_filt, collapse='|'),IDOC_RelData$Commit,ignore.case=T),]
# TGA_IDOC<-TGA_IDOC[-1]
# 
# TGA_IDOC %>% mutate(TOTAL=as.numeric(gsub(",", "", TOTAL))) %>% summarize(sumTot=sum(TOTAL))

#retreive list of pdf and use above code as basis for a function to structure all of adult release pdf data

Operations_rel<- function(i){
  text<-pdf_text(paste(pdf_rel_dir,i,sep="")) %>% strsplit(split = "\r","\n", " ") 
  
  text1 <- data.frame(text[1]) 
  text2 <- data.frame(text[2])
  
  colnames(text1) <-"Var1"
  text1$Var1<-as.character(text1$Var1)
  colnames(text2) <-"Var1"
  text2$Var1<-as.character(text2$Var1)
  
  # regtest <- text1 %>% filter(row_number()==6) %>% select(Var1) %>% mutate(splits = strsplit(Var1, "\\s+")) %>% select(splits)
  
  grouping=c("","Commit", "M_CTP", "M_PROB", "M_PAROLE", "M_DISCHG", "M_TOTAL", "F_CTP", "F_PROB", "F_PAROLE", "F_DISCHG", "F_TOTAL", "TOTAL")
  
  count_grp1<-text1 %>% filter(between(row_number(), 7, 61)) %>% separate(Var1,into=grouping,sep="\\s+")
  
  count_grp2<-text2 %>% filter(between(row_number(), 7, 46)) %>% separate(Var1,into=grouping,sep="\\s+")
  
  IDOC_RelData<-rbind(count_grp1,count_grp2) 
  IDOC_RelData$Commit
  
  TGA_IDOC<-IDOC_RelData[grep(paste(County_filt, collapse='|'),IDOC_RelData$Commit,ignore.case=T),]
  TGA_IDOC<-TGA_IDOC[-1]
  TGA_IDOC<-TGA_IDOC %>% mutate(Year=substr(i,start=18, stop=21))
  
  #TGA_IDOC %>% mutate(TOTAL_ofCounties=as.numeric(gsub(",", "", TOTAL))) %>% summarize(sumTot=sum(TOTAL_ofCounties)) 
  
  assign(i,TGA_IDOC,envir=.GlobalEnv)
}

County_filt<-c("total")
a<-lapply(pdf_rel_list, Operations_rel)
b1<-do.call("rbind", a)
View(b1)

County_filt<-c("Marion")
a<-lapply(pdf_rel_list, Operations_rel)
b2<-do.call("rbind", a)
View(b2)

Operations_adm<- function(i){
  text<-pdf_text(paste(pdf_adm_dir,i,sep="")) %>% strsplit(split = "\r","\n", " ") 
  
  text1 <- data.frame(text[1]) 
  text2 <- data.frame(text[2])
  
  colnames(text1) <-"Var1"
  text1$Var1<-as.character(text1$Var1)
  # colnames(text2) <-"Var1"
  # text2$Var1<-as.character(text2$Var1)
  
  regtest <- text1 %>% filter(row_number()==6) %>% select(Var1) %>% mutate(splits = strsplit(Var1, "\\s+")) %>% select(splits)
  
  grouping=c("","Commit", "M_M", "M_FA", "M_FB", "M_FC", "M_FD", "M_F1", "M_F2", "M_F3", "M_F4", "M_F5", "M_F6", "M_HO", "M_TOTAL", "F_M", "F_FA", "F_FB", "F_FC", "F_FD", "F_F1", "F_F2", "F_F3", "F_F4", "F_F5", "F_F6", "F_HO", "F_TOTAL", "TOTAL")
  
  count_grp1<-text1 %>% filter(between(row_number(), 7, 100)) %>% separate(Var1,into=grouping,sep="\\s+")
  
  #count_grp2<-text2 %>% filter(between(row_number(), 0, 46)) %>% separate(Var1,into=grouping,sep="\\s+")
  
  IDOC_RelData<-rbind(count_grp1) 
  IDOC_RelData$Commit
  
  TGA_IDOC<-IDOC_RelData[grep(paste(County_filt, collapse='|'),IDOC_RelData$Commit,ignore.case=T),]
  TGA_IDOC<-TGA_IDOC[-1]
  TGA_IDOC<-TGA_IDOC %>% mutate(Year=substr(i,start=15, stop=18))
  #TGA_IDOC %>% mutate(TOTAL_ofCounties=as.numeric(gsub(",", "", TOTAL))) %>% summarize(sumTot=sum(TOTAL_ofCounties)) 
  assign(i,TGA_IDOC,envir=.GlobalEnv)    
}
###


County_filt<-c("Total")
a<-lapply(pdf_adm_list, Operations_adm)
b3<-do.call("rbind", a)
View(b3)


County_filt<-c("Marion")
a<-lapply(pdf_adm_list, Operations_adm)
b4<-do.call("rbind", a)
View(b4)



#   5) remove all list of objects for clean environment after analysis:

rm(list = ls())

