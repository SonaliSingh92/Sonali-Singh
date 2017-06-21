###################################################################
##############--------Author : Sonali Singh---------###############
###################################################################

#set working directory
setwd("C:\Users\chaud\Desktop\CapitalOneDataChallenge")

#Libraries which are required
#install.packages("jsonlite")
library(jsonlite)
#install.packages("RJSONIO")
library(RJSONIO)
#install.packages("rjson")
library(rjson)
#install.packages('plotly')
library(plotly)
#install.packages("plyr",dependencies = T)
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)


#Univariate Analysis on both the datesets(Institutional and loans)

#Analysis on Institutional Data between 2012 to 2014
`2012_to_2014_institutions_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_institutions_data.csv", na.strings = "")
institutions_data <- `2012_to_2014_institutions_data`
summary(institutions_data)

#Analysis on loans Data between 2012 to 2014
`2012_to_2014_loans_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_loans_data.csv", na.strings = "")
loans_data <- `2012_to_2014_loans_data`
summary(loans_data)

###################################################################
#############--------------Data Munging-------------###############
###################################################################

#a) Merging two datasets and creating new column having 4 different buckets(Low, Medium, High, Very high) of loan amount

#Merging the institutions_data and loans_data into merged_data by Respondent_ID,Agency_Code,As_of_Year
merged_data <-merge(institutions_data,loans_data,by=c("Respondent_ID","Agency_Code","As_of_Year"))

#Creating a new column "Loan Category" with four bucket sizes - Low, Medium, High, Very High
merged_data <- transform(merged_data,Loan_Category=cut(Loan_Amount_000,breaks=c(1,500,2000,20000,100000),labels=c('Low','Medium','High',"Very High")))
View(merged_data)
summary(merged_data)


#b) APIs of 2 functions [ hmda_init() and hmda_to_jason(data, states, conventional_conforming) ]

#-------------------------------Function: hmda_init()----------------------------------------
# Reads the data files and return a pointer or object containing the expanded HMDA data
#-------------------------------------------------------------------------------------------

hmda_init=function(){
  
   #reading the institution data
  `2012_to_2014_institutions_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_institutions_data.csv", na.strings = "")
  institutions_data <- `2012_to_2014_institutions_data`

  #reading the loans data
  `2012_to_2014_loans_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_loans_data.csv", na.strings = "")
  loans_data <- `2012_to_2014_loans_data`
  
  #Merging both the above datasets
   merged_data <-merge(institutions_data,loans_data,by=c("Respondent_ID","Agency_Code","As_of_Year"))
   
   #Creating a new column in merged dataset
   merged_data <- transform(merged_data,Loan_Category=cut(Loan_Amount_000,breaks=c(1,153,235,347,99625.0),labels=c('Q1','Q2','Q3','Q4')))
   
   # String to Numeric Conversion
   merged_data$As_of_Year <- as.numeric(merged_data$As_of_Year)
   merged_data$Sequence_Number <- as.numeric(merged_data$Sequence_Number)
   
   merged_data$Loan_Amount_000 <- as.numeric(merged_data$Loan_Amount_000)
   merged_data$Applicant_Income_000 <-
     as.numeric(merged_data$Applicant_Income_000)
   merged_data$FFIEC_Median_Family_Income <-
     as.numeric(merged_data$FFIEC_Median_Family_Income)
   merged_data$Tract_to_MSA_MD_Income_Pct <-
     as.numeric(merged_data$Tract_to_MSA_MD_Income_Pct)
   merged_data$Number_of_Owner_Occupied_Units <-
     as.numeric(merged_data$Number_of_Owner_Occupied_Units)
   merged_data$Conforming_Limit_000 <-
     as.numeric(merged_data$Conforming_Limit_000)
   
   # String to Factor Conversion
   merged_data$Conventional_Status <-
     as.factor(merged_data$Conventional_Status)
   merged_data$Conforming_Status <-
     as.factor(merged_data$Conforming_Status)
   merged_data$Conventional_Conforming_Flag <-
     as.factor(merged_data$Conventional_Conforming_Flag)
   
   #Return the merged data frame
   return(merged_data)
}

#Calling the hmda_init() function and storing the result in a variable
readthedata = hmda_init()

# ----------------Function: hmda_to_json(data, states, conventional_conforming)---------------
# Export the expanded data set to disk for the states filtered by product segment.
#---------------------------------------------------------------------------------------------

hmda_to_jason = function(data,states = levels(data$State),conventional_conforming= levels(data$Conventional_Conforming_Flag), year = levels(as.factor(institutions_data$As_of_Year)))
{
  loans_data = data[(data$State %in% states & data$Conventional_Conforming_Flag %in% conventional_conforming & data$As_of_Year %in% year),]
  x = toJSON(loans_data)
  write(x,"test.json")
}

#Calling the function hmda_to_jason and storing the value in a variable
stored_data = hmda_to_jason(readthedata,"DE","Y","2012")


###################################################################
#############--------------Data Quality-------------###############
###################################################################


############################################################################################################################################
#Q2

#Creating institutions_data function to assess the quality for the column loan amount
loan_quality_assessment = function(data = readthedata)
{
  #Checking for missing values
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(is.na(readthedata$Loan_Amount_000[i])== T)
    {
      print("Quality assessment failed ")
      print(paste(merged_data("Missing value present at:",i),collapse = " "))
    }
    
  }
  
  #Checking for negative values
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(data$Loan_Amount_000[i]<0)
    {
      print("Quality assessment failed")
      print(paste(merged_data("Negative value present at:",i),collapse = " "))
    }
  }
  
  #Checking for the data type of the column loan amount
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(class(readthedata$Loan_Amount_000[i])!="integer")
    {
      print("Quality assessment failed")
      print(paste(merged_data("Unacceptable value present at:",i),collapse = " "))
    }
  }
  
  #Creating institutions_data new column ratio(loan amount/applicant income) and looking for possible data entry errors which may require further investigation
  readthedata$ratio = (readthedata$Loan_Amount_000/(as.numeric(readthedata$Applicant_Income_000)))
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(readthedata$ratio[i]>10)
    {
      print((paste(merged_data("Suspicious loan amount present at:",i),collapse = " ")))
      print("Further investigation required")
    }
  }
  
}


#Calling the function for quality assessment of the variable loan amount and identifying the rows with suspicious values
loan_quality_assessment(readthedata)


#Creating institutions_data function to assess the quality for the column respondent name
respondent_quality_assessment = function(data = readthedata)
{
  
  #Checking for missing values
  for(i in 1:length(readthedata$Respondent_Name_TS))
  {
    if(is.na(readthedata$Respondent_Name_TS[i])== T)
    {
      print("Quality assessment failed ")
      print(paste(merged_data("Missing value present at:",i),collapse = " "))
    }
  }
  
  
  special_ch = "[~!@#$%^*]"
  
  #Checking for specail characters
  for(i in 1:length(readthedata$Respondent_Name_TS))
  {
    ch = as.character(readthedata$Respondent_Name_TS[i]) 
    if((grepl(special_ch, ch))==TRUE)
    {
      print("Quality assessment failed ")
      print(ch)
      print(paste(merged_data("Special character present at:",i),collapse = " "))
    }
    
  }
  
  
  #Checking for discrepancy in the Respondent name 
  len1 = length(unique(readthedata$Respondent_Name_TS))
  len2 = length(unique(tolower((readthedata$Respondent_Name_TS))))
  if(len1-len2>0)
  {
    print("Data discrepancy in the  Repondent Name")
  }
  
}

#Calling the function for quality assessment of the variable respondent name and identifying the rows with suspicious values
respondent_quality_assessment(readthedata)

#Checking whether the flags conforming limit has been properly updated
checkflag = (readthedata$Loan_Amount_000>readthedata$Conforming_Limit_000 & readthedata$Conforming_Status != "Jumbo")
updateflag = nrow(readthedata[checkflag,])
updateflag


