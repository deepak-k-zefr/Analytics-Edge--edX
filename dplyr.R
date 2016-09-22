#############dplyr...............http://rpubs.com/justmarkham/dplyr-tutorial

#Why do I use dplyr?
#Great for data exploration and transformation
#Intuitive to write and easy to read, especially when using the "chaining" syntax (covered below)
#Fast on data frames


library(dplyr)
library(plyr)
library(hflights)
data("hflights")
head(hflights)
#convert to local data frame
flights=tbl_df(hflights)
flights

#flights on January first
flights[flights$Month==1 & flights$DayofMonth==1,]
#dplyr appraoch
filter(flights,Month==1, DayofMonth==1)
##use pipe for or condition
filter(flights,UniqueCarrier=='AA' | UniqueCarrier=='UA')

###Rbloggers--------------------dplyr###############################
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(compare)
spending=fromJSON("https://data.medicare.gov/api/views/nrth-mfg3/rows.json?accessType=DOWNLOAD")
names(spending)
meta=spending$meta
hospital_spending=data.frame(spending$data)
colnames(hospital_spending)=make.names(meta$view$columns$name)
hospital_spending=select(hospital_spending,-c(sid:meta))
glimpse(hospital_spending)
cols = 6:11; # These are the columns to be changed to numeric.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], as.numeric)
cols = 12:13; # These are the columns to be changed to dates.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], ymd_hms)
#Now, let's check if the columns have the classes we want.

sapply(hospital_spending, class)



#######Create data table
#We can create a data.table using the data.table() function.
hospital_spending_DT = data.table(hospital_spending)
class(hospital_spending_DT)

#To select columns, we use the verb select in dplyr. In data.table, on the other hand, we can specify the column names.



#################Selecting one variable
#Let's selet the "Hospital Name" variable
from_dplyr = select(hospital_spending, Hospital.Name)
from_data_table = hospital_spending_DT[,.(Hospital.Name)]
#Now, let's compare if the results from dplyr and data.table are the same.

compare(from_dplyr,from_data_table, allowAll=TRUE)

##################Removing one variable
from_dplyr = select(hospital_spending, -Hospital.Name)
from_data_table = hospital_spending_DT[,!c("Hospital.Name"),with=FALSE]
compare(from_dplyr,from_data_table, allowAll=TRUE)

###Dropping multiple variables
#Now, let's remove the variables Hospital.Name,State,Measure.Start.Date,and Measure.End.Date from the original 
#data frame hospital_spending and the data.table hospital_spending_DT.

from_dplyr = select(hospital_spending, -c(Hospital.Name,State,Measure.Start.Date,Measure.End.Date))
from_data_table = hospital_spending_DT[,!c("Hospital.Name","State","Measure.Start.Date","Measure.End.Date"),with=FALSE]
compare(from_dplyr,from_data_table, allowAll=TRUE)

#dplyr has functions contains(), starts_with() and, ends_with() which we can use with the verb select. In data.table, we can use regular expressions. Let's select columns that contain the word Date to demonstrate by example.
from_dplyr = select(hospital_spending,contains("Date"))
from_data_table = subset(hospital_spending_DT,select=grep("Date",names(hospital_spending_DT)))
compare(from_dplyr,from_data_table, allowAll=TRUE)
names(from_dplyr)


#Rename columns
setnames(hospital_spending_DT,c("Hospital.Name", "Measure.Start.Date","Measure.End.Date"), c("Hospital","Start_Date","End_Date"))
names(hospital_spending_DT)

hospital_spending = rename(hospital_spending,Hospital= Hospital.Name, Start_Date=Measure.Start.Date,End_Date=Measure.End.Date)
compare(hospital_spending,hospital_spending_DT, allowAll=TRUE)


#Filtering data to select certain rows
#To filter data to select specific rows, we use the verb filter from dplyr with logical statements that could include regular expressions. In data.table, we need the logical statements only.

#Filter based on one variable
from_dplyr = filter(hospital_spending,State=='CA') # selecting rows for California
from_data_table = hospital_spending_DT[State=='CA']
compare(from_dplyr,from_data_table, allowAll=TRUE)
from_dplyr = filter(hospital_spending,State=='CA' & Claim.Type!="Hospice") 
from_data_table = hospital_spending_DT[State=='CA' & Claim.Type!="Hospice"]
compare(from_dplyr,from_data_table, allowAll=TRUE)
from_dplyr = filter(hospital_spending,State %in% c('CA','MA',"TX")) 
from_data_table = hospital_spending_DT[State %in% c('CA','MA',"TX")]
unique(from_dplyr$State)

compare(from_dplyr,from_data_table, allowAll=TRUE)

##########Adding/updating column(s)
##########Summarizing columns
##########Chaining


hospital_spending%>%group_by(State)%>%summarize(mean=mean(Avg.Spending.Per.Episode..Hospital.))%>%
  arrange(desc(mean))%>%head(10)%>%
  mutate(State = factor(State,levels = State[order(mean,decreasing =TRUE)]))%>%
ggplot(aes(x=State,y=mean))+geom_bar(stat='identity',color='darkred',fill='skyblue')+
  xlab("")+ggtitle('Average Spending Per Episode by State')+
  ylab('Average')+ coord_cartesian(ylim = c(3800, 4000))

hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                    by=.(State)][order(-mean)][1:10]%>% 
  mutate(State = factor(State,levels = State[order(mean,decreasing =TRUE)]))%>%
  ggplot(aes(x=State,y=mean))+geom_bar(stat='identity',color='darkred',fill='skyblue')+
  xlab("")+ggtitle('Average Spending Per Episode by State')+
  ylab('Average')+ coord_cartesian(ylim = c(3800, 4000))

