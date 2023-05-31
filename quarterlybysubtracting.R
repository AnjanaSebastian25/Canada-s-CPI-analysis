getwd()
library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
#loading data
mydata = read.csv("consumerpriceindex.csv",strip.white=TRUE)

#Checking the type of date
typeof(mydata$REF_DATE)
typeof(mydata$VALUE)

#Converting date to date format
mydata$REF_DATE<- as.Date(paste0(mydata$REF_DATE, "-01"))

# Selecting only columns relevant for the analysis 
df = data.frame(mydata$REF_DATE,mydata$Products.and.product.groups,mydata$VALUE)
df

#Filtering the data for All items and calculating the change over 3 months(Quarterly)
df1 = df%>%filter(mydata.Products.and.product.groups =='All-items')
df1
df1$change = ((df1$mydata.VALUE-lag(df1$mydata.VALUE, 3))/lag(df1$mydata.VALUE, 3))*100
df1

#Filtering the data for All items excluding food and energy and calculating the change over 3 months(Quarterly)
df2 = df%>%filter(mydata.Products.and.product.groups =='All-items excluding food and energy')
df2
df2$Change_withoutfoodenergy = ((df2$mydata.VALUE-lag(df2$mydata.VALUE, 3))/lag(df2$mydata.VALUE, 3))*100
df2

#Plotting the quarterly changes 
png(filename="QuarterChanges.png", height=1000, width=1000,  
    bg="white")
plot(df1$mydata.REF_DATE, df1$change,ylim = c(-5,5), type = "o", col = "red", xlab = "Date", ylab = "Change (%)", main = "Quarterly Changes")
lines(df2$mydata.REF_DATE, df2$Change_withoutfoodenergy, col = "blue")
legend("topright", legend = c("Quarter_Change", "Quarter_Change_Without_FoodEnergy"), col = c("red", "blue"), lty = 1, cex = 0.8)
dev.off()

