getwd()
library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
#loading data
mydata = read.csv("consumerpriceindex.csv",strip.white=TRUE)
#Checking the type of date
typeof(mydata$REF_DATE)
#Converting date to date format
mydata$REF_DATE<- as.Date(paste0(mydata$REF_DATE, "-01"))
# Selecting only columns relevant for the analysis 
df = data.frame(mydata$REF_DATE,mydata$Products.and.product.groups,mydata$VALUE)
#Converting the value to numeric format
df$VALUE <- as.numeric(df$mydata.VALUE)

df = data.frame(mydata$REF_DATE,mydata$Products.and.product.groups,mydata$VALUE)
df
df = df%>%filter(mydata.REF_DATE >'2020-08-01')

df1 = df%>%filter(mydata.Products.and.product.groups =='Food')
df1

df1 = df1 %>%  mutate(change= ((df1$mydata.VALUE - lag(df1$mydata.VALUE))/lag(df1$mydata.VALUE))*100)
df1
png(filename="monthovermonth_change_Food_percnt.png", height=295, width=300,  
    bg="white") 
plot(df1$mydata.REF_DATE,df1$change, main = "MOM_Food_Percent",type="o", col="red", xlab = "Date", ylab = "change")
dev.off() 

df2 = df%>%filter(mydata.Products.and.product.groups =='Shelter')
df2
df2 = df2 %>%  mutate(change= ((df2$mydata.VALUE - lag(df2$mydata.VALUE))/lag(df2$mydata.VALUE))*100)
df2
png(filename="monthovermonth_change_Shelter_percent.png", height=295, width=300,  
    bg="white") 
plot(df2$mydata.REF_DATE,df2$change,main = "MOM_Shelter_Percent",type="o", col="red", xlab = "Date", ylab = "change")
dev.off() 

df3 = df%>%filter(mydata.Products.and.product.groups =='Energy')
df3
df3=df3 %>%  mutate(change= ((df3$mydata.VALUE - lag(df3$mydata.VALUE))/lag(df3$mydata.VALUE))*100)
df3
png(filename="monthovermonth_change_Energy_percnt.png", height=295, width=300,  
    bg="white") 
plot(df3$mydata.REF_DATE,df3$change, main = "MOM_Energy_Percent",type="o", col="red", xlab = "Date", ylab = "change")
dev.off() 




# Create the plot
png(filename="Month_over_Month.png", height=1000, width=1000,  
    bg="white")
plot(df1$mydata.REF_DATE, df1$change,ylim = c(-15,15), type = "o", col = "red", xlab = "Date", ylab = "Change (%)", main = "Month-over-Month Changes")
lines(df2$mydata.REF_DATE, df2$change, col = "blue")
lines(df3$mydata.REF_DATE, df3$change, col = "green")
legend("topright", legend = c("Food", "Shelter", "Energy"), col = c("red", "blue", "green"), lty = 1, cex = 0.8)
dev.off()




