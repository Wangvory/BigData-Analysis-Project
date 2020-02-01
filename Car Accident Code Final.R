#John Part 1
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)
library(mlbench)
library(car)
library(corrplot)

setwd('/Users/zhoujiawang/RStudio Project')

#data import
Accidents <- read_csv("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Accidents0514.csv")
Casualties <- read_csv("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Casualties0514.csv")
Vehicles <- read_csv("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Vehicles0514.csv")
#merge into 1 data set
df <- merge(Accidents, Casualties, by='Accident_Index')
df <- merge(df, Vehicles, by='Accident_Index')
rm(Accidents, Casualties, Vehicles)

# change variable code into words
Location_code <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                            sheet="Police Force")
df <- left_join(df, Location_code, by=c("Police_Force"="code"))
df <- rename(df, Location=label)
rm(Location_code)

Junction_type <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls", 
                            sheet="Junction Detail")
df <- left_join(df, Junction_type, by=c("Junction_Detail"="code"))
df <- rename(df, Junction=label)
rm(Junction_type)

Light_conditions <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                               sheet="Light Conditions")
df <- left_join(df, Light_conditions, by=c("Light_Conditions" = "code"))
df <- rename(df, Lighting = label)
rm(Light_conditions)

Weather_conditions <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                                 sheet="Weather")
df <- left_join(df, Weather_conditions, by=c("Weather_Conditions"="code"))
df <- rename(df, Weather = label)
rm(Weather_conditions)

Surface_conditions <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                                 sheet="Road Surface")
df <- left_join(df, Surface_conditions, by = c("Road_Surface_Conditions" = "code"))
df <- rename(df, Surface = label)
rm(Surface_conditions)

Vehicle_type <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                           sheet="Vehicle Type")
df <- left_join(df, Vehicle_type, by = c("Vehicle_Type" = "code"))
df <- rename(df, Vehicle = label)
rm(Vehicle_type)

Vehicle_manoeuvre <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                                sheet="Vehicle Manoeuvre")
df <- left_join(df, Vehicle_manoeuvre, by = c("Vehicle_Manoeuvre" = "code"))
df <- rename(df, Manoeuvre = label)
rm(Vehicle_manoeuvre)

Skidding <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                       sheet="Skidding and Overturning")
df <- left_join(df, Skidding, by = c("Skidding_and_Overturning" = "code"))
df <- rename(df, Skidding = label)
rm(Skidding)

Journey_purpose <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                              sheet="Journey Purpose")
df <- left_join(df, Journey_purpose, by = c("Journey_Purpose_of_Driver" = "code"))
df <- rename(df, Journey = label)
rm(Journey_purpose)

Age_band <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                       sheet="Age Band")
df <- left_join(df, Age_band, by = c("Age_Band_of_Driver" = "code"))
df <- rename(df, Age_Band = label)
rm(Age_band)

Casualty_severity <- read_excel("/Users/zhoujiawang/Desktop/BigData Final/uk-accidents-10-years-history-with-many-variables/Road-Accident-Safety-Data-Guide.xls",
                                sheet="Accident Severity")
df <- left_join(df, Casualty_severity, by=c("Casualty_Severity"="code"))
df <- rename(df, Casualty_Outcome=label)
rm(Casualty_severity)

df$Date<- as.Date(df$Date, "%d/%m/%Y")
df$Year <- format(as.Date(df$Date), "%Y")
df$Month <- format(as.Date(df$Date), "%m")
df$Time<-format(strptime(df$Time,"%H:%M:%S"),'%H')

#shrink the data
set.seed(7)
df1<-sample_n(df, 10000)

#use the selected variables
df2<-df1 %>%
  select(Accident_Severity,Number_of_Vehicles,Number_of_Casualties,Day_of_Week,Time,`1st_Road_Class`,Year,
         Light_Conditions,Weather_Conditions,Road_Surface_Conditions,Urban_or_Rural_Area,Vehicle_Type,`1st_Point_of_Impact`,
         `Was_Vehicle_Left_Hand_Drive?`,Sex_of_Driver,Age_of_Driver,`Engine_Capacity_(CC)`,Age_of_Vehicle,Casualty_Class,
         Sex_of_Casualty,Age_of_Casualty)

#delete unclassified number
df_new <- df2 %>% 
  filter(`1st_Road_Class`!=6,Light_Conditions!=-1,Light_Conditions!=7, Weather_Conditions!=8,Weather_Conditions!=9,Weather_Conditions!=-1,
         Road_Surface_Conditions!=-1,Urban_or_Rural_Area!=3,Vehicle_Type!=-1,`1st_Point_of_Impact`!=-1,`Was_Vehicle_Left_Hand_Drive?`!=-1,
         Sex_of_Driver!=3,Sex_of_Driver!=-1,Sex_of_Casualty!=-1,Age_of_Driver!=-1,`Engine_Capacity_(CC)`!=-1,Age_of_Vehicle!=-1,Age_of_Casualty!=-1,
         Vehicle_Type!=90,Vehicle_Type!=97,Vehicle_Type!=98
  )

df_new$Accident_Severity<-factor(df_new$Accident_Severity,levels=c(1,2,3),labels=c("Fatal","Serious","Slight"))

write.csv(df_new,file = 'NewSet.csv')







#Grace Part 2
#Data description
df_new<-read_csv("E:/BRANDEIS/2nd semester class/Big data/project/accident/NewSet-1.csv")
df_new<-df_new[,-1] #remove col X1 if necessary
#REMEBER TO REMOVE THE CATEGORIES SHOWN IN THE CODE BELOW!!!!!!
df_new1<-df_new %>%
  filter(Age_of_Driver!=-1,`Engine_Capacity_(CC)`!=-1,Age_of_Vehicle!=-1,Age_of_Casualty!=-1,Vehicle_Type!=90,Vehicle_Type!=97,Vehicle_Type!=98)



#Data rename
df_new1$Day_of_Week<-factor(df_new1$Day_of_Week,levels=c(1,2,3,4,5,6,7),labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
df_new1$Weather_Conditions<-factor(df_new1$Weather_Conditions,levels=c(1,2,3,4,5,6,7),labels=c("Fine no high winds", "Raining no high winds"," Snowing no high winds","Fine+high winds","Raining+high winds","Snowing+high winds","Fog or mist"))
df_new1$Road_Surface_Conditions<-factor(df_new1$Road_Surface_Conditions,levels=c(1,2,3,4,5,6,7),labels=c("Dry","Wet or damp","Snow","Frost or ice","Flood over 3cm","Oil or diesel","Mud"))
#df_new1$Accident_Severity<-factor(df_new1$Accident_Severity,levels=c(1,2,3),labels=c("Fatal","Serious","Slight"))
df_new1$Vehicle_Type<-factor(df_new1$Vehicle_Type,levels=c(1,2,3,4,5,8,9,10,11,16,17,18,19,20,21,22,23),labels=c("Pedal cycle","Motorcycle <50cc","Motorcycle <125cc","Motorcycle <500cc",
                                                                                         "Motorcycle >500cc","Taxi/Private hire car","Car","Minibus","Bus","Horse","Agri Vehicle","Tram","Van <3.5t","Van <7.5t","Van >7.5t","Mobility Scooter","Electric Motorcycle"))
df_new1$'1st_Point_of_Impact'<-factor(df_new1$'1st_Point_of_Impact',levels=c(0,1,2,3,4),labels=c("Did not impact","Front","Back","Offside","Nearside"))
df_new1$Urban_or_Rural_Area<-factor(df_new1$Urban_or_Rural_Area,levels=c(1,2),labels=c("Urban","Rural"))
df_new1$`Was_Vehicle_Left_Hand_Drive?`<-factor(df_new1$`Was_Vehicle_Left_Hand_Drive?`,levels=c(1,2),labels=c("Right-hand","Left-hand"))
df_new1$Sex_of_Driver<-factor(df_new1$Sex_of_Driver,levels=c(1,2),labels=c("Male","Female"))
df_new1$Sex_of_Casualty<-factor(df_new1$Sex_of_Casualty,levels=c(1,2),labels=c("Male","Female"))
df_new1$Casualty_Class<-factor(df_new1$Casualty_Class,levels=c(1,2,3),labels=c("Driver or rider","Passenger","Pedestrian"))

options(scipen=5)
options(digits=3)
#where is year column?

par(mfrow=c(1,1))
barplot(table(df_new1$Accident_Severity),col = c("Red","Orange","#FFCC99"))

counts_day<- table(df_new1$Accident_Severity,df_new1$Day_of_Week) 
frame_day<-data.frame(counts_day)
names(frame_day) = c("Severity","Day","Frequency")
ggplot(frame_day,aes(Day,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))
#barplot(counts_day,legend=rownames(counts_day),col = c("Red","Orange",#FFCC99),ylim = c(0,8000))

counts_time<- table(df_new1$Accident_Severity,df_new1$Time) 
frame_time<-data.frame(counts_time)
names(frame_time) = c("Severity","Time","Frequency")
frame_time
ggplot(frame_time,aes(Time,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))
#counts_time<- table(df_new1$Accident_Severity,df_new1$Time) 
#barplot(counts_time,legend=rownames(counts_time),col = c("Red","Orange","Yellow"),ylim = c(0,5000))

counts_road<- table(df_new1$Accident_Severity,df_new1$Road_Surface_Conditions) 
frame_road<-data.frame(counts_road)
names(frame_road) = c("Severity","Surface","Frequency")
frame_road
ggplot(frame_road,aes(Surface,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))

counts_weather<- table(df_new1$Accident_Severity,df_new1$Weather_Conditions) 
frame_weather<-data.frame(counts_weather)
names(frame_weather) = c("Severity","Weather","Frequency")
frame_weather
ggplot(frame_weather,aes(Weather,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))

counts_vehicle<- table(df_new1$Accident_Severity,df_new1$Vehicle_Type) 
frame_vehicle<-data.frame(counts_vehicle)
names(frame_vehicle) = c("Severity","Vehicle","Frequency")
frame_vehicle<-frame_vehicle[c(5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,33,37,38,39,40,41,42,43,44,45),]
ggplot(frame_vehicle,aes(Vehicle,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))

counts_impact<- table(df_new1$Accident_Severity,df_new1$'1st_Point_of_Impact') 
frame_impact<-data.frame(counts_impact)
names(frame_impact) = c("Severity","Impact","Frequency")
frame_impact
ggplot(frame_impact,aes(Impact,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))

counts_class<- table(df_new1$Accident_Severity,df_new1$Casualty_Class) 
frame_class<-data.frame(counts_class)
names(frame_class) = c("Severity","Class","Frequency")
frame_class
ggplot(frame_class,aes(Class,Frequency,fill=Severity))+
  geom_bar(stat="identity",position="stack")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("Red","orange","#FFCC99"))

par(mfrow=c(1,4))
barplot(table(df_new1$Accident_Severity,df_new1$Sex_of_Driver),col = c("red","orange","#FFCC99"),xlab="Gender of Drivers")
barplot(table(df_new1$Accident_Severity,df_new1$Sex_of_Casualty),col = c("red","orange","#FFCC99"),xlab="Gender of Causalties")
barplot(table(df_new1$Accident_Severity,df_new1$`Was_Vehicle_Left_Hand_Drive?`),col = c("red","orange","#FFCC99"),xlab="Dominant Hand")
barplot(table(df_new1$Accident_Severity,df_new1$Urban_or_Rural_Area),col = c("red","orange","#FFCC99"),xlab="Place of Accident")



#Correlation between casualty and categorical variables
table(df_new1$Day_of_Week,df_new1$Accident_Severity)
matrix_day<-as.matrix(table(df_new1$Day_of_Week,df_new1$Accident_Severity))
chi2.1<-chisq.test(matrix_day,correct = F)
c(chi2.1$statistic, chi2.1$p.value)

table(df_new1$Time,df_new1$Accident_Severity)
matrix_time<-as.matrix(table(df_new1$Time,df_new1$Accident_Severity))
chi2.2<-chisq.test(matrix_time,correct = F)
c(chi2.2$statistic, chi2.2$p.value)


table(df_new1$`1st_Road_Class`,df_new1$Accident_Severity)
matrix_roadclass<-as.matrix(table(df_new1$`1st_Road_Class`,df_new1$Accident_Severity))
chi2.3<-chisq.test(matrix_roadclass,correct = F)
c(chi2.3$statistic, chi2.3$p.value)

table(df_new1$Light_Conditions,df_new1$Accident_Severity)
matrix_light<-as.matrix(table(df_new1$Light_Conditions,df_new1$Accident_Severity))
chi2.4<-chisq.test(matrix_light,correct = F)
c(chi2.4$statistic, chi2.4$p.value)

table(df_new1$Weather_Conditions,df_new1$Accident_Severity)
matrix_weather<-as.matrix(table(df_new1$Weather_Conditions,df_new1$Accident_Severity))
chi2.5<-chisq.test(matrix_weather,correct = F)
c(chi2.5$statistic, chi2.5$p.value)


table(df_new1$Road_Surface_Conditions,df_new1$Accident_Severity)
matrix_road<-as.matrix(table(df_new1$Road_Surface_Conditions,df_new1$Accident_Severity))
matrix_road<-matrix_road[1:5]
chi2.6<-chisq.test(matrix_road,correct = F)
c(chi2.6$statistic, chi2.6$p.value)

table(df_new1$Urban_or_Rural_Area,df_new1$Accident_Severity)
matrix_urban<-as.matrix(table(df_new1$Urban_or_Rural_Area,df_new1$Accident_Severity))
chi2.7<-chisq.test(matrix_urban,correct = F)
c(chi2.7$statistic, chi2.7$p.value)

table(df_new1$Vehicle_Type,df_new1$Accident_Severity)
matrix_vehicle<-as.matrix(table(df_new1$Vehicle_Type,df_new1$Accident_Severity))
matrix_vehicle<-matrix_vehicle[c(-1,-10,-12,-16,-17),]
matrix_vehicle
chi2.8<-chisq.test(matrix_vehicle,correct = F)
c(chi2.8$statistic, chi2.8$p.value)

table(df_new1$`1st_Point_of_Impact`,df_new1$Accident_Severity)
matrix_impact<-as.matrix(table(df_new1$`1st_Point_of_Impact`,df_new1$Accident_Severity))
chi2.9<-chisq.test(matrix_impact,correct = F)
c(chi2.9$statistic, chi2.9$p.value)

table(df_new1$`Was_Vehicle_Left_Hand_Drive?`,df_new1$Accident_Severity)
matrix_hand<-as.matrix(table(df_new1$`Was_Vehicle_Left_Hand_Drive?`,df_new1$Accident_Severity))
chi2.10<-chisq.test(matrix_hand,correct = F)
c(chi2.10$statistic, chi2.10$p.value)

table(df_new1$Sex_of_Driver,df_new1$Accident_Severity)
matrix_sexd<-as.matrix(table(df_new1$Sex_of_Driver,df_new1$Accident_Severity))
chi2.11<-chisq.test(matrix_sexd,correct = F)
c(chi2.11$statistic, chi2.11$p.value)

table(df_new1$Sex_of_Casualty,df_new1$Accident_Severity)
matrix_class<-as.matrix(table(df_new1$Casualty_Class,df_new1$Accident_Severity))
chi2.12<-chisq.test(matrix_class,correct = F)
c(chi2.12$statistic, chi2.12$p.value)

table(df_new1$Casualty_Class,df_new1$Accident_Severity)
matrix_class<-as.matrix(table(df_new1$Casualty_Class,df_new1$Accident_Severity))
chi2.13<-chisq.test(matrix_class,correct = F)
c(chi2.13$statistic, chi2.13$p.value)

c(chi2.1$p.value,chi2.2$p.value,chi2.3$p.value,chi2.4$p.value,chi2.5$p.value,chi2.6$p.value,
  chi2.7$p.value,chi2.8$p.value,chi2.9$p.value,chi2.10$p.value,chi2.11$p.value,chi2.12$p.value,
  chi2.13$p.value)

#Correlation between numeric variables
#df_new1$Accident_Severity<-factor(df_new1$Accident_Severity,levels=c("Fatal","Serious","Slight"),labels=c(1,2,3))
#df_new1$Accident_Severity<-as.numeric(df_new1$Accident_Severity)
#numericVars <- names(which(sapply(df_new1, is.numeric))) #index vector numeric variables
#all_numVar <- df_new1[, numericVars]
#cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#corrplot(cor_numVar)





#John Part 3 Modeling
# Load Libraries
library(caret)

# 1. Load Data
# define the filename
filename <- "NewSet.csv"
# load the CSV file from the local directory
dataset <- read.csv(filename, header=TRUE)
# preview the first 5 rows
head(dataset)

# 2. Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Accident_Severity, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# 3. Summarize Dataset
# dimensions of dataset
dim(dataset)
# list types for each attribute
sapply(dataset, class)
# take a peek at the first 5 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$Accident_Severity)
# split input and output
x <- dataset[,3:22]
y <- dataset[,2]
# summarize the class distribution
percentage <- prop.table(table(dataset$Accident_Severity)) * 100
cbind(freq=table(dataset$Accident_Severity), percentage=percentage)
# summarize attribute distributions
summary(dataset)

# 4. Visualize Dataset
# a) Univariate
# boxplots for numeric
par(mfrow=c(3,4))
for(i in 1:20) {
  boxplot(x[,i], main=names(dataset)[i])
}
# barplot for class breakdown
plot(y)

# b) Multivariate
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# 5. Evaluate Algorithms
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# a) linear algorithms
set.seed(7)
fit.lda <- train(Accident_Severity~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Accident_Severity~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Accident_Severity~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Accident_Severity~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Accident_Severity~., data=dataset, method="rf", metric=metric, trControl=control)
# d) compare algorithms
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
# summarize Best Model
print(fit.lda)

# 6. Estimate Skill on Validation Dataset
set.seed(7)
predictions <- predict(fit.lda, newdata=validation)
confusionMatrix(predictions, validation$Accident_Severity)

