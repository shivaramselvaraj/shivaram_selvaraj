

# Loading the input files #

bill_amount<-read.csv("C://Users//Arun//Desktop//holmusk//bill_amount.csv")
bill_id<-read.csv("C://Users//Arun//Desktop//holmusk//bill_id.csv")
clinical_data<-read.csv("C://Users//Arun//Desktop//holmusk//clinical_data.csv")
demographics<-read.csv("C://Users//Arun//Desktop//holmusk//demographics.csv")

# Installing packages #

install.packages('sqldf')
install.packages('plyr')
install.packages('dplyr')
install.packages('eeptools')
install.packages('lubridate')
install.packages("imputeTS")
install.packages('xlsx')
install.packages('tidyverse')
install.packages('readxl')
install.packages('knitr')
install.packages('arules')
install.packages('arulesViz')
install.packages('ggplot2')
install.packages('measurements')

# Loading Packages #

library('sqldf')
library('plyr')
library('dplyr')#left join
library('eeptools')
library('lubridate')
library("imputeTS")
library(xlsx)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library('measurements')


## Data Engineering ##


lj<-left_join(bill_id, bill_amount, by = c("bill_id"="bill_id"))

lj <- lj %>%
  group_by(patient_id,date_of_admission) %>%
  summarise(amount_per_admit_per_person=sum(amount),
            no_bills_per_admit=n())

lj1<-left_join(clinical_data,lj, by=c("id"="patient_id","date_of_admission"="date_of_admission"))
lj2<-left_join(demographics,lj1,by=c("patient_id"="id"))


# Data Imputation

lj2<-na.mean(lj2, option = "mode")


# Data Manipulation #

lj2$medical_history_3<-replace(lj2$medical_history_3,lj2$medical_history_3=='No',0)
lj2$medical_history_3<-replace(lj2$medical_history_3,lj2$medical_history_3=='Yes',1)
lj2$gender <- replace(lj2$gender, lj2$gender=='f', 'Female')
lj2$gender <- replace(lj2$gender, lj2$gender=='m', 'Male')
lj2$race <- replace(lj2$race, lj2$race=='chinese', 'Chinese')
lj2$race <- replace(lj2$race, lj2$race=='India', 'Indian')
lj2$resident_status <- replace(lj2$resident_status, lj2$resident_status=='Singapore citizen', 'Singaporean')
lj2$date_of_birth<-as.Date(lj2$date_of_birth, format="%Y-%m-%d")
lj2$Age <- floor(age_calc(lj2$date_of_birth, units = "years"))
lj2$days_in_hosp<-as.Date(lj2$date_of_discharge, format="%Y-%m-%d")-as.Date(lj2$date_of_admission, format="%Y-%m-%d")


#Descriptive Analytics  

#Pie Chart for Gender
mytable <- table(lj2$gender)
a<-table(lj2$gender)
female<-(a[2]/sum(a))*100
male<-(a[4]/sum(a))*100
lbls <- c("Male", "Female")
lbls <- paste(lbls,c(male,female))
pie(mytable, labels = lbls, 
    main="Pie Chart of Gender")

# Count of Age groups
hist(lj2$Age,main="Count of Age groups" ,xlab="Age Group", ylab = "Frequency", col='blue')

#Men have faster recovery time than female
gender_male<-filter(lj2, lj2$gender == 'Male')
gender_male$days_in_hosp<-as.numeric(gender_male$days_in_hosp)
gender_female<-filter(lj2, lj2$gender == 'Female')
gender_female$days_in_hosp<-as.numeric(gender_female$days_in_hosp)
hist(gender_male$days_in_hosp)
hist(gender_female$days_in_hosp)
lj2$bin_height <- cut(lj2$height, breaks = seq(150, 190, by = 10))
lj2$bin_weight <- cut(lj2$weight, breaks = seq(40, 130, by = 10))


# Feature Engineering for Date

lj2$Year_of_admit<-year(as.Date(lj2$date_of_admission,"%Y-%m-%d"))
lj2$Month_of_admit<-month(as.Date(lj2$date_of_admission,"%Y-%m-%d"))
lj2$Weekday_of_admit<-weekdays(as.Date(lj2$date_of_admission,"%Y-%m-%d"))
x <-(as.Date(lj2$date_of_admission,"%Y-%m-%d"))
lj2$nthweek_of_admit<-format(x, "%V")


# Preparing data for Association rules using Apriori 

lj2_mba_input<-lj2[,8:25]
lj2_mba_input$medical_history_3<-as.integer(lj2_mba_input$medical_history_3)
lj2_mba_input$medical_history_3 <- replace(lj2_mba_input$medical_history_3, lj2_mba_input$medical_history_3==1, 0)
lj2_mba_input$medical_history_3 <- replace(lj2_mba_input$medical_history_3, lj2_mba_input$medical_history_3==2, 1)

lj2_mba_input$preop_medication_1 <- replace(lj2_mba_input$preop_medication_1, lj2_mba_input$preop_medication_1==1, 'preop_medication_1')
lj2_mba_input$preop_medication_2 <- replace(lj2_mba_input$preop_medication_2, lj2_mba_input$preop_medication_2==1, 'preop_medication_2')
lj2_mba_input$preop_medication_3 <- replace(lj2_mba_input$preop_medication_3, lj2_mba_input$preop_medication_3==1, 'preop_medication_3')
lj2_mba_input$preop_medication_4 <- replace(lj2_mba_input$preop_medication_4, lj2_mba_input$preop_medication_4==1, 'preop_medication_4')
lj2_mba_input$preop_medication_5 <- replace(lj2_mba_input$preop_medication_5, lj2_mba_input$preop_medication_5==1, 'preop_medication_5')
lj2_mba_input$preop_medication_6 <- replace(lj2_mba_input$preop_medication_6, lj2_mba_input$preop_medication_6==1, 'preop_medication_6')

lj2_mba_input$medical_history_1 <- replace(lj2_mba_input$medical_history_1, lj2_mba_input$medical_history_1==1, 'medical_history_1')
lj2_mba_input$medical_history_2 <- replace(lj2_mba_input$medical_history_2, lj2_mba_input$medical_history_2==1, 'medical_history_2')
lj2_mba_input$medical_history_3 <- replace(lj2_mba_input$medical_history_3, lj2_mba_input$medical_history_3==1, 'medical_history_3')
lj2_mba_input$medical_history_4 <- replace(lj2_mba_input$medical_history_4, lj2_mba_input$medical_history_4==1, 'medical_history_4')
lj2_mba_input$medical_history_5 <- replace(lj2_mba_input$medical_history_5, lj2_mba_input$medical_history_5==1, 'medical_history_5')
lj2_mba_input$medical_history_6 <- replace(lj2_mba_input$medical_history_6, lj2_mba_input$medical_history_6==1, 'medical_history_6')
lj2_mba_input$medical_history_7 <- replace(lj2_mba_input$medical_history_7, lj2_mba_input$medical_history_7==1, 'medical_history_7')

lj2_mba_input$symptom_1 <- replace(lj2_mba_input$symptom_1, lj2_mba_input$symptom_1==1, 'symptom_1')
lj2_mba_input$symptom_2 <- replace(lj2_mba_input$symptom_2, lj2_mba_input$symptom_2==1, 'symptom_2')
lj2_mba_input$symptom_3 <- replace(lj2_mba_input$symptom_3, lj2_mba_input$symptom_3==1, 'symptom_3')
lj2_mba_input$symptom_4 <- replace(lj2_mba_input$symptom_4, lj2_mba_input$symptom_4==1, 'symptom_4')
lj2_mba_input$symptom_5 <- replace(lj2_mba_input$symptom_5, lj2_mba_input$symptom_5==1, 'symptom_5')


test<-lj2_mba_input
test <- replace(test, test==0, NA)
test2 = as.data.frame(t(apply(test,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(test2) = colnames(test)
test2 <- replace(test2, is.na(test2), '')
test2 <- sapply(test2, as.character)
test2[is.na(test2)] <- ""

write.csv(test2,"C://Users//Arun//Desktop//holmusk//test2.csv")
tr <- read.transactions("C://Users//Arun//Desktop//holmusk//test2.csv", format = 'basket', sep=',')
tr

# Market Basket Analysis #

summary(tr)
itemFrequencyPlot(tr, topN=20, type='absolute') 
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

'set of 68349 rules

rule length distribution (lhs + rhs):sizes
1     2     3     4     5     6     7     8     9    10 
2    33   257  1272  4435 10490 16653 17629 12324  5254 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.000   7.000   8.000   7.492   9.000  10.000 

summary of quality measures:
support           confidence          lift            count        
Min.   :0.001177   Min.   :0.8000   Min.   :0.9746   Min.   :   4.00  
1st Qu.:0.001471   1st Qu.:0.8182   1st Qu.:1.0414   1st Qu.:   5.00  
Median :0.002353   Median :0.8571   Median :1.1471   Median :   8.00  
Mean   :0.008360   Mean   :0.8787   Mean   :1.2097   Mean   :  28.42  
3rd Qu.:0.005294   3rd Qu.:0.9286   3rd Qu.:1.3420   3rd Qu.:  18.00  
Max.   :0.820882   Max.   :1.0000   Max.   :3.9306   Max.   :2791.00  

mining info:
data ntransactions support confidence
tr          3400   0.001        0.8'

inspect(rules[1:10])

  'lhs                    rhs                      support confidence     lift count
  [1]  {medical_history_4,                                                              
  medical_history_5} => {preop_medication_6} 0.002941176          1 1.343874    10
  [2]  {medical_history_4,                                                              
  medical_history_5} => {preop_medication_3} 0.002941176          1 1.218201    10
  [3]  {medical_history_3,                                                              
  medical_history_4,                                                              
  medical_history_5} => {medical_history_2}  0.001176471          1 3.448276     4
  [4]  {medical_history_3,                                                              
  medical_history_4,                                                              
  medical_history_5} => {preop_medication_6} 0.001176471          1 1.343874     4
  [5]  {medical_history_3,                                                              
  medical_history_4,                                                              
  medical_history_5} => {preop_medication_5} 0.001176471          1 1.219950     4
  [6]  {medical_history_3,                                                              
  medical_history_4,                                                              
  medical_history_5} => {preop_medication_3} 0.001176471          1 1.218201     4
  [7]  {medical_history_4,                                                              
  medical_history_5,                                                              
  medical_history_7} => {preop_medication_6} 0.001176471          1 1.343874     4
  [8]  {medical_history_4,                                                              
  medical_history_5,                                                              
  medical_history_7} => {preop_medication_5} 0.001176471          1 1.219950     4
  [9]  {medical_history_4,                                                              
  medical_history_5,                                                              
  medical_history_7} => {preop_medication_3} 0.001176471          1 1.218201     4
  [10] {medical_history_2,                                                              
  medical_history_4,                                                              
  medical_history_5} => {preop_medication_6} 0.001764706          1 1.343874     6'


topRules <- rules[1:10]
#Plotting the top 10 rules -Pic3
plot(topRules)
#Plotting the top 10 rules graphically -Pic4 
plot(topRules, method="graph")


# Cluster Analysis #

lj2_clust_input<-lj2[,c(8:34)]
lj2_clust_input<-lj2_clust_input[,c(-25,-27,-3)]
mydata <- scale(lj2_clust_input) # standardize variables


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,1,var))
for (i in 1:26) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:26, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


# BMI Calculation #

lj2$bmi_height<-conv_unit(lj2$height, 'cm', 'm')
lj2$bmi_bmi_height<-bmi_height^2
lj2$bmi_bmi<-lj2$weight/bmi_height
lj2$bin_bmi <- cut(lj2$bmi_bmi, breaks = seq(17.5, 47.5, by = 5))

lj2_bmi<-lj2[,c(1,43,44)]
write.xlsx(lj2_bmi, "C://Users//Arun//Desktop//holmusk//lj2_bmi.xlsx")


# Power BI input

power_input<-lj2[,c(1,2,3,4,26,27,28,29,30,31,33,34,35,36,37,38,40)]
write.xlsx(power_input, "C://Users//Arun//Desktop//holmusk//power_input.xlsx")


# Power BI other input

binary_input<-lj2[,c(8:25)]
write.xlsx(binary_input, "C://Users//Arun//Desktop//holmusk//binary_input.xlsx")
