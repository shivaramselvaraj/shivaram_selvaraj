rd_train<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_data_70_new.csv")
enq_train<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_enquiry_70_new.csv")
acc_train<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_account_70_new.csv")

rd_test<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_data_30_new.csv")
enq_test<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_enquiry_30_new.csv")
acc_test<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\raw_account_30_new.csv")


#Funneling train and test together 
rd<-rbind(rd_train,rd_test)
enq<-rbind(enq_train,enq_test)
acc<-rbind(acc_train,acc_test)
set.seed(100)

#Data Imputation

acc$cashlimit[is.na(acc$cashlimit)] <- 0
acc$cur_balance_amt[is.na(acc$cur_balance_amt)] <- 0
acc$cur_balance_amt[acc$cur_balance_amt == "0"] <- 1#util_tren
acc$cur_balance_amt[acc$cur_balance_amt == "-1"] <- 1#util_tren

install.packages(dplyr)
install.packages(plyr)
install.packages(data.table)
install.packages(sqldf)
install.packages("unbalanced")
install.packages(rpart)
install.packages('DMwR')
install.packages('gdata')
install.packages('rpart.plot')
install.packages('party')
install.packages('rattle')
install.packages('randomForest')
install.packages('ROCR')
install.packages('gplots')
library(ROCR)
library(gdata)
library(gdata)
library("party")
library('rattle')
library(rpart.plot)
library(rpart)
library(unbalanced)
library(dplyr)#left join
library(plyr)
library(data.table)
library(sqldf)
library(DMwR)
library(gdata)
library(randomForest)

#Feature extraction

#The total duration between last payment date and account opened date of all accounts
acc$e_total_diff_lastpaymt_opened_dt <- as.Date(as.character(acc$last_paymt_dt), format="%d-%b-%y")-
  as.Date(as.character(acc$opened_dt), format="%d-%b-%y")

acc$e_total_diff_lastpaymt_opened_dt[acc$e_total_diff_lastpaymt_opened_dt<0]<-0

e_total_dur_pay_opendt<-aggregate(e_total_diff_lastpaymt_opened_dt ~ customer_no, acc, sum)

#Ratio_currbalance_creditlimit
e_curr_balance<-aggregate(cur_balance_amt ~ customer_no, acc, sum)
acc$creditlimit[acc$creditlimit == "0"] <- 1
acc$creditlimit[is.na(acc$creditlimit)] <- 1 
e_credit_limit<-aggregate(creditlimit ~ customer_no, acc, sum)

Ratio_currbalance_creditlimit <- (e_curr_balance/e_credit_limit)
dist_c = acc %>% distinct(customer_no)
R<-cbind(dist_c,Ratio_currbalance_creditlimit)
R<-R[,-2]

x <- data.frame(customer_no = 1,cur_balance_amt = 2)
colnames(R) <- c("customer_no","Ratio_currbalance_creditlimit")


#The average duration between last payment date and account opened date of all accounts 
e_mean_diff_lastpaymt_opened_dt <- as.Date(as.character(acc$last_paymt_dt), format="%d-%b-%Y")-
  as.Date(as.character(acc$opened_dt), format="%d-%b-%Y")
e_mean_diff_lastpaymt_opened_dt[e_mean_diff_lastpaymt_opened_dt<0]<-0

e_mean_diff_lastpaymt_opened_dt_final<-aggregate(e_mean_diff_lastpaymt_opened_dt ~ customer_no, acc, mean)

#most frequent enquiry purpose

#enq$enq_purpose[is.na(enq$enq_purpose)] <- 999

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

enq$freq_enq <-  with(enq, ave( enq$enq_purpose,enq$customer_no, FUN=Mode))
bigdf <- sqldf("select distinct(customer_no),freq_enq from enq")
bigdf$freq_enq<-as.numeric(bigdf$freq_enq)


#ratio of secured loan type enquiry purpose to total enquiry purpose made

eg<-aggregate(enq_purpose~customer_no,FUN=length,data=enq)
sqldf("select customer_no, count(enq_purpose) from enq GROUP BY customer_no")
enq_filter_1<-filter(enq, enq$enq_purpose == c(1,2,3,4))
enq_filter_2<-filter(enq, enq$enq_purpose == c(7,11,55,56))
enq_filter_3<-filter(enq, enq$enq_purpose == c(57,58,59,60))
enq_filter_4<-filter(enq, enq$enq_purpose == c(33,34,42,51))
enq_filter_5<-filter(enq, enq$enq_purpose == c(52,53,54,13))
enq_filter_6<-filter(enq, enq$enq_purpose == c(14,15,17,31))
enq_filter_7<-filter(enq, enq$enq_purpose == c(32))
enq_filter<-rbind(enq_filter_1,enq_filter_2,enq_filter_3,enq_filter_4,enq_filter_5,enq_filter_6,enq_filter_7)

eg_fiter<-aggregate(enq_purpose~customer_no,FUN=length,data=enq_filter)


eg_join<-left_join(eg, eg_fiter, by = c("customer_no"="customer_no"))


eg_join$ratio<-(eg_join$enq_purpose.y/eg_join$enq_purpose.x)
eg_join$ratio[is.na(eg_join$ratio)] <- '0' #customer has no secured loan, didnt impute 1 bcoz already 1 is there so bias
ratio_securedloan<-eg_join[,c(1,4)]

#average length of payment_history variable

e_payment_history_variable<-as.Date(as.character(acc$paymt_str_dt), format="%d-%b-%Y")-
  as.Date(as.character(acc$paymt_end_dt), format="%d-%b-%Y")

e_payment_history_mean_length<-aggregate(e_payment_history_variable ~ customer_no, acc, mean)


#average difference between enquiry dt_opened date and enquiry date

e_diff_enquiry<-as.Date(as.character(enq$dt_opened), format="%d-%b-%Y")-
  as.Date(as.character(enq$enquiry_dt), format="%d-%b-%Y")

e_avg_diff_enquiry<-aggregate(e_diff_enquiry ~ customer_no, enq, mean)

#mean count of accounts that is in 0-29 dpd bucket throughout the payment history

acc$paymenthistory2<-substr(acc$paymenthistory2, 4, 36)
acc$dpd_29 <- paste(acc$paymenthistory1,acc$paymenthistory2)
grx <- glob2rx("*1*|*2*|*3*|*4*|*5*|*6*|*7*|*8*|*9*|*STD*|*SMA*|*SUB*|*DBT*|*LSS*") 
acc_new <- acc[grepl(grx, acc[["dpd_29"]]), ]

dpds_29 <- aggregate(dpd_29~customer_no,acc_new,FUN=NROW)
lj9$dpd_29[is.na(lj9$dpd_29)] <- '?' #to avoid na

#utilisation_trend
  
#[total cur_bal_amt / total credit limit]/[mean cur_bal_amt / (mean credit limit+ mean_cashlimit)]


total_cur_bal_amt <-aggregate(cur_balance_amt ~ customer_no, acc, sum)
total_credit_limit <-aggregate(creditlimit ~ customer_no, acc, sum)
mean_cur_bal_amt <-aggregate(cur_balance_amt ~ customer_no, acc, mean)
mean_credit_limit <- aggregate(creditlimit ~ customer_no, acc, mean)
mean_cashlimit <-aggregate(cashlimit ~ customer_no, acc, mean)

utilisation_trend_1<-(total_cur_bal_amt/total_credit_limit)
utilisation_trend_2<-(mean_credit_limit+mean_cashlimit)
utilisation_trend_3<-(mean_cur_bal_amt/utilisation_trend_2)
utilisation_trend_f<- (utilisation_trend_1/utilisation_trend_3)

dist_c = acc %>% distinct(customer_no)
dist_ca<-sort(dist_c$customer_no)

utilisation_trend<-cbind(dist_ca,utilisation_trend_f)
utilisation_trend<-utilisation_trend[,-2]

x <- data.frame(customer_no = 1,cur_balance_amt = 2)
colnames(utilisation_trend) <- c("customer_no","utilisation_trend")


#Merging multiple dataframes

ljoin_1<-left_join(rd, e_total_dur_pay_opendt, by = c("customer_no"="customer_no"))
lj2<-left_join(ljoin_1, R, by = c("customer_no"="customer_no"))
lj3<-left_join(lj2, e_mean_diff_lastpaymt_opened_dt_final, by = c("customer_no"="customer_no"))
lj4<-left_join(lj3, bigdf, by = c("customer_no"="customer_no"))
lj5<-left_join(lj4, ratio_securedloan, by = c("customer_no"="customer_no"))
lj6<-left_join(lj5, e_payment_history_mean_length, by = c("customer_no"="customer_no"))
lj7<-left_join(lj6, e_avg_diff_enquiry, by = c("customer_no"="customer_no"))
lj8<-left_join(lj7, dpds_29, by = c("customer_no"="customer_no"))
lj9<-left_join(lj8, utilisation_trend, by = c("customer_no"="customer_no"))
lj9$dpd_29[is.na(lj9$dpd_29)] <- '?' #to avoid na
lj9$dpd_29<-as.factor(lj9$dpd_29)
lj9$ratio<-as.factor(lj9$ratio)
lj9$e_total_diff_lastpaymt_opened_dt[is.na(lj9$e_total_diff_lastpaymt_opened_dt)] <- 0
lj9$e_mean_diff_lastpaymt_opened_dt[is.na(lj9$e_mean_diff_lastpaymt_opened_dt)] <- 0
lj9$e_diff_enquiry[is.na(lj9$e_diff_enquiry)]<-0
lj9$ratio[is.na(lj9$ratio)]<-'0'
#Subsetting data
lj9<-lj9[,c(4,30,35,39,40,54,83,84,85,86,87,88,89,90,91,92)]
write.csv(lj9,"C:\\Users\\andiapps\\Desktop\\Data\\lj9.csv")
lj9_d<-read.csv("C:\\Users\\andiapps\\Desktop\\Data\\lj9_d.csv")#reading discretized output from Genie

#Train and Test split

cutoff_d = round(0.7*nrow(lj9_d))
train_d<-lj9_d[1:cutoff_d,]
test_d<-lj9_d[-(1:cutoff_d),]

write.csv(train_d,"C:\\Users\\andiapps\\Desktop\\Data\\train.csv")
write.csv(test_d,"C:\\Users\\andiapps\\Desktop\\Data\\cats.csv")
#test<-test_d[,-16]

train_d<-train_d[,c(1:6,8:16,7)]
colnames(train_d)[16]<-"out"

test<-test_d[,-7]#test data

#Data Balancing

input<-train_d[,-16]
output<-train_d[,16]
train_d[,16]<-as.factor(train_d[,16])

data<-ubBalance(X= train_d[,-16], Y=train_d[,16], type="ubSMOTE", percOver=500, percUnder=250, verbose=TRUE)
out<-data$Y
balancedData<-cbind(data$X,out)
colnames(train_d)[16]<-'out'
filter_balance=filter(balancedData, out==1)
Bigdata_credit<-rbind(train_d,balancedData)#train data
Bigdata_credit_decisiontree<-Bigdata_credit
write.csv(Bigdata_credit,"C:\\Users\\andiapps\\Desktop\\Data\\bigdatacredit.csv")
Bigdata_credit$freq_enq[is.na(Bigdata_credit$freq_enq)] <- 111

#Model Building

#Decision Tree

# grow tree 
fit <- rpart(out ~ ., data = Bigdata_credit_decisiontree, method="class")
#Predict Output 
predicted= predict(fit,test,type="class")
test$out <- predict(fit, newdata = test, type="class")
table(predicted)
table<-table(predicted,test_d$Bad_label)
accuracy<-sum(diag(table))/sum(table)
fancyRpartPlot(fit)
asRules(fit)


#Random Forest
x_train<-lj9[,-16]
y_train<-lj9[,16]

x <- cbind(x_train,y_train)
# Fitting model
fit <- randomForest(out ~ utilisation_trend+e_payment_history_variable+e_diff_enquiry
                    +Ratio_currbalance_creditlimit+e_total_diff_lastpaymt_opened_dt
                    +freq_enq+feature_1+feature_27+feature_32+feature_36+feature_37+feature_51
                    +e_mean_diff_lastpaymt_opened_dt ,Bigdata_credit ,ntree=500)

summary(fit)
#Predict Output 
predicted= predict(fit,test)
table(predicted)
table<-table(predicted,test_d$Bad_label)
accuracy<-sum(diag(table))/sum(table)
importance(fit)
varImpPlot(fit)

pred1=predict(fit,type = "prob")

perf = prediction(pred1[,2], Bigdata_credit$out)

# 1. Area under curve
auc = performance(perf, "auc")
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

