data_new=read.csv("C:/Users/venuk/OneDrive/Desktop/projects/Project 2_Logistic_R (1)/bankloan.csv")
View(data_new)
str(data_new)
summary(data_new)


#_______________________________________________

###EDA
boxplot(data_new)
sum(is.na(data_new))
names(data_new)

table(data_new$age)

###29 age people are having more loan (44)
###in general people from age 49-56 are having less loans


ed_data=table(data_new$ed)

ed_data
barplot(ed_data,xlab = "Education Level",ylab = "freq",main = "BARPLOT FOR EDUCATION LEVEL",col = c("green","blue","grey","orange","yellow"))



##  53% people have level 1 education 


emp_data=table(data_new$employ)
emp_data

##people who have less exp less than 1 took loan mostly(62)

#470      1 to 10 
#187      11 to 20
#43        21 to 31

#people having less that 10 years of exp havng more loan (67%)

exp_1_10=470
exp_11_20=187
exp_21_31=43

exP_data=barplot(c(exp_1_10,exp_11_20,exp_21_31),names.arg = c("exp 1:10","exp 11:20","exp 21:31"),main="BARPLOT FOr EMP",col = c("blue","pink","grey"))

#____________________________________________________________________________________


address_data=data_new$address
hist(address_data,col = c("grey","pink","orange","blue","green"),xlab = "people living in same house(years)")
table(address_data)

#0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 29 31 34 
#50 57 59 48 49 34 43 33 40 39 32 27 20 18 22 16 18 17  9 13  7  9  7  9  3  7  7  3  1  2  1 

##

income_data=data_new$income
hist(income_data,col = c("grey","pink","orange"))


#most of people are having income lessthan 50


#____________________________________________________________________________________

balanced_data=table(data_new$default)
balanced_data
barplot(balanced_data)



##conclude that the data is unbalanced data


#______________________________________________________________________________________
set.seed(100)
sample_data=sample(2,nrow(data_new),replace = T,prob = c(.7,.3))
train_data=data_new[sample_data==1,]
test_data=data_new[sample_data==2,]
dim(train_data)
dim(test_data)






#__________________________________________________________________________________

model=glm(default~.,data=train_data,family = "binomial")
summary(model)

model_1=glm(default~employ+address+creddebt,data=train_data,family = "binomial")
summary(model_1)


#confusion_matrix_train


predicted_data_train=predict(model_1,train_data,type="response")
head(predicted_data_train)
table(train_data$default,predicted_data_train>=0.4)
acc_train_data=(307+78)/(307+52+46+78)
acc_train_data

#0.797



#confusion_matrix_test


predicted_data=predict(model_1,test_data,type="response")
head(predicted_data)
table(test_data$default,predicted_data>=0.4)

accu_test_data=(128+37)/(128+30+37+22)
accu_test_data  #0.7603687









library(ROCR)
roc_vector_train=prediction(predicted_data_train,train_data$default)
roc_data_train=performance(roc_vector_train,"tpr","fpr")
plot(roc_data_train,colorize=T)
accuracy_data_curve_train=performance(roc_vector_train,"acc")
plot(accuracy_data_curve_train)

#______________________________________________________________________________

library(ROCR)
roc_vector=prediction(predicted_data,test_data$default)
roc_data=performance(roc_vector,"tpr","fpr")
plot(roc_data,colorize=T)
accuracy_data_curve=performance(roc_vector,"acc")
plot(accuracy_data_curve)



#threshold  0.5
#_______________________________________________________________________
###TEST DATA


#FALSE TRUE
#0   128   30
#1    22   37
#FALSE TRUE
#0   147   11
#1    36   23

senstivity=(37)/(37+22)     ###true positive rate      

senstivity     #0.6271186


precision=(37)/(37+30)
precision       #0.6764706

specificity=147/(147+11)
specificity     #0.9303797

fscore=(2*senstivity*precision)/(precision+senstivity)
fscore    #0.5873016

true_negative_rate=(128)/(128+22)
true_negative_rate#0.8533333



#__________________________________________________________________________
#confusion_matrix_train

#FALSE TRUE
#0   327   32         
#1    67   57

#FALSE TRUE
#0   307   52
#1    46   78


senstivity_train=(78)/(78+46)     ###true positive rate     

senstivity_train   # 0.6290323


precision_train=(78)/(78+52)
precision_train     #0.6

specificity_train=307/(307+52)
specificity_train    #0.8551532
 
fscore_train=(2*senstivity_train*precision_train)/(precision_train+senstivity_train)
fscore_train   #0.6141732

true_negative_rate_train=(307)/(307+46)
true_negative_rate_train

#0.8696884



#senstivity=(37)/(37+22)     ###true positive rate      

#senstivity     #0.6271186


#precision=(37)/(37+30)
#precision       #0.6764706

#specificity=147/(147+11)
#specificity     #0.9303

#fscore=(2*senstivity*precision)/(precision+senstivity)
#fscore    #0.5873016

#true_negative_rate=(128)/(128+22)
#true_negative_rate#0.8533333


#___________________________________________________________________________________________________
i



#_________________________________________________________________________________
#area under curve

auc_data=performance(roc_vector,"auc")
auc_data
auc_value=unlist(slot(auc_data,"y.values"))
auc_value
plot(roc_data,colorize=T)   #0.8163484
                                           


auc_data_train=performance(roc_vector_train,"auc")
auc_data_train
auc_value_train=unlist(slot(auc_data_train,"y.values"))
auc_value_train
plot(roc_data_train,colorize=T)    #0.8515702


                              
                                    



   

#_____________________________________________________________________________________________
#____________________________________________________________________________________________
#_________________________________________________________________________________________________
#____________________________________________________________________________________________--

#__________________________________________________________________________________________________

#the data is imbalaned so we are doing over sampling
library(ROSE)
dim(data_new)
table(data_new$default)
data_balanced_over = ovun.sample(default ~ ., data = data_new, method = "over",N =1034)$data
class(data_balanced_over)
dim(data_balanced_over)
table(data_balanced_over$default)
set.seed(100)
sample_data_balanced=sample(2,nrow(data_balanced_over),replace = T,prob = c(.7,.3))
train_data_balanced=data_balanced_over[sample_data_balanced==1,]
dim(train_data_balanced)
test_data_balanced=data_balanced_over[sample_data_balanced==2,]
dim(test_data_balanced)
balanced_glm=glm(default~.,data=train_data_balanced,family = "binomial")
summary(balanced_glm)
balanced_model=glm(default~employ+address+income+creddebt,data=train_data_balanced,family = "binomial")


summary(balanced_model)


#confusion_matrix for train_data


predicted_data_balanced=predict(balanced_model,train_data_balanced,type="response")
head(predicted_data_balanced)
length(predicted_data_balanced)
table(train_data_balanced$default,predicted_data1>=0.5)

#    FALSE TRUE
#0   278   87
#1    76  263

acu_data_train_balance=(278+263)/(278+263+87+76)
acu_data_train_balance     #0.7684659

#confusion matrix for test data
predicted_data_balanced_test=predict(balanced_model,test_data_balanced,type="response")
head(predicted_data_balanced_test)
length(predicted_data_balanced_test)
table(test_data_balanced$default,predicted_data_balanced_test>=0.5)

acc_data_test_balance=(110+140)/(110+140+38+42)
acc_data_test_balance    #0.7575758
#FALSE TRUE
#0   110   42
#1    38  140



##for train_data

library(ROCR)
roc_vector1_train_balance=prediction(predicted_data_balanced,train_data_balanced$default)
roc_data_data_balance_train=performance(roc_vector1_train_balance,"tpr","fpr")
accuracy_1=performance(roc_vector1_train_balance,"acc")
plot(roc_data1,colorize=T)
plot(accuracy_1)


#for test data

roc_vector1_test_balance=prediction(predicted_data_balanced_test,test_data_balanced$default)
roc_data_data_balance_test=performance(roc_vector1_test_balance,"tpr","fpr")
accuracy__test_balance=performance(roc_vector1_test_balance,"acc")

plot(roc_data1,colorize=T)
plot(accuracy_1)





###threshold 0.6 we are getting more accuracy


#_________________________________________________________________________________
#area under curve

auc_data1_balance_train=performance(roc_vector1_train_balance,"auc")
auc_data1_balance_train
auc_value_train_value=unlist(slot(auc_data1_balance_train,"y.values"))
auc_value_train_value

#0.849307




auc_data1_balance_test=performance(roc_vector1_test_balance,"auc")
auc_data1_balance_test
auc_value_test_value=unlist(slot(auc_data1_balance_test,"y.values"))
auc_value_test_value

#0.8446555

##plotiinf roc curves in one graph





#___________________________________________________________________________________________________________-
  
