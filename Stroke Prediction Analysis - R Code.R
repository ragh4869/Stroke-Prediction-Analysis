
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(corrplot)
library(GoodmanKruskal)
library(dplyr)
library(rpart.plot)
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)

#load the dataset into the environment
df<-healthcare.dataset.stroke.data
df <- df %>% subset(bmi!='N/A' & gender!='Other')
df$stroke<-as.character(df$stroke)

df1<-subset(df,stroke==1)
df0<-subset(df,stroke==0)


my.theme <- theme_classic()  + theme(aspect.ratio = 1)

ggplot(df0, aes(x=avg_glucose_level, fill = stroke)) +
  geom_density(alpha=0.3)+
geom_density(data=df1,aes(x=avg_glucose_level,fill=stroke),alpha=0.3)+
my.theme+
labs(x="Average Glucose Level",y="Density",title="Distribution of Average Glucose Level Among the Cohorts")


ggplot(df, aes(x=bmi,  group=stroke)) + 
  geom_boxplot(aes(fill=stroke))+my.theme+labs(x="BMI",title="Distribution of BMI Among the Cohorts")


ggplot(df0, aes(x=age, fill = stroke)) +
  geom_density(alpha=0.3)+
geom_density(data=df1,aes(x=age,fill=stroke),alpha=0.3)+my.theme+
labs(x="Age",title="Distribution of Age Among the Cohorts")


s0 <- df %>% subset(stroke==0) %>% group_by(smoking_status) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s1 <- df %>% subset(stroke==1) %>% group_by(smoking_status) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s0$stroke <- 0
s1$stroke <- 1
ss <- rbind(s0,s1)
#ss$smoking_status <- as.integer(ss$smoking_status)
ggplot(ss, aes(fill=smoking_status, y=freq, x=factor(stroke))) + 
  geom_bar(position="stack", stat="identity") + labs(x = "Stroke", y = "Percentage", title = "Plot of Smoking Status vs Stroke") 
#+ scale_color_manual(name = "Gender", values = c("Female" = "magenta", "Male" = "blue"))



ggplot(df) + 
  geom_mosaic(aes(product(stroke,gender),fill= gender)) + facet_wrap(~heart_disease) + 
  labs(x='Heart Disease', y='Stroke', title='Mosaic Plot of Gender & Heart Disease vs Stroke')


ggplot(df) + 
  geom_mosaic(aes(product(stroke,Residence_type),fill= Residence_type)) + facet_wrap(~hypertension) + 
  labs(x='Hypertension', y='Stroke', title='Mosaic Plot of Type of Residence & Hypertension vs Stroke')


s0 <- df %>% subset(stroke==0) %>% group_by(work_type) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt),2))
s1 <- df %>% subset(stroke==1) %>% group_by(work_type) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt),2))
s1 <- rbind(s1,c('Never_worked',0,0.000000000))
s0$stroke <- 0
s1$stroke <- 1
ss <- rbind(s0,s1)
ss$stroke <- as.factor(ss$stroke)
ggplot(ss, aes(fill=stroke, y=freq, x=work_type)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Work Type ", y = "Percentage", title = "Plot of Work Type vs Stroke")+ 
  scale_color_manual(name = "Stroke", values = c("0" = "magenta", "1" = "blue"))

age_bracket <- c()
for (i in df$age) {
  if (i<=10) {
    age_bracket <- c(age_bracket,"1-10")
  } else if (i<=20) {
    age_bracket <- c(age_bracket,"11-20")
  } else if (i<=30) {
    age_bracket <- c(age_bracket,"21-30")
  } else if (i<=40) {
    age_bracket <- c(age_bracket,"31-40")
  } else if (i<=50) {
    age_bracket <- c(age_bracket,"41-50")
  } else if (i<=60) {
    age_bracket <- c(age_bracket,"51-60")
  } else if (i<=70) {
    age_bracket <- c(age_bracket,"61-70")
  } else {
    age_bracket <- c(age_bracket,"70+")
  }
} 

df$age_bracket <- age_bracket


df %>% subset(ever_married=='Yes') %>% ggplot() + 
  geom_mosaic(aes(product(stroke,age_bracket),fill= age_bracket)) +
  labs(x = "Age Bracket", y = "Stroke", title = "Mosaic Plot of Age Bracket vs Stroke for the Married")


df %>% subset(ever_married=='No') %>% ggplot() + 
  geom_mosaic(aes(product(stroke,age_bracket),fill= age_bracket)) +
  labs(x = "Age Bracket", y = "Stroke", title = "Mosaic Plot of Age Bracket vs Stroke for the Un-Married")

varSet1 <- c( "gender","heart_disease","hypertension","ever_married","work_type","Residence_type","smoking_status","stroke" )
CarFrame1 <- subset(df, select = varSet1)
GKmatrix1 <- GKtauDataframe(CarFrame1)
plot(GKmatrix1)

s0 <- df %>% subset(ever_married=='Yes') %>% group_by(work_type) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s1 <- df %>% subset(ever_married=='No') %>% group_by(work_type) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s0 <- rbind(s0,c('Never_worked',0,0.00000))
s0 <- rbind(s0,c('children',0,0.00000))
s0$ever_married <- 'Yes'
s1$ever_married <- 'No'
ss <- rbind(s0,s1)
ggplot(ss, aes(fill=ever_married, y=freq, x=work_type)) + 
  geom_bar(position="dodge", stat="identity")


my.theme <- theme_classic()  + theme(aspect.ratio = 1)
ggplot(df, aes(y = avg_glucose_level, x = age)) + geom_point(aes(color=stroke),alpha=0.8) 
+geom_density2d()+my.theme



df <- df %>% subset(bmi!='N/A' & gender!='Other')
varSet2 <- c( 'age', 'avg_glucose_level','bmi','stroke')
dfnum <- subset(df, select = varSet2)	


dfnum <- sapply(dfnum, as.numeric)
res <- cor(dfnum)


corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)+my.theme


df0sub<-df0[0:209,]
df1sub<-df1[0:209,]
rf <- rbind(df1sub, df0sub)

shuffle_index <- sample(1:nrow(rf))
rf <- rf[shuffle_index, ]

drop_rf <- rf %>%select(-c(id, Residence_type, gender,bmi))
rf<-drop_rf

create_train_test <- function(data, size = 0.9, rf = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  
  if (rf == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(rf, 0.8, rf = TRUE)
data_test <- create_train_test(rf, 0.8, rf = FALSE)
dim(data_train)

print(dim(data_train))
print(dim(data_test))


data_train$ever_married = as.factor(data_train$ever_married)
data_train$work_type = as.factor(data_train$work_type)
data_train$smoking_status = as.factor(data_train$smoking_status)
data_train$hypertension = as.factor(data_train$hypertension)
data_train$heart_disease = as.numeric(data_train$heart_disease)
data_train$stroke = as.factor(data_train$stroke)


data_test$ever_married = as.factor(data_test$ever_married)
data_test$work_type = as.factor(data_test$work_type)
data_test$smoking_status = as.factor(data_test$smoking_status)
data_test$hypertension = as.factor(data_test$hypertension)
data_test$heart_disease = as.numeric(data_test$heart_disease)
data_test$stroke = as.factor(data_test$stroke)


#min-max scalar
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}


data_train$avg_glucose_level<-normalize(data_train$avg_glucose_level)
data_test$avg_glucose_level<-normalize(data_test$avg_glucose_level)


# define function that reruns factor() for the right columns
rerun_factor <- function(x) {
  if (is.factor(x)) return(factor(x))
  return(x)
}

# run on all columns of your data
training_data <- as.data.frame(lapply(data_train, rerun_factor))
training_data <-as.data.frame(training_data)


testing_data <- as.data.frame(lapply(data_test, rerun_factor))
testing_data <-as.data.frame(testing_data)

tr <- rpart(stroke~.,data = training_data, method = 'class',minsplit =10, minbucket =10,cp=0.005)	
rpart.plot(tr)

predict_unseen <-predict(tr, testing_data, type = 'class')

table_mat <- table(testing_data$stroke, predict_unseen)
table_mat

cfm<-as.data.frame(table_mat)
cfm

library(cvms)
library(tibble)
plot_confusion_matrix(cfm, 
                      target_col = "Var1", 
                      prediction_col = "predict_unseen",
                      counts_col = "Freq")

drop_rf <- rf %>%select(-c(id, Residence_type, gender,bmi))

suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))

## FAMD
res.famd <- FAMD(drop_rf, 
                 sup.var = 20,  ## Set the target variable "Churn" as a supplementary variable
                 graph = FALSE, 
                 ncp=25)

## Inspect principal components
get_eigenvalue(res.famd)

library(PCAmixdata)
split <- splitmix(rf[1:7])  

## FAMD
res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, 
                     rename.level=TRUE, 
                     graph=FALSE, 
                     ndim=10)

## Inspect principal components

fviz_famd_ind(res.famd, label = "none", 
              habillage = "stroke", palette = c("#00AFBB", "#FC4E07"), # color by groups 
              repel = TRUE,alpha.ind = 0.5) + 
  xlim(-6, 6) + ylim (-5, 5) +
  theme(text = element_text(size=10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+my.theme

#appendix
s0 <- df %>% subset(stroke==0) %>% group_by(gender) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s1 <- df %>% subset(stroke==1) %>% group_by(gender) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s0$stroke <- 0
s1$stroke <- 1
ss <- rbind(s0,s1)
ss$stroke <- as.integer(ss$stroke)
ggplot(ss, aes(fill=gender, y=freq, x=factor(stroke))) + 
  geom_bar(position="stack", stat="identity") + labs(x = "Stroke", y = "Percentage", title = "Plot of Gender vs Stroke") + scale_color_manual(name = "Gender", values = c("Female" = "magenta", "Male" = "blue"))
#+ scale_x_discrete(name="stroke") 
#+ 
#+ 
s0 <- df %>% subset(stroke==0) %>% group_by(Residence_type) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s1 <- df %>% subset(stroke==1) %>% group_by(Residence_type) %>% summarise(cnt = n()) %>% mutate(freq = (cnt / sum(cnt)))
s0$stroke <- 0
s1$stroke <- 1
ss <- rbind(s0,s1)
ggplot(ss, aes(fill=Residence_type, y=freq, x=factor(stroke))) + 
  geom_bar(position="stack", stat="identity") + labs(x = "Stroke", y = "Percentage", title = "Plot of Residence Type vs Stroke") + scale_color_manual(name = "Gender", values = c("Female" = "magenta", "Male" = "blue"))

