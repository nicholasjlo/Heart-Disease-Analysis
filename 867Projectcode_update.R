library(tidyverse)


# download the dataset
heart <- read.csv(file.choose())
# investigate the dataset
str(heart)
summary(heart)  
# This dataset had 303 observations and 14 variables 
glimpse(heart)
# The columns have names that are difficult to intrepret - rename them
colnames(heart) <- c('Age','Sex','Chest_Pain_Type','Resting_Blood_Pressure','Cholesterol','Fasting_Blood_Sugar','Resting_Electrocardio_Result',
                  'Max_Heart_Rate', 'Exercise_Induced_Angina','Oldpeak','Slope','Num_Major_Vessels','Thalium_Stress_Test','Target')
head(heart)

#Data Clean
# check for missing values
install.packages("Amelia")
library(Amelia)

missmap(heart, col=c("red", "black"), legend=TRUE, main="Missing values", margins = c(10, 5))
colSums(is.na(heart))
#  no missing values 

# investigate duplicates
subset(heart,duplicated(heart[1:14]))
# there may be a duplicate - use Age 38 t0 investigate further
heart[heart$Age == 38,]
# there are 2 that are exactly the same - remove one

heart <- distinct(heart)

# check the description of each of the available variables and look for problems
library(psych)
describe(heart)

# check the values for each variable
values <- lapply(heart, unique)
values

# compare with the directory there are there are 2 issues:
# Thalium_Stress_Test if there is a 0 this is NA
#Num_Major_Vessels if there is a 4 this is NA

heart[heart$Num_Major_Vessels == 4,]
heart[heart$Thalium_Stress_Test == 0,]
# 6 rows in total I will remove
heart<-subset(heart, Num_Major_Vessels != 4  & Thalium_Stress_Test != 0)
dim(heart)
# now 296 14

# let's check the correlation 
library(corrplot)
cor_heart <- cor(heart)
corrplot(cor_heart)
# Not many of the x variables are strongly correlated with the y

pairs.panels(heart)
# review correlations/distributions


# need to change the categorical columns to factors
  #'Sex','Chest_Pain_Type','Resting_Blood_Pressure','Cholesterol','Fasting_Blood_Sugar','Resting_Electrocardio_Result',
                  
heart$Sex<- as.factor(heart$Sex)
heart$Chest_Pain_Type<- as.factor(heart$Chest_Pain_Type)
heart$Fasting_Blood_Sugar<- as.factor(heart$Fasting_Blood_Sugar)
heart$Slope<- as.factor(heart$Slope)
heart$Resting_Electrocardio_Result<- as.factor(heart$Resting_Electrocardio_Result)
heart$Exercise_Induced_Angina<- as.factor(heart$Exercise_Induced_Angina)
heart$Thalium_Stress_Test<- as.factor(heart$Thalium_Stress_Test)
heart$Num_Major_Vessels<- as.factor(heart$Num_Major_Vessels)
heart$Target<- as.factor(heart$Target)


str(heart)
table(heart$Target)

#variable exploration 


# Target (Heart disease) 

ggplot(heart, aes(x=Target, fill=Target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Existence and NonExistence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("NonExistence", "Existence"))

# Age variable review

hist(heart$Age)

younger <- heart[which((heart$Age<45)), ]
middle <- heart[which((heart$Age>=45)&(heart$Age<55)), ]
older <- heart[which(heart$Age>55), ]
groups <- data.frame(age_group = c("younger","middle","older"), group_count = c(NROW(younger$Age), NROW(middle$Age), NROW(older$Age)))

ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Insights") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Age Group", labels = c("Older", "Middle", "Younger"))


# If in agree add extra age groups to the main dataset
#heart <- cbind(heart, groups = ifelse((heart$Age<45), 0, ifelse((heart$Age>=45)&(heart$Age<55), 1, 2)))
#heart$groups <- as.factor(heart$groups)
# we will remove the age column as this is very generalised column and we have divided it, group, to include 
# that in our analysis more specifically.

#heart = subset(heart, select = c(-age))

#Sex and target insights 0 is female, 1 is male
ggplot(heart, aes(x= Sex, fill=Target)) + 
  geom_bar() +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("Analysis of Sex") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


# Chest pain
# Bar plot for The chest pain experienced 
ggplot(heart, aes(x= Chest_Pain_Type , fill=Chest_Pain_Type )) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Investigating Chest Pain") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))


#How does this compare with Target variable
ggplot(heart, aes(x= Chest_Pain_Type, fill=Target)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle(" Chest Pain and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#  (number of major vessels which brings blood to the heart)
ggplot(heart, aes(x= Num_Major_Vessels, fill=Num_Major_Vessels)) + 
  geom_bar() +
  xlab("Number of Major Blood vessels") +
  ylab("Count") +
  ggtitle("Number of Major Blood Vessels") +
  theme(legend.position="none")

# how does this affect the target variable? - more vessels bring blood to heart less problems
ggplot(heart, aes(x=Num_Major_Vessels, fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab("Number of Major Blood Vessels") +
  ylab("Count") +
  ggtitle("Number of Major Blood Vessels and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

### Resting Blood presuure

ggplot(heart, aes(x=Resting_Blood_Pressure)) + 
  geom_histogram(bins = 30) +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

# note there are some outliers - should we drop them?
#heart$Resting_Blood_Pressure = ifelse(heart$Resting_Blood_Pressure> 180, NA, heart$Resting_Blood_Pressure)
#heart$Resting_Blood_Pressure = ifelse(is.na(heart$Resting_Blood_Pressure), median(heart$Resting_Blood_Pressure[which(!is.na(heart$Resting_Blood_Pressure))]), heart$Resting_Blood_Pressure)

# resting blood pressure and target insights
ggplot(heart, aes(x = Resting_Blood_Pressure, fill = Target)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

### old peak Analysis

ggplot(heart, aes(x=Oldpeak)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("ST depression induced by Exercise : Rest") +
  ylab("Count") +
  ggtitle("ST depression induced by Exercise : Rest")

# notice this is skewed # should be log this?

#heart$Oldpeak <- log1p(heart$Oldpeak)
# see new histogram repeat above

# Oldpeak compared to the target variable

ggplot(heart, aes(x = Oldpeak, fill = Target)) +
  geom_density(alpha=0.5) +
  xlab("ST drepression Exercise: Rest") +
  ylab("Count") +
  ggtitle( "ST depression and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


# investigate the slope 1 = up slope 2= flat  3 = down slope


ggplot(heart, aes(x= Slope, fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab("Slope of the Peak ST Segment") +
  ylab("Count") +
  ggtitle("Slope of the peak ST segment with Existence or Non of Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


# review of max heart rate
ggplot(heart, aes(x= Max_Heart_Rate )) + 
  geom_histogram() +
  xlab("Max_Heart_Rate ") +
  ylab("Count") +
  ggtitle("Max_Heart_Rate ")


# Note there are also some outliers here - should we remove?
#heart$Max_Heart_Rate= ifelse(heart$Max_Heart_Rate < 75, NA, heart$Max_Heart_Rate)
#heart$Max_Heart_Rate = ifelse(is.na(heart$Max_Heart_Rate), median(heart$Max_Heart_Rate[which(!is.na(heart$Max_Heart_Rate))]), heart$Max_Heart_Rate)

#How does Max Heart this affect target?

ggplot(heart, aes(x = Max_Heart_Rate , fill = Target)) +
  geom_density(alpha=0.5) +
  xlab("Maximum Heart Rate") +
  ylab("Count") +
  ggtitle("Max heart rate and Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Cholesterol

ggplot(heart, aes(x= Cholesterol )) + 
  geom_histogram(binwidth = 30) +
  xlab("Cholesterol ") +
  ylab("Count") +
  ggtitle("Cholesterol")

### note the outlier? remove?
#heart$Cholesterol= ifelse(heart$Cholesterol > 500, NA, heart$Cholesterol)
#heart$Cholesterol = ifelse(is.na(heart$Cholesterol), median(heart$Cholesterol[which(!is.na(heart$Cholesterol))]), heart$Cholesterol)


# Plot vs Target
ggplot(heart, aes(x = Cholesterol , fill = Target)) +
  geom_density(alpha=0.5) +
  xlab("Cholesterol") +
  ylab("Count") +
  ggtitle("Cholesterol and Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))


# Fasting blood sugar
ggplot(heart, aes(x= Fasting_Blood_Sugar , fill=Fasting_Blood_Sugar)) + 
  geom_bar() +
  xlab("Fasting_Blood_Sugar ") +
  ylab("Count") +
  ggtitle("Fasting_Blood_Sugar ") +
  theme(legend.position="none")

# how does this affect the target variable? - more vessels bring blood to heart less problems
ggplot(heart, aes(x=Fasting_Blood_Sugar , fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab("Fasting_Blood_Sugar ") +
  ylab("Count") +
  ggtitle("Fasting_Blood_Sugar and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Thalium Stress test
ggplot(heart, aes(x= Thalium_Stress_Test  , fill=Thalium_Stress_Test)) + 
  geom_bar() +
  xlab("Thalium_Stress_Test") +
  ylab("Count") +
  ggtitle("Thalium_Stress_Test") +
  theme(legend.position="none")

# how does this affect the target variable? - 
ggplot(heart, aes(x= Thalium_Stress_Test , fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab(" Thalium_Stress_Test ") +
  ylab("Count") +
  ggtitle("Thalium_Stress_Test  and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


# Exersise Induced Angina

ggplot(heart, aes(x= Exercise_Induced_Angina, fill=Exercise_Induced_Angina )) + 
  geom_bar() +
  xlab("Exercise_Induced_Angina ") +
  ylab("Count") +
  ggtitle("Exercise_Induced_Angina") +
  theme(legend.position="none")

# how does this affect the target variable? - 
ggplot(heart, aes(x= Exercise_Induced_Angina, fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab("Exercise_Induced_Angina") +
  ylab("Count") +
  ggtitle("Exercise_Induced_Angina  and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#Resting_Electrocardio_Result
ggplot(heart, aes(x=Resting_Electrocardio_Result, fill=Resting_Electrocardio_Result)) + 
  geom_bar() +
  xlab("Resting_Electrocardio_Result") +
  ylab("Count") +
  ggtitle("Resting_Electrocardio_Result") +
  theme(legend.position="none")

# how does this affect the target variable? - 
ggplot(heart, aes(x= Resting_Electrocardio_Result, fill=Target)) + 
  geom_bar(position = 'dodge') +
  xlab("Resting_Electrocardio_Result") +
  ylab("Count") +
  ggtitle(" Resting_Electrocardio_Result and Heart Disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# what does these visuals tell us?



# Create new combinations of variables?
#Running Welch Two Sample t- tests between variables

mask <- heart$Target==0 
Age_no_disease <- heart[mask,'Age']
Age_disease <- heart[!mask,'Age']
t.test(Age_no_disease, Age_disease, alternative = "two.sided", var.equal = FALSE)

#####The Welch test tells us that indeed the means of age for the two classes
#are statistically different. Hence overall the value of age has an impact in 
#determining the probability of having heart disease.

mask <- heart$Target==0 
RBP_no_disease <- heart[mask,'Resting_Blood_Pressure']
RBP_disease <- heart[!mask,'Resting_Blood_Pressure']
t.test(RBP_no_disease, RBP_disease, alternative = "two.sided", var.equal = FALSE)

#Welch test says we cannot reject the null hypothesis 
#(the difference of the means is zero), there is no overall impact of resting blood pressure

mask <- heart$Target==0 
Chol_no_disease <- heart[mask,'Cholesterol']
Chol_disease <- heart[!mask,'Cholesterol']
t.test(Chol_no_disease, Chol_disease, alternative = "two.sided", var.equal = FALSE)

#there seems to be no overall impact due to this attribute.

mask <- heart$Target==0 
MHR_no_disease <- heart[mask,'Max_Heart_Rate']
MHR_disease <- heart[!mask,'Max_Heart_Rate']
t.test(MHR_no_disease, MHR_disease, alternative = "two.sided", var.equal = FALSE)

# Max heart rate has an impact



#### Building the Logestic Regression Model

library(caTools)

# Need to split the dataset
set.seed(123)

inx <- sample.split(seq_len(nrow(heart)), 0.75)
heart.train <- heart[inx,]
heart.test <- heart[!inx,]

logis <- glm(Target~.,data = heart.train, family = "binomial")
summary(logis)

# this model needs some work 

logis2 <- glm(Target~.-Cholesterol-Resting_Blood_Pressure-Fasting_Blood_Sugar-Slope-Age-Thalium_Stress_Test, data = heart.train, family = "binomial")
summary(logis2)


logis3 <- glm(Target~.-Cholesterol-Resting_Blood_Pressure-Fasting_Blood_Sugar-Slope-Age, data = heart.train, family = "binomial")
summary(logis3)

logis4 <- glm(Target~.-Cholesterol-Resting_Blood_Pressure-Fasting_Blood_Sugar-Slope, data = heart.train, family = "binomial")
summary(logis4)

anova(logis2,logis3,logis4,logis, test ='Chisq')






#### prediction


pred_train <- predict(logis, heart.train, type = "response")
# compare the predicted value of target with actual value of the train
# Confusion Matrix for the training dataset
confusion_matrix_train <- table(Actual_target = heart.train$Target, Predicted_target = pred_train > 0.6)
confusion_matrix_train

# Model accuracy of the training data set
round(((confusion_matrix_train[[1,1]] + confusion_matrix_train[[2,2]])/ sum(confusion_matrix_train)), 3)


# Run the test data through the model
pred_test <- predict(logis, heart.test, type = "response")

# Confusion Matrix for the test dataset
confusion_matrix_test <- table(Actual_target = heart.test$Target, Predicted_target = predict_test > 0.6)
confusion_matrix_test

# Calculate model accuracy for test data set
round(((confusion_matrix_test[[1,1]] + confusion_matrix_test[[2,2]])/ sum(confusion_matrix_test)), 3)

# Calculate area under the curve for test dataset

library(ROCR)
roc_prediction <- prediction(predict_test, heart.test$Target)
roc_performance_auc <- performance(roc_prediction,"auc")
roc_performance_auc
#roc_performance_auc@y.values

library(pROC)
test_prob = predict(logis3, newdata = heart.test, type = "response")
test_roc = roc( heart.test$Target ~ test_prob , plot = TRUE, print.auc = TRUE)


install.packages("factoextra")

#K-mean cluster
install.packages("funModeling")
library(funModeling)
profiling_num(heart)

#re-scale
rescale_df <- heart %>%
  mutate(Age_scal = scale(Age),
         Resting_Blood_Pressure_scal = scale(Resting_Blood_Pressure),
         Cholesterol_scal = scale( Cholesterol),
         Max_Heart_Rate_scal = scale(Max_Heart_Rate),
         Oldpeak_scal = scale( Oldpeak))  %>%
  select(c(Age_scal, Cholesterol_scal,Resting_Blood_Pressure_scal,Max_Heart_Rate_scal, Oldpeak_scal ))

head(as_tibble(rescale_df))

#get distance
distance <- get_dist(rescale_df)
head(distance)

#build the cluster 

k2 <- kmeans(rescale_df, 
             center = 2,
             nstart = 25  )

str(k2)

#plot
fviz_cluster(k2, data = rescale_df)


#k3 k4 k5
k3 <- kmeans(rescale_df, centers = 3, nstart = 25)
k4 <- kmeans(rescale_df, centers = 4, nstart = 25)
k5 <- kmeans(rescale_df, centers = 5, nstart = 25)

fviz_cluster(k3, data = rescale_df)

#optimal value of K is 2
set.seed(123)

fviz_nbclust(rescale_df, kmeans, method = "wss")
fviz_nbclust(rescale_df, kmeans, method = "silhouette")

#final K mean
set.seed(123)
final <- kmeans(rescale_df, 2, nstart = 25)
print(final)

fviz_cluster(final, data = rescale_df)

# Descriptive Statistics for Clusters
heart_kmean <- heart %>%
            mutate(Cluster = final$cluster) %>%
            group_by(Cluster) %>%
           summarise_all("mean")