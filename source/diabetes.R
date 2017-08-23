# Install required packages and load the libraries
#install.packages("caTools")
#install.packages("ROCR")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(ROCR)
library(nnet)

## Load the data sets
# create an empty data frame
df <- data.frame()
# for loop for data files 1:70
for(i in 1:70) {
  num = paste("", i, sep = "")
# if i < 10, paste 0 followed by the file name, e.g 01, 02, etc.
  if (i < 10) num = paste("0", i, sep = "")
  fname = paste("~/R Projects/diabetes-dataset/Diabetes-Data/data-", num, sep = "")
# creat vector to read files and add column names  
  temp <- read_delim(fname, col_names = c("date", "time", "code", "bg_conc"), 
   col_types = cols(
    date = col_date(format = "%m-%d-%Y"),
# %R = Equivalent to %H:%M.    
    time = col_time(format = "%R"),
# original string converted to integer
    code = col_integer(),
    bg_conc = col_character()
  ), "\t",
    escape_double = FALSE, trim_ws = TRUE);
# add patient_num column to represent each patients records (1:70)
  temp <- mutate(temp, patient_num = i)
# bind data files (1:70) & create df file
  df <- rbind(df, temp);
}

## Data wrangling
# create clean_df to clean the data
# remove "000" & "3A" from bg_conc as they are illogical values
clean_df <- subset(df, bg_conc != "000" & bg_conc != "3A")
# clean_df contains code "4", "36, & "56", but since there is no entry of such codes in
# the data description, these values are removed as well
clean_df <- subset(clean_df, code != "4" & code != "36" & code != "56")

#For further EDA, convert clean_df$bg_conc from character to numeric
clean_df$bg_conc <- as.numeric(clean_df$bg_conc)

#remove NAs from above
clean_df <- na.omit(clean_df)

#clean data csv
write.csv(clean_df, file = "../data wrangling/diabetes_clean.csv")

## Data wrangling
# Match the pattern with regex from time column and create 2 hours time bins
clean_df <- clean_df %>%
  mutate(time_bin = gsub("^00:[0-9][0-9]:00|^01:[0-9][0-9]:00", "00-02", x = time))%>%
  mutate(time_bin = gsub("^02:[0-9][0-9]:00|^03:[0-9][0-9]:00", "02-04", x = time_bin))%>%
  mutate(time_bin = gsub("^04:[0-9][0-9]:00|^05:[0-9][0-9]:00", "04-06", x = time_bin))%>%   
  mutate(time_bin = gsub("^06:[0-9][0-9]:00|^07:[0-9][0-9]:00", "06-08", x = time_bin))%>%
  mutate(time_bin = gsub("^08:[0-9][0-9]:00|^09:[0-9][0-9]:00", "08-10", x = time_bin))%>%
  mutate(time_bin = gsub("^10:[0-9][0-9]:00|^11:[0-9][0-9]:00", "10-12", x = time_bin))%>%
  mutate(time_bin = gsub("^12:[0-9][0-9]:00|^13:[0-9][0-9]:00", "12-14", x = time_bin))%>%
  mutate(time_bin = gsub("^14:[0-9][0-9]:00|^15:[0-9][0-9]:00", "14-16", x = time_bin))%>%    
  mutate(time_bin = gsub("^16:[0-9][0-9]:00|^17:[0-9][0-9]:00", "16-18", x = time_bin))%>%
  mutate(time_bin = gsub("^18:[0-9][0-9]:00|^19:[0-9][0-9]:00", "18-20", x = time_bin))%>%     
  mutate(time_bin = gsub("^20:[0-9][0-9]:00|^21:[0-9][0-9]:00", "20-22", x = time_bin))%>%
  mutate(time_bin = gsub("^22:[0-9][0-9]:00|^23:[0-9][0-9]:00", "22-24", x = time_bin))

# From BG concentration measurements, we can derive if the patient is Hypoglycemic, 
# Hyperglycemic or has a blood glucose in Normal range. 
# To simplify and study the effects of time and activity against the BG symptoms;
# 3 bins are created
# Hypoglycemic = 0-80mg/dl
# Normal = 81-199mg/dl
# Hyperglycemic > 200 mg/dl
# As per WHO the normal Blood glucose concentration of a diabetic person is 80 - 150 mg/dl
# But the BG > 200 is considered as Hyerglycemic. That is why in this project, we'll consider 
# BG < 200 mg/dl as normal BG

clean_df <- clean_df %>%
  mutate(symptom = gsub("^([0-9]|[0-7][0-9])$", "Hypoglycemia", x = bg_conc))%>%
  mutate(symptom = gsub("^(8[0-9]|9[0-9]|1[0-9][0-9])$", "Normal", x = symptom))%>%
  mutate(symptom = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", 
                        "Hyperglycemia", x = symptom))%>%
  mutate(symptom = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", "Hypoglycemia", x = symptom))


clean_df$code <- as.factor(clean_df$code)

# Let's add a new column "activity" which represent a type of activity patient has done just
# before or after measuring the BG
# code        type of activity
# 33 - 35       insulin dose
# 58 - 64       meal (pre - post)
# 69 - 71       exercise

clean_df <- clean_df %>%
  mutate(activity = gsub("^(3[3-5])$", "insulin", x = code)) %>%
  mutate(activity = gsub("^(5[8-9]|6[0-4]|66|67|68)$", "meal", x = activity)) %>%
  mutate(activity = gsub("^(6[9]|7[0-1])$", "exercise", x = activity)) %>%
  mutate(activity = gsub("^(48|57|72)$", "unspecified", x = activity)) %>%
  mutate(activity = gsub("^(65)$", "hypo-measurement", x = activity))

# Add one more column similar to the time_bin. 
# This represents time groups in a numeric format e.g. 00-02 time_bin = 0 bin_num, etc.
clean_df <- clean_df %>%
  mutate(bin_num = gsub("^00:[0-9][0-9]:00|^01:[0-9][0-9]:00", "0", x = time))%>%
  mutate(bin_num = gsub("^02:[0-9][0-9]:00|^03:[0-9][0-9]:00", "2", x = bin_num))%>%
  mutate(bin_num = gsub("^04:[0-9][0-9]:00|^05:[0-9][0-9]:00", "4", x = bin_num))%>%   
  mutate(bin_num = gsub("^06:[0-9][0-9]:00|^07:[0-9][0-9]:00", "6", x = bin_num))%>%
  mutate(bin_num = gsub("^08:[0-9][0-9]:00|^09:[0-9][0-9]:00", "8", x = bin_num))%>%
  mutate(bin_num = gsub("^10:[0-9][0-9]:00|^11:[0-9][0-9]:00", "10", x = bin_num))%>%
  mutate(bin_num = gsub("^12:[0-9][0-9]:00|^13:[0-9][0-9]:00", "12", x = bin_num))%>%
  mutate(bin_num = gsub("^14:[0-9][0-9]:00|^15:[0-9][0-9]:00", "14", x = bin_num))%>%    
  mutate(bin_num = gsub("^16:[0-9][0-9]:00|^17:[0-9][0-9]:00", "16", x = bin_num))%>%
  mutate(bin_num = gsub("^18:[0-9][0-9]:00|^19:[0-9][0-9]:00", "18", x = bin_num))%>%     
  mutate(bin_num = gsub("^20:[0-9][0-9]:00|^21:[0-9][0-9]:00", "20", x = bin_num))%>%
  mutate(bin_num = gsub("^22:[0-9][0-9]:00|^23:[0-9][0-9]:00", "22", x = bin_num))

str(clean_df)
clean_df$bin_num <- as.numeric(clean_df$bin_num)

## Exploratory data analysis
# Since bg_conc is the only numeric variable in the data set; 
# it’s distribution is observed by plotting the histogram.
# Fig.1

ggplot(clean_df, aes(bg_conc)) +
  geom_histogram(binwidth = 5, fill = "black", col = "red", alpha = 0.2) +
  labs(x = "Code", y = "BG concentration, mg/dl") +
  ggtitle("Distribution of Blood glucose measurements", subtitle = "Figure 1")

# High peak at 10 - 20 mg/dl BG 
# Now that we have determined the distribution of BG concentration, the focus of the further 
# data visualisation and exploration will be to deep dive into other variables 
# and determine how they interact with one another.

#The following variables will be investigated in this section:
  # 1. Code vs Blood Glucose concentration
  # 2. Time vs Blood Glucose concentration


## 1. Code vs Blood Glucose concentration

# BG values from code 48 - 64 show most hyperglycemic symptoms
ggplot(clean_df, aes(factor(code), bg_conc, col = symptom)) +
  geom_point() +
  geom_jitter() +
  labs(x = "Code", y = "Blood glucose value mg/dl") +
  ggtitle("Distribution of BG measurements based on Code and BG concentration", 
          subtitle = "Figure 2")

# Fig.3
ggplot(clean_df, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Code and Blood Glucose concentration", subtitle = "Figure 3")

summary(clean_df$bg_conc)

#To gain a better perspective at this, let’s plot the graph of BG concentration vs code by 
#grouping the codes into 3 sub categories.

# group 1: code 33 - 35 represents codes for type of insulin dose
# Fig.4
c1 <- c(33:35)
code_df1 <- clean_df[clean_df$code %in% c1,]
ggplot(code_df1, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 
          4")

summary(code_df1$bg_conc)

# patients take Regular insulin dose more often than NPH and Ultralente insulin dose 
# Fig.5
ggplot(code_df1, aes(factor(code))) +
  geom_histogram(stat = "count", fill= "black", col= "red", alpha= 0.2) +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Distribution of code (Insulin)", subtitle = "Figure 5")
  
# group 2: code 48, 57 - 64 represents meal
# Fig.6
c2 <- c(48, 57:64)
code_df2 <- clean_df[clean_df$code %in% c2,]
ggplot(code_df2, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Meal and Blood Glucose concentration", subtitle = "Figure 6")

summary(code_df2$bg_conc)

# Histogram Distribution of measurements based on Pre/ Post Meal
# Fig. 7
ggplot(code_df2, aes(factor(code))) +
  geom_histogram(fill= "black", col= "red", alpha= 0.2, stat = "count") +
  labs(x = "Codes representing Meal", y = "BG concentration (mg/dl)") +
  ggtitle("Distribution of measurements based on Pre/ Post Meal", subtitle = "Figure 7")
# patients rarely measured BG after a meal

#group 3: code 65 - 72 represents exercise
# Fig.8
c3 <- c(65:72)
code_df3 <- clean_df[clean_df$code %in% c3,]
ggplot(code_df3, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between exercise and Blood Glucose concentration", subtitle = "Figure 8")
# code 65 represents the Hypoglycemic symptoms, it is normal to have BG concentration value 
# around 0 mg/dl. But, for the remaining code, it is not logical. Hence we assume that these 
# values were supposed to be measured but they are just the place holder for now.

summary(code_df3$bg_conc)

# To see the comparison between types of activities, let's plot a box plot
# Fig.9
ggplot(clean_df, aes(factor(activity), bg_conc, group=activity, col=activity)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Type of activity", y = "Blood glucose concentration, mg/dl") +
  ggtitle("Comparison between activity and BG concentration", subtitle = "Figure 9")
# From fig.9, we can see that the blood glucose level decreases just after taking the isulin
# dose. It even drops down further after the exercise. However, the BG notably increases 
# after a meal with a median of around 150 mg/dl


## 2. Time vs Blood Glucose concentration

#Since time is a continuous variable, it will be difficult to explore the data when time is 
#plotted on x- axis. Which is why the time intervals of 2 hours (time_bin) were created

# The distribution of BG measurements across various time intervals is explored using a simple
# bar plot.
# Fig.10
ggplot(clean_df, aes(factor(time_bin))) +
 geom_histogram(stat = "count", fill= "black", col= "red", alpha= 0.5) +
 labs(x = "Hours") +
 ggtitle("Distribition of BG measurements across time", subtitle = "Figure 10")
# minimum number of measurements taken from 00 - 04 in the night. 
#The maximum number of BG concentration are taken during the day time.
 
# see the distribution of Hypoglycemia, Normal BG concentration and Hyperglycemia.
# Each point here represents number of Blood glucose measurements by the patients in 24 hours
# for several weeks or months.
# Fig.11

ggplot(clean_df, aes(factor(time_bin), bg_conc, col = symptom)) +
  geom_point() +
  geom_jitter() +
  labs(x = "Hours", y = "Blood glucose value mg/dl") +
  ggtitle("Distribution of BG symptoms over 24 hours", subtitle = "Figure 11")

# We do not see any precise time at which the patient was particulary showing Hypoglycemic or
# Hyperglycemic symptoms, as the symptoms are distributed across 24 hours. However we can 
# say that there are comparatively less measurements showing these symptoms from 00 - 04 in 
# the morning
# This could be due to the fact that there were less number of measurements taken at these
# time intervals.

# Association between time interval and BG concentration box plot.
# Fig.12
ggplot(clean_df, aes(factor(time_bin), bg_conc)) +
  geom_boxplot() +
  labs(x = "Hours", y = "Blood glucose concentration") +
  ggtitle("Association between time and BG concentration", subtitle = "Figure 12")


# working plot Add facet layer
# Fig.14
clean_df$symptom <- factor(clean_df$symptom, levels = c("Hypoglycemia", "Normal",
                                                          "Hyperglycemia"))
ggplot(clean_df, aes(symptom, bg_conc, col = symptom)) +
  geom_point() +
  geom_boxplot(alpha = 0.4, width = 4.5) +
  facet_grid(.~ (symptom)) +
  labs(x = "Symptoms", y = "Blood glucose concentration, mg/dl") +
  ggtitle("Distribution of BG symptoms over 24 hours", subtitle = "Figure 14")


## Machine learning
# Add a column and encode,
# hyperglycemia = 1
# Normal, Hypoglycemia = 0

clean_df <- clean_df %>%
  mutate(sympcode = gsub("^([0-9]|[0-7][0-9]|80)$", 0, x = bg_conc))%>%
  mutate(sympcode = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", 0, x = sympcode))%>%
  mutate(sympcode = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$",
                         1, x = sympcode)) %>%
  mutate(sympcode = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", 0, x = sympcode))

str(clean_df)
clean_df$sympcode <- as.factor(clean_df$sympcode)
clean_df$bin_num <- as.numeric(clean_df$bin_num)
clean_df$activity <- as.factor(clean_df$activity)
str(clean_df)


# First, let's find the accuracy of baseline model
table(clean_df$sympcode)
# 24995 patients not hyperglycemic & 3851 are hyperglycemic
# since not hyperglycemic are more common than hyperglycemic, we'll predict all patients are
# not hyperglycemic
24995/28847
# Baseline model accuracy = 87%

# Now, let's create a binomial logistic regression model that Predicts Hyperglycemic symptoms 
# based on codes

set.seed(844)
# split data into  70:30 ratio
split <- sample.split(clean_df$sympcode, SplitRatio = 0.7)
train <- subset(clean_df, split == TRUE)
test <- subset(clean_df, split == FALSE)
#create a model
symp.logit <- glm(sympcode~code+bin_num, data = train, family = "binomial")
summary(symp.logit) 
# code 65 - 72 have negative coefficients and are not significant
# Null deviance: 15872, Residual deviance: 11151, AIC: 11193

# higher value in coefficients table are indicative of hyperglycemia

pred.test <- predict(symp.logit, type = "response", newdata = test)
summary(pred.test)

# all true hyperglycemia cases we predicted probability of 0.3
# all true not hyperglycemia cases we predicted probability of 0.1

# confusion matrix, t = 0.1
table(test$sympcode, pred.test > 0.1)
# accuracy of the model = (TP+TN)/(TP+TN+FP+FN)
(4555+1155)/(4555+1155+2944+1) # 0.66
# sensitivity/recall
1155/(1+1155) # 0.99 actual not hyperglycemia measured correctly
# specificity 
4555/(4555+2944) #0.6 actual hyperglycemia measured correctly

#precision = TP/(TP+FP)
4555/(4555+1)  # 0.99

#######
# confusion matrix, t = 0.3
table(test$sympcode, pred.test > 0.3)
# accuracy of the model
(6221+610)/(6221+610+1278+546) # 0.79

# sensitivity/recall
610/(546+610) #0.52 

#specificity/ True negative rate (TNR)
14565/(14565+2931) # 0.8

#precision = TP/(TP+FP)
6221/(6221+546) # 0.92

#######
# confusion matrix, t = 0.5
table(test$sympcode, pred.test > 0.5)
# accuracy of the model
(7449+35)/(7449+35+50+1121) # 0.86

# sensitivity/recall = TN/(TN+FP)
35/(1121+35) #0.03 

#specificity/ True negative rate (TNR)
7449/(7449+50) # 0.99  

#precision = TP/(TP+FP)
(7449)/(7449+1121) # 0.87
#####

# ROC curve
ROCRpred <- prediction(pred.test, test$sympcode)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# calculate AUC
as.numeric(performance(ROCRpred, "auc")@y.values) # 0.83
# the area under curve is 0.83 on our test set which means that the model can differentiate 
# between patients who might have hyperglycemia or not.

# Calculate the F1 score/ harmonic mean = 2TP/(2TP+FP+FN)  (t = 0.5)
(2*7449)/(2*7449+1121+50) # 0.92

# Conclusion: our model rarely predicts the risk of hyperglycemia above 50% and the accuracy 
# of the model (86%) was very near to the accuracy of the baseline model (87%)
# Model can differentiate between patients who have probability of suffering from 
# hyperglycemia (AUC = 0.83)
# The codes representing meal activity suggest that the patient will be hyperglycemic


# Binomial regression activity ~ bg_conc

clean_df <- clean_df %>%
  mutate(act.code = gsub("^(3[3-5])$", 0, x = code)) %>%  # insulin = 0
  mutate(act.code = gsub("^(5[8-9]|6[0-4]|66|67|68)$", 1, x = act.code)) %>%   #meal = 1
  mutate(act.code = gsub("^(6[9]|7[0-1])$", 0, x = act.code)) %>% #exercise = 0
  mutate(act.code = gsub("^(48|57|72)$", 0, x = act.code)) %>% #unspecified = 0
  mutate(act.code = gsub("^(65)$", 0, x = act.code)) #hypo-measurement = 0

str(clean_df)
clean_df$act.code <- as.factor(clean_df$act.code)
# First, let's find the accuracy of baseline model
table(clean_df$act.code)
# 17842 patients have not had meal related activity & 11005 have
# since patients who have not had meal are more common than patients who have, 
# we'll predict all patients whi have not had meal related activity.
17842/28847
# Baseline model accuracy = 61%

# Now, let's create a binomial logistic regression model that Predicts Hyperglycemic symptoms 
# based on codes

set.seed(844)
# split data into  70:30 ratio
act.split <- sample.split(clean_df$act.code, SplitRatio = 0.7)
act.train <- subset(clean_df, split == TRUE)
act.test <- subset(clean_df, split == FALSE)
#create a model
act.logit <- glm(act.code~bg_conc, data = act.train, family = "binomial")
summary(act.logit) 


pred.test1 <- predict(act.logit, type = "response", newdata = act.test)
summary(pred.test1)

# confusion matrix

table(act.test$act.code, pred.test1 > 0.3)
# accuracy of the model = (TP+TN)/(TP+TN+FP+FN)
(4534+1155)/(4534+1155+2944+1) # 0.65

table(act.test$act.code, pred.test1 > 0.5)
# accuracy of the model = (TP+TN)/(TP+TN+FP+FN)
(4679+2200)/(4679+2200+640+1136) # 0.79


# ROC curve
ROCRact.pred <- prediction(pred.test1, act.test$act.code)
ROCRact.perf <- performance(ROCRact.pred, "tpr", "fpr")
plot(ROCRact.perf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# calculate AUC
as.numeric(performance(ROCRact.pred, "auc")@y.values) # 0.88

# Calculate the F1 score/ harmonic mean = 2TP/(2TP+FP+FN)  (t = 0.5)
(2*4679)/(2*4679+1136+640) # 0.84



####

# Predict Hypo or Hyperglycemia using multinomial logistic regression

# assign, Normal = 1
# Hypoglycemia = 2
# Hyperglycemia = 3
# Store the results in a new column "sympcode"

clean_df <- clean_df %>%
  mutate(sympcode.mult = gsub("^([0-9]|[0-7][0-9]|80)$", 2, x = bg_conc))%>%
  mutate(sympcode.mult = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", 1, x = sympcode.mult))%>%
  mutate(sympcode.mult = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$",
                              3, x = sympcode.mult)) %>%
  mutate(sympcode.mult = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", 2, x = sympcode.mult))

clean_df$sympcode.mult <- as.factor(clean_df$sympcode.mult)

# install.packages("nnet")
# library(nnet)

clean_df$sympcode.mult <- as.factor(clean_df$sympcode.mult)

# Relevel the column; where Normal = 1
clean_df$out <- relevel(clean_df$sympcode.mult, ref = "1")

set.seed(844)
mult.split <- sample.split(clean_df$sympcode.mult, SplitRatio = 0.7)
mult.train <- subset(clean_df, split == TRUE)
mult.test <- subset(clean_df, split == FALSE)


# Let's create a multinomial model to predict Normal BG, hypoglycemia & hyperglycemia
# based on code
mult.mod <- multinom(out~code, data = mult.train)
summary(mult.mod)

p <- predict(mult.mod, newdata = mult.test)
# create confusion matrix
cm <- table(p, mult.test$sympcode.mult)
print(cm)

#correct classification
(2222+4555+35)/8665 # 0.78

# misclassification by model = 22%

#We have created a model (mult.mod) that correctly classifies the data 78% of the time

# 2-tailed z test

z <- summary(mult.mod)$coefficients/summary(mult.mod)$standard.errors
p <- (1 - pnorm(abs(z), 0 , 1)) * 2
p

# 1 - p value = confidence interval
# since code 33, 34, 48, 57, 58, 59, 60, 61, 62, 63, 64 have p value almost 0
# hence higher confidence interval and therefore more significant in predicting hypoglycemia




