# load the libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# create an empty data frame
master <- data.frame()
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
# add file_name column to represent each patients records (1:70)
  temp <- mutate(temp, file_name = i)
# bind data files (1:70) & create master file
  master <- rbind(master, temp);
}
# check the class and NAs
class(master)
summary(master)

# create clean_df to clean the data
## remove "000" & "3A" from bg_conc as they are illogical values
clean_df <- subset(master, bg_conc != "000" & bg_conc != "3A")
# clean_df contains code "4", "36, & "56", but since there is no entry of such codes in
# the data description, these values are removed as well
clean_df <- subset(clean_df, code != "4" & code != "36" & code != "56")

#For further EDA, convert clean_df$bg_conc from character to numeric
clean_df$bg_conc <- as.numeric(clean_df$bg_conc)

#remove NAs from above
clean_df <- na.omit(clean_df)

#clean data csv
write.csv(clean_df, file = "../data wrangling/diabetes_clean.csv")

#check for NA = 8 NAs introduced by coercion in bg_conc
summary(clean_df)
glimpse(clean_df)
length(unique(clean_df$code))
class(clean_df)

# Since bg_conc is the only numeric variable in the data set; 
# it’s distribution is observed by plotting the histogram.

qplot(clean_df$bg_conc,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Blood Glucose", 
      xlab = "Blood Glucose",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(0.2))

#Now that we have determined the distribution of BG concentration, the focus of the further 
#data visualisation and exploration will be to deep dive into other variables 
#and determine how they interact with one another.

#The following variables will be investigated in this section:
  #Code vs Blood Glucose concentration
  #Time vs Blood Glucose concentration


#Code vs Blood Glucose concentration
ggplot(clean_df, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Code and Blood Glucose concentration", subtitle = "Figure 2")

summary(clean_df$bg_conc)

#To gain a better perspective at this, let’s plot the graph of BG concentration vs code by 
#grouping the codes into 3 sub categories.

# group 1: code 33 - 35 represents codes for type of insulin dose
c1 <- c(33:35)
clean_df1 <- clean_df[clean_df$code %in% c1,]
ggplot(clean_df1, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 3")

summary(clean_df1$bg_conc)

# group 2: code 48, 57 - 64 represents meal
c2 <- c(48, 57:64)
clean_df2 <- clean_df[clean_df$code %in% c2,]
ggplot(clean_df2, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 4")

summary(clean_df2$bg_conc)

#group 3: code 65 - 72 represents exercise
c3 <- c(65:72)
clean_df3 <- clean_df[clean_df$code %in% c3,]
ggplot(clean_df3, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between exercise and Blood Glucose concentration", subtitle = "Figure 5")
# code 65 represents the Hypoglycemic symptoms, it is normal to have BG concentration value 
# around 0 mg/dl. But, for the remaining code, it is not logical. Hence we assume that these 
# values were supposed to be measured but they are just the place holder for now.

summary(clean_df3$bg_conc)

#Since time is a continuous variable, it will be difficult to explore the data when time is 
#plotted on x- axis. Which is why the time intervals of 2 hours (time_grp) were created

clean_df <- clean_df %>%
  mutate(time_grp = gsub("^00:[0-9][0-9]:00|^01:[0-9][0-9]:00", "00-02", x = time))%>%
  mutate(time_grp = gsub("^02:[0-9][0-9]:00|^03:[0-9][0-9]:00", "02-04", x = time_grp))%>%
  mutate(time_grp = gsub("^04:[0-9][0-9]:00|^05:[0-9][0-9]:00", "04-06", x = time_grp))%>%   
  mutate(time_grp = gsub("^06:[0-9][0-9]:00|^07:[0-9][0-9]:00", "06-08", x = time_grp))%>%
  mutate(time_grp = gsub("^08:[0-9][0-9]:00|^09:[0-9][0-9]:00", "08-10", x = time_grp))%>%
  mutate(time_grp = gsub("^10:[0-9][0-9]:00|^11:[0-9][0-9]:00", "10-12", x = time_grp))%>%
  mutate(time_grp = gsub("^12:[0-9][0-9]:00|^13:[0-9][0-9]:00", "12-14", x = time_grp))%>%
  mutate(time_grp = gsub("^14:[0-9][0-9]:00|^15:[0-9][0-9]:00", "14-16", x = time_grp))%>%    
  mutate(time_grp = gsub("^16:[0-9][0-9]:00|^17:[0-9][0-9]:00", "16-18", x = time_grp))%>%
  mutate(time_grp = gsub("^18:[0-9][0-9]:00|^19:[0-9][0-9]:00", "18-20", x = time_grp))%>%     
  mutate(time_grp = gsub("^20:[0-9][0-9]:00|^21:[0-9][0-9]:00", "20-22", x = time_grp))%>%
  mutate(time_grp = gsub("^22:[0-9][0-9]:00|^23:[0-9][0-9]:00", "22-24", x = time_grp))

# From BG concentration measurements, we can derive if the patient is Hypoglycemic, 
# Hyperglycemic or has a blood glucose in average range. 
# To study the effects, 3 groups are createrd
#bg_level: 0-80mg/dl = Hypoglycaemic, 81-199mg/dl = Average, above 200mh/dl = Hyperglycemic

clean_df <- clean_df %>%
  mutate(bg_symp = gsub("^([0-9]|[0-7][0-9]|80)$", "Hypoglycemia", x = bg_conc))%>%
  mutate(bg_symp = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", "Average", x = bg_symp))%>%
  mutate(bg_symp = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", 
                        "Hyperglycemia", x = bg_symp)) %>%
  mutate(bg_symp = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", "Hypoglycemia", x = bg_symp))

clean_df <- clean_df %>%
  mutate(symp_num = gsub("^([0-9]|[0-7][0-9]|80)$", 0, x = bg_conc))%>%
  mutate(symp_num = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", 1, x = symp_num))%>%
  mutate(symp_num = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", 
                        2, x = symp_num)) %>%
  mutate(symp_num = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", 0, x = symp_num))


# The distribution of BG measurements across various time intervals is explored using a simple
# bar plot.
ggplot(clean_df, aes(time_grp)) +
 geom_bar() +
 labs(x = "Hours") +
 ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 6")

  
# Relationship between time interval and BG concentration box plot.
ggplot(clean_df, aes(factor(time_grp), bg_conc)) +
  geom_boxplot() +
  labs(x = "Hours", y = "Blood glucose concentration") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 7")


# minimum number of measurements taken from 00 - 04 in the night. 
#The maximum number of BG concentration are taken during the day time.
ggplot(clean_df, aes(factor(time_grp), bg_conc)) +
  geom_point(alpha = 0.1) +
  scale_shape(1, solid = FALSE) +
  geom_jitter(width = 0.1) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Hours", y = "Blood glucose value") +
  ggtitle("Distribution of blood glucose measurements over 24 hours", subtitle = "Figure 8")


# see the distribution of Hypoglycemia, average BG concentration and Hyperglycemia.
# Each point here represents number of Blood glucose measurements by the patients in 24 hours
# for several weeks or months.
ggplot(clean_df, aes(factor(time_grp), bg_conc, col = bg_symp)) +
  geom_point() +
  ggtitle("Distribution of BG symptoms over 24 hours", subtitle = "Figure 9")


# working plot Add facet layer
clean_df$bg_symp <- factor(clean_df$bg_symp, levels = c("Hypoglycemia", "Average",
                                                          "Hyperglycemia"))
ggplot(clean_df, aes(bg_symp, bg_conc, col = bg_symp)) +
  geom_point() +
  geom_boxplot(alpha = 0.4, width = 4.5) +
  facet_grid(.~ (bg_symp)) +
  labs(x = "Symptoms", y = "Blood glucose concentration, mg/dl") +
  ggtitle("Distribution of BG symptoms over 24 hours", subtitle = "Figure 10")

# # boxplot with overlaid scatterplot
# ggplot(clean_df, aes(factor(time_grp), bg_conc)) +
#   geom_point(alpha = 0.1) +
#   scale_shape(1, solid = FALSE) +
#   geom_boxplot(alpha = 0.4) +
#   labs(x = "Hours", y = "Blood glucose concentration")

# machine learning
# Let's add a new column "type" which represent a type of activity patient has done just before
# or after measuring the BG
# code        type of activity
# 33 - 35       insulin dose
# 58 - 64       meal (pre - post)
# 69 - 71       exercise

clean_df <- clean_df %>%
  mutate(type = gsub("^(3[3-5])$", "insulin", x = code)) %>%
  mutate(type = gsub("^(5[8-9]|6[0-4])$", "meal", x = type)) %>%
  mutate(type = gsub("^(6[9]|7[0-1])$", "exercise", x = type))

# To see the comparison between these 3 types of activities, let's plot a box plot

ggplot(subset(clean_df,type %in% c("insulin", "meal", "exercise"))) + 
  geom_boxplot(aes(factor(type), bg_conc, group=type, col=type)) +
  labs(x = "Type of activity", y = "Blood glucose concentration, mg/dl") +
  ggtitle("Comparison between type of activity and BG concentration", subtitle = "Figure 11")

# Install required packages
#install.packages("caTools")
library(caTools)

set.seed(144)

clean_df$symp_num <- as.numeric(clean_df$symp_num)

split <- sample.split(clean_df$symp_num, SplitRatio = 0.7)
train <- subset(clean_df, split == TRUE)
test <- subset(clean_df, split == FALSE)


sympLog <- glm(symp_num~bg_conc, data = train)
summary(sympLog)

predictTest <- predict(sympLog, type = "response", newdata = test)
table(test$bg_symp, predictTest > 0.5)
