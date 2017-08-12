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

# create newmaster to clean the data
## remove "000" & "3A" from bg_conc as they are illogical values
newmaster <- subset(master, bg_conc != "000" & bg_conc != "3A")
# newmaster contains code "4", "36, & "56", but since there is no entry of such codes in
# the data description, these values are removed as well
newmaster <- subset(newmaster, code != "4" & code != "36" & code != "56")

#For further EDA, convert newmaster$bg_conc from character to numeric
newmaster$bg_conc <- as.numeric(newmaster$bg_conc)

#remove NAs from above
newmaster <- na.omit(newmaster)

#clean data csv
write.csv(newmaster, file = "../data wrangling/diabetes_clean.csv")

#check for NA = 8 NAs introduced by coercion in bg_conc
summary(newmaster)
glimpse(newmaster)
length(unique(newmaster$code))
class(newmaster)

# Since bg_conc is the only numeric variable in the data set; 
# it’s distribution is observed by plotting the histogram.

qplot(newmaster$bg_conc,
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
ggplot(newmaster, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Code and Blood Glucose concentration", subtitle = "Figure 2")

summary(newmaster$bg_conc)

#To gain a better perspective at this, let’s plot the graph of BG concentration vs code by 
#grouping the codes into 3 sub categories.

# group 1: code 33 - 35 represents codes for type of insulin dose
c1 <- c(33:35)
newmaster1 <- newmaster[newmaster$code %in% c1,]
ggplot(newmaster1, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 3")

summary(newmaster1$bg_conc)

# group 2: code 48, 57 - 64 represents meal
c2 <- c(48, 57:64)
newmaster2 <- newmaster[newmaster$code %in% c2,]
ggplot(newmaster2, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 4")

summary(newmaster2$bg_conc)

#group 3: code 65 - 72 represents exercise
c3 <- c(65:72)
newmaster3 <- newmaster[newmaster$code %in% c3,]
ggplot(newmaster3, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)") +
  ggtitle("Relation between exercise and Blood Glucose concentration", subtitle = "Figure 5")
# code 65 represents the Hypoglycemic symptoms, it is normal to have BG concentration value 
# around 0 mg/dl. But, for the remaining code, it is not logical. Hence we assume that these 
# values were supposed to be measured but they are just the place holder for now.

summary(newmaster3$bg_conc)

#Since time is a continuous variable, it will be difficult to export the data when time is 
#plotted on x- axis. Which is why the time intervals of 2 hours (time_grp) were created

newmaster <- newmaster %>%
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

newmaster <- newmaster %>%
  mutate(bg_symp = gsub("^([0-9]|[0-7][0-9]|80)$", "Hypoglycemia", x = bg_conc))%>%
  mutate(bg_symp = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", "Average", x = bg_symp))%>%
  mutate(bg_symp = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", 
                        "Hyperglycemia", x = bg_symp)) %>%
  mutate(bg_symp = gsub("^(1.5|2.5|3.5|4.5|6.5|7.5)$", "Hypoglycemia", x = bg_symp))


# The distribution of BG measurements across various time intervals is explored using a simple
# bar plot.
ggplot(newmaster, aes(time_grp)) +
 geom_bar() +
 labs(x = "Hours") +
 ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 6")

  
# Relationship between time interval and BG concentration box plot.

ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_boxplot() +
  labs(x = "Hours", y = "Blood glucose concentration") +
  ggtitle("Relation between Insulin dose and Blood Glucose concentration", subtitle = "Figure 7")

# see the distribution of values in a particular time interval
ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_point(alpha = 0.1) +
  scale_shape(1, solid = FALSE) +
  geom_jitter(width = 0.1) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Hours", y = "Blood glucose value")

# working point plot
ggplot(newmaster, aes(factor(time_grp), bg_conc, col = bg_symp)) +
  geom_point()

# working plot Add facet layer

newmaster$bg_symp <- factor(newmaster$bg_symp, levels = c("Hypoglycemia", "Average",
                                                          "Hyperglycemia"))
ggplot(newmaster, aes(bg_symp, bg_conc, col = bg_symp)) +
  geom_point() +
  geom_boxplot(alpha = 0.4, width = 4.5) +
  facet_grid(.~ (bg_symp)) +
  labs(x = "Symptoms", y = "Blood glucose concentration, mg/dl")

# # boxplot with overlaid scatterplot
# ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
#   geom_point(alpha = 0.1) +
#   scale_shape(1, solid = FALSE) +
#   geom_boxplot(alpha = 0.4) +
#   labs(x = "Hours", y = "Blood glucose concentration")

  
  