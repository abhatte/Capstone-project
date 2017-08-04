library(readr)
library(dplyr)
library(tidyr)

master <- data.frame()
for(i in 1:70) {
  num = paste("", i, sep = "")
  if (i < 10) num = paste("0", i, sep = "")
  fname = paste("~/R Projects/diabetes-dataset/Diabetes-Data/data-", num, sep = "")
  temp <- read_delim(fname, col_names = c("date", "time", "code", "bg_conc"), 
   col_types = cols(
    date = col_date(format = "%m-%d-%Y"),
    time = col_time(format = "%R"),
    code = col_integer(),
    bg_conc = col_character()
  ), "\t",
    escape_double = FALSE, trim_ws = TRUE);
  temp <- mutate(temp, file_name = i)
  master <- rbind(master, temp);
}
summary(master)
# create newmaster = remove "000" & "3A" from bg_conc
newmaster <- subset(master, bg_conc != "000" & bg_conc != "3A")
# update newmaster = remove discrepancies in code "4", "36, & "56"
newmaster <- subset(newmaster, code != "4" & code != "36" & code != "56")
#convert newmaster$bg_conc from character to numeric
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


library(ggplot2)
# to see the distribution of bg_conc, a qplot is created
qplot(newmaster$bg_conc,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Blood Glucose", 
      xlab = "Blood Glucose",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(0.2))

#working boxplot code
ggplot(newmaster, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)")

summary(newmaster$bg_conc)

#grouped codes
c1 <- c(33:35)
newmaster1 <- newmaster[newmaster$code %in% c1,]
ggplot(newmaster1, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)")

summary(newmaster1$bg_conc)

c2 <- c(48, 57:64)
newmaster2 <- newmaster[newmaster$code %in% c2,]
ggplot(newmaster2, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)")

summary(newmaster2$bg_conc)

c3 <- c(65:72)
newmaster3 <- newmaster[newmaster$code %in% c3,]
ggplot(newmaster3, aes(factor(code), bg_conc)) +
  geom_boxplot() +
  labs(x = "Code", y = "BG concentration (mg/dl)")

summary(newmaster3$bg_conc)

#create 2 hour time intervals
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


#bg_level: 0-80mg/dl = Hypoglycaemic, 81-199mg/dl = Average, above 200mh/dl = Hyperglycemic
newmaster <- newmaster %>%
  mutate(bg_symp = gsub("^([0-9]|[0-7][0-9]|80|1.5|2.5|3.5|4.5|6.5|7.5)$", 
                        "Hypoglycemia", x = bg_conc))%>%
  mutate(bg_symp = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", "Average", x = bg_symp))%>%
  mutate(bg_symp = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", 
                        "Hyperglycemia", x = bg_symp))

 # ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
 #   geom_bar(stat = "identity")

# working barchart with time_grp vs count
ggplot(newmaster, aes(time_grp)) +
 geom_bar() +
 labs(x = "Hours")

# working boxplot with time vs bg_conc 
ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_boxplot() +
  labs(x = "Hours", y = "Blood glucose concentration")

# working point plot
ggplot(newmaster, aes(factor(time_grp), bg_conc, col = bg_symp)) +
  geom_point()

# boxplot with overlaid scatterplot
ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_point() +
  geom_boxplot(alpha = 0.4) +
  labs(x = "Hours", y = "Blood glucose concentration")
  # 10-15 increases, 22-24 increases

ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_point(alpha = 0.3) +
  geom_jitter(width = 0.1) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Hours", y = "Blood glucose value")

# Add facet layer
ggplot(newmaster, aes(factor(time_grp), bg_conc)) +
  geom_point() +
  facet_grid(.~ factor(time_grp)) +
  geom_boxplot(alpha = 0.4, width = 4.5)
  

