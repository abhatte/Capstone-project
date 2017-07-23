library(readr)
library(dplyr)
library(tidyr)

master <- data.frame()
for(i in 1:70) {
  num = paste("", i, sep = "")
  if (i < 10) num = paste("0", i, sep = "")
  fname = paste("~/R Projects/diabetes-dataset/Diabetes-Data/data-", num, sep = "")
  temp <- read_delim(fname, col_names = c("date", "time", "code", "bg_value"), 
   col_types = cols(
    date = col_date(format = "%m-%d-%Y"),
    time = col_time(format = "%R"),
    code = col_integer(),
    bg_value = col_character()
  ), "\t",
    escape_double = FALSE, trim_ws = TRUE);
  temp <- mutate(temp, file_name = i)
  master <- rbind(master, temp);
}
summary(master)
# create newmaster = remove "000" & "3A" from bg_value
newmaster <- subset(master, bg_value != "000" & bg_value != "3A")
# update newmaster = remove discrepancies in code "4", "36, & "56"
newmaster <- subset(newmaster, code != "4" & code != "36" & code != "56")
#convert newmaster$bg_value from character to numeric
newmaster$bg_value <- as.numeric(newmaster$bg_value)
#remove NAs from above
newmaster <- na.omit(newmaster)
write.csv(newmaster, file = "../data wrangling/diabetes_clean.csv")
#check for NA = 8 NAs introduced by coercion in bg_value
summary(newmaster)
glimpse(newmaster)
length(unique(newmaster$code))
class(newmaster)

#working boxplot code
library(ggplot2)
ggplot(newmaster, aes(factor(code), bg_value)) +
  geom_boxplot()

#   scale_y_continuous("Blood glucode concentration (mg/dl)", # added - 34,35,36,37
#                      limits = c(0, 501)) +
#   scale_x_discrete("Code") +
#   theme_classic()

#grouped codes
c1 <- c(33:35)
newmaster1 <- newmaster[newmaster$code %in% c1,]
ggplot(newmaster1, aes(factor(code), bg_value)) +
  geom_boxplot()

c2 <- c(48, 57:64)
newmaster1 <- newmaster[newmaster$code %in% c2,]
ggplot(newmaster1, aes(factor(code), bg_value)) +
  geom_boxplot()

c3 <- c(65:72)
newmaster1 <- newmaster[newmaster$code %in% c3,]
ggplot(newmaster1, aes(factor(code), bg_value)) +
  geom_boxplot()

#create 2 hour time intervals
newmaster <- newmaster %>%
  mutate(time1 = gsub("^00:[0-9][0-9]:00|^01:[0-9][0-9]:00", "01", x = time))%>%
  mutate(time1 = gsub("^02:[0-9][0-9]:00|^03:[0-9][0-9]:00", "02", x = time1))%>%
  mutate(time1 = gsub("^04:[0-9][0-9]:00|^05:[0-9][0-9]:00", "03", x = time1))%>%   
  mutate(time1 = gsub("^06:[0-9][0-9]:00|^07:[0-9][0-9]:00", "04", x = time1))%>%
  mutate(time1 = gsub("^08:[0-9][0-9]:00|^09:[0-9][0-9]:00", "05", x = time1))%>%
  mutate(time1 = gsub("^10:[0-9][0-9]:00|^11:[0-9][0-9]:00", "06", x = time1))%>%
  mutate(time1 = gsub("^12:[0-9][0-9]:00|^13:[0-9][0-9]:00", "07", x = time1))%>%
  mutate(time1 = gsub("^14:[0-9][0-9]:00|^15:[0-9][0-9]:00", "08", x = time1))%>%    
  mutate(time1 = gsub("^16:[0-9][0-9]:00|^17:[0-9][0-9]:00", "09", x = time1))%>%
  mutate(time1 = gsub("^18:[0-9][0-9]:00|^19:[0-9][0-9]:00", "10", x = time1))%>%     
  mutate(time1 = gsub("^20:[0-9][0-9]:00|^21:[0-9][0-9]:00", "11", x = time1))%>%
  mutate(time1 = gsub("^22:[0-9][0-9]:00|^23:[0-9][0-9]:00", "12", x = time1))


#bg_level: 0-80mg/dl = Hypoglycemic, 81-199mg/dl = Normal, above 200mh/dl = Hyperglycemic
newmaster <- newmaster %>%
  mutate(bg_conc = gsub("^([0-9]|[0-7][0-9]|80)$", "Hypoglycemia", x = bg_value))%>%
  mutate(bg_conc = gsub("^(8[1-9]|9[0-9]|1[0-9][0-9])$", "Normal", x = bg_conc))%>%
  mutate(bg_conc = gsub("^(2[0-9][0-9]|3[0-9][0-9]|4[0-9][0-9]|50[0-1])$", "Hyperglycemia", x = bg_conc))


 ggplot(newmaster, aes(factor(time1), bg_conc)) +
   geom_bar(stat = "identity")

ggplot(newmaster, aes(factor(time1))) +
 geom_bar()



