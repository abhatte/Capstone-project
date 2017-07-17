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
    date = col_character(),
    time = col_time(format = "%AT"),
    code = col_integer(),
    bg_value = col_character()
  ), "\t",
    escape_double = FALSE, trim_ws = TRUE);
  temp <- mutate(temp, file_name = i)
  master <- rbind(master, temp);
}
# create newmaster = remove "000" & "3A" from bg_value
newmaster <- subset(master, bg_value != "000" & bg_value != "3A")
# update newmaster = remove discrepancies in code "4", "36, & "56"
newmaster <- subset(newmaster, code != "4" & code != "36" & code != "56")
#convert newmaster$bg_value from character to numeric
newmaster$bg_value <- as.numeric(newmaster$bg_value)
summary(newmaster)
#remove NAs from above
newmaster <- na.omit(newmaster)
#check for NA = 8 NAs introduced by coercion in bg_value
summary(newmaster)
glimpse(newmaster)
length(unique(newmaster$code))

library(ggplot2)
ggplot(newmaster, aes(factor(code), bg_value)) +
  geom_boxplot()

#   scale_y_continuous("Blood glucode concentration (mg/dl)", # added - 34,35,36,37
#                      limits = c(0, 501)) +
#   scale_x_discrete("Code") +
#   theme_classic()


#Time value = high/low

# t1 <- ggplot(newmaster, aes(x = time))+
#    geom_histogram(binwidth = 20)
# print(t1)
# ggplot(newmaster, aes(x = time, y = bg_value, col = code)) +
#   geom_ribbon(aes(ymax = bg_value, ymin = 0), alpha = 0.3)

#creat newmaster1 for testing
newmaster1 <- newmaster
newmaster1 %>%
  mutate(time = as.POSIXct(time, format = "%H:%M:%S"))

# time <- as.POSIXct("2016-01-01 00:00:00", tz="UTC") + as.difftime(30*(0:47),units="mins")
# cut(time, breaks="2 hours", labels=FALSE)
# data.frame(time, cuts = cut(x, breaks="2 hours", labels=FALSE))

ggplot(newmaster1, aes(x = time, y = bg_value)) + geom_boxplot() +
    scale_x_time(breaks = "2 hours", labels = "%H:%M:%S")
