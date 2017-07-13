library(readr)
library(dplyr)
library(tidyr)
library(stringr)
master <- data.frame()
for(i in 1:70) {
  num = paste("", i, sep = "")
  if (i < 10) num = paste("0", i, sep = "")
  fname = paste("~/R Projects/diabetes-dataset/Diabetes-Data/data-", num, sep = "")
  temp <- read_delim(fname, col_names = c("Date", "Time", "Code", "Value"),"\t",
                     escape_double = FALSE, trim_ws = TRUE);
  temp <- mutate(temp, FileName = i)
  master <- rbind(master, temp);
}
#master %>%
#as.numeric(master$Value)
#transmute(master$Value, gsub("000", "NA", x = "Value"))
#as.numeric(master$Value)
#is.na(master$Value) <- Value == "000"
#filter(master, Value >= 1)
#master %>% filter(!is.na(Value))
#any(is.na(master))
#sum(is.na(master))
#summary(master)
#str_replace(master$Value, "000", "NA")
# 000 values are strings with str_detect
#class(master$Value)
newmaster <- master[! master$Value == "000",]
length(unique(newmaster$Code))