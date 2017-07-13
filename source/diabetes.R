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

newmaster <- master[! master$Value == "000",]
length(unique(newmaster$Code))