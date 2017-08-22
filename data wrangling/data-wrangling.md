# Capstone project data wrangling and cleaning
******

## Reading the dataset
The diabetes data set from UCI machine learning repository is relatively comprehensive and well put together and would not require any major transformations. 
However, it consist of data of  several weeks' to months' worth of outpatient care on 70 patients; which needs to be merged together to form a data frame.
As the data missed column names, they are given as “date”, “time”, “code” & “bg_value”. Individual columns are read in specific format for date(MM-DD-YYYY) and time(HH:MM).
An additional column "file_name" is added to specify the patient/ file number.

```
library(readr)
library(dplyr)
library(tidyr)
{master <- data.frame()
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
}}
```

## Dealing with the missing values

After initial inspection, it is found that there are some missing values in the “bg_value” column; in the form of “000" strings. As these values do not make any sense, considering that there are “0” blood glucose values for hypoglycaemia; the “000” values are be removed. In addition to that, other illogical values are also removed from “code” column. Also the Date column of master data set showed 40 NA’s.



```r
   summary(master) 
```

```
##       date                time               code         bg_value        
##  Min.   :1988-03-27   Length:29330      Min.   : 0.00   Length:29330      
##  1st Qu.:1990-01-03   Class1:hms        1st Qu.:33.00   Class :character  
##  Median :1990-09-20   Class2:difftime   Median :48.00   Mode  :character  
##  Mean   :1990-08-19   Mode  :numeric    Mean   :46.43                     
##  3rd Qu.:1991-05-08                     3rd Qu.:60.00                     
##  Max.   :1991-09-23                     Max.   :72.00                     
##  NA's   :40                                                               
##    file_name    
##  Min.   : 1.00  
##  1st Qu.:21.00  
##  Median :34.00  
##  Mean   :36.44  
##  3rd Qu.:55.00  
##  Max.   :70.00  
## 
```

```
newmaster <- subset(master, bg_value != "000" & bg_value != "3A”)
newmaster <- subset(newmaster, code != "4" & code != "36" & code != "56”)
```

## Formatting of data

For exploratory data analysis, the “code” column is from a character to a numeric format.

```
newmaster$bg_value <- as.numeric(newmaster$bg_value)
```

To make sure all the values in dataset make sense, the data is summarised

```
summary(newmaster)
```

All the NAs introduced after transforming the “bg_value” from character to numeric are then removed

```
newmaster <- na.omit(newmaster)
```
