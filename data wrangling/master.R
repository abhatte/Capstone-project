library(knitr); 
#knit('data-wrangling.Rmd')
#knitr::knit2html('data-wrangling.Rmd') 

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('data-wrangling', 'data-wrangling') # creates md file
markdownToHTML('data-wrangling', 'data-wrangling') # creates html file
browseURL(paste('file://', file.path(getwd(),'data-wrangling.html'), sep='')) # open file in browser