# main
cat("\014")
rm(list = ls())
# .pardefault = par()
wd = getwd()

# Fix the repos... ITU IT gods
# install.packages('package', repos='http://cran.us.r-project.org')

data_file = "all_data.RData"

source("libraries.R")

source("functions.R")

source("data_prep.R")


# record()
# save.image(paste0(getwd(),"/","all_data.RData"))
# save.image(paste0(getwd(),"/bucket.RData"))
# browseURL(paste('file://', file.path(getwd(),'notes.html'), sep=''))



  
