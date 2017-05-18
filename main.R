# main
cat("\014")
rm(list = ls())
# .pardefault = par()
wd = getwd()

# Fix the repos... ITU IT gods
# install.packages('package', repos='http://cran.us.r-project.org')

data_file = ".RData"

source("libraries.R")

source("Rfunctions/Rfunctions.R")

source("functions.R")

source("data_prep.R")

source("Hluca.R")

source("Hluca2.R")

record()
# save.image(paste0(getwd(),"/","all_data.RData"))
# save.image(paste0(getwd(),"/bucket.RData"))
# browseURL(paste('file://', file.path(getwd(),'notes.html'), sep=''))



  
# bucket.classes(classes=c("data.frame"))
# with(.BucketEnv,save.image(paste0(getwd(),"/bucket.RData")))