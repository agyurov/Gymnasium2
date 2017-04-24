## data prep

fb = read.csv(dir()[grepl("facebook",tolower(dir()))],encoding = "UTF-8")
ig = read.csv(dir()[grepl("instagram",tolower(dir()))],encoding = "UTF-8")
sc = read.csv(dir()[grepl("snapchat",tolower(dir()))],encoding = "UTF-8")

## Column names

names(fb) = tolower(names(fb))
names(ig) = tolower(names(ig))
names(sc) = tolower(names(sc))

data_names = list()
data_names$fb = names(fb)
data_names$ig = names(ig)
data_names$sc = names(sc)


## english names
eng_names = "empty"
eng_names = c(eng_names,"time")
eng_names = c(eng_names,paste0("use_",c("fb","ig","sc")),"nocare")
eng_names = c(eng_names,"gender","age","digedu","school")
eng_names = c(eng_names,"editedprivacy")
eng_names = c(eng_names,paste0("freq",c("post","send","read")))
eng_names = c(eng_names,"seecontent","choosefeed","seemypost","controlseemypost","audience")
eng_names = c(eng_names,"trustconn","trustsell","trustpriv")
eng_names = c(eng_names,"targetme","targetfr","comfortsell")
eng_names = eng_names[-1]

## check
# cbind(eng_names,names(fb))
# cbind(eng_names,names(ig))
# cbind(eng_names,names(sc))

names(fb) = names(ig) = names(sc) = eng_names
fb$use_fb

## Create a master data frame
df = list(fb=fb,ig=ig,sc=sc)

## Manual releveling
x = fb$use_fb
levels(fb$use_fb) = c("never","rarely","often","some")
fb$use_fb = factor(fb$use_fb,ordered = T)
levels(fb$use_ig) = c("never","rarely","often","some")
fb$use_ig = factor(fb$use_ig,ordered = T)

y = fb$use_sc
fb$use_sc = factor(fb$use_sc, labels = c("never","rarely","often","some"))

