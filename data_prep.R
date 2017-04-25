## data prep

fb = read.csv(dir()[grepl("facebook",tolower(dir()),fixed=T)],encoding = "UTF-8")
ig = read.csv(dir()[grepl("instagram",tolower(dir()),fixed=T)],encoding = "UTF-8")
sc = read.csv(dir()[grepl("snapchat",tolower(dir()),fixed=T)],encoding = "UTF-8")

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
eng_names = c(eng_names,paste0("use",c("fb","ig","sc")),"nocare")
eng_names = c(eng_names,"gender","age","digedu","school")
eng_names = c(eng_names,"editedprivacy")
eng_names = c(eng_names,paste0("freq",c("post","send","read")))
eng_names = c(eng_names,"seecontent","understand","seemypost","controlseemypost","audience")
eng_names = c(eng_names,"trustconn","trustsell","trustpriv")
eng_names = c(eng_names,"targetme","targetfr","comfortsell")
eng_names = eng_names[-1]

## check
# cbind(eng_names,names(fb))
# cbind(eng_names,names(ig))
# cbind(eng_names,names(sc))

names(fb) = names(ig) = names(sc) = eng_names




# Question numbers --------------------------------------------------------

qlen = c(1,3,rep(1,6),3,rep(1,11))
long_q = 0:(length(qlen)-1)
short_q_num = unlist(lapply(qlen,function(x) 1:x))
long_q_num = paste0("q",rep(long_q,qlen))

full_q_num = paste0(long_q_num,".",short_q_num)
final_names = paste0(full_q_num,"_",eng_names)

names(fb) = final_names
names(ig) = final_names
names(sc) = final_names


# Fix variable classes ----------------------------------------------------

fb$q0.1_time = as.Date(fb$q0.1_time)
ig$q0.1_time = as.Date(ig$q0.1_time)
sc$q0.1_time = as.Date(sc$q0.1_time)

fb$q4.1_age = as.numeric(fb$q4.1_age)
ig$q4.1_age = as.numeric(ig$q4.1_age)
sc$q4.1_age = as.numeric(sc$q4.1_age)

fb$platform = factor(rep("fb",nrow(fb)),levels = c("fb","ig","sc"))
ig$platform = factor(rep("ig",nrow(ig)),levels = c("fb","ig","sc"))
sc$platform = factor(rep("sc",nrow(sc)),levels = c("fb","ig","sc"))

## Create a master data frame
df = list(fb=fb,ig=ig,sc=sc)

# Fix factor levels -------------------------------------------------------

# Remove Danish characters from factors
rm.char.factor = function(x){
  if(is.factor(x)){
    for(i in c("æ","ø","å")){
      x = gsub(i,"",x)
      x = factor(x,levels = gsub(i,"",unique(x)))
    }
    return(x)
  }
  return(x)
}

fb = do.call(cbind.data.frame,lapply(fb,rm.char.factor))
ig = do.call(cbind.data.frame,lapply(ig,rm.char.factor))
sc = do.call(cbind.data.frame,lapply(sc,rm.char.factor))

# Add missing factor levels
fill.levels = function(x,y,...){
  # x the data frame
  # y the qustions as character vector
  tmp = x[,grepl(y,names(x),fixed=T)]
  maxlev = which.max(unlist(lapply(tmp,function(x) length(levels(x)))))
  maxlev = levels(tmp[,maxlev])
  tmp = do.call(cbind.data.frame,lapply(tmp,function(x,y) x=factor(x,levels=y),y=maxlev))
  x[,grepl(y,names(x),fixed=T)] = tmp
  return(x)
}
# 
fb = fill.levels(fb,"q1.")
fb = fill.levels(fb,"q8.")
sc = fill.levels(sc,"q1.")
sc = fill.levels(sc,"q8.")
ig = fill.levels(ig,"q1.")
ig = fill.levels(ig,"q8.")








