## data prep

fb = read.csv(dir()[grepl("facebook",tolower(dir()),fixed=T)],encoding = "UTF-8")
ig = read.csv(dir()[grepl("instagram",tolower(dir()),fixed=T)],encoding = "UTF-8")
sc = read.csv(dir()[grepl("snapchat",tolower(dir()),fixed=T)],encoding = "UTF-8")


# busted ------------------------------------------------------------------
fb = busted(fb,3,4)
ig = busted(ig,3,4)
sc = busted(sc,3,4)

# Column names ------------------------------------------------------------


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


# Original data -----------------------------------------------------------

df0.list = list(fb0 = fb, ig0 = ig, sc0 = sc)
original.data = do.call(rbind.data.frame,df0.list)

tmp = original.data$time
tmp = as.character(tmp)
tmp = gsub(" EET","",tmp)
tmp = gsub(" ","_",tmp)

# "2017/04/18_7:57:05_am_EET"
tmp = strptime(tmp,"%Y/%m/%d_%I:%M:%S_%p",tz="")
thetime = tmp

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
fbhour = as.numeric(hour(fb$q0.1_time))
ighour = as.numeric(hour(ig$q0.1_time))
schour = as.numeric(hour(sc$q0.1_time))


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

# Fix factor levels -------------------------------------------------------

fb = do.call(cbind.data.frame,lapply(fb,rm.char.factor))
ig = do.call(cbind.data.frame,lapply(ig,rm.char.factor))
sc = do.call(cbind.data.frame,lapply(sc,rm.char.factor))


# Rename factor levels ----------------------------------------------------
# convert levels to num to char to fact
# First define question groups

#
fb = fill.levels(fb,"q1.")
fb = fill.levels(fb,"q8.")
ig = fill.levels(ig,"q1.")
ig = fill.levels(ig,"q8.")
sc = fill.levels(sc,"q1.")
sc = fill.levels(sc,"q8.")



# Manually fix factor levels, grammar mistakes stc ------------------------
fblev = lapply(fb,levels)
iglev = lapply(ig,levels)
sclev = lapply(sc,levels)

# for(i in 1:length(fblev)){
#   cat("\014")
#   print(names(fblev)[i])
#   x = c(fb=fblev[i],sc=sclev[i],ig=iglev[i])
#   print(table(unlist(x)))
#   readline("Press any key to continue lol...")
# }
# ig q1, sjldent to sjlendt
# ig q8, add level engangitimenellermere

# ...the fix
x = ig
levels(ig$q1.1_usefb) = levels(fb$q1.1_usefb)
levels(ig$q1.2_useig) = levels(fb$q1.1_usefb)
levels(ig$q1.3_usesc) = levels(fb$q1.1_usefb)

levels(ig$q8.1_freqpost) = c(levels(ig$q8.1_freqpost),
                             levels(fb$q8.1_freqpost)[!levels(fb$q8.1_freqpost) %in% levels(ig$q8.1_freqpost)])
levels(ig$q8.2_freqsend) = c(levels(ig$q8.2_freqsend),
                             levels(fb$q8.2_freqsend)[!levels(fb$q8.2_freqsend) %in% levels(ig$q8.2_freqsend)])
levels(ig$q8.3_freqread) = c(levels(ig$q8.3_freqread),
                             levels(fb$q8.3_freqread)[!levels(fb$q8.3_freqread) %in% levels(ig$q8.3_freqread)])

# Factor levels to character numbers --------------------------------------

qlev = list()
qlev$aldrig = 1
qlev$sjldent = 2
qlev$sjlendt = 2 # cool grammar mistake
qlev$noglegange = 3
qlev$ofte = 4

qlev$megetuenig = 1
qlev$uenig = 2
qlev$enig = 3
qlev$megetenig = 4

qlev$engangitimenellermere = 7
qlev$fleregangeomdagen = 6
qlev$ca.engangomdagen = 5
qlev$fleregangeomugen = 4
qlev$ca.engangomugen = 3
qlev$mindre = 2

qlev$megetsikker = 4
qlev$sikker = 3
qlev$usikker = 2
qlev$megetusikker = 1 #unlist(qlev[levels(x)])

# Levels to character numbers (easier) ------------------------------------
fbX = fb
# fb
for(i in 1:(ncol(fb)-1)){ # do not include the labeling column
  if(is.factor(fb[,i]) && levels(fb[,i]) %in% names(qlev)){
    levels(fb[,i]) = unlist(qlev[levels(fb[,i])])
    fb[,i] = factor(as.numeric(as.character(fb[,i])))
  }
}

# ig
igX = ig
for(i in 1:(ncol(ig)-1)){ # do not include the labeling column
  if(is.factor(ig[,i]) && levels(ig[,i]) %in% names(qlev)){
    levels(ig[,i]) = unlist(qlev[levels(ig[,i])])
    ig[,i] = factor(as.numeric(as.character(ig[,i])))
  }
}

# sc
scX = sc
for(i in 1:(ncol(sc)-1)){ # do not include the labeling column
  if(is.factor(sc[,i]) && levels(sc[,i]) %in% names(qlev)){
    levels(sc[,i]) = unlist(qlev[levels(sc[,i])])
    sc[,i] = factor(as.numeric(as.character(sc[,i])))
  }
}

# Master df list ----------------------------------------------------------
df.list = list(fb = fb,ig = ig, sc = sc)
dfu = rbind(fb,ig,sc)

# change order of levels (NOT ordering the factor!!!!)
for(i in (1:ncol(dfu))[-c(1,6,7,8,9,ncol(dfu))]){
  dfu[,i] = factor(dfu[,i],levels=sort(as.numeric(as.character(levels(dfu[,i])))))
}


# Invert levels for FB q9 and IG q9 ---------------------------------------

x = dfu$q9.1_seecontent[dfu$platform == "fb" | dfu$platform == "ig"]
x = invert.level(x)
dfu$q9.1_seecontent[dfu$platform == "fb" | dfu$platform == "ig"] = x$x
tmp = which(dfu$q6.1_school == "")
dfu = dfu[-tmp,]
thetime = thetime[-tmp]


# Remove weird schools ----------------------------------------------------
tmp = which(dfu$q6.1_school == "knord")
dfu = dfu[-tmp,]
thetime = thetime[-tmp]
levels(dfu$q6.1_school)
table(dfu$q6.1_school)
dfu$q6.1_school = factor(dfu$q6.1_school,levels=levels(dfu$q6.1_school)[-which(levels(dfu$q6.1_school) == "knord")])
dfu$q6.1_school = factor(dfu$q6.1_school,levels=levels(dfu$q6.1_school)[1:13])

# Identify wrongly placed respondents -------------------------------------

misplaced = list()
misplaced$fb1 = which(fb$q1.1_usefb == 1)
misplaced$fb2 = which(fb$q1.1_usefb == 2)

misplaced$ig1 = which(ig$q1.2_useig == 1)
misplaced$ig2 = which(ig$q1.2_useig == 2)

misplaced$sc1 = which(sc$q1.3_usesc == 1)
misplaced$sc2 = which(sc$q1.3_usesc == 2)

misplaced = do.call(c,misplaced)
dfu = dfu[-misplaced,]
thetime = thetime[-misplaced]



# Binaries ----------------------------------------------------------------

dfu$bin_care = as.character(dfu$q2.1_nocare)
dfu$bin_care[dfu$bin_care == 1 | dfu$bin_care  == 2] = "disagree"
dfu$bin_care[dfu$bin_care == 3 | dfu$bin_care  == 4] = "agree"
dfu$bin_care[dfu$bin_care == "disagree"] = 0
dfu$bin_care[dfu$bin_care == "agree"] = 1
dfu$bin_care = as.factor(dfu$bin_care)

dfu$bin_edit = as.character(dfu$q7.1_editedprivacy)
dfu$bin_edit[dfu$bin_edit == 1 | dfu$bin_edit  == 2] = "disagree"
dfu$bin_edit[dfu$bin_edit == 3 | dfu$bin_edit  == 4] = "agree"
dfu$bin_edit[dfu$bin_edit == "disagree"] = 0
dfu$bin_edit[dfu$bin_edit == "agree"] = 1
dfu$bin_edit = as.factor(dfu$bin_edit)


# knowledge ---------------------------------------------------------------
  
# dfu$k1_ = factor(paste0(dfu$q9.1_seecontent,dfu$q10.1_understand))
# dfu$k2_ = factor(paste0(dfu$q11.1_seemypost,dfu$q12.1_controlseemypost))
# dfu$k3_ = factor(paste0(dfu$q9.1_seecontent,dfu$q11.1_seemypost))
# dfu$k4_ = factor(paste0(dfu$q9.1_seecontent,dfu$q12.1_controlseemypost))
# dfu$k5_ = factor(paste0(dfu$q10.1_understand,dfu$q11.1_seemypost))
# dfu$k6_ = factor(paste0(dfu$q10.1_understand,dfu$q12.1_controlseemypost))

# numeric version ---------------------------------------------------------

dfnum = fact2num(dfu,all=c(2,3,4,5,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))


# Identify infrequent users -----------------------------------------------

x = dfnum[,grepl("q8.",names(dfnum),fixed=T)]
table(apply(x,1,function(x) sum(x>4)))
which(apply(x,1,function(x) sum(x>4))==0)

dfu = dfu[-which(apply(x,1,function(x) sum(x>4))==0),]
dfnum = dfnum[-which(apply(x,1,function(x) sum(x>4))==0),]
thetime = thetime[-which(apply(x,1,function(x) sum(x>4))==0)]
  
# splitting ---------------------------------------------------------------

# split into before and after education
dfub = dfu[dfu$q5.1_digedu == "nej",]
dfua = dfu[dfu$q5.1_digedu == "ja",]

dfnumb = dfnum[dfnum$q5.1_digedu == "nej",]
dfnuma = dfnum[dfnum$q5.1_digedu == "ja",]

fb = dfu[dfu$platform=="fb",]
ig = dfu[dfu$platform=="ig",]
sc = dfu[dfu$platform=="sc",]

fbb = fb[fb$q5.1_digedu=="nej",]
igb = ig[ig$q5.1_digedu=="nej",]
scb = sc[sc$q5.1_digedu=="nej",]

fba = fb[fb$q5.1_digedu=="ja",]
iga = ig[ig$q5.1_digedu=="ja",]
sca = sc[sc$q5.1_digedu=="ja",]

# ordered factors ---------------------------------------------------------
# 
dfo = ordfactordf(dfu,ordered=T)
dfo$q3.1_gender = factor(dfo$q3.1_gender,ordered=F)
dfo$q5.1_digedu = factor(dfo$q5.1_digedu,ordered=F)
dfo$q6.1_school = factor(dfo$q6.1_school,ordered=F)
dfo$platform = factor(dfo$platform,ordered=F)

# split into before and after education
dfob = dfo[dfo$q5.1_digedu == "nej",]
dfoa = dfo[dfo$q5.1_digedu == "ja",]

fbo = dfo[dfo$platform=="fb",]
igo = dfo[dfo$platform=="ig",]
sco = dfo[dfo$platform=="sc",]

fbob = fbo[fbo$q5.1_digedu=="nej",]
igob = fbo[igo$q5.1_digedu=="nej",]
scob = fbo[sco$q5.1_digedu=="nej",]

fboa = fbo[fbo$q5.1_digedu=="ja",]
igoa = fbo[igo$q5.1_digedu=="ja",]
scoa = fbo[sco$q5.1_digedu=="ja",]

fbnum = dfnum[dfnum$platform=="fb",]
ignum = dfnum[dfnum$platform=="ig",]
scnum = dfnum[dfnum$platform=="sc",]

fbnumb = fbnum[fbnum$q5.1_digedu=="nej",]
ignumb = ignum[ignum$q5.1_digedu=="nej",]
scnumb = scnum[scnum$q5.1_digedu=="nej",]

fbnuma = fbnum[fbnum$q5.1_digedu=="ja",]
ignuma = ignum[ignum$q5.1_digedu=="ja",]
scnuma = scnum[scnum$q5.1_digedu=="ja",]

# Verify factor level conversion ------------------------------------------

# for(i in 2:ncol(df0.df)){ # dfo and dfu have 1 more column "platform"
#   x = df0.df[,i]
#   y = dfo[,i]
#   t = table(x,y)
#   if(any(apply(t,2,function(x) length(x[x!=0])) > 1)){ # check in dfo, cuz levels are fixed
#     print(table(x,y))
#     cat(print(paste0("Differences in ",names(dfo)[i])))
#     # readline("Next..")
#   } 
# }

# Cool everything is fine

demo.dat = dfu[,c("q3.1_gender","q4.1_age","q6.1_school")]




dfu$hour = hour(thetime)
dfu$day = weekdays(thetime)
