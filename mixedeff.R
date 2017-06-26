# Models for q8, q9 - q15

bindf = dfu
names(bindf) = gsub("^.*?_","",names(bindf))
names(dfu) = names(bindf)

bin = function(x){
  lvlz = as.numeric(levels(x))
  if(length(lvlz) %% 2 == 0){ # even
    x = as.numeric(x)
    xleft = x < median(lvlz)
    xright = x > median(lvlz)
    x[xleft] = 0
    x[xright] = 1
    return(factor(x))
  }
  if(length(lvlz) %% 2 == 1){ # odd
    x = as.numeric(x)
    xleft = x < mean(lvlz)
    xright = x > mean(lvlz)
    xmid = x == mean(lvlz)
    x[xleft] = -1
    x[xright] = 1
    x[xmid] = 0
    return(factor(x))
  } 
}


what = sapply(bindf,function(x) is.factor(x) & length(levels(x))>2)
what["school"] = what["platform"] = what["freqpost"] = what["freqsend"] = what["freqread"] = FALSE
for(i in (1:length(what))[what]){
  bindf[,i] = bin(bindf[,i])
}
dfubin = bindf
bindf = split(bindf,bindf$platform)




# clms for platform on DFU ------------------------------------------------

m1 = clm(platform ~ 1,
         data = dfu,family=binomial)

m2 = update(m1,.~. + gender)
anova(m1,m2)
# gender > 1

m3 = update(m2,.~. + nocare)
anova(m2,m3)
# gender + nocare > gender

m4 = update(m1,.~. + nocare)
anova(m3,m4)
# gender + nocare > nocare

m5 = update(m3,.~. + digedu)
anova(m3,m5)
# digedu insignif

m6 = update(m3,.~. + audience)
anova(m3,m6)
# audience quite important
class.pred(m6)$table

m7 = update(m1,.~. + audience)
anova(m3,m7)
# audience > gender + nocare
anova(m7,m2)
# audience > gender

m8 = update(m7,.~. + gender)
anova(m7,m8)
class.pred(m8)$table
# audience + gender > audience

m9 = update(m7,.~. + nocare)
anova(m9,m4)
# audience + nocare > nocare

m10 = update(m8,.~. + nocare)
anova(m10,m8)
class.pred(m10)$table

m11 = clm(platform ~ (gender+digedu+nocare)^2,
          data = dfu, family = binomial)
class.pred(m11)$table


# ig models ---------------------------------------------------------------

igm0 = clm(care ~ 1,
           data = bindf$ig, family = binomial)

igm1 = update(igm0,.~. + edit)
anova(igm0,igm1) 
class.pred(igm1)$table
# 1 > gender > nocare > edit


# clusters ----------------------------------------------------------------

# kmeans
cldf = sapply(dfnum,is.numeric)
cldf = dfnum[,cldf]
c1 = kmeans(cldf,centers=3)

x = sample(1:nrow(cldf),nrow(cldf)/2)
tr.cldf = cldf[x,]
te.cldf = cldf[-x,]

c2 = knn(tr.cldf,te.cldf,dfnum$platform[x])
