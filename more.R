# More anlaysis


# Same question data set --------------------------------------------------

sameq = c("time","usefb","useig","usesc","nocare","gender","age","digedu","school","editedprivacy")
sameq = unlist(lapply(sameq,function(x,y) grep(x,y,fixed=T),y=names(dfu)))
sameq = dfu[,sameq]

# Identify wrongly placed respondents -------------------------------------
# good
misplaced = list()
misplaced$fb1 = which(fb$q1.1_usefb == 1)
misplaced$fb2 = which(fb$q1.1_usefb == 2)

misplaced$ig1 = which(ig$q1.2_useig == 1)
misplaced$ig2 = which(ig$q1.2_useig == 2)

misplaced$sc1 = which(sc$q1.3_usesc == 1)
misplaced$sc2 = which(sc$q1.3_usesc == 2)

misplaced = do.call(c,misplaced)

# Identify slackers -------------------------------------------------------
# BAD
str(dfnum)
slack = sapply(dfnum,is.numeric)
slack = data.matrix(dfnum[,slack])
slacksd = apply(slack,1,sd)
fk = sort(slacksd,decreasing = T)
fk = head(fk,10)
fk = as.numeric(names(fk))

dfu[fk,]

unlist(lapply(misplaced,function(x,y) y[y%in%x],y=fk))


# Identify knowledgable and ignorant respondents --------------------------


# Identify trusting and distrusting respondents ---------------------------


m2 = clm(q7.1_editedprivacy ~ q2.1_nocare , nominal =~ (q5.1_digedu+ q3.1_gender)^2,data=dfu,
         link = "loglog",threshold="symmetric2")
m2 = step(m2,test="Chisq",trace=0)
plot.ts(predict(m2)$fit)
summary(m2)
class.pred(m2)

bin_edit1 = as.numeric(dfu$q7.1_editedprivacy)
bin_edit1[bin_edit1 == 4] = 1337
bin_edit1[bin_edit1 < 5] = 0
bin_edit1[bin_edit1 == 1337] = 1
bin_edit1 = factor(bin_edit1)

mx = glm(bin_edit1 ~ (q2.1_nocare + q5.1_digedu+ q3.1_gender)^2,data=dfu,family=binomial(identity))
mx = step(mx,test="Chi")
table(dfu$bin_edit,round(predict(mx,type="response")))



# clusters ----------------------------------------------------------------

fk1 = kmeans(dfnum[,sapply(dfnum,is.numeric)],centers=2)
lapply(dfnum,function(x,y)table(x,y),y=fk1$cluster)
