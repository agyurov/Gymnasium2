# Model for Q7


# confirm procedure -------------------------------------------------------

# fakeresp = sample(c(0,1),50,replace = T)
# fakepred = fakeresp
# fakepred[1:9] = c(1,0,1,1,1,1,0,1,0)
# 
# fk = glm(fakeresp~fakepred,family=binomial)
# glm.pred(fk)
# goodness(fk)

# Alternative binary q2 and q7 --------------------------------------------

# Init

m0 = glm(bin_edit ~ q3.1_gender + q5.1_digedu + bin_care, data = dfu,binomial(cauchit))
m1 = update(m0,.~. - q5.1_digedu)
anova(m0,m1,test="Chisq")
m2 = update(m1,.~. - bin_care)
anova(m1,m2,test="Chisq")
# if platform instead of education in m3 we get 5% difference ~
m3 = glm(bin_edit ~ q5.1_digedu + bin_care, data = dfu,binomial(cauchit)) 
anova(m2,m3,test="Chisq")

summary(m2)
goodness(m2)

hist(predict(m2,type="response"))
glm.pred(m2,tresh=.8)



# with platform

mm0 = glm(bin_edit ~ q3.1_gender + q5.1_digedu + bin_care + platform, data = dfu,quasibinomial)
mm1 = update(mm0,.~. - q5.1_digedu)
anova(mm0,mm1,test="Chisq")
mm2 = update(mm1,.~. - platform)
anova(mm1,mm2,test="Chisq")
mm3 = update(mm2,.~. - q3.1_gender)
anova(mm2,mm3,test="Chisq")

summary(mm2)
goodness(mm2)

hist(predict(mm2,type="response"))
glm.pred(mm2,tresh=.65)
table(mm2$y,bin_edit)

# now from the null model

# n0 = glm(bin_edit ~ )



# bottom up ---------------------------------------------------------------

m0 = glm(bin_edit ~ (q1.1_usefb + q1.2_useig + q1.3_usesc) : platform
         ,dfu, family=quasibinomial)
m1 = m0
# m1 = step(m0,test="Chisq",trace=F)
hist(predict(m1,type="response"))
summary(m1)
goodness(m1)
goodness(m1,null=T)
1-summary(m1)$deviance/summary(m1)$null.deviance
glm.pred(m1)
# cat("\014")
str(dfu)



# omg ---------------------------------------------------------------------

fk = c(25,27,6,8,9,26)
dfx = dfu[,fk]
str(dfx)
levels(dfx$bin_care) = c("Do","Not")
levels(dfx$bin_edit) = c("Not","Do")
levels(dfx$q5.1_digedu) = c("Yes","No")


# fk = apply(dfx[-2],1,function(x) paste0(x,collapse=" "))
# lol = data.frame(table(fk))
# lol

fk = data.frame(aggregate(bin_edit~.,data=dfx,table))
#fk$n = fk$bin_edit.NotEdit + fk$bin_edit.Edit
#fk$resp = cbind(fk$bin_edit.Edit,fk$bin_edit.NotEdit)
m0 = glm(bin_edit ~ (q3.1_gender + q5.1_digedu + bin_care + platform)^2, data = fk,binomial(cauchit))
m0 = step(m0,test="Chisq",trace=0)
summary(m0)
goodness(m0)
goodness(m0,null=T)
hist(predict(m0,type="response"))
glm.pred(m0,tresh=.8)

print(cbind(fk,round(predict(m0,type="response"),2)))
whch = which(round(predict(m0,type="response"))==1)
print(cbind(fk,predict(m0,type="response")) [whch,])
summary(m0)
