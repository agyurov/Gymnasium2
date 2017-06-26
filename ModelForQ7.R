dff = fb
names(dff) = gsub("^.*?_","",names(dff))

# ------------------------------------------
cat("\014")
str(dfu)

# Q7 only for fbook
# binary ------------------------------------------------------------------

mf0 = glm(edit ~ 1,
         data = dff, family = binomial())
goodness(mf0)
summary(mf0)

mf1 = glm(edit ~ gender,
          data = dff, family = binomial())
goodness(mf1)
glm.pred(mf1)
summary(mf1)
anova(mf0,mf1,test="Chisq")

# gender > 1

mf2 = glm(edit ~ gender + digedu,
          data = dff, family = binomial())
goodness(mf2)
glm.pred(mf2)
summary(mf2)
anova(mf1,mf2,test="Chisq")

# gender > gender + digedu
 
mf3 = glm(edit ~ gender + nocare, # BEST
          data = dff, family = binomial())
goodness(mf3)
glm.pred(mf3)
summary(mf3)
anova(mf1,mf3,test="Chisq")

# Best model mf3



# unique obs on common vars -----------------------------------------------
fk = do.call(rbind.data.frame,bindf)
fk = fk[,c(6,8,25,26,27)]
fk$id = apply(fk,1,function(x) paste0(x,collapse=""))
x = duplicated(fk)
fk2 = fk[!x,]


x = aggregate(id~.,fk,sum)
y = table(fk$id)
z = fk[fk$id %in% names(y),]
tmp = unlist(lapply(names(y),function(x,y) which(y == x)[1],y=fk$id))
LOL = fk[tmp,]
LOL$count = y








