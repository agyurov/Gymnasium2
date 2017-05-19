# Hluca


# Q2 v Q7 on combined data ------------------------------------------------
# w/o demo data
hluca.dat1 = dfu[,c("q5.1_digedu","platform","q2.1_nocare")]
hluca.dat2 = dfu[,c("q5.1_digedu","platform","q7.1_editedprivacy")]

luca1 = model.listZ(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca1$resp)
my.barplot(luca1$resp$beta,namez = names(luca1$resp$beta),col=3:4)
luca1o2 = model.listZ2(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca1o2$resp)
my.barplot(luca1o2$resp$beta,namez = names(luca1o2$resp$beta),col=grey.colors(2,start=0,end=.4,alpha = .5))

luca2 = model.listZ(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca2$resp)
my.barplot(luca2$resp$beta,namez = names(luca2$resp$beta),col=rainbow(2,s=.3,v=.5,alpha = .7))
luca2o2 = model.listZ2(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca2o2$resp)
my.barplot(luca2o2$resp$beta,namez = names(luca2o2$resp$beta),col=3:4)


# W demo data
hluca.dat1demo = cbind(hluca.dat1,demo.dat)
hluca.dat2demo = cbind(hluca.dat2,demo.dat)

luca3 = model.listZ(pred = hluca.dat2demo, resp = dfu$q2.1_nocare)
summary(luca3$resp)
my.barplot(luca3$resp$beta,namez = names(luca3$resp$beta),col=3:4)
luca3o2 = model.listZ2(pred = hluca.dat2demo, resp = dfu$q2.1_nocare)
summary(luca3o2$resp)
my.barplot(luca3o2$resp$beta,namez = names(luca3o2$resp$beta),col=3:4)

luca4 = model.listZ(pred = hluca.dat1demo, resp = dfu$q7.1_editedprivacy)
summary(luca4$resp)
#my.barplot(luca4$resp$beta,namez = names(luca4$resp$beta))
luca4o2 = model.listZ2(pred = hluca.dat1demo, resp = dfu$q7.1_editedprivacy,trace=1)
summary(luca4o2$resp)
my.barplot(luca4o2$resp$beta,namez = names(luca4o2$resp$beta),col=3:4)


