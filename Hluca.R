# Hluca


# Q2 v Q7 on combined data ------------------------------------------------
# w/o demo data
hluca.dat1 = dfu[,c("q5.1_digedu","platform","q2.1_nocare")]
hluca.dat2 = dfu[,c("q5.1_digedu","platform","q7.1_editedprivacy")]

luca1 = model.listZ(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca1$resp)
luca1o2 = model.listZ2(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca1o2$resp)

luca2 = model.listZ(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca2$resp)
luca2o2 = model.listZ2(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca2o2$resp)


# W demo data
hluca.dat1 = cbind(hluca.dat1,demo.dat)
hluca.dat2 = cbind(hluca.dat2,demo.dat)

luca3 = model.listZ(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca3$resp)
luca3o2 = model.listZ2(pred = hluca.dat2, resp = dfu$q2.1_nocare)
summary(luca3o2$resp)

luca4 = model.listZ(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca4$resp)
luca4o2 = model.listZ2(pred = hluca.dat1, resp = dfu$q7.1_editedprivacy)
summary(luca4o2$resp)


frmla = as.formula(paste0("Y"," ~ ","( ",paste0(names(pred),collapse=" + ")," )^",order))