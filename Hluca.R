# Hluca


# Q2 v Q7 on combined data ------------------------------------------------
# w/o demo data
hluca.dat1 = dfu[,c("q5.1_digedu","platform","q2.1_nocare")]
hluca.dat2 = dfu[,c("q5.1_digedu","platform","q7.1_editedprivacy")]

luca1 = model.listZ(pred = hluca.dat1, resp = dfu["q7.1_editedprivacy"])
summary(luca1[[1]])
lapply(luca1,class.pred)
# luca1o2 = model.listZ2(pred = hluca.dat1, resp = dfu["q7.1_editedprivacy"])
# summary(luca1o2[[1]])
# lapply(luca1o2,class.pred)


luca2 = model.listZ(pred = hluca.dat2, resp = dfu["q2.1_nocare"])
summary(luca2[[1]])
lapply(luca2,class.pred)
# luca2o2 = model.listZ2(pred = hluca.dat2, resp = dfu["q2.1_nocare"],trace=1)
# summary(luca2o2[[1]])
# lapply(luca2o2,class.pred)


# W demo data
hluca.dat1demo = cbind(hluca.dat1,demo.dat)
hluca.dat2demo = cbind(hluca.dat2,demo.dat)

luca3 = model.listZ(pred = hluca.dat2demo, resp = dfu["q2.1_nocare"])
summary(luca3[[1]])
lapply(luca3,class.pred)
# luca3o2 = model.listZ2(pred = hluca.dat2demo, resp = dfu["q2.1_nocare"])
# summary(luca3o2[[1]])
# lapply(luca3o2,class.pred)

luca4 = model.listZ(pred = hluca.dat1demo, resp = dfu["q7.1_editedprivacy"])
lapply(luca4,class.pred)
# luca4o2 = model.listZ2(pred = hluca.dat1demo, resp = dfu["q7.1_editedprivacy"],trace=1)



# Set of models -----------------------------------------------------------
# 
# mnames = paste0("luca",1:4,c("","o2"))
# # remove named lists
# mlist1 = lapply(mnames,function(x) eval(parse(text=x)))
# mnames = rep(mnames,unlist(lapply(mlist1,length)))
# mlist1 = break.list(mlist1)
# names(mlist1) = mnames
# lapply(mlist1,function(x) as.character(x$formula)[2])
# names(mlist1) = paste0(names(mlist1)," ",unlist(lapply(mlist1,function(x) as.character(x$formula)[2])))
# mlist1[lapply(mlist1,function(x) x$convergence$code) > 1] = NULL
# lapply(mlist1,summary)
# lapply(mlist1,function(x)x$beta)
# lapply(mlist1,function(x) x$convergence$code)
