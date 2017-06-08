# Hluca3


# Frequency ---------------------------------------------------------------

# Usage frequency
hluca3.dat1 = dfo[,c("q1.1_usefb","q1.2_useig","q1.3_usesc","q5.1_digedu")]
hluca3.dat2 = dfo[,c("q8.1_freqpost","q8.2_freqsend","q8.3_freqread","q5.1_digedu")]

nluca1 = model.listZ(pred = hluca3.dat1, resp = dfo[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca1,class.pred)
lapply(nluca1,summary)
nluca2 = model.listZ(pred = hluca3.dat2, resp = dfo[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca2,class.pred)
lapply(nluca2,summary)

# Trust
hluca3.dat3 = dfo[,c("q14.1_trustconn","q15.1_trustsell","q16.1_trustpriv","q5.1_digedu","platform")]
hluca3.dat3demo = cbind(hluca3.dat3,demo.dat)

nluca3 = model.listZ(pred = hluca3.dat3, resp = dfo[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca3,class.pred)
lapply(nluca3,summary)
# nluca3o2 = model.listZ2(pred = hluca3.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
# lapply(nluca3o2,class.pred)
nluca4 = model.listZ(pred = hluca3.dat3demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca4,class.pred)


# Audience vs trust audience
hluca3.dat4 = dfu[,c("q11.1_seemypost","q13.1_audience","q5.1_digedu","platform")]
hluca3.dat4demo = cbind(hluca3.dat4,demo.dat)

nluca5 = model.listZ(pred = hluca3.dat4, resp = dfu["q14.1_trustconn"])
lapply(nluca5,class.pred)
nluca5aic = model.listZ(pred = dfo[,-19], resp = dfo["q14.1_trustconn"],link="loglog", threshold = "symmetric")
lapply(nluca5aic,class.pred)
nluca6 = model.listZ(pred = hluca3.dat4demo, resp = dfu["q14.1_trustconn"])
lapply(nluca6,class.pred)

# Little hypocrits

hypocrits1 = model.listZ(pred = dfu["q18.1_targetfr"], resp = dfu["q14.1_trustconn"],link="cauchit")
lapply(hypocrits1,class.pred)


