# Hluca3


# Frequency ---------------------------------------------------------------

# Usage frequency
hluca3.dat1 = dfu[,c("q1.1_usefb","q1.2_useig","q1.3_usesc","q5.1_digedu","platform")]
hluca3.dat2 = dfu[,c("q8.1_freqpost","q8.2_freqsend","q8.3_freqread","q5.1_digedu","platform")]

nluca1 = model.listZ(pred = hluca3.dat1, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
nluca2 = model.listZ(pred = hluca3.dat2, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])


# Trust
hluca3.dat3 = dfu[,c("q14.1_trustconn","q15.1_trustsell","q16.1_trustpriv","q5.1_digedu","platform")]
hluca3.dat3demo = cbind(hluca3.dat3,demo.dat)

nluca3 = model.listZ(pred = hluca3.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca3,summary)
nluca3o2 = model.listZ2(pred = hluca3.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca3o2,summary)
nluca4 = model.listZ(pred = hluca3.dat3demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(nluca4,summary)


# Audience vs trust audience
hluca3.dat4 = dfu[,c("q11.1_seemypost","q13.1_audience","q5.1_digedu","platform")]
hluca3.dat4demo = cbind(hluca3.dat4,demo.dat)

nluca5 = model.listZ(pred = hluca3.dat4, resp = dfu["q14.1_trustconn"])
lapply(nluca5,summary)
nluca6 = model.listZ(pred = hluca3.dat4demo, resp = dfu["q14.1_trustconn"])
lapply(nluca6,summary)
class.pred(nluca5$q14.1_trustconn)

# Little hypocrits

hypocrits1 = model.listZ(pred = dfu["q18.1_targetfr"], resp = dfu["q14.1_trustconn"])
lapply(hypocrits1,summary)


