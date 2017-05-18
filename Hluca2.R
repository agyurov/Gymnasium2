# Q2, Q7 vs knowledge
# w/o demo data
hluca.dat1 = dfu[,c("k1_","q5.1_digedu","platform")]
hluca.dat2 = dfu[,c("k2_","q5.1_digedu","platform")]
hluca.dat3 = dfu[,c("q13.1_audience","q5.1_digedu","platform")]

# Models for k1
mluca1 = model.listZ(pred = hluca.dat1, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca1,summary) # nothing
mluca1o2 = model.listZ2(pred = hluca.dat1, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca1o2,summary) # nothing

# Models for k2
mluca2 = model.listZ(pred = hluca.dat2, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca2,summary) # only for q7
mluca2o2 = model.listZ2(pred = hluca.dat2, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca2o2,summary) # only for q7

# Models for Q13
mluca3 = model.listZ(pred = hluca.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca3,summary) # only for q7
mluca3o2 = model.listZ2(pred = hluca.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca3o2,summary) # only for q7

# w demo data
hluca.dat1.demo = cbind(hluca.dat1,demo.dat)
hluca.dat2.demo = cbind(hluca.dat2,demo.dat)
hluca.dat3.demo = cbind(hluca.dat3,demo.dat)

# Models for k1
mluca4 = model.listZ(pred = hluca.dat1.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca4,summary) # nothing
mluca4o2 = model.listZ2(pred = hluca.dat1.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca4o2,summary) # nothing

# Models for k2
mluca5 = model.listZ(pred = hluca.dat2.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca5,summary) # only for q7
mluca5o2 = model.listZ2(pred = hluca.dat2.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca5o2,summary) # only for q7

# Models for Q13
mluca6 = model.listZ(pred = hluca.dat3.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca6,summary) # only for q7
mluca6o2 = model.listZ2(pred = hluca.dat3.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca6o2,summary) # only for q7
