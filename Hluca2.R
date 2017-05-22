# Q2, Q7 vs knowledge


# K1 & K2 -----------------------------------------------------------------

# w/o demo data
hluca.dat1 = dfu[,c("k1_","q5.1_digedu","platform")]
hluca.dat2 = dfu[,c("k2_","q5.1_digedu","platform")]

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

# w demo data
hluca.dat1.demo = cbind(hluca.dat1,demo.dat)
hluca.dat2.demo = cbind(hluca.dat2,demo.dat)

# Models for k1
mluca4 = model.listZ(pred = hluca.dat1.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca4,summary) # nothing
mluca4o2 = model.listZ2(pred = hluca.dat1.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca4o2,summary) # nothing

# Models for k2
mluca5 = model.listZ(pred = hluca.dat2.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca5,summary) # only for q7
mluca5o2 = model.listZ2(pred = hluca.dat2.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca5o2,summary) # only for q7


# K3 & K4 ----------------------------------------------------------------

# w/o demo data
hluca.dat1k3 = dfu[,c("k3_","q5.1_digedu","platform")]
hluca.dat2k4 = dfu[,c("k4_","q5.1_digedu","platform")]

# Models for k3
mluca6 = model.listZ(pred = hluca.dat1k3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca6,summary) # nothing
mluca6o2 = model.listZ2(pred = hluca.dat1k3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca6o2,summary) # nothing

# Models for k4
mluca7 = model.listZ(pred = hluca.dat2k4, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca7,summary) # only for q7
mluca7o2 = model.listZ2(pred = hluca.dat2k4, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca7o2,summary) # only for q7

# w demo data
hluca.dat1k3.demo = cbind(hluca.dat1k3,demo.dat)
hluca.dat2k4.demo = cbind(hluca.dat2k4,demo.dat)

# Models for k3
mluca8 = model.listZ(pred = hluca.dat1k3.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca8,summary) # nothing
mluca8o2 = model.listZ2(pred = hluca.dat1k3.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca8o2,summary) # nothing

# Models for k4
mluca9 = model.listZ(pred = hluca.dat2k4.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca9,summary) # only for q7
mluca9o2 = model.listZ2(pred = hluca.dat2k4.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca9o2,summary) # onlywa for q7

# K5 & K6 -----------------------------------------------------------------

# w/o demo data
hluca.dat1k5 = dfu[,c("k3_","q5.1_digedu","platform")]
hluca.dat2k6 = dfu[,c("k4_","q5.1_digedu","platform")]

# Models for k3
mluca10 = model.listZ(pred = hluca.dat1k5, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca10,summary) # nothing
mluca10o2 = model.listZ2(pred = hluca.dat1k5, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca10o2,summary) # nothing

# Models for k4
mluca11 = model.listZ(pred = hluca.dat2k6, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca11,summary) # only for q7
mluca11o2 = model.listZ2(pred = hluca.dat2k6, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca11o2,summary) # only for q7

# w demo data
hluca.dat1k5.demo = cbind(hluca.dat1k5,demo.dat)
hluca.dat2k6.demo = cbind(hluca.dat2k6,demo.dat)

# Models for k3
mluca12 = model.listZ(pred = hluca.dat1k5.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca12,summary) # nothing
mluca12o2 = model.listZ2(pred = hluca.dat1k5.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca12o2,summary) # nothing

# Models for k4
mluca13 = model.listZ(pred = hluca.dat2k6.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca13,summary) # only for q7
mluca13o2 = model.listZ2(pred = hluca.dat2k6.demo, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")],exclude.warnings = F)
lapply(mluca13o2,summary) # onlywa for q7



# Set of models -----------------------------------------------------------

mnames = paste0("mluca",c(1,2,4,6,7,8,9,10,11,12,13),c("","o2"))

# remove named lists
m.list = lapply(mnames,function(x) eval(parse(text=x)))
m.list = break.list(m.list)
lapply(m.list,function(x) as.character(x$formula)[2])
names(m.list) = unlist(lapply(m.list,function(x) as.character(x$formula)[2]))

# Chisq.tests for the knowledge measures ----------------------------------

chisq.test(dfu$k1_,dfu$k2_)
chisq.test(dfu$k1_,dfu$k3_)
chisq.test(dfu$k1_,dfu$k4_)
chisq.test(dfu$k1_,dfu$k5_)
chisq.test(dfu$k1_,dfu$k6_)

chisq.test(dfu$k2_,dfu$k3_)
chisq.test(dfu$k2_,dfu$k4_)
chisq.test(dfu$k2_,dfu$k5_)
chisq.test(dfu$k2_,dfu$k6_)

chisq.test(dfu$k3_,dfu$k4_)
chisq.test(dfu$k3_,dfu$k5_)
chisq.test(dfu$k3_,dfu$k6_)

chisq.test(dfu$k4_,dfu$k5_)
chisq.test(dfu$k4_,dfu$k6_)

chisq.test(dfu$k5_,dfu$k6_)

