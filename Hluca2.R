# Q2, Q7 vs knowledge
# w/o demo data
hluca.dat1 = dfu[,c("k1_","q5.1_digedu","platform")]
hluca.dat2 = dfu[,c("k2_","q5.1_digedu","platform")]
hluca.dat3 = dfu[,c("q13.1_audience","q5.1_digedu","platform")]

# Models for k1
mluca1 = model.listZ(pred = hluca.dat1, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca1,summary) # nothing

# Models for k2
mluca2 = model.listZ(pred = hluca.dat2, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca2,summary) # only for q7
coefz2 = mluca2$q7.1_editedprivacy$beta
(coefz2 = sort(coefz2,decreasing = T))
my.barplot(coefz2,namez = names(coefz2),col=3:4)

# Models for Q13
mluca3 = model.listZ(pred = hluca.dat3, resp = dfu[,c("q2.1_nocare","q7.1_editedprivacy")])
lapply(mluca3,summary) # only for q7
coefz3 = mluca3$q7.1_editedprivacy$beta
(coefz3 = sort(coefz3,decreasing = T))


# w demo data

