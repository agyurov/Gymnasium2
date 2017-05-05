# H6
# chisq test high p-value = there is a relatoin
# chisq test low p-value = there is no relatoin


# x vs y
h6b1 = gym.test(fbb$q13.1_audience,fbb$q14.1_trustconn)
h6b2 = gym.test(igb$q13.1_audience,igb$q14.1_trustconn)
h6b3 = gym.test(scb$q13.1_audience,scb$q14.1_trustconn)

h6b4 = gym.test(fbnumb$q13.1_audience,fbnumb$q14.1_trustconn)
h6b5 = gym.test(ignumb$q13.1_audience,ignumb$q14.1_trustconn)
h6b6 = gym.test(scnumb$q13.1_audience,scnumb$q14.1_trustconn)

# After education
h6a1 = gym.test(fba$q13.1_audience,fba$q14.1_trustconn)
h6a2 = gym.test(iga$q13.1_audience,iga$q14.1_trustconn)
h6a3 = gym.test(sca$q13.1_audience,sca$q14.1_trustconn)

h6a4 = gym.test(fbnuma$q13.1_audience,fbnuma$q14.1_trustconn)
h6a5 = gym.test(ignuma$q13.1_audience,ignuma$q14.1_trustconn)
h6a6 = gym.test(scnuma$q13.1_audience,scnuma$q14.1_trustconn)

# x vs x
# Before
(h6btotal = round(chisq.test(dfo$q13.1_audience,dfo$q14.1_trustconn)$p.value,3))
a=table(fbb$q13.1_audience)
b=table(fba$q13.1_audience)
c = rbind(a,b)



a = c(10,10,20,15)
b = c(70,5,2,10)
c = c(12,11,18,16)
d = c(20,20,30,16)
e = c(112,11,22,15)


chisq.test(cbind(a,b))
chisq.test(rbind(a,c))
chisq.test(cbind(a,d),simulate.p.value = T)
chisq.test(a,e)
chisq.test(rbind(a,e))

fisher.test(a,b)
fisher.test(rbind(a,b))

fisher.test(a,c)
fisher.test(rbind(a,c))
fisher.test(rbind(a,d))
