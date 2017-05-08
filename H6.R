# H6
# chisq test high p-value = there is a relatoin
# chisq test low p-value = there is no relatoin


# Before
# chisq
h6b1 = gym.test(fbb$q13.1_audience,fbb$q14.1_trustconn)
h6b2 = gym.test(igb$q13.1_audience,igb$q14.1_trustconn)
h6b3 = gym.test(scb$q13.1_audience,scb$q14.1_trustconn)
# correlation
cor(fbnumb$q13.1_audience,fbnumb$q14.1_trustconn)
cor(ignumb$q13.1_audience,ignumb$q14.1_trustconn)
cor(scnumb$q13.1_audience,scnumb$q14.1_trustconn)

# After 
h6a1 = gym.test(fba$q13.1_audience,fba$q14.1_trustconn)
h6a2 = gym.test(iga$q13.1_audience,iga$q14.1_trustconn)
h6a3 = gym.test(sca$q13.1_audience,sca$q14.1_trustconn)
# correlation
cor(fbnuma$q13.1_audience,fbnuma$q14.1_trustconn)
cor(ignuma$q13.1_audience,ignuma$q14.1_trustconn)
cor(scnuma$q13.1_audience,scnuma$q14.1_trustconn)

