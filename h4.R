# Analysis 1



# chisq test high p-value = different [can not reject H0 of independence]
# chisq test low p-value = [Reject H0 of independence]
# chisq.test LOW P-Value => dependent
# fisher.test = chisq.test 
# t.test high p-value = same mean


# H4 ----------------------------------------------------------------------
# H4a: Frequency between platforms
par(mfrow=c(3,1))
plot(dfu$q1.1_usefb)
plot(dfu$q1.2_useig)
plot(dfu$q1.3_usesc)

# T-test: no population variance, >>30 sample size. Usage frequency differs...

# Overall
t.test(dfnum$q1.1_usefb, dfnum$q1.2_useig, paired=T)
t.test(dfnum$q1.1_usefb, dfnum$q1.3_usesc, paired=T)
t.test(dfnum$q1.2_useig, dfnum$q1.3_usesc, paired=T)

# Before edu
t.test(dfnum$q1.1_usefb[dfnum$q5.1_digedu == "nej"], dfnum$q1.2_useig[dfnum$q5.1_digedu == "nej"], paired=T)
t.test(dfnum$q1.1_usefb[dfnum$q5.1_digedu == "nej"], dfnum$q1.3_usesc[dfnum$q5.1_digedu == "nej"], paired=T)
t.test(dfnum$q1.2_useig[dfnum$q5.1_digedu == "nej"], dfnum$q1.3_usesc[dfnum$q5.1_digedu == "nej"], paired=T)

# After edu
t.test(dfnum$q1.1_usefb[dfnum$q5.1_digedu == "ja"], dfnum$q1.2_useig[dfnum$q5.1_digedu == "ja"], paired=T)
t.test(dfnum$q1.1_usefb[dfnum$q5.1_digedu == "ja"], dfnum$q1.3_usesc[dfnum$q5.1_digedu == "ja"], paired=T)
t.test(dfnum$q1.2_useig[dfnum$q5.1_digedu == "ja"], dfnum$q1.3_usesc[dfnum$q5.1_digedu == "ja"], paired=T)
# All t-tests indicate the same mean value across platform usage


# H4b: q8.1_freqpost ------------------------------------------------------
# Between platforms
# Before
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="fb"],dfnumb$q8.1_freqpost[dfnumb$platform=="ig"]) # diff
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="fb"],dfnumb$q8.1_freqpost[dfnumb$platform=="sc"]) # diff
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="ig"],dfnumb$q8.1_freqpost[dfnumb$platform=="sc"]) # diff

# After
t.test(dfnuma$q8.1_freqpost[dfnuma$platform=="fb"],dfnuma$q8.1_freqpost[dfnuma$platform=="ig"]) # diff
t.test(dfnuma$q8.1_freqpost[dfnuma$platform=="fb"],dfnuma$q8.1_freqpost[dfnuma$platform=="sc"]) # diff
t.test(dfnuma$q8.1_freqpost[dfnuma$platform=="ig"],dfnuma$q8.1_freqpost[dfnuma$platform=="sc"]) # same!

# observe snapchat usage decreases enough to make the difference in usage insignificant
lapply(list(ig=dfnumb$q8.1_freqpost[dfnumb$platform=="ig"],sc=dfnumb$q8.1_freqpost[dfnumb$platform=="sc"]),mean)
lapply(list(ig=dfnuma$q8.1_freqpost[dfnuma$platform=="ig"],sc=dfnuma$q8.1_freqpost[dfnuma$platform=="sc"]) ,mean)

# Within platforms
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="fb"],dfnuma$q8.1_freqpost[dfnuma$platform=="fb"]) # same
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="ig"],dfnuma$q8.1_freqpost[dfnuma$platform=="ig"]) # same
t.test(dfnumb$q8.1_freqpost[dfnumb$platform=="sc"],dfnuma$q8.1_freqpost[dfnuma$platform=="sc"]) # same


# H4b: q8.2_freqsend ------------------------------------------------------

# Before
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="fb"],dfnumb$q8.2_freqsend[dfnumb$platform=="ig"]) # diff
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="fb"],dfnumb$q8.2_freqsend[dfnumb$platform=="sc"]) # same!
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="ig"],dfnumb$q8.2_freqsend[dfnumb$platform=="sc"]) # diff

# After
t.test(dfnuma$q8.2_freqsend[dfnuma$platform=="fb"],dfnuma$q8.2_freqsend[dfnuma$platform=="ig"]) # diff
t.test(dfnuma$q8.2_freqsend[dfnuma$platform=="fb"],dfnuma$q8.2_freqsend[dfnuma$platform=="sc"]) # still same!
t.test(dfnuma$q8.2_freqsend[dfnuma$platform=="ig"],dfnuma$q8.2_freqsend[dfnuma$platform=="sc"]) # diff

# Within platforms
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="fb"],dfnuma$q8.2_freqsend[dfnuma$platform=="fb"]) # same
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="ig"],dfnuma$q8.2_freqsend[dfnuma$platform=="ig"]) # same
t.test(dfnumb$q8.2_freqsend[dfnumb$platform=="sc"],dfnuma$q8.2_freqsend[dfnuma$platform=="sc"]) # same


# H4b: q8.3_freqread ------------------------------------------------------

# Before
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="fb"],dfnumb$q8.3_freqread[dfnumb$platform=="ig"]) # diff
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="fb"],dfnumb$q8.3_freqread[dfnumb$platform=="sc"]) # same!
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="ig"],dfnumb$q8.3_freqread[dfnumb$platform=="sc"]) # same!

# After
t.test(dfnuma$q8.3_freqread[dfnuma$platform=="fb"],dfnuma$q8.3_freqread[dfnuma$platform=="ig"]) # same!
t.test(dfnuma$q8.3_freqread[dfnuma$platform=="fb"],dfnuma$q8.3_freqread[dfnuma$platform=="sc"]) # still same!
t.test(dfnuma$q8.3_freqread[dfnuma$platform=="ig"],dfnuma$q8.3_freqread[dfnuma$platform=="sc"]) # still same!

# observe a reduction in fb usage and a simultaneous increase in ig usage
lapply(list(fb=dfnumb$q8.3_freqread[dfnumb$platform=="fb"],ig=dfnumb$q8.3_freqread[dfnumb$platform=="ig"]),
       mean,na.rm=T)
lapply(list(fb=dfnuma$q8.3_freqread[dfnuma$platform=="fb"],ig=dfnuma$q8.3_freqread[dfnuma$platform=="ig"]),
       mean,na.rm=T)

# Within platforms
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="fb"],dfnuma$q8.3_freqread[dfnuma$platform=="fb"]) # same
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="ig"],dfnuma$q8.3_freqread[dfnuma$platform=="ig"]) # same
t.test(dfnumb$q8.3_freqread[dfnumb$platform=="sc"],dfnuma$q8.3_freqread[dfnuma$platform=="sc"]) # same
