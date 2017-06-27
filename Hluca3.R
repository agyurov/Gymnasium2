


par(mfrow=c(3,3),mar=c(4,4,6,.5))
barplot(table(a), col=grey.colors(1,.9,.3),ylab="Unconditional",xpd=NA)
fkit(a,cex=2)
barplot(table(b), col=grey.colors(1,.9,.3))
fkit(b)
barplot(table(c), col=grey.colors(1,.9,.3))
fkit(c)

barplot(rbindme(am,af),beside=T,col=rainbow(2,start=.55,end=0,alpha = .3), ylab="Gender")
fkit(am,af)
barplot(rbindme(bm,bf),beside=T,col=rainbow(2,start=.55,end=0,alpha = .3))
fkit(bm,bf)
barplot(rbindme(cm,cf),beside=T,col=rainbow(2,start=.55,end=0,alpha = .3))
legend("right",c("M", "F"), fill=rainbow(2,start=.55,end=0,alpha = .3),bty="n",horiz=F,xpd=NA)

barplot(rbindme(aja,anej),beside=T,col=grey.colors(2,.9,.3), ylab="Dig. education", xlab = "PR")
barplot(rbindme(bja,bnej),beside=T,col=grey.colors(2,.9,.3), xlab = "TR")
barplot(rbindme(cja,cnej),beside=T,col=grey.colors(2,.9,.3), xlab = "ACP")
legend("right",c("Ja", "Nej"), fill=grey.colors(2,.9,.3),bty="n",horiz=F,xpd=NA)

fkit = function(x,y=NULL,cex=2,col=3:4,...){
  horiz = par("usr")[2]
  vert = par("usr")[4]
  m1 = round(mean(x),2)
  sd1 = round(sd(x),2)
  t1 = bquote(mu~.(m1)~sigma~.(sd1))
  if(is.null(y)){
    text(x = horiz/2, y = vert, labels = t1, xpd = NA, cex = cex, col = col[1], ...)
    return()
  }
  m2 = round(mean(y),2)
  sd2 = round(sd(y),2)
  t2 = bquote(mu~.(m2)~sigma~.(sd2))
  text(x=horiz/4, y = vert, labels = t1, col=col[1], xpd=NA, cex=cex,...)
  text(x=3/4*horiz, y = vert, labels = t2, xpd = NA, cex = cex, col = col[2], ...)
}





