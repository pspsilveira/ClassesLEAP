layout(matrix(1:6,nrow=2,ncol=3))
x <- c(1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6,7,7,7,7,8,8,9,9,10,10,11,11)
hist(x,main="A",breaks=seq( 0,12,1 ),freq=FALSE,ylim=c(0,0.25),axes=FALSE)
axis(2,las=1)
axis(1,at=seq( 0,12,1 ),las=2)
hist(x,main="D",breaks=seq(-2.0,12,2.0),freq=FALSE,ylim=c(0,0.25),axes=FALSE)
axis(2,las=1)
axis(1,at=seq(-2.0,12,2.0),las=2)
hist(x,main="B",breaks=seq(-1.5,12,1.5),freq=FALSE,ylim=c(0,0.25),axes=FALSE)
axis(2,las=1)
axis(1,at=seq(-1.5,12,1.5),las=2)
hist(x,main="E",breaks=seq(-2.0,12,1.9),freq=FALSE,ylim=c(0,0.25),axes=FALSE)
axis(2,las=1)
axis(1,at=seq(-2.0,12,1.9),las=2)
hist(x,main="C",breaks=seq(-0.5,12,1.5),freq=FALSE,ylim=c(0,0.25),axes=FALSE)
axis(2,las=1)
axis(1,at=seq(-0.5,12,1.5),las=2)
plot(density(x),main="F",xlab="x",axes=FALSE)
axis(2,las=1)
axis(1,at=seq(0,12,2),las=2)
par(mfrow=c(1,1))