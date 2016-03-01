#Gaussian data generation
NSamples <- 200;
Samples1 <- rnorm(NSamples);
Samples2 <- 5*rnorm(NSamples);
data <- matrix(c(Samples1,Samples2),NSamples)
plot(data[,1],data[,2],xlim=c(-15, 15), ylim=c(-15, 15),col="red");
box(lwd=2);

#Computation of the  PC
pc <- princomp(data)
summary(pc)
vec <- pc$loadings
val <- pc$sdev
arrows(0,0,val[1]*vec[1,1],val[1]*vec[2,1],col="black",lwd=2,length=0.1)
arrows(0,0,val[2]*vec[1,2],val[1]*vec[2,2],col="blue",lwd=2,length=0.1)

#2D Example in R
X <- c(2.5,0.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
Y <- c(2.4,0.7,2.9,2.2,3,2.7,1.6,1.1,1.6,0.9)

#mean of the data
mX <- mean(X)
mY <- mean(Y)

#Removing the mean
Data <- cbind(X-mX,Y-mY)

#Computing the covariance matrix
covData <- (1/10)*t(Data)%*%Data

#Eigenvectors of the covariance matrix
pcas <- eigen(covData)
pcas$vectors

#High dimensional example
dataCancer <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",");

#More available data sets...
#http://archive.ics.uci.edu/ml/

summary(dataCancer)

#take the labels
labCancer <- dataCancer[,2]
dataCancer <- dataCancer[,3:32]
pcaCancer <- princomp(dataCancer)
plot(cumsum(pcaCancer$sdev)/sum(pcaCancer$sdev))

#Look at the two first PCs and plot the resulting data
scoresCancer <- pcaCancer$scores
plot(scoresCancer[,1],scoresCancer[,2])

#We can use PCA for visualization: f.e. depicting the labels on top of the 2D plot
plot(scoresCancer[labCancer=="M",1],scoresCancer[labCancer=="M",2],col="red")
points(scoresCancer[labCancer=="B",1],scoresCancer[labCancer=="B",2],col="green")
