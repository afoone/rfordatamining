require(rpart);
require(rpart.plot);
require(car);
require(caret);

wdbc=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",", header=FALSE);

dim(wdbc);

wdbc$V7=as.numeric(recode(wdbc$V7, "'?'=NA"));

x=wdbc[complete.cases(wdbc),];

dim(x);

# train (2/3), test (1/3)
N=dim(x)[1];

all=seq(1,N);

# select 2/3 randomly
train=sort(sample(N,N*2/3.0));
test=setdiff(all,train);

xtrain=x[train,];
xtest=x[test,];

# create first tree using default parameters
t1=rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10, data=xtrain, method="class");

prp(t1, extra=1);

y1train=predict(t1, xtrain, type="class");
y1test=predict(t1, xtest, type="class");

# analyze how good is t1 as a classifier
confusionMatrix(table(xtrain$V11,y1train));
confusionMatrix(table(xtest$V11,y1test));

# compute PCA for the training set
pca=princomp(xtrain[,2:10]);

summary(pca);
pca$loadings[,1];

# add a new variable with the result of PCA
xtrain$C1=pca$scores[,1];
xtest$C1=predict(pca,xtest)[,1];

# create a new tree t2 using this variable
t2=rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1, data=xtrain, method="class");

prp(t2, extra=1);

y2train=predict(t2, xtrain, type="class");
y2test=predict(t2, xtest, type="class");

confusionMatrix(table(xtrain$V11,y2train))
confusionMatrix(table(xtest$V11,y2test))

# force a complete (pure) tree t3
t3=rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1, 
         data=xtrain, method="class", 
         control=rpart.control(minsplit=0, cp=0.0));

prp(t3, extra=1);

y3train=predict(t3, xtrain, type="class");
y3test=predict(t3, xtest, type="class");

confusionMatrix(table(xtrain$V11,y3train))
confusionMatrix(table(xtest$V11,y3test))
