hh=read.csv("hair.csv")
summary(hh)
str(hh)
hh=hh[,-1]
dim(hh)

library(DataExplorer)
plot_histogram(hh)
#Boxplots of all numerical variables- Uni-Variate
library(RColorBrewer)
boxplot(hh[,1:11],
        las=1,
        horizontal = TRUE,
        cex= 0.6,
        par(cex.axis = 0.7),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots of DV")

#Correlation
library(corrplot)
corrplot(cor(hh),type="upper",method="number")

##Linear Reg
ll=lm(Satisfaction~.,data = hh)
summary(ll)

step(ll,direction = "both")

ll1=lm(formula = Satisfaction ~ ProdQual + Ecom + CompRes + ProdLine + 
     SalesFImage + OrdBilling, data = hh)
summary(ll1)

library(car)
vif(ll1)
vif(ll)

##outliers
cd=cooks.distance(ll)
which(cd>4*mean(cd))

hh1=hh[-c(2,20,45,52,59,71),]
dim(hh1)

###Run model with new data
ll2=lm(Satisfaction~.,data=hh1)
step(ll2,direction = "both")

ll3=lm(formula = Satisfaction ~ ProdQual + Ecom + CompRes + ProdLine + 
     SalesFImage + DelSpeed, data = hh1)
summary(ll3)
vif(ll3)
##Check for Heteroscedasticity
library(lmtest)
bptest(ll3)

#check for normality of residuals
install.packages('olsrr')
library(olsrr)
ols_plot_resid_qq(ll3)
ols_plot_resid_hist(ll3)
ols_plot_resid_fit(ll3)

##PCA 
library(psych)
cormatrix=cor(hh1)
cortest.bartlett(cormatrix)
KMO(cormatrix)

h1=hh1[,-12]
str(h1)
#eigen value
ev=eigen(cor(h1))
eigenval=ev$values

#Scree plot
fact=c(1,2,3,4,5,6,7,8,9,10,11)
scree=data.frame(fact,eigenval)
plot(scree,main="scree plot",col="Blue")
lines(scree,col="Red")

#Factor Loadings kaiser_mayer rule
unrotate=principal(h1,nfactors = 4,rotate="none")
print(unrotate,digits=3)

#Orthogonal Rotation of Factor loadings Using Varimax
rotate=principal(h1,nfactors = 4,rotate="varimax")
print(rotate,digits=3)
loadings=loadings(rotate)
print(loadings,digits=3,cutoff=0.4,sort=T)
fa.diagram(rotate)

#Naming the Factors as Efficiency, Marketing, Support & Product
y<-rotate$scores
h2=cbind(hh1,y)
head(h2)
dim(h2)
h3<-h2[,c(12:16)]
names(h3)<-c("CSAT","Efficiency","Marketing","Support","Product")
head(h3)

###Linear reg
l5=lm(CSAT~.,data=h3)
summary(l5)
step(l5,direction = "both")

l6=lm(formula = CSAT ~ Efficiency + Marketing + Product, data = h3)
summary(l6)

vif(l5)

#Backtracking the model
prediction<-predict(l6)
actual<-h3$CSAT
plot(actual,col="Red",xlab="data points")
lines(actual,col="Red")
plot(prediction,col="Blue")
lines(prediction,col="Blue")
lines(actual,col="Red")

library(caTools)
set.seed(12)
spl=sample.split(h3,SplitRatio = 0.7)
tr=subset(h3,spl==T)
ts=subset(h3,spl==F)

f=lm(CSAT~Efficiency + Marketing + Product,data=tr)
summary(f)
coef(f)
confint(f)

ts$pred=predict(f,newdata=ts,type="response")
library(Metrics)
rmse(ts$CSAT,ts$pred)
mse(ts$CSAT,ts$pred)
