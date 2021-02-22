data=read.csv("/Users/zhang/Desktop/courses/else/2019 jsm/data720.csv",header=T)
datast=read.csv("/Users/zhang/Desktop/courses/else/2019 jsm/datast.csv",header=T)
datast=cbind(data,datast[,3:4])
s=5e-6
len=8902
id=1:104564
ing=c(10,18,10,14,3)
for(j in 1:ing[1]){
  id[which(datast$Borough==1 & datast$Sub_Borough==j)]=j
}
for(j in 1:ing[2]){
  id[which(datast$Borough==2 & datast$Sub_Borough==j)]=10+j
}
for(j in 1:ing[3]){
  id[which(datast$Borough==3 & datast$Sub_Borough==j)]=28+j
}
for(j in 1:ing[4]){
  id[which(datast$Borough==4 & datast$Sub_Borough==j)]=38+j
}
for(j in 1:ing[5]){
  id[which(datast$Borough==5 & datast$Sub_Borough==j)]=52+j
}
datast=cbind.data.frame(id,datast)

library(dplyr)

a <- datast %>% 
  group_by(Year,Borough,Sub_Borough) %>%
  summarise_all(mean)

lat=a$lat
long=a$long
x=(lat-mean(lat)+0.2)*10
y=(long-mean(long)+0.2)*10

ID=factor(rep(1:55,10))
a=data.frame(ID,a)
rent=matrix(a$Rent,10,55)
for(i in 1:10)
  for(j in 1:55)
    rent[i,j]=a$Rent[(i-1)*55+j]
colnames(rent)=unique(a$ID)
rownames(rent)=unique(a$date)
names(a)[names(a)=="Year"]="date"
names(a)[names(a)=="Rent"]="obs"
obs=rent
colnames(obs)=unique(a$ID)
rownames(obs)=unique(a$date)
covars=cbind.data.frame(a$ID,x,y,lat,long)[1:55,]
colnames(covars)=c("ID","x","y","lat","long")

obs=(obs-mean(obs))/sd(obs)

datanew=createSTdata(obs=obs,covars=covars)

D <- createDataMatrix(datanew)
SVD.cv <- SVDsmoothCV(D, 1:5)
print(SVD.cv)
plot(SVD.cv)

plot(SVD.cv, pairs = TRUE)

datanew <- updateTrend(datanew, n.basis = 4)
beta <- estimateBetaFields(datanew)
LUR <- list(~x+y+lat+long,
            ~x+y,~x+y,~x+y,~x+y)

cov.beta <- list(covf = "exp")
cov.nu <- list(covf = "exp",random.effect = FALSE)

locations <- list(coords = c("x","y"),long.lat=c("long","lat"))
model <- createSTmodel(datanew, LUR=NULL,locations=locations)
print(model)      
model.dim <- loglikeSTdim(model)
x.init <-c(rep(0,model.dim$nparam.cov-1),0)
est.model <- estimate(model,x.init,type="p")

x <- coef(est.model)$par
time=2020
datatest=updateTrend(datanew,n.basis = 4,extra.dates = "2020")
modeltest=createSTmodel(datatest,LUR=NULL,locations=locations)
predtest=predict(modeltest,x)
predrent=predtest$EX*sd(datast$Rent)+mean(datast$Rent)

rent1=1:104564
year=unique(datast$Year)
for(i in 1:10)
  for(j in 1:55){
    rent1[which(datast$Year==year[i] & datast$id==j)]=predrent[i,j]
  }
datast=cbind(datast,rent1)
re2=lm(Rent~Number_of_Persons+Length_of_Lease+index+rent1,data=datast)
summary(re2)
