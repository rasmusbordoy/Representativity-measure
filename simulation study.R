#############################################
# Data frame with 2 covariates where the 
# first has two groups (male, female) and 
# the second covariates has three groups
# (rural, urban and suburban)
#############################################

df <- data.frame(matrix(0,nrow=500,ncol=6))

colnames(df) <- c("Response","Male","Female","Rural","Suburban","Urban")

df[,2] <- rbinom(500,1,0.5) #Male
df[,3] <- ifelse(df[,2] == 0,1,0) #Female
df[,4] <- rbinom(500,1,0.2) #rural
df[which(df[,4]==0),5] <- rbinom(length(which(df[,4]==0)),1,0.2) #suburban
df[which(df[,4]==0 & df[,5]==0),6] <- 1

df$Response <- 0
df[which(df$Male == 1 & df$Rural == 1),1] <-        rbinom(length(which(df$Male == 1 & df$Rural == 1)),1,0.45)
df[which(df$Male == 1 & df$Suburban == 1),1] <-     rbinom(length(which(df$Male == 1 & df$Suburban == 1)),1,0.5)
df[which(df$Male == 1 & df$Urban == 1),1] <-        rbinom(length(which(df$Male == 1 & df$Urban == 1)),1,0.3)
df[which(df$Female == 1 & df$Rural == 1),1] <-      rbinom(length(which(df$Female == 1 & df$Rural == 1)),1,0.5)
df[which(df$Female == 1 & df$Suburban == 1),1] <-   rbinom(length(which(df$Female == 1 & df$Suburban == 1)),1,0.55)
df[which(df$Female == 1 & df$Urban == 1),1] <-      rbinom(length(which(df$Female == 1 & df$Urban == 1)),1,0.4)


df$Response <- 0
df[which(df$Male == 1 & df$Rural == 1),1] <-        rbinom(length(which(df$Male == 1 & df$Rural == 1)),1,0.5)
df[which(df$Male == 1 & df$Suburban == 1),1] <-     rbinom(length(which(df$Male == 1 & df$Suburban == 1)),1,0.5)
df[which(df$Male == 1 & df$Urban == 1),1] <-        rbinom(length(which(df$Male == 1 & df$Urban == 1)),1,0.5)
df[which(df$Female == 1 & df$Rural == 1),1] <-      rbinom(length(which(df$Female == 1 & df$Rural == 1)),1,0.5)
df[which(df$Female == 1 & df$Suburban == 1),1] <-   rbinom(length(which(df$Female == 1 & df$Suburban == 1)),1,0.5)
df[which(df$Female == 1 & df$Urban == 1),1] <-      rbinom(length(which(df$Female == 1 & df$Urban == 1)),1,0.5)

df
library(plyr)
test_df <- matrix(c(ddply(df,.(Male,Female,Rural,Suburban,Urban),summarize,sum(Response))[,6],
ddply(df,.(Male,Female,Rural,Suburban,Urban),summarize,length(Response)-sum(Response))[,6]),ncol=6,byrow=TRUE)

chisq.test(test_df)


##########
library(data.table)
n <- 1000
df <- data.frame(matrix(0,nrow=n,ncol=4))
setDT(df)
#Assign half of the records as male and female
df[,2] <- ifelse(rbinom(n,1,0.5) == 1, "Male","Female")

#Assign 20 percent of the records as living in the rural area, 40 percent in suburban and 40 in urban
df[,3] <- ifelse(rbinom(n,1,0.2)==1,"Rural",ifelse(rbinom(n,1,0.5),"Suburban","Urban"))

#Assign ages randomly from a uniform distribution
df[,4] <- round(runif(n,min=35,max=82),0)
colnames(df) <- c("Response","Sex","Area","Age")

unlist(df[,.N,by=Sex][,1])
p1 <- (unlist(df[,.N,by=Sex][,2]))/n
names(p1) <- unlist(df[,.N,by=Sex][,1])
barplot(p1,main="Distribution of sex")

p2 <- (unlist(df[,.N,by=Area][,2]))/n
names(p2) <- unlist(df[,.N,by=Area][,1])
barplot(p2,main="Distribution of area")

par(mfrow=c(1,3))
barplot(p1,main="Distribution of sex")
barplot(p2,main="Distribution of area")
hist(unlist(df[,"Age"]),main="Distribution of age",prob=TRUE,xlab="Age")

df[Sex=="Male" & Area=="Rural"][,1] <- rbinom(dim(df[Sex=="Male" & Area=="Rural"][,1])[1],1,0.2)
df[Sex=="Male" & Area=="Suburban"][,1] <- rbinom(dim(df[Sex=="Male" & Area=="Suburban"][,1])[1],1,0.5)
df[Sex=="Male" & Area=="Urban"][,1] <- rbinom(dim(df[Sex=="Male" & Area=="Urban"][,1])[1],1,0.4)
df[Sex=="Female" & Area=="Rural"][,1] <- rbinom(dim(df[Sex=="Female" & Area=="Rural"][,1])[1],1,0.4)
df[Sex=="Female" & Area=="Suburban"][,1] <- rbinom(dim(df[Sex=="Female" & Area=="Suburban"][,1])[1],1,0.8)
df[Sex=="Female" & Area=="Urban"][,1] <- rbinom(dim(df[Sex=="Female" & Area=="Urban"][,1])[1],1,0.6)


sex_rep <- 1+sum(apply(df[,mean(Response),by=Sex],1,function(x){ifelse(as.numeric(x[2])<df[,mean(Response)],as.numeric(x[2])-df[,mean(Response)],0)}))/df[,mean(Response)]
area_rep <- 1+sum(apply(df[,mean(Response),by=Area],1,function(x){ifelse(as.numeric(x[2])<df[,mean(Response)],as.numeric(x[2])-df[,mean(Response)],0)}))/df[,mean(Response)]

p_age <- Vectorize(function(a){
sum(df[Age==a,Response])/nrow(df[Age==a])
})
age_rep <- 1+sum(ifelse(p_age(sort(unlist(unique(df[,"Age"]))))<df[,mean(Response)],p_age(sort(unlist(unique(df[,"Age"]))))-df[,mean(Response)],0))/(df[,mean(Response)]*(max(unlist(unique(df[,"Age"])))-min(unlist(unique(df[,"Age"])))))

list("Representativity Sex"=round(sex_rep,4),
     "Representativity Area" = round(area_rep,4),
     "Representativity Age"=round(age_rep,4))
