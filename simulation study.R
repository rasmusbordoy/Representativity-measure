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

