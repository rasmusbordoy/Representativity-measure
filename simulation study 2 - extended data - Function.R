
representativity_simulation <- function(n,mod,p){
men <- matrix(NA,nrow=6,ncol=6)
men[1,] <- c("","Education","","","","")
men[,1] <- c("","Heart Disease","HF","IHD","VHD","AF")
men[2,2:ncol(men)] <- c("Basic education","Postgraduate education","Secondary education","Tertiary education","Unknown education")
men[3:nrow(men),2:ncol(men)] <- matrix(c(c(5.7,1.5,9.5,2.4,0.4),
                                         c(7.5,1.9,12,4.5,0.7),
                                         c(1.7,0.6,3,1.2,0.1),
                                         c(3.2,2,7.1,2.9,0.2)),ncol=5,nrow=4,byrow=TRUE)

men <- data.frame(men)
colnames(men) <- colnames(c(rep("",6)))
men[1,1] <- "Men"

women <- matrix(NA,nrow=6,ncol=6)
women[1,] <- c("","Education","","","","")
women[,1] <- c("","Heart Disease","HF","IHD","VHD","AF")
women[2,2:ncol(women)] <- c("Basic education","Postgraduate education","Secondary education","Tertiary education","Unknown education")
women[3:nrow(men),2:ncol(women)] <- matrix(c(c(3.4,0.3,2.7,1.1,0.2),
                                             c(4.1,0.2,3.9,1.9,0.2),
                                             c(1.7,0.1,1.4,0.8,0.1),
                                             c(3.4,0.4,3.7,2.2,0.1)),ncol=5,nrow=4,byrow=TRUE)

women <- data.frame(women)
colnames(women) <- colnames(c(rep("",6)))
women[1,1] <- "Women"


#n <- 5000

df <- data.frame(matrix(0,nrow=n,ncol=11))
setDT(df)
colnames(df) <- c("Male","Female","HF","IHD","VHD","AF","Basic education","Postgraduate education","Secondary education","Tertiary education","Unknown education")

#Assign half of the records as male and female
#colnames(df) <- c("Sex","Heart Disease")
men <- data.frame(men[-1,])
women <- data.frame(women[-1,])

round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Basic education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Secondary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Basic education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Secondary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Basic education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Secondary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Basic education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Secondary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Basic education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Secondary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Basic education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Secondary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Basic education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Secondary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Unknown education")])/100)+
  
  round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Basic education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Postgraduate education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Secondary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Tertiary education")])/100)+
  round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Unknown education")])/100)


ind <- 
  cumsum(c(round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Basic education")])/100),
           round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Secondary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Tertiary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "HF"),which(men[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Basic education")])/100),
           round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Secondary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Tertiary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "IHD"),which(men[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Basic education")])/100),
           round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Secondary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Tertiary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "VHD"),which(men[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Basic education")])/100),
           round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Secondary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Tertiary education")])/100),
           round(n*as.numeric(men[which(men[,1] == "AF"),which(men[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Basic education")])/100),
           round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Secondary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Tertiary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "HF"),which(women[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Basic education")])/100),
           round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Secondary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Tertiary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "IHD"),which(women[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Basic education")])/100),
           round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Secondary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Tertiary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "VHD"),which(women[1,] == "Unknown education")])/100),
           
           round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Basic education")])/100),
           round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Postgraduate education")])/100),
           round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Secondary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Tertiary education")])/100),
           round(n*as.numeric(women[which(women[,1] == "AF"),which(women[1,] == "Unknown education")])/100)))


df[1:ind[1],c("Male","HF","Basic education")] <- 1
df[(ind[1]+1):ind[2],c("Male","HF","Postgraduate education")] <- 1
df[(ind[2]+1):ind[3],c("Male","HF","Secondary education")] <- 1
df[(ind[3]+1):ind[4],c("Male","HF","Tertiary education")] <- 1
df[(ind[4]+1):ind[5],c("Male","HF","Unknown education")] <- 1

df[(ind[5]+1):ind[6],c("Male","IHD","Basic education")] <- 1
df[(ind[6]+1):ind[7],c("Male","IHD","Postgraduate education")] <- 1
df[(ind[7]+1):ind[8],c("Male","IHD","Secondary education")] <- 1
df[(ind[8]+1):ind[9],c("Male","IHD","Tertiary education")] <- 1
df[(ind[9]+1):ind[10],c("Male","IHD","Unknown education")] <- 1

df[(ind[10]+1):ind[11],c("Male","VHD","Basic education")] <- 1
df[(ind[11]+1):ind[12],c("Male","VHD","Postgraduate education")] <- 1
df[(ind[12]+1):ind[13],c("Male","VHD","Secondary education")] <- 1
df[(ind[13]+1):ind[14],c("Male","VHD","Tertiary education")] <- 1
df[(ind[14]+1):ind[15],c("Male","VHD","Unknown education")] <- 1

df[(ind[15]+1):ind[16],c("Male","AF","Basic education")] <- 1
df[(ind[16]+1):ind[17],c("Male","AF","Postgraduate education")] <- 1
df[(ind[17]+1):ind[18],c("Male","AF","Secondary education")] <- 1
df[(ind[18]+1):ind[19],c("Male","AF","Tertiary education")] <- 1
df[(ind[19]+1):ind[20],c("Male","AF","Unknown education")] <- 1


df[(ind[20]+1):ind[21],c("Female","HF","Basic education")] <- 1
df[(ind[21]+1):ind[22],c("Female","HF","Postgraduate education")] <- 1
df[(ind[22]+1):ind[23],c("Female","HF","Secondary education")] <- 1
df[(ind[23]+1):ind[24],c("Female","HF","Tertiary education")] <- 1
df[(ind[24]+1):ind[25],c("Female","HF","Unknown education")] <- 1

df[(ind[25]+1):ind[26],c("Female","IHD","Basic education")] <- 1
df[(ind[26]+1):ind[27],c("Female","IHD","Postgraduate education")] <- 1
df[(ind[27]+1):ind[28],c("Female","IHD","Secondary education")] <- 1
df[(ind[28]+1):ind[29],c("Female","IHD","Tertiary education")] <- 1
df[(ind[29]+1):ind[30],c("Female","IHD","Unknown education")] <- 1

df[(ind[30]+1):ind[31],c("Female","VHD","Basic education")] <- 1
df[(ind[31]+1):ind[32],c("Female","VHD","Postgraduate education")] <- 1
df[(ind[32]+1):ind[33],c("Female","VHD","Secondary education")] <- 1
df[(ind[33]+1):ind[34],c("Female","VHD","Tertiary education")] <- 1
df[(ind[34]+1):ind[35],c("Female","VHD","Unknown education")] <- 1

df[(ind[35]+1):ind[36],c("Female","AF","Basic education")] <- 1
df[(ind[36]+1):ind[37],c("Female","AF","Postgraduate education")] <- 1
df[(ind[37]+1):ind[38],c("Female","AF","Secondary education")] <- 1
df[(ind[38]+1):ind[39],c("Female","AF","Tertiary education")] <- 1
df[(ind[39]+1):ind[40],c("Female","AF","Unknown education")] <- 1



df$Response <- 0
# 
# p <- c(rep(0.5,20),rep(0.5,20))
df[1:ind[1],"Response"] <- rbinom(ind[1],1,p[1]) #1
df[(ind[1]+1):ind[2],"Response"] <- rbinom(ind[2]-ind[1],1,p[2]) #2
df[(ind[2]+1):ind[3],"Response"] <- rbinom(ind[3]-ind[2],1,p[3]) #3
df[(ind[3]+1):ind[4],"Response"] <- rbinom(ind[4]-ind[3],1,p[4]) #4
df[(ind[4]+1):ind[5],"Response"] <- rbinom(ind[5]-ind[4],1,p[5]) #5
 
df[(ind[5]+1):ind[6],"Response"] <- rbinom(ind[6]-ind[5],1,p[6]) #6
df[(ind[6]+1):ind[7],"Response"] <- rbinom(ind[7]-ind[6],1,p[7]) #7
df[(ind[7]+1):ind[8],"Response"] <- rbinom(ind[8]-ind[7],1,p[8]) #8
df[(ind[8]+1):ind[9],"Response"] <- rbinom(ind[9]-ind[8],1,p[9]) #9
df[(ind[9]+1):ind[10],"Response"] <- rbinom(ind[10]-ind[9],1,p[10]) #10

df[(ind[10]+1):ind[11],"Response"] <- rbinom(ind[11]-ind[10],1,p[11]) #11
df[(ind[11]+1):ind[12],"Response"] <- rbinom(ind[12]-ind[11],1,p[12]) #12
df[(ind[12]+1):ind[13],"Response"] <- rbinom(ind[13]-ind[12],1,p[13]) #13
df[(ind[13]+1):ind[14],"Response"] <- rbinom(ind[14]-ind[13],1,p[14]) #14
df[(ind[14]+1):ind[15],"Response"] <- rbinom(ind[15]-ind[14],1,p[15]) #15

df[(ind[15]+1):ind[16],"Response"] <- rbinom(ind[16]-ind[15],1,p[16]) #16
df[(ind[16]+1):ind[17],"Response"] <- rbinom(ind[17]-ind[16],1,p[17]) #17
df[(ind[17]+1):ind[18],"Response"] <- rbinom(ind[18]-ind[17],1,p[18]) #18
df[(ind[18]+1):ind[19],"Response"] <- rbinom(ind[19]-ind[18],1,p[19]) #19
df[(ind[19]+1):ind[20],"Response"] <- rbinom(ind[20]-ind[19],1,p[20]) #20


df[(ind[20]+1):ind[21],"Response"] <- rbinom(ind[21]-ind[20],1,p[21]) #21
df[(ind[21]+1):ind[22],"Response"] <- rbinom(ind[22]-ind[21],1,p[22]) #22
df[(ind[22]+1):ind[23],"Response"] <- rbinom(ind[23]-ind[22],1,p[23]) #23
df[(ind[23]+1):ind[24],"Response"] <- rbinom(ind[24]-ind[23],1,p[24]) #24
df[(ind[24]+1):ind[25],"Response"] <- rbinom(ind[25]-ind[24],1,p[25]) #25

df[(ind[25]+1):ind[26],"Response"] <- rbinom(ind[26]-ind[25],1,p[26]) #26
df[(ind[26]+1):ind[27],"Response"] <- rbinom(ind[27]-ind[26],1,p[27]) #27
df[(ind[27]+1):ind[28],"Response"] <- rbinom(ind[28]-ind[27],1,p[28]) #28
df[(ind[28]+1):ind[29],"Response"] <- rbinom(ind[29]-ind[28],1,p[29]) #29
df[(ind[29]+1):ind[30],"Response"] <- rbinom(ind[30]-ind[29],1,p[30]) #30

df[(ind[30]+1):ind[31],"Response"] <- rbinom(ind[31]-ind[30],1,p[31]) #31
df[(ind[31]+1):ind[32],"Response"] <- rbinom(ind[32]-ind[31],1,p[32]) #32
df[(ind[32]+1):ind[33],"Response"] <- rbinom(ind[33]-ind[32],1,p[33]) #33
df[(ind[33]+1):ind[34],"Response"] <- rbinom(ind[34]-ind[33],1,p[34]) #34
df[(ind[34]+1):ind[35],"Response"] <- rbinom(ind[35]-ind[34],1,p[35]) #35

df[(ind[35]+1):ind[36],"Response"] <- rbinom(ind[36]-ind[35],1,p[36]) #36
df[(ind[36]+1):ind[37],"Response"] <- rbinom(ind[37]-ind[36],1,p[37]) #37
df[(ind[37]+1):ind[38],"Response"] <- rbinom(ind[38]-ind[37],1,p[38]) #38
df[(ind[38]+1):ind[39],"Response"] <- rbinom(ind[39]-ind[38],1,p[39]) #39
df[(ind[39]+1):ind[40],"Response"] <- rbinom(ind[40]-ind[39],1,p[40]) #40


df$COV <- NA
df$COV[which(apply(df[,c("Male","HF","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","HF","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Male","HF","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","HF","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Male","HF","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","HF","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Male","HF","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","HF","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Male","HF","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","HF","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Male","IHD","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","IHD","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Male","IHD","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","IHD","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Male","IHD","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","IHD","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Male","IHD","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","IHD","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Male","IHD","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","IHD","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Male","VHD","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","VHD","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Male","VHD","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","VHD","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Male","VHD","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","VHD","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Male","VHD","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","VHD","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Male","VHD","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","VHD","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Male","AF","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","AF","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Male","AF","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","AF","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Male","AF","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","AF","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Male","AF","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","AF","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Male","AF","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Male","AF","Unknown education"),collapse="_")



df$COV[which(apply(df[,c("Female","HF","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","HF","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Female","HF","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","HF","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Female","HF","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","HF","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Female","HF","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","HF","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Female","HF","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","HF","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Female","IHD","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","IHD","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Female","IHD","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","IHD","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Female","IHD","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","IHD","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Female","IHD","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","IHD","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Female","IHD","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","IHD","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Female","VHD","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","VHD","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Female","VHD","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","VHD","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Female","VHD","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","VHD","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Female","VHD","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","VHD","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Female","VHD","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","VHD","Unknown education"),collapse="_")

df$COV[which(apply(df[,c("Female","AF","Basic education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","AF","Basic education"),collapse="_")
df$COV[which(apply(df[,c("Female","AF","Postgraduate education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","AF","Postgraduate education"),collapse="_")
df$COV[which(apply(df[,c("Female","AF","Secondary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","AF","Secondary education"),collapse="_")
df$COV[which(apply(df[,c("Female","AF","Tertiary education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","AF","Tertiary education") ,collapse="_")
df$COV[which(apply(df[,c("Female","AF","Unknown education")],1,FUN=function(x){x[1]*x[2]*x[3]}) == 1)] <- paste(c("Female","AF","Unknown education"),collapse="_")

#mod <- "glm"
if(mod == "glm"){m <- glm(Response~COV,data=df,family="binomial")}
if(mod == "cv.glmnet"){m <- cv.glmnet(y=df$Response,x=as.matrix(df[,-c("COV","Response")]),family="binomial",intercept=FALSE,type.measure = "deviance")}


if(mod == "cv.glmnet"){
  sel_cov <- coef(m,s=m$lambda.1se)
  sel_cov <- sel_cov[,1][-c(which(names(sel_cov[,1]) == "(Intercept)"))]
  sel_cov <- sel_cov[sel_cov!= 0]
  sel_cov_names <- names(sel_cov)
}

if(class(m)[1] == "glm"){pred_mat <- data.frame(distinct(df[,c("COV")]))}

#Total variation distance
if(class(m)[1] == "glm"){
  
  A <- matrix(predict(m,pred_mat,type="response"))
  
  colnames(A) <- "P"
  
  G_s <- dim(A)[1]
  for(i in 1:(dim(A)[1]-1)){
    if( i == 1){
      
      a <- abs(as.numeric(unlist(A[1])-unlist(A[-1])))
      #names(a) <- paste(unlist(A[1,1]),unlist(A[-1,1]),sep="+")
      A <- A[-1]
      
    }else{
      a <- c(a,abs(as.numeric(unlist(A[1])-unlist(A[-1]))))
      #names(a) <- c(names(a)[names(a) != ""],paste(unlist(A[1,1]),unlist(A[-1,1]),sep="+"))
      A <- A[-1]
    }
  }
  1-sum(a)*2/((G_s-1)*G_s) 
}
if(class(m)[1] == "cv.glmnet")
{
  if(length(sel_cov)>0){
  pred_mat <- distinct(df[,..sel_cov_names])
  temp_df <- copy(distinct(df[,-..sel_cov_names][,-c("COV","Response")]))
  pred_mat <- cbind(pred_mat,apply(temp_df,c(1,2),function(x){x <- 0})[1:nrow(pred_mat),])
  pred_mat
  col_ind_pred_mat <- match(colnames(distinct(df[,-c("COV","Response")])), colnames(pred_mat))
  pred_mat <- as.matrix(pred_mat[,..col_ind_pred_mat])
  
  #A <- data.frame(COV=distinct(df[,-c("Response")])[,"COV"],P=predict(m,pred_mat,type="response"))
  A <- matrix(predict(m,pred_mat,type="response"))
  
  colnames(A) <- "P"
  
  G_s <- dim(A)[1]
  for(i in 1:(dim(A)[1]-1)){
    if( i == 1){
      
      a <- abs(as.numeric(unlist(A[1])-unlist(A[-1])))
      #names(a) <- paste(unlist(A[1,1]),unlist(A[-1,1]),sep="+")
      A <- A[-1]
      
    }else{
      a <- c(a,abs(as.numeric(unlist(A[1])-unlist(A[-1]))))
      #names(a) <- c(names(a)[names(a) != ""],paste(unlist(A[1,1]),unlist(A[-1,1]),sep="+"))
      A <- A[-1]
    }
  }
  1-sum(a)*2/((G_s-1)*G_s) 
}else{
  a <- 0
  G_s <- 2
}}

#R-indicator
if(class(m)[1] == "glm"){
  df_R <- cbind(df,P=predict(m,data.frame(df[,c("COV")]),type="response"))
  beta <- m$coefficients
  mod_mat <- model.matrix(m)
  lin_pred <- (model.matrix(m)) %*% beta
}

if(class(m)[1] == "cv.glmnet"){ 
  df_R <- cbind(df,P=predict(m,as.matrix(df[,-c("COV","Response")]),type="response",s=m$lambda.min))
  colnames(df_R)[ncol(df_R)] <- "P"
  beta <- coef(m,s=m$lambda.min)
  mod_mat <- as.matrix(df[,-c("COV","Response")])
  beta <- beta[,1][-c(which(names(beta[,1]) == "(Intercept)"))]
  }

denom <- solve(sum(apply(mod_mat,1,FUN=function(x){as.numeric(exp(x %*% beta)/((1+exp(x %*% beta))^2))*( (x) %*% t(x))})))

S_2 <- (sum(((df_R$P - mean(df_R$Response))^2))/n)
data.frame(n=n,
  Model=paste(mod),
  V_d=1-sum(a)*2/((G_s-1)*G_s),
  R_tilde=1-2*sqrt(S_2-sum(apply(mod_mat,1,FUN=function(x){((exp(x %*% beta)/((1+exp(x %*% beta))^2))^2 %*% t(x)) %*% (as.numeric(denom)* matrix(x))}))/n),
  R_hat=1-2*sqrt(S_2))


}
m <- glm(Response~COV,data=df,family="binomial")
m <- cv.glmnet(y=df$Response,x=as.matrix(df[,-c("COV","Response")]),family="binomial",intercept=FALSE,type.measure = "class")

rbind(representativity_simulation(n=800,mod="glm",p=rep(0.5,40)),
representativity_simulation(n=800,mod="cv.glmnet",p=rep(0.2,40)),
representativity_simulation(n=5000,mod="glm",p=rep(0.5,40)),
representativity_simulation(n=5000,mod="cv.glmnet",p=rep(0.2,40)))
