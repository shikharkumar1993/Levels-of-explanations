library(lme4)
library(ggplot2)
library(emmeans)
library(lmerTest)
library(lme4)
library(MASS)
library(car)
library(ggplot2)
library(pbkrtest)
library(rstatix)
library(RLRsim)
library(pbnm)
#install.packages('coin')
library(plyr)
library(multcomp)
library(ggpattern)
library(nlme)
library(buildmer)
library(multcompView)
library(FSA)
library(PMCMR)
library(PMCMRplus)
library(ordinal)
#install.packages("TOSTER")
library("TOSTER")
#install.packages("pwrss")
library(pwrss)
library(ltm)
library(reshape2)
#library(coin)
#install.packages('epitools')
library('epitools')
setwd("C:/Users/shikhar/OneDrive - post.bgu.ac.il/LOE_paper_data_omer")
dat1<-read.csv("without_time_subjective.csv")
#dat1<-read.csv("With_time_limit_subjective.csv")
#dat1<-read.csv("combine_data.csv")
colnames(dat1)<-c("Participant",'group','order','LOE',"Fluency","Adequacy_of_Explanations","Trust","completion_time")
dat1$LOE[dat1$LOE=='Level 1']<-"High LOE"
dat1$LOE[dat1$LOE=='Level 2']<-"Medium LOE"
dat1$LOE[dat1$LOE=='Level 3']<-"Low LOE"
shapiro.test(dat1$Fluency)
shapiro.test(dat1$Adequacy_of_Explanations)
shapiro.test(dat1$Trust)
ks.test(dat1$completion_time,"pnorm",mean(dat1$completion_time),sd(dat1$completion_time))
a<-friedman.test(Fluency~LOE|Participant,data=dat1)
a
#c<-posthoc.friedman.nemenyi.test(a)
#b<-pairwise.wilcox.test(dat1$Fluency,dat1$LOE,p.adj = "bonf")
b<-wilcox_test(Fluency~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
friedman_effsize(Fluency~LOE|Participant,data=dat1)
b
z<-qnorm(b$p/2)
z
a<-friedman.test(Adequacy_of_Explanations~LOE|Participant,data=dat1)
friedman_effsize(Adequacy_of_Explanations~LOE|Participant,data=dat1)
a#c<-posthoc.friedman.nemenyi.test(a)
#b<-pairwise.wilcox.test(dat1$Fluency,dat1$LOE,p.adj = "bonf")
b<-wilcox_test(Adequacy_of_Explanations~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
a<-friedman.test(Trust~LOE|Participant,data=dat1)
friedman_effsize(Trust~LOE|Participant,data=dat1)
a
tapply(dat1$completion_time,dat1$LOE,summary)
b<-wilcox_test(Trust~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
a<-aov(completion_time~LOE,data=dat1)
summary(a)
eta_squared(a)
TukeyHSD(a)
pairs(emmeans(a,~LOE))
b<-wilcox_test(completion_time~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
colnames(dat1)<-c("Participant",'group','order','LOE',"Human oriented \n Fluency of Interaction","Adequacy of \nExplanations","Trust","Completion_time")
dat1$LOW<-as.character(dat1$LOE)
dat1$LOE<-factor(dat1$LOE,levels=unique(dat1$LOE))
dat2<-melt(dat1,id.vars = "LOE",measure.vars = c('Human oriented \n Fluency of Interaction','Adequacy of \nExplanations','Trust'))
p<-ggplot(data=dat2,aes(x=variable,y=value,fill=LOE))+
  geom_boxplot()+
  theme(axis.text = element_text(size = 24,face = "bold"),
        axis.title.x = element_text(size = 24,face = "bold"),
        legend.text = element_text(size = 24,face = "bold"),
        legend.title = element_text(size = 24,face = "bold"),
        axis.title.y = element_text(size = 24,face = "bold")
  )+expand_limits(y=1)
p+xlab("")+ylab("Scores from questionnaire")
dat2<-melt(data = dat1,id.vars = "LOE",measure.vars=c(Completion_time))
p<-ggplot(data=dat1,aes(x=group,y=Completion_time,fill=LOE))+
  geom_boxplot()+
  theme(axis.text = element_text(size = 24,face = "bold"),
        axis.title.x = element_text(size = 24,face = "bold"),
        legend.text = element_text(size = 24,face = "bold"),
        legend.title = element_text(size = 24,face = "bold"),
        axis.title.y = element_text(size = 24,face = "bold")
  )+expand_limits(y=1)
p+xlab("")+ylab("Completion time in seconds")
tapply(dat1$completion_time, dat1$LOE, summary)
##########objective
setwd("C:/Users/shikhar/OneDrive - post.bgu.ac.il/LOE_paper_data_omer")
dat1<-read.csv("without_time_limit_objective.csv")
#dat1<-read.csv("with_time_limit_objective.csv")
colnames(dat1)<-c("Participant",'group','order','LOE',"Collision_with_obstacles","Incorrect_movements","clarifications")
dat1$LOE[dat1$LOE=='Level 1']<-"High LOE"
dat1$LOE[dat1$LOE=='Level 2']<-"Medium LOE"
dat1$LOE[dat1$LOE=='Level 3']<-"Low LOE"
a<-friedman.test(Collision_with_obstacles~LOE|Participant,data=dat1)
#c<-posthoc.friedman.nemenyi.test(a)
#b<-pairwise.wilcox.test(dat1$Fluency,dat1$LOE,p.adj = "bonf")
friedman_effsize(Collision_with_obstacles~LOE|Participant,data=dat1)
a
b<-wilcox_test(Collision_with_obstacles~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
a<-friedman.test(Incorrect_movements~LOE|Participant,data=dat1)
friedman_effsize(Incorrect_movements~LOE|Participant,data=dat1)
a
#c<-posthoc.friedman.nemenyi.test(a)
#b<-pairwise.wilcox.test(dat1$Fluency,dat1$LOE,p.adj = "bonf")
b<-wilcox_test(Incorrect_movements~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
a<-friedman.test(clarifications~LOE|Participant,data=dat1)
friedman_effsize(clarifications~LOE|Participant,data=dat1)
a
#c<-posthoc.friedman.nemenyi.test(a)
#b<-pairwise.wilcox.test(dat1$Fluency,dat1$LOE,p.adj = "bonf")
b<-wilcox_test(clarifications~LOE,data = dat1,paired = TRUE,p.adjust.method = "bonferroni")
b
z<-qnorm(b$p/2)
z
colnames(dat1)<-c("Participant",'group','order','LOE',"Number of \n Collisions","Wrong \n Movements","Number of \n Clarification")
dat1$LOE<-as.character(dat1$LOE)
dat1$LOE<-factor(dat1$LOE,levels=unique(dat1$LOE))
dat2<-melt(dat1,id.vars = "LOE",measure.vars = c("Number of \n Collisions","Wrong \n Movements","Number of \n Clarification"))
p<-ggplot(data=dat2,aes(x=variable,y=value,fill=LOE))+
  geom_boxplot()+
  theme(axis.text = element_text(size = 24,face = "bold"),
        axis.title.x = element_text(size = 24,face = "bold"),
        legend.text = element_text(size = 24,face = "bold"),
        legend.title = element_text(size = 24,face = "bold"),
        axis.title.y = element_text(size = 24,face = "bold")
  )+expand_limits(y=1)
p+xlab("")+ylab("Frequency")

