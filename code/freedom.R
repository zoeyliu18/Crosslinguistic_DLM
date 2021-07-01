library(ggplot2)
library(gridExtra)
library(wesanderson)
library(scales)


data1<-read.csv(file="en-en-freedom.csv",header=T,sep=",")

data1$Len_diff <- rescale(data1$Len_diff, to = c(-1, 1), from = range(data1$Len_diff, na.rm = TRUE, finite = TRUE))

data1$Likelihood<-as.numeric(data1$Likelihood)
data1$Len_diff<-as.numeric(data1$Len_diff)

data2<-read.csv(file="de-zh-freedom.csv",header=T,sep=",")

data2$Len_diff <- rescale(data2$Len_diff, to = c(-1, 1), from = range(data2$Len_diff, na.rm = TRUE, finite = TRUE))

data2$Likelihood<-as.numeric(data2$Likelihood)
data2$Len_diff<-as.numeric(data2$Len_diff)



par(mfrow=c(2, 1))

boxplot(data1$Likelihood, main="", sub=paste("Outlier rows: ", boxplot.stats(data1$Likelihood)$out)) 
boxplot(data2$Likelihood, main="", sub=paste("Outlier rows: ", boxplot.stats(data2$Likelihood)$out))  

scatter.smooth(x=data1$Likelihood, y=data1$Len_diff, main="")
scatter.smooth(x=data2$Likelihood, y=data2$Len_diff, main="")

plot(density(data1$Likelihood))
plot(density(data2$Likelihood))


cor(data$Likelihood, data$Len_diff)

model = lm(Len_diff ~ Likelihood, data=data)
summary(model)

options (contrasts = rep("contr.sum", 2))

se <- function(x){sd(x)/sqrt(length(x))}

test_num = round(nrow(data)/10, digit=0)
train_num = nrow(data) - test_num

c = rep(10000,0)

for (i in 1:10000){
  use = sample(1:nrow(data), test_num,replace=FALSE)
  
  train = data[-which(1:nrow(data) %in% use),]
  test = data[use,]
  
  model = lm(Len_diff ~ Likelihood, data=train)
  
  c1[i] = coef(model)[2]
 
}


round(mean(c1),2)
se(c1)



p1<-ggplot(data1[,],aes(x=Len,y=LenV,fill=Len)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(LenV,"%")), vjust=-3.8,position=position_dodge(.9),size=2.5)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=c("#009E73","#999999", "#E69F00"))+
  labs(x="Corpus")+labs(y="Percent (%)")+labs(fill="PP ordering")+
  scale_y_continuous(limits=c(0,90))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=6)#+ theme(legend.position = "none")
# + theme(legend.position=c(1,0),legend.justification=c(1,.1))

ggsave("dlm-ptb.pdf", width=8, height=3)

data2<-read.csv(file="dlm-ctb.csv",header=T,sep=",")
data2$LenV<-as.numeric(data2$LenV)
data2$Len<-factor(data2$Len,levels=c("Short PP Closer","Long PP Closer", "Equal Length"))
p2<-ggplot(data2[,],aes(x=Len,y=LenV,fill=Len)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(LenV,"%")), vjust=-1,position=position_dodge(.9),size=2.5)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=c("#009E73","#999999", "#E69F00"))+
  labs(x="Corpus")+labs(y="Percent (%)")+labs(fill="PP ordering")+
  scale_y_continuous(limits=c(0,90))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=6)#+ theme(legend.position = "none")
# + theme(legend.position=c(1,0),legend.justification=c(1,.1))

ggsave("dlm-ctb.pdf", width=4.5, height=2.5)

data3<-read.csv(file="sc-ptb.csv",header=T,sep=",")
data3$SCV<-as.numeric(data3$SCV)
data3$Mean<-as.numeric(data3$Mean)
data3$Corpus<-factor(data3$Corpus,levels=c("WSJ", 'Brown', 'Switchboard', 'CTB'))
data3$SC<-factor(data3$SC,levels=c("Argument closer", "Adjunct closer"))
p3<-ggplot(data3[,],aes(x=SC,y=Mean,fill=SC)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(SCV,"%")), vjust=-4.2,position=position_dodge(.9),size=2.5)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=c("#9999CC","#56B4E9"))+
  labs(x="Corpus")+labs(y="Percent (%)")+labs(fill="Argument status")+
  scale_y_continuous(limits=c(0,90))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=6)

data3<-read.csv(file="test.csv",header=T,sep=",")
data3$SCV<-as.numeric(data3$SCV)
data3$Mean<-as.numeric(data3$Mean)
data3$Corpus<-factor(data3$Corpus,levels=c("WSJ", 'Brown', 'Switchboard', 'CTB'))
data3$SC<-factor(data3$SC,levels=c("Argument-like PP closer"))
p3<-ggplot(data3[,],aes(x=SC,y=Mean,fill=SC)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(SCV,"%")), vjust=-3.2,position=position_dodge(.9),size=2.5)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=c("#9999CC","#56B4E9"))+
  labs(x="Corpus")+labs(y="Percent (%)")+labs(fill="Argument status")+
  scale_y_continuous(limits=c(0,90))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=8)

ggsave("sc-ptb.pdf", width=4, height=2.6)

data4<-read.csv(file="sc-ctb.csv",header=T,sep=",")
data4$SCV<-as.numeric(data4$SCV)
data4$Mean<-as.numeric(data4$Mean)
data4$SC<-factor(data4$SC,levels=c("Semantically dependent closer", "Semantically independent closer"))
p4<-ggplot(data4[,],aes(x=SC,y=Mean,fill=SC)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(SCV,"%")), vjust=-1,position=position_dodge(.9),size=2.5)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=c("#9999CC","#56B4E9"))+
  labs(x="Corpus")+labs(y="Percent (%)")+labs(fill="Semantic dependency")+
  scale_y_continuous(limits=c(0,90))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=6)#+ theme(legend.position = "none")
# + theme(legend.position=c(1,0),legend.justification=c(1,.1))

ggsave("sc-ctb.pdf", width=4.5, height=2.5)



df <- data.frame(Corpus=factor(c('WSJ', 'WSJ', 'Brown', 'Brown', 'Switchboard', 'Switchboard', 'CTB', 'CTB')),
                 factor = factor(c('Dependency length', 'Argument status')),
                 Coeff = c(1.01, 0.91, 1.25,0.45, 0.55, 0.98, 0.36, 0.97),
                 se = c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.001, 0.01))

df$Corpus<-factor(df$Corpus,levels=c("WSJ", 'Brown', 'Switchboard','CTB'))
df$factor<-factor(df$factor,levels=c("Dependency length", "Argument status"))

p<-ggplot(df, aes(x=factor,y=Coeff,colour=factor)) +
  geom_errorbar(aes(ymax = Coeff + se, ymin = Coeff - se), width=.1, ,position=position_dodge(.9)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=Coeff), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  labs(x="Corpus")+labs(fill='Factor')+
  scale_y_continuous(limits=c(0.36,1.27)) + 
  facet_wrap(~Corpus,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

ggsave("coeff.pdf", width=6, height=3)


data1<-read.csv(file="len-sc.csv",header=T,sep=",")
data1$SCV<-as.numeric(data1$SCV)
data1$Corpus<-factor(data1$Corpus,levels=c("WSJ", 'Brown', 'Switchboard', 'CTB'))
data1$Len<-factor(data1$Len,levels=c("Short PP Closer","Long PP Closer"))
p1<-ggplot(data1[,],aes(x=Len,y=SCV,fill=Len)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(SCV,"%")), vjust=-2.5,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=2, name="Moonrise2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP ordering")+
  scale_y_continuous(limits=c(0,95))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  theme(legend.position="top")+
  #       axis.ticks.x=element_blank())
  facet_wrap(~Corpus,ncol=6)#+ theme(legend.position = "none")
# + theme(legend.position=c(1,0),legend.justification=c(1,.1))

ggsave("len-sc.pdf", width=6, height=3.2)
