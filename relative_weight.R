library(ggplot2)
library(wesanderson)


##################### Plotting relative weight ####################3

data <- read.csv('relative_weight.csv',header=T,sep=',')

vprep_lg <- c('Danish', 'Norwegian', 'Swedish', 'Arabic', 'Hebrew', 'Greek', 'Indonesian', 'Galician', 'Latvian', 'Irish', 'Serbian', 'Slovak')

vprep <- subset(data, Language %in% vprep_lg)

vprep$Language <- factor(vprep$Language, levels = vprep_lg)

p<-ggplot(vprep[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-2.5,position=position_dodge(.9),size=2.6)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="Chevalier1"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,120))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11),
        strip.text = element_text(size=11),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) + 
  theme(legend.position="top") +
  facet_wrap(~Language,ncol=4)
p

####################### OV languages #######################

postpv_lg <- c('Japanese', 'Hindi', 'Urdu')

postpv <- subset(data, Language %in% postpv_lg)

postpv$Language <- factor(postpv$Language, levels = postpv_lg)

p<-ggplot(postpv[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-5,position=position_dodge(.9),size=3.5)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="Chevalier1"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,120))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=13),
        strip.text = element_text(size=13),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) + 
  theme(legend.position="top") +
  facet_wrap(~Language,ncol=4)
p

ggsave("rw-postpv.pdf", p, height = 4)

################# Only preposition and preverbal ########

prepv_lg <- c('Afrikaans', 'Persian', 'Chinese')

prepv <- subset(data, Language %in% prepv_lg)

prepv$Language <- factor(prepv$Language, levels = prepv_lg)

p<-ggplot(prepv[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-8,position=position_dodge(.9),size=3.5)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="Chevalier1"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,120))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=13),
        strip.text = element_text(size=13),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) + 
  theme(legend.position="top") +
  facet_wrap(~Language,ncol=4)
p

ggsave("rw-prepv.pdf", p, height = 4)

##################### Mixed ######################

mixed_lg <- c('English', 'German', 'Dutch', 'Bulgarian', 'Croatian', 'Czech', 'Russian', 'Slovenian', 'Ukrainian', 
               'Polish', 'Catalan', 'French', 'Italian', 'Portuguese', 'Romanian', 'Spanish')

mixed <- subset(data, Language %in% mixed_lg)
mixed$Language <- factor(mixed$Language, levels = mixed_lg)
mixed_preverbal = subset(mixed, position %in% c('zh'))
mixed_postverbal = subset(mixed, position %in% c('en'))

p<-ggplot(mixed_preverbal[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-1.8,position=position_dodge(.9),size=2.6)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="Chevalier1"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,120))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11),
        strip.text = element_text(size=11),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) + 
  theme(legend.position="top") +
  facet_wrap(~Language,ncol=4)
p


p<-ggplot(mixed_postverbal[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-2.5,position=position_dodge(.9),size=2.6)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="Chevalier1"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,120))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11),
        strip.text = element_text(size=11),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) + 
  theme(legend.position="top") +
  facet_wrap(~Language,ncol=4)
p



################## Plotting length distribution ##########################

data <- read.csv('len_distribution.csv',header=T,sep=',')

vprep_lg <- c('Danish', 'Norwegian', 'Swedish', 'Arabic', 'Hebrew', 'Greek', 'Indonesian', 'Galician', 'Latvian', 'Irish', 'Serbian', 'Slovak')

vprep <- subset(data, Language %in% vprep_lg)

vprep$Language <- factor(vprep$Language, levels = vprep_lg)

p<-ggplot(vprep[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-2.5,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length differences ")+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  facet_wrap(~Language,ncol=4)
p



####################### OV languages #######################

postpv_lg <- c('Japanese', 'Hindi', 'Urdu')

postpv <- subset(data, Language %in% postpv_lg)

postpv$Language <- factor(postpv$Language, levels = postpv_lg)

p<-ggplot(postpv[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-5.5,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  facet_wrap(~Language,ncol=4)
p

ggsave("dist-postpv.pdf", p, height = 5)


################# Only preposition and preverbal ########

prepv_lg <- c('Afrikaans', 'Persian', 'Chinese')

prepv <- subset(data, Language %in% prepv_lg)

prepv$Language <- factor(prepv$Language, levels = prepv_lg)

p<-ggplot(prepv[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-7.5,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  facet_wrap(~Language,ncol=4)
p

ggsave("dist-prepv.pdf", p, height = 5)


##################### Mixed ######################

mixed_lg <- c('English', 'German', 'Dutch', 'Bulgarian', 'Ukrainian', 'Slovenian', 'Russian', 'Czech',
              'Croatian', 'Polish', 'French', 'Spanish', 'Portuguese', 'Romanian', 'Italian', 'Catalan')

mixed <- subset(data, Language %in% mixed_lg)
mixed$Language <- factor(mixed$Language, levels = mixed_lg)
mixed_preverbal = subset(mixed, position %in% c('zh'))
mixed_postverbal = subset(mixed, position %in% c('en'))

p<-ggplot(mixed_preverbal[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-2,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  facet_wrap(~Language,ncol=4)
p


p<-ggplot(mixed_postverbal[,],aes(x=diff,y=round(Mean,2),fill=diff)) + 
  geom_bar(position=position_dodge(),stat='identity',colour='black')+
  geom_text(aes(label=paste(round(Mean,2),"%")), vjust=-2.5,position=position_dodge(.9),size=3)+
  geom_errorbar(aes(ymin=CI25, ymax=CI975),width=.1,position=position_dodge(.9))+
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2"))+
  labs(x="")+labs(y="Percent (%)")+labs(fill="PP length difference")+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme(legend.position="top")+
  facet_wrap(~Language,ncol=4)
p


###################################

for (i in 1 : 10000) {
  sample(data, len(data), replace = TRUE, prob = NULL)
}



par(mfrow = c(4, 1)) #row, col

