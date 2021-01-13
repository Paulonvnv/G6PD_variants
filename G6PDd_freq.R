


G6PDpop<-data.frame("gender"=c(rep("Male",5000),rep("Female",5000)))

q<-0.1#freq of recesive Allele
p<-1-q#freq of dominan Allele 


nM<-p#Hemizigotes normal males
dM<-q#Hemizigotes deficient males

nF<-p^2#Homozigotes normal females
hF<-2*p*q#Heterozigotes females
dF<-q^2#Homozigotes deficient females


n.nM<-as.integer(round(nM*length(G6PDpop[G6PDpop$gender=="Male",])))
n.dM<-as.integer(round(dM*length(G6PDpop[G6PDpop$gender=="Male",])))

n.nF<-as.integer(round(nF*length(G6PDpop[G6PDpop$gender=="Female",])))
n.hF<-as.integer(round(hF*length(G6PDpop[G6PDpop$gender=="Female",])))
n.dF<-as.integer(round(dF*length(G6PDpop[G6PDpop$gender=="Female",])))

G6PDpop$condition<-NA
G6PDpop[G6PDpop$gender=="Male",][["condition"]]<-c(rep("Normal",n.nM),
                                                   rep("Deficient",n.dM))

G6PDpop[G6PDpop$gender=="Female",][["condition"]]<-c(rep("Normal",n.nF),
                                                     rep("Heterozygote",n.hF),
                                                   rep("Deficient",n.dF))

G6PDpop$activity<-NA

nr.act<-8.5
sd.nr.act<-(nr.act-0.7*nr.act)/3
act.nM<-rnorm(n = n.nM ,mean = nr.act, sd = sd.nr.act)
G6PDpop[G6PDpop$gender=="Male"&G6PDpop$condition=="Normal",][["activity"]]<-act.nM
act.nF<-rnorm(n = n.nF ,mean = nr.act, sd = sd.nr.act)
G6PDpop[G6PDpop$gender=="Female"&G6PDpop$condition=="Normal",][["activity"]]<-act.nF


df.act<-0.1*nr.act
sd.df.act<-df.act/3
act.dM<-rnorm(n = n.dM ,mean = df.act, sd = sd.df.act)
G6PDpop[G6PDpop$gender=="Male"&G6PDpop$condition=="Deficient",][["activity"]]<-act.dM
act.dF<-rnorm(n = n.dF ,mean = df.act, sd = sd.df.act)
G6PDpop[G6PDpop$gender=="Female"&G6PDpop$condition=="Deficient",][["activity"]]<-act.dF


ht.act<-mean(c(nr.act,df.act))
sd.ht.act<-ht.act/3
act.hF<-rnorm(n = n.hF ,mean = ht.act, sd = sd.ht.act)
G6PDpop[G6PDpop$gender=="Female"&G6PDpop$condition=="Heterozygote",][["activity"]]<-act.hF


dist_g6pd_act<- ggplot(G6PDpop, aes(x=activity, color=condition, fill=condition))+
  geom_histogram(alpha=0.2, position="identity", binwidth = 0.2)+
  geom_vline(xintercept = 0.7*nr.act, color = "darkblue", size = 1.25)+
  geom_vline(xintercept = 0.3*nr.act, color = "red", size = 1.25)+
  scale_color_manual(values=c("red", "green", "darkblue"))+
  scale_fill_manual(values=c("red", "green", "darkblue"))+
  labs(title="Frequency distribution of G6PD activity",
       y="Number of individuals",
       x="G6PD activity (IU/gHb)",
       fill = "Condition",
       color ="Condition")+
  facet_wrap(~ gender, ncol = 1)+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5, size=14))+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.text= element_text(size=11),
        legend.position = "bottom")


G6PDpop$condition<-as.factor(G6PDpop$condition)

plot1<-ggplot(G6PDpop, aes(x=condition,y=activity,color=condition,fill=condition))+
  geom_violin(alpha=0.2)+
  stat_summary(fun.y=median, geom="point", size=2, color="red")+
  geom_boxplot(width=0.1,alpha=0.2)+
  scale_color_manual(values=c("darkblue", "red","green"))+
  scale_fill_manual(values=c("darkblue", "red", "green"))+
  facet_grid(. ~ gender)+
  theme(axis.text.x= element_text(size=11,angle=45,vjust = 0.5))
