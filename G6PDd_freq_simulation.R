
# Defining deficient prevalence (allele frequency) in men population
q_range <- seq(from = 0.005, to = 0.6, by = 0.005)

# Defining G6PD activity in normal men population
nr.act <-8.5
sd.nr.act <- (nr.act-0.7*nr.act)/3

# Defining G6PD activity in deficient men population
df.act<-0.1*nr.act
sd.df.act<-df.act/3

males_prop <- 0.5

popsize <- 10000

G6PDpop <- NULL

for (q in q_range){
  temp<-data.frame("gender" = c(rep("Male", popsize*males_prop), rep("Female", popsize*(1 - males_prop))),
                   "allele_freq" = q)
  
  #q<-0.1#freq of recesive Allele
  p<-1-q#freq of dominan Allele 
  
  
  # Defining genotype frequencies
  
  # in males
  nM<-p#Hemizigotes normal males
  dM<-q#Hemizigotes deficient males
  
  # in females
  nF<-p^2#Homozigotes normal females
  hF<-2*p*q#Heterozigotes females
  dF<-q^2#Homozigotes deficient females
  
  
  # Defining absolute number of genotypes in a population of 10000 individuals
  n.nM<-as.integer(round(nM*nrow(temp[temp$gender=="Male",])))
  n.dM<-as.integer(round(dM*nrow(temp[temp$gender=="Male",])))
  
  n.hF<-as.integer(round(hF*nrow(temp[temp$gender=="Female",])))
  n.nF<-as.integer(round(nF*nrow(temp[temp$gender=="Female",])))
  n.dF<-as.integer(popsize*(1 - males_prop) - n.hF - n.nF)
  
  # if(round(q/0.005)%%2 == 0){
  #     n.dF<-as.integer(ceiling(dF*nrow(temp[temp$gender=="Female",])))
  #     #n.nF<-as.integer(ceiling(nF*nrow(temp[temp$gender=="Female",])))
  #   }else{
  #     n.dF<-as.integer(round(dF*nrow(temp[temp$gender=="Female",])))
  #     #n.nF<-as.integer(round(nF*nrow(temp[temp$gender=="Female",])))
  #   }  


  
  # Defining the phenotype
  temp$condition<-NA
  temp[temp$gender=="Male",][["condition"]]<-c(rep("Normal",n.nM),
                                               rep("Deficient",n.dM))
  
  temp[temp$gender=="Female",][["condition"]]<-c(rep("Normal",n.nF),
                                                 rep("Heterozygote",n.hF),
                                                 rep("Deficient",n.dF))
  
  # Expected G6PD activity
  temp$activity<-NA
  
  # for normal males and females
  act.nM<-rnorm(n = n.nM ,mean = nr.act, sd = sd.nr.act)
  temp[temp$gender=="Male"&temp$condition=="Normal",][["activity"]]<-act.nM
  act.nF<-rnorm(n = n.nF ,mean = nr.act, sd = sd.nr.act)
  temp[temp$gender=="Female"&temp$condition=="Normal",][["activity"]]<-act.nF
  
  # for deficient males and females
  act.dM<-rnorm(n = n.dM ,mean = df.act, sd = sd.df.act)
  temp[temp$gender=="Male"&temp$condition=="Deficient",][["activity"]]<-act.dM
  act.dF<-rnorm(n = n.dF ,mean = df.act, sd = sd.df.act)
  temp[temp$gender=="Female"&temp$condition=="Deficient",][["activity"]]<-act.dF
  
  # for heterozygous females
  ht.act<-mean(c(nr.act,df.act))
  sd.ht.act<-ht.act/3
  act.hF<-rnorm(n = n.hF ,mean = ht.act, sd = sd.ht.act)
  temp[temp$gender=="Female"&temp$condition=="Heterozygote",][["activity"]]<-act.hF
  
  G6PDpop <- rbind(G6PDpop, temp)
}

library(tidyr)
library(dplyr)
library(magrittr)



for(i in names(G6PDpop)[-4]){
  G6PDpop[[i]] <- as.factor(G6PDpop[[i]])  
}

g6pd_by_allele.freq<-NULL

for(i in levels(G6PDpop$allele_freq)){
  temp <- tibble(allele_freq = as.numeric(i))
  temp$pop_less30 <- nrow(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                    G6PDpop[["activity"]] <
                                    0.3*median(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                                         G6PDpop[["gender"]] == "Male" &
                                                         G6PDpop[["condition"]] == "Normal",][["activity"]]),])
  
  temp$females_less30 <- nrow(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                        G6PDpop[["gender"]] == "Female" &
                                        G6PDpop[["activity"]] <
                                        0.3*median(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                                             G6PDpop[["gender"]] == "Male" &
                                                             G6PDpop[["condition"]] == "Normal",][["activity"]]),])
  
  temp$pop_less70 <- nrow(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                    G6PDpop[["activity"]] <
                                    0.7*median(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                                         G6PDpop[["gender"]] == "Male" &
                                                         G6PDpop[["condition"]] == "Normal",][["activity"]]),])
  
  temp$females_less70 <- nrow(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                        G6PDpop[["gender"]] == "Female" &
                                        G6PDpop[["activity"]] <
                                        0.7*median(G6PDpop[G6PDpop[["allele_freq"]] == i &
                                                             G6PDpop[["gender"]] == "Male" &
                                                             G6PDpop[["condition"]] == "Normal",][["activity"]]),])
  
  
  g6pd_by_allele.freq <- rbind(g6pd_by_allele.freq,temp)
}

g6pd_by_allele.freq %<>% mutate(
  per_less30 = (pop_less30/popsize)*100,
  perfe_less30 = (females_less30/(popsize/2))*100,
  per_less70 = (pop_less70/popsize)*100,
  perfe_less70 = (females_less70/(popsize/2))*100,
  miss_pop = (pop_less70 - pop_less30)/(popsize)*100,
  miss_fem = (females_less70 - females_less30)/(popsize/2)*100)

library(ggplot2)

g6pd_by_allele.freq %>% ggplot(aes(x = allele_freq,y = miss_fem)) + 
  geom_line()+
  geom_line(aes(x = allele_freq, miss_pop))




