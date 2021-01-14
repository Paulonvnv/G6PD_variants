
source("G6PD_LOVD.R")
source("merge_LOVD_dfs.R")
source("haplotype_deffinition.R")
source("who_classification.R")

# Descriptive analysis ----

# Number of tested individuals----

nlevels(as.factor(as.character(G6PD_LOVD_df$individualid)))
# 10459 individuals

# Number of Locus ----

nlevels(as.factor(as.character(G6PD_LOVD_df$position_g_start)))
nlevels(as.factor(as.character(G6PD_LOVD_df$position_c_start)))

# discrepancies are because differences in reading frame
# To quantify the number of locus we are going to extract information from `VariantOnTranscript/DNA`

nlevels(as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/DNA`)))
levels(as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/DNA`)))



G6PD_LOVD_df[["Locus"]] <- str_extract(G6PD_LOVD_df$`VariantOnTranscript/DNA`,
            "[0-9]+(?:_|-|\\+)?[0-9]*(?:_|-|\\+)?[0-9]*")

nlevels(as.factor(G6PD_LOVD_df$Locus))

# There are 203 positions or Locus

# Locus type----

G6PD_LOVD_df[["Locus.type"]]<-NA

for (n in levels(as.factor(G6PD_LOVD_df$Locus))) {
  G6PD_LOVD_df[G6PD_LOVD_df[["Locus"]]==n,][["Locus.type"]]<-if_else(is_empty(grep(n, pattern = "(?:_|-|\\+)")),
          "Subst",
          "Indel")
}

levels(as.factor(G6PD_LOVD_df$Locus.type))

# Locus Location ----

source("fx_functions.R")
source("probe.R")
G6PD_LOVD_df$DNA.region<-NA

for (n in levels(as.factor(G6PD_LOVD_df[["Locus"]]))) {
  G6PD_LOVD_df[G6PD_LOVD_df[["Locus"]]==n,][["DNA.region"]]<-get.exon(position=n,fasta = G6PD_DNA.fata,gff = Exons,start = 111,end = 1658)
}


#g6pd.dna.hap_df----

g6pd.dna.hap_df<-NULL


variables<-c("aa.Haplotype","DNA.Haplotype", "WHO.Classification","Locus","Locus.type","DNA.region")

n="G6PD-No name, No Synonym 5"

for (n in levels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`))) {
  temp<-data.frame("Haplotype" = n)
  temp[["aa.Haplotype"]] <- levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["aa.Haplotype"]])))
  temp[["DNA.Haplotype"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.Haplotype"]]))))>1,
                                    paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.Haplotype"]]))),collapse=" "), 
                                    levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.Haplotype"]]))))
  temp[["WHO.Classification"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["WHO.Classification"]]))))==0,
                                         NA, 
                                         levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["WHO.Classification"]]))))
  temp[["Locus"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus"]]))))>1,
                            paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus"]]))),collapse =" "), 
                            levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus"]]))))
  temp[["Locus.type"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus.type"]]))))>1,
                                 paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus.type"]]))),collapse=" "), 
                                 levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Locus.type"]]))))
  temp[["DNA.region"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.region"]]))))>1,
                                 paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.region"]]))),collapse=" "), 
                                 levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["DNA.region"]]))))
  temp["freq"] <- length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["individualid"]]))))/length(levels(as.factor(as.character(G6PD_LOVD_df[["individualid"]]))))
  temp[["Country"]] <- ifelse(length(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Individual/Origin/Geographic"]]))))>1,
                              paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Individual/Origin/Geographic"]]))),collapse=";"), 
                              levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Individual/Origin/Geographic"]]))))
  
  g6pd.dna.hap_df<-rbind(g6pd.dna.hap_df,temp)
}

g6pd.dna.hap_df

#g6pd.aa.hap_df----

g6pd.aa.hap_df<-NULL


for (n in levels(as.factor(G6PD_LOVD_df$aa.Haplotype))) {
  temp<-data.frame("Haplotype" = n)
  temp[["Haplotype.name"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["VariantOnTranscript/Haplotype"]]))))>1,
                                    paste0(levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["VariantOnTranscript/Haplotype"]]))),collapse=" "), 
                                    levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["VariantOnTranscript/Haplotype"]]))))
  
  temp[["DNA.Haplotype"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.Haplotype"]]))))>1,
                                    paste0(levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.Haplotype"]]))),collapse=" "), 
                                    levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.Haplotype"]]))))
  temp[["WHO.Classification"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["WHO.Classification"]]))))==0,
                                         NA, 
                                         levels(as.factor(as.character(
                                           G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["WHO.Classification"]]))))
  temp[["Locus"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus"]]))))>1,
                            paste0(levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus"]]))),collapse =" "), 
                            levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus"]]))))
  temp[["Locus.type"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus.type"]]))))>1,
                            paste0(levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus.type"]]))),collapse=" "), 
                            levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Locus.type"]]))))
  temp[["DNA.region"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.region"]]))))>1,
                            paste0(levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.region"]]))),collapse=" "), 
                            levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["DNA.region"]]))))
  temp["freq"] <- length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["individualid"]]))))/length(levels(as.factor(as.character(G6PD_LOVD_df[["individualid"]]))))
  temp[["Country"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Individual/Origin/Geographic"]]))))>1,
                            paste0(levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Individual/Origin/Geographic"]]))),collapse=";"), 
                            levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Individual/Origin/Geographic"]]))))
  
  g6pd.aa.hap_df<-rbind(g6pd.aa.hap_df,temp)
  }

g6pd.aa.hap_df


#g6pd.dna.var_df----

g6pd.dna.var_df<-NULL


variables<-c("aa.Haplotype","DNA.Haplotype", "WHO.Classification","Locus","Locus.type","DNA.region")

n="G6PD-No name, No Synonym 5"

levels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`))

for (n in levels(as.factor(levels(as.factor(G6PD_LOVD_df$`VariantOnGenome/DNA`))))) {
  temp<-tibble("DNA.variant" = n)
  temp[["cDNA.variant"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/DNA"]]))))>1,
    paste0(levels(as.factor(as.character(
      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/DNA"]]))),collapse=" "), 
    levels(as.factor(as.character(
      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/DNA"]]))))
  temp[["aa.variant"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Protien"]]))))>1,
    paste0(levels(as.factor(as.character(
      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Protein"]]))),collapse=" "), 
    levels(as.factor(as.character(
      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Protein"]]))))
  temp[["Haplotype"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Haplotype"]]))))>1,
                                    paste0(levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Haplotype"]]))),collapse=" "), 
                                    levels(as.factor(as.character(
                                      G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["VariantOnTranscript/Haplotype"]]))))
  temp[["WHO.Classification"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["WHO.Classification"]]))))==0,
                                         NA,
                                         levels(as.factor(as.character(
                                           G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["WHO.Classification"]]))))
  temp[["Locus"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus"]]))))>1,
                            paste0(levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus"]]))),collapse =" "), 
                            levels(as.factor(as.character(
                              G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus"]]))))
  temp[["Locus.type"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus.type"]]))))>1,
                                 paste0(levels(as.factor(as.character(
                                   G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus.type"]]))),collapse=" "), 
                                 levels(as.factor(as.character(
                                   G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Locus.type"]]))))
  temp[["DNA.region"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["DNA.region"]]))))>1,
                                 paste0(levels(as.factor(as.character(
                                   G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["DNA.region"]]))),collapse=" "), 
                                 levels(as.factor(as.character(
                                   G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["DNA.region"]]))))
  temp[["Country"]] <- ifelse(length(levels(as.factor(as.character(
    G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Individual/Origin/Geographic"]]))))>1,
                              paste0(levels(as.factor(as.character(
                                G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Individual/Origin/Geographic"]]))),collapse=";"), 
                              levels(as.factor(as.character(
                                G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Individual/Origin/Geographic"]]))))
  
  g6pd.dna.var_df<-rbind(g6pd.dna.var_df,temp)
}


for(i in names(g6pd.dna.var_df)){
  g6pd.dna.var_df[[i]]<-as.factor(g6pd.dna.var_df[[i]])
}


g6pd.dna.var_df %>% group_by(Locus.type) %>%
  dplyr::summarise(N = n())

levels(g6pd.dna.var_df$DNA.region)



# Results---

# number of polymorphism and haplotypes at DNA and aa level
summary_table <- tibble(
  "N of DNA Polymorphism" = nlevels(as.factor(G6PD_LOVD_df$`VariantOnGenome/DNA`)),
  "N of aa Polymorphism" = nlevels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/Protein`)),
  "N of DNA haplotypes" = nlevels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`)),
  "N of aa Haplotypes" = nlevels(as.factor(G6PD_LOVD_df$aa.Haplotype))
)

# Number of polymorphism by gene region

ploy_genedist<-g6pd.dna.var_df %>% mutate(WHO.Class = case_when(
  grepl("Class I:|Class I-II", WHO.Classification) ~ "Class I",
  grepl("Class II:|Class II-III", WHO.Classification) ~ "Class II",
  grepl("Class III:", WHO.Classification) ~ "Class III",
  grepl("Class IV:", WHO.Classification) ~ "Class IV",
  is.na(WHO.Classification) ~ "Undetermined"
)) %>% select(DNA.region, WHO.Classification, WHO.Class)%>%
  group_by(DNA.region, WHO.Class) %>%
  dplyr::summarise(N = n(),
                   freq = n()/nrow(g6pd.dna.var_df))%>%
  arrange(DNA.region,desc(WHO.Class))%>%
  ddply(.(DNA.region), transform, label_ypos=cumsum(N))%>%
  ggplot(aes(x = DNA.region, y = N, fill = WHO.Class))+
  geom_bar(stat = "identity", alpha = 0.65)+
  geom_text(aes(y = label_ypos,label = N), vjust = 1, color = "black", size = 3)+
  scale_fill_manual(values = c(brewer.pal(n = 4, name = "Spectral"),
                               "gray30"))+
  scale_x_discrete(limits = c("Exon 2", "Exon 3", "Intron 2", "Exon 4", "Exon 5",
                              "Intron 5", "Exon 6", "Exon 7", "Exon 8", "Exon 9",
                              "Exon 10", "Intron 10","Exon 11", "Intron 11",
                              "Exon 12", "Intron 12", "Exon 13"))+
  labs(x = "Gene region",
       y = "Number of nucleotide polymorphisms",
       fill = "WHO Classification")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "none")

# number of haplotypes by WHO classification

g6pd.aa.hap_df %>% mutate(WHO.Class = case_when(
  grepl("Class I:|Class I-II", WHO.Classification) ~ "Class I",
  grepl("Class II:|Class II-III", WHO.Classification) ~ "Class II",
  grepl("Class III:|Class III-IV", WHO.Classification) ~ "Class III",
  grepl("Class IV:", WHO.Classification) ~ "Class IV",
  is.na(WHO.Classification) ~ "Undetermined"
)) %>% filter(WHO.Class == "Undetermined")

who.class_freq<-g6pd.aa.hap_df %>% mutate(WHO.Class = case_when(
  grepl("Class I:|Class I-II", WHO.Classification) ~ "Class I",
  grepl("Class II:|Class II-III", WHO.Classification) ~ "Class II",
  grepl("Class III:|Class III-IV", WHO.Classification) ~ "Class III",
  grepl("Class IV:", WHO.Classification) ~ "Class IV",
  is.na(WHO.Classification) ~ "Undetermined"
)) %>% select(WHO.Class)%>%
  group_by(WHO.Class) %>%
  dplyr::summarise(N = n()) %>%
  ungroup()%>%arrange(desc(WHO.Class))%>%
  dplyr::summarise(
    WHO.Class = WHO.Class,
    N = N,
    perc = 100*N/sum(N),
    Y_lable = cumsum(N)-N/2)%>%
  ggplot(aes(x = "", y = N, fill = WHO.Class))+
  geom_bar(width = 1, stat = "identity", alpha = 0.65)+
  coord_polar("y", start = 0, direction = 1)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text.x=element_blank(),
    legend.position = "bottom"
  ) +
  geom_text(aes(y =Y_lable, 
                label = paste0(percent(perc/100), sep = " (", N, sep = ")")), size=3.5)+
  scale_fill_manual(values = c(brewer.pal(n = 4, name = "Spectral"),
                               "gray30"))

plot1 <- ggdraw()+
  draw_plot(dist_g6pd_act, x = 0, y = 0, width = .5, height = 1) +
  draw_plot(ploy_genedist, x = 0.5, y = 0.5, width = .5, height = .5) +
  draw_plot(who.class_freq, x = .5, y = 0, width = 0.5, height = .5)









