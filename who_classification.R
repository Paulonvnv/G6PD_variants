# WHO classification----
nlevels(G6PD_LOVD_df$`VariantOnGenome/Remarks`)
levels(G6PD_LOVD_df$`VariantOnGenome/Remarks`)

G6PD_LOVD_df<-mutate(G6PD_LOVD_df, WHO.Classification = case_when(
  `VariantOnGenome/Remarks`=="WHO classification-Class I" | 
    `VariantOnGenome/Remarks`=="WHO classification-Class I: severe enzyme deficiency with chronic non-spherocytic haemolytic anaemia (CNSHA)" | 
    `VariantOnGenome/Remarks`=="WHO classification-Class I: severe enzyme deficiency with chronic non-spherocytic haemolytic anaemia (CNSHA); G6PD activity 0.05" ~ "Class I: severe enzyme deficiency with chronic non-spherocytic haemolytic anaemia (CNSHA)",
  `VariantOnGenome/Remarks`=="allele WHO classification-Class II"|
    `VariantOnGenome/Remarks`=="WHO classification-Class II"|
    `VariantOnGenome/Remarks`=="WHO classification-Class II: severe enzyme deficiency with <0.10 normal activity"~"Class II: severe enzyme deficiency with <0.10 normal activity",
  `VariantOnGenome/Remarks`=="allele WHO classification-Class III"|
    `VariantOnGenome/Remarks`=="WHO classification-Class III"|
    `VariantOnGenome/Remarks`=="WHO classification-Class III: mild to moderate enzyme deficiency (0.10-0.60 normal activity)"|
    `VariantOnGenome/Remarks`=="WHO classification-Class III: mild to moderate enzyme deficiency (0.10-0.60 normal activity); G6PD activity 0.14-0.39"|
    `VariantOnGenome/Remarks`=="WHO classification-Class III: mild to moderate enzyme deficiency (0.10-0.60 normal activity); G6PD activity 0.25"~"Class III: mild to moderate enzyme deficiency (0.10-0.60 normal activity)",
  `VariantOnGenome/Remarks`=="WHO classification-Class IIV"|
    `VariantOnGenome/Remarks`=="WHO classification-Class IV"|
    `VariantOnGenome/Remarks`=="WHO classification-Class IV: very mild or almost normal enzyme activity (>0.60 normal activity, no clinical problem)"~"Class IV: very mild or almost normal enzyme activity (>0.60 normal activity, no clinical problem)",
  `VariantOnGenome/Remarks`=="WHO classification-Class I-II"~"Class I-II",
  `VariantOnGenome/Remarks`=="WHO classification-Class II-III"~"Class II-III",
  `VariantOnGenome/Remarks`=="WHO classification-Class III-IV"~"Class III-IV"
))


length(G6PD_LOVD_df[!is.na(G6PD_LOVD_df$WHO.Classification),][["WHO.Classification"]]) # only 285 records have this information


G6PD_LOVD_df$`VariantOnGenome/DNA`<-as.character(G6PD_LOVD_df$`VariantOnGenome/DNA`)
nlevels(as.factor(G6PD_LOVD_df$`VariantOnGenome/DNA`))# 220 levels, no NA's, some `VariantOnGenome/DNA` have two different `VariantOnGenome/DBID` because differences in its effect id, their remarks or variant classification
summary(as.factor(G6PD_LOVD_df$`VariantOnGenome/DNA`))

G6PD_LOVD_df$`VariantOnTranscript/DNA`<-as.character(G6PD_LOVD_df$`VariantOnTranscript/DNA`)
nlevels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/DNA`))# 254 levels
summary(as.factor(G6PD_LOVD_df$`VariantOnTranscript/DNA`))

G6PD_LOVD_df$DNA.Haplotype<-as.character(G6PD_LOVD_df$DNA.Haplotype)
nlevels(as.factor(G6PD_LOVD_df$DNA.Haplotype))# 254 levels

G6PD_LOVD_df$aa.Haplotype<-as.character(G6PD_LOVD_df$aa.Haplotype)
nlevels(as.factor(G6PD_LOVD_df$aa.Haplotype))# 241 levels


# Generate a data.frame with records that have WHO.classification information

a<-G6PD_LOVD_df[!is.na(G6PD_LOVD_df$WHO.Classification),]

nlevels(as.factor(as.character(a$`VariantOnTranscript/Haplotype`)))# 180 levels, no NA's, 74 no present respect to the complete data base


# Generate a variable that counts the number of different WHO.classification's by variant
a[["WHOrep"]]<-NA

nlevels(as.factor(as.character(a$`VariantOnTranscript/Haplotype`)))
for (n in levels(as.factor(as.character(a$`VariantOnTranscript/Haplotype`)))) {
  a[a$`VariantOnTranscript/Haplotype`==n,][["WHOrep"]]<-nlevels(as.factor(as.character(a[a$`VariantOnTranscript/Haplotype`==n,][["WHO.Classification"]])))
}

a$WHOrep<-as.factor(a$WHOrep)

table(a$WHOrep)

nlevels(as.factor(as.character(a[a[["WHOrep"]]==2,][["WHO.Classification"]])))
levels(as.factor(as.character(a[a[["WHOrep"]]==2,][["WHO.Classification"]])))

nlevels(as.factor(as.character(a[a[["WHOrep"]]==2,][["VariantOnTranscript/Haplotype"]])))
levels(as.factor(as.character(a[a[["WHOrep"]]==2,][["VariantOnTranscript/Haplotype"]])))# Variants with 2 different WHO.classification's

View(a[a[["WHOrep"]]==2,][c("VariantOnTranscript/Haplotype","WHO.Classification")])

# Manually,  generate a data.frame that merge WHO.classification for variants with more that one classification
b<-data.frame("VariantOnTranscript/Haplotype"=levels(as.factor(as.character(a[a[["WHOrep"]]==2,][["VariantOnTranscript/Haplotype"]]))),
              "WHO.Classification"=c("Class II-III",
                                     "Class II-III",
                                     "Class II-III",
                                     "Class II-III",
                                     "Class II-III",
                                     "Class I-II",
                                     "Class I-II",
                                     "Class I-II",
                                     "Class II-III"))

# Modification and imputation of WHO.classification in the orginal G6PD_LOVD_df data.frame

b$WHO.Classification<-as.character(b$WHO.Classification)

a$WHO.Classification<-as.character(a$WHO.Classification)

for (n in levels(b$VariantOnGenome.DNA)){
  a[a[["VariantOnTranscript/Haplotype"]]==n,][["WHO.Classification"]]<-b[b[["VariantOnTranscript.Haplotype"]]==n,][["WHO.Classification"]]
}

a$WHO.Classification<-as.factor(a$WHO.Classification)
levels(a$WHO.Classification)

a$WHO.Classification<-as.character(a$WHO.Classification)
G6PD_LOVD_df$WHO.Classification<-as.character(G6PD_LOVD_df$WHO.Classification)


for (n in levels(as.factor(a$`VariantOnTranscript/Haplotype`))){
  G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["WHO.Classification"]]<-a[a[["VariantOnTranscript/Haplotype"]]==n,][1,"WHO.Classification"]
}


rm(list = c("a", "b","n", "m"))

sum(table(G6PD_LOVD_df$WHO.Classification)) # 10174 records (180 Haplotypes) has WHO.Classification information

summary(as.factor(G6PD_LOVD_df$WHO.Classification)) # 820 records (74) lacks WHO.Classification information

nlevels(as.factor(as.character(G6PD_LOVD_df[is.na(G6PD_LOVD_df$WHO.Classification),][["VariantOnTranscript/Haplotype"]])))
levels(as.factor(as.character(G6PD_LOVD_df[is.na(G6PD_LOVD_df$WHO.Classification),][["VariantOnTranscript/Haplotype"]])))