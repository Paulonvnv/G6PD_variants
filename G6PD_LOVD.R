# Call packages----
source("requiredPackages.R")

# Upload data ----

Genes<-read.xlsx("G6PD_LOVD.xlsx", sheet=1, colNames=TRUE)

Genes$created_date<-convertToDateTime(Genes$created_date)
Genes$edited_date<-convertToDateTime(Genes$edited_date)
Genes$updated_date<-convertToDateTime(Genes$updated_date)

Transcripts<-read.xlsx("G6PD_LOVD.xlsx", sheet=2,  colNames=TRUE)

Transcripts$created_date<-convertToDateTime(Transcripts$created_date)
Transcripts$edited_date<-convertToDateTime(Transcripts$edited_date)

a<-names(Transcripts)

for (n in a[c(-11:-15,-17,-19)]){
  Transcripts[[n]]<-as.factor(as.character(Transcripts[[n]]))
}


Diseases<-read.xlsx("G6PD_LOVD.xlsx", sheet=3,  colNames=TRUE)

Diseases$created_date<-convertToDateTime(Diseases$created_date)
Diseases$edited_date<-convertToDateTime(Diseases$edited_date)


a<-names(Diseases)

for (n in a[c(-10,-12)]){
  Diseases[[n]]<-as.factor(as.character(Diseases[[n]]))
}

Genes_to_Diseases<-read.xlsx("G6PD_LOVD.xlsx", sheet=4,  colNames=TRUE)

for (n in names(Genes_to_Diseases)){
  Genes_to_Diseases[[n]]<-as.factor(as.character(Genes_to_Diseases[[n]]))
}


Individuals<-read.xlsx("G6PD_LOVD.xlsx", sheet=5,  colNames=TRUE)

for (n in names(Individuals)){
  Individuals[[n]]<-as.factor(as.character(Individuals[[n]]))
}


Individuals_to_Diseases<-read.xlsx("G6PD_LOVD.xlsx", sheet=6,  colNames=TRUE)

for (n in names(Individuals_to_Diseases)){
  Individuals_to_Diseases[[n]]<-as.factor(as.character(Individuals_to_Diseases[[n]]))
}


Phenotypes<-read.xlsx("G6PD_LOVD.xlsx", sheet=7,  colNames=TRUE)

for (n in names(Phenotypes)){
  Phenotypes[[n]]<-as.factor(as.character(Phenotypes[[n]]))
}

Screenings<-read.xlsx("G6PD_LOVD.xlsx", sheet=8,  colNames=TRUE)
Screenings$created_date<-convertToDateTime(Screenings$created_date)
Screenings$edited_date<-convertToDateTime(Screenings$edited_date)

a<-names(Screenings)

for (n in a[c(-6,-8)]){
  Screenings[[n]]<-as.factor(as.character(Screenings[[n]]))
}


Screenings_to_genes<-read.xlsx("G6PD_LOVD.xlsx", sheet=9,  colNames=TRUE)

for (n in names(Screenings_to_genes)){
  Screenings_to_genes[[n]]<-as.factor(as.character(Screenings_to_genes[[n]]))
}

Variants_on_genome<-read.xlsx("G6PD_LOVD.xlsx", sheet=10,  colNames=TRUE)

a<-names(Variants_on_genome)

for (n in a[c(-5,-6,-8,-12)]){
  Variants_on_genome[[n]]<-as.factor(as.character(Variants_on_genome[[n]]))
}


Variants_on_transcripts<-read.xlsx("G6PD_LOVD.xlsx", sheet=11,  colNames=TRUE)

a<-names(Variants_on_transcripts)

for (n in a[c(-4:-7)]){
  Variants_on_transcripts[[n]]<-as.factor(as.character(Variants_on_transcripts[[n]]))
}


Screenings_to_variants<-read.xlsx("G6PD_LOVD.xlsx", sheet=12,  colNames=TRUE)

for (n in names(Screenings_to_variants)){
  Screenings_to_variants[[n]]<-as.factor(as.character(Screenings_to_variants[[n]]))
}


G6PD_LOVD<-list("Genes"=Genes,
                "Transcripts"=Transcripts,
                "Diseases"=Diseases,
                "Genes_to_Diseases"=Genes_to_Diseases,
                "Individuals"=Individuals,
                "Individuals_to_Diseases"=Individuals_to_Diseases,
                "Phenotypes"=Phenotypes,
                "Screenings"=Screenings,
                "Screenings_to_genes"=Screenings_to_genes,
                "Variants_on_genome"=Variants_on_genome,
                "Variants_on_transcripts"=Variants_on_transcripts,
                "Screenings_to_variants"=Screenings_to_variants)

a<-ls()

rm(list = a[-3])


# Rename id variables ----
names(G6PD_LOVD$Genes)<-c("geneid",names(G6PD_LOVD$Genes)[-1])

names(G6PD_LOVD$Transcripts)<-c("transcriptid",names(G6PD_LOVD$Transcripts)[-1])

names(G6PD_LOVD$Diseases)<-c("diseaseid",names(G6PD_LOVD$Diseases)[-1])

names(G6PD_LOVD$Individuals)<-c("individualid",names(G6PD_LOVD$Individuals)[-1])

names(G6PD_LOVD$Phenotypes)<-c("phenotypeid",names(G6PD_LOVD$Phenotypes)[-1])

names(G6PD_LOVD$Screenings)<-c("screeningid",names(G6PD_LOVD$Screenings)[-1])

names(G6PD_LOVD$Variants_on_genome)<-c("variantid",names(G6PD_LOVD$Variants_on_genome)[-1])

names(G6PD_LOVD$Variants_on_transcripts)<-c("variantid",names(G6PD_LOVD$Variants_on_transcripts)[-1])

# Create a long data.frame with all records and variables----

# Merge Individuals records (10462x17) with Phenotypes records (10449x19)

nlevels(G6PD_LOVD$Individuals$individualid)
summary(G6PD_LOVD$Individuals)
summary(G6PD_LOVD$Individuals[c(-2:-6,-8,-10,-13,-14,-17)])

nlevels(G6PD_LOVD$Phenotypes$individualid)
summary(G6PD_LOVD$Phenotypes)
summary(G6PD_LOVD$Phenotypes[c(-4:-5,-9,-11,-13:-17,-19)])


G6PD_LOVD_df<-merge(G6PD_LOVD$Individuals[c(-2:-6,-8,-10,-13,-14,-17)],
                    G6PD_LOVD$Phenotypes[c(-4:-5,-9,-11,-13:-17,-19)],
                    by = "individualid", all = TRUE)

# Merge with Screenings (10462x12)

G6PD_LOVD_df<-merge(G6PD_LOVD_df,G6PD_LOVD$Screenings[c(1,2)], by = "individualid", all = TRUE)

# Merge with Screenings to variants (10860x2)

G6PD_LOVD_df<-merge(G6PD_LOVD_df,G6PD_LOVD$Screenings_to_variants, by = "screeningid", all = TRUE)

# Merge with Variants to genes (1080x2)

G6PD_LOVD_df<-merge(G6PD_LOVD_df,G6PD_LOVD$Screenings_to_genes, by = "screeningid", all = TRUE)

# Merge with Variants on genome (11031x24)

G6PD_LOVD_df<-merge(G6PD_LOVD_df,
                    G6PD_LOVD$Variants_on_genome[c(-2,-4,-8,-9,-12,-15,-17,-18,-19,-20,-21,-22,-24)],
                    by = "variantid", all = TRUE)

# Merge with Variants on Transcripts (22061x14)

G6PD_LOVD_df<-merge(G6PD_LOVD_df,G6PD_LOVD$Variants_on_transcripts[-9], by = "variantid", all = TRUE)

# Adding variables----

# G6PD activity & G6P/6PG ratio----

G6PD_LOVD_df[["G6PD activity"]]<-NA
G6PD_LOVD_df[["G6P/6PG ratio"]]<-NA

G6PD_LOVD_df[["G6PD activity"]]<-as.numeric(gsub(".*?[[:alpha:]] ","",gsub(", .*[[:alnum:]]", "", as.character(G6PD_LOVD_df[["Phenotype/Protein"]]))))
G6PD_LOVD_df[["G6P/6PG ratio"]]<-as.numeric(gsub(".*?[[:alpha:]] ", "", as.character(G6PD_LOVD_df[["Phenotype/Protein"]])))

# Reduce observations (just observations of transcript 2 "transcriptid"==25458) ----
G6PD_LOVD_df<-G6PD_LOVD_df[G6PD_LOVD_df$transcriptid=="25458",]
G6PD_LOVD_df$transcriptid<-as.factor(as.character(G6PD_LOVD_df$transcriptid))

# Keep only mutations in G6PD gene ----
G6PD_LOVD_df$DBID<-as.factor(substr(G6PD_LOVD_df$`VariantOnGenome/DBID`,start = 1,stop = 4))
G6PD_LOVD_df<-rbind(G6PD_LOVD_df[G6PD_LOVD_df$DBID=="G6PD",],G6PD_LOVD_df[G6PD_LOVD_df$DBID!="G6PD" & G6PD_LOVD_df$geneid=="G6PD" & !is.na(G6PD_LOVD_df$geneid),])
G6PD_LOVD_df$geneid<-as.factor(as.character(G6PD_LOVD_df$geneid))
G6PD_LOVD_df$DBID<-as.factor(as.character(G6PD_LOVD_df$DBID))

# Haplotypes----

# 191 Known haplotypes
G6PD_LOVD_df$`VariantOnTranscript/Haplotype`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`))
# 221 cDNA Variants
G6PD_LOVD_df$`VariantOnTranscript/DNA`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/DNA`))
# 206 aa Variants
G6PD_LOVD_df$`VariantOnTranscript/Protein`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/Protein`))

G6PD_LOVD_df$DNA.Haplotype<-NA
G6PD_LOVD_df$aa.Haplotype<-NA

# Haplotypes from tested individuals
a<-G6PD_LOVD_df[!is.na(G6PD_LOVD_df$individualid),]
a$`VariantOnTranscript/Haplotype`<-as.factor(as.character(a$`VariantOnTranscript/Haplotype`))
nlevels(a$`VariantOnTranscript/Haplotype`) # 181/191 known haplotypes

a_levels<-levels(a$`VariantOnTranscript/Haplotype`)

a$`VariantOnTranscript/DNA`<-as.factor(as.character(a$`VariantOnTranscript/DNA`))
nlevels(a$`VariantOnTranscript/DNA`) # 212/221 cDNA variants

a$`VariantOnTranscript/Protein`<-as.factor(as.character(a$`VariantOnTranscript/Protein`))
nlevels(a$`VariantOnTranscript/Protein`) # 204/206 aa variants


# Identify the number of cDNA variants per tested individual ----
a$individualid<-as.factor(as.character(a$individualid))

a[["IndRep"]]<-NA

for (n in levels(a$individualid)) {
  a[a$individualid==n,][["IndRep"]]<-length(a[a$individualid==n,][["individualid"]])
}

a$IndRep<-as.factor(a$IndRep)
summary(a$IndRep) # 10064 individuals with 1 variant, 392 with 2, and 3 individuals with 3 cDNA variants

# Define DNA.Haplotype and aa.Haplotype (cDNA variant and aminoacid variant combination within individuals)

for(n in levels(a$individualid)){
  a[a$individualid==n,][["DNA.Haplotype"]]<-paste0(a[a$individualid==n,][["VariantOnTranscript/DNA"]], collapse = " ")
  a[a$individualid==n,][["aa.Haplotype"]]<-paste0(a[a$individualid==n,][["VariantOnTranscript/Protein"]], collapse = " ")
}

a$DNA.Haplotype<-as.factor(a$DNA.Haplotype)
nlevels(a$DNA.Haplotype) # 245 DNA.Haplotypes

a$aa.Haplotype<-as.factor(a$aa.Haplotype)
nlevels(a$aa.Haplotype) # 239 aa.Haplotypes (48 additional Haplotyes from the previous 191 known haplotypes)
levels(a$aa.Haplotype)

sort(levels(a$aa.Haplotype))

# Haplotypes from previous databases (individuals and screenings ID's missed)
b<-G6PD_LOVD_df[is.na(G6PD_LOVD_df$individualid),] # haplotypes from other database (VKGL)
b$`VariantOnTranscript/Haplotype`<-as.factor(as.character(b$`VariantOnTranscript/Haplotype`))
nlevels(b$`VariantOnTranscript/Haplotype`)# 81/191 known haplotypes, 10 variants not present in "a" data.frame

b$`VariantOnTranscript/DNA`<-as.factor(as.character(b$`VariantOnTranscript/DNA`))
nlevels(b$`VariantOnTranscript/DNA`) # 117/221 cDNA variants, 9 not present in "a" data.frame

b$`VariantOnTranscript/Protein`<-as.factor(as.character(b$`VariantOnTranscript/Protein`))
nlevels(b$`VariantOnTranscript/Protein`) # 109/206 aa variants, 2 not present in "a" data.frame

b_levels<-levels(b$`VariantOnTranscript/Haplotype`)

sh.haplotypes<-b_levels[(b_levels %in% a_levels)]# Share haplotypes between a and b

un.haplotypes<-b_levels[!(b_levels %in% a_levels)]# unique haplotypes in b

b$IndRep<-NA

# Define DNA.Haplotype and aa.Haplotype for records that do not have `VariantOnTranscript/Haplotype` information

b[is.na(b$`VariantOnTranscript/Haplotype`),][["DNA.Haplotype"]]<-as.character(b[is.na(b$`VariantOnTranscript/Haplotype`),][["VariantOnTranscript/DNA"]])
b[is.na(b$`VariantOnTranscript/Haplotype`),][["aa.Haplotype"]]<-as.character(b[is.na(b$`VariantOnTranscript/Haplotype`),][["VariantOnTranscript/Protein"]])
b[is.na(b$`VariantOnTranscript/Haplotype`),][["IndRep"]]<-1

# Define DNA.Haplotype and aa.Haplotype for records with `VariantOnTranscript/Haplotype` information not present in a data.frame

b[b$`VariantOnTranscript/Haplotype`%in%un.haplotypes,][["DNA.Haplotype"]]<-as.character(b[b$`VariantOnTranscript/Haplotype`%in%un.haplotypes,][["VariantOnTranscript/DNA"]])
b[b$`VariantOnTranscript/Haplotype`%in%un.haplotypes,][["aa.Haplotype"]]<-as.character(b[b$`VariantOnTranscript/Haplotype`%in%un.haplotypes,][["VariantOnTranscript/Protein"]])
b[b$`VariantOnTranscript/Haplotype`%in%un.haplotypes,][["IndRep"]]<-1

# Define DNA.Haplotype and aa.Haplotype for records with `VariantOnTranscript/Haplotype` information present in a data.frame

for(n in sh.haplotypes){
  b[b$`VariantOnTranscript/Haplotype`==n&!is.na(b$`VariantOnTranscript/Haplotype`),][["DNA.Haplotype"]]<-as.character(a[a$`VariantOnTranscript/Haplotype`==n&!is.na(a$`VariantOnTranscript/Haplotype`),][1,"DNA.Haplotype"])
  b[b$`VariantOnTranscript/Haplotype`==n&!is.na(b$`VariantOnTranscript/Haplotype`),][["aa.Haplotype"]]<-as.character(a[a$`VariantOnTranscript/Haplotype`==n&!is.na(a$`VariantOnTranscript/Haplotype`),][1,"aa.Haplotype"])
  b[b$`VariantOnTranscript/Haplotype`==n&!is.na(b$`VariantOnTranscript/Haplotype`),][["IndRep"]]<-as.character(a[a$`VariantOnTranscript/Haplotype`==n&!is.na(a$`VariantOnTranscript/Haplotype`),][1,"IndRep"])
}

# Combine a and b in G6PD_LOVD_df

G6PD_LOVD_df<-rbind(a,b)

rm(list=c("a","b","a_levels", "b_levels", "n", "sh.haplotypes", "un.haplotypes"))

nlevels(G6PD_LOVD_df$aa.Haplotype)# 240 Haplotypes (49 additional Haplotyes from the previous 191 known haplotypes), "p.(=)" and "p.(=) p.(=)" won't be count as haplotypes as they produce the same protein

# Known haplotypes----

nlevels(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`)

d<-G6PD_LOVD_df[!is.na(G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]),]

# Generate a variable that counts the number of different aa.Haplotyps per kown haplotype ("VariantOnTranscript/Haplotype")
# Every "VariantOnTranscript/Haplotype" must have just one aa.Haplotype

d[["VarRep"]]<-NA

for (n in levels(d$`VariantOnTranscript/Haplotype`)) {
  d[d$`VariantOnTranscript/Haplotype`==n,][["VarRep"]]<-nlevels(as.factor(as.character(d[d$`VariantOnTranscript/Haplotype`==n,][["aa.Haplotype"]])))
}

d$VarRep<-as.factor(d$VarRep)

table(d$VarRep) # 6 records have 2 aa.Haplotypes per known haplotype ("VariantOnTranscript/Haplotype")

nlevels(as.factor(as.character(d[d[["VarRep"]]==2,][["VariantOnTranscript/Haplotype"]])))
levels(as.factor(as.character(d[d[["VarRep"]]==2,][["VariantOnTranscript/Haplotype"]]))) # 2 "VariantOnTranscript/Haplotype": "G6PD-Nara" and "G6PD-Santamaria"

nlevels(as.factor(as.character(d[d[["VarRep"]]==2,][["aa.Haplotype"]])))
levels(as.factor(as.character(d[d[["VarRep"]]==2,][["aa.Haplotype"]])))# 4 aa.Haplotype: "p.(Asn126Asp) p.(Asp181Val)", "p.(Asp181Val) p.(Asn126Asp)", "p.(Lys320_Thr327del)" and "p.(Ser189del)"


View(d[d[["VarRep"]]==2,])


d$`VariantOnTranscript/Haplotype`<-as.character(d$`VariantOnTranscript/Haplotype`)

# G6PD-Nara haplotype was incorrectly assigned to the "p.(Ser189del)" aa.Haplotype, the correct name is "G6PD-Tsukui"
d[!is.na(d$aa.Haplotype)&d$aa.Haplotype=="p.(Ser189del)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Tsukui"

# In "G6PD-Santamaria" Haplotype "p.(Asn126Asp) p.(Asp181Val)" and "p.(Asp181Val) p.(Asn126Asp)" are the same but in the incorrect order

d[d$`VariantOnTranscript/Haplotype`=="G6PD-Santamaria",][["aa.Haplotype"]]<-"p.(Asn126Asp) p.(Asp181Val)"
d[d$`VariantOnTranscript/Haplotype`=="G6PD-Santamaria",][["DNA.Haplotype"]]<-"c.376A>G c.542A>T"

d[["VarRep"]]<-NULL

e<-G6PD_LOVD_df[is.na(G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]),]

G6PD_LOVD_df<-rbind(d,e)

rm(list = c("d","e","n"))

G6PD_LOVD_df$`VariantOnTranscript/Haplotype`<-as.factor(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`)
nlevels(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`) # Now there are 192 Known Haplotypes

d<-G6PD_LOVD_df[!is.na(G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]),]

# Generate a variable that counts the number of different aa.Haplotyps per kown haplotype ("VariantOnTranscript/Haplotype")
# Every "VariantOnTranscript/Haplotype" must have just one aa.Haplotype

d[["VarRep"]]<-NA

d$aa.Haplotype<-as.factor(as.character(d$aa.Haplotype))

for (n in levels(d$aa.Haplotype)) {
  d[d$aa.Haplotype==n,][["VarRep"]]<-nlevels(as.factor(as.character(d[d$aa.Haplotype==n,][["VariantOnTranscript/Haplotype"]])))
}

d$VarRep<-as.factor(d$VarRep)

table(d$VarRep) # 6 records have 2 aa.Haplotypes per known haplotype ("VariantOnTranscript/Haplotype")

nlevels(as.factor(as.character(d[d[["VarRep"]]==2,][["VariantOnTranscript/Haplotype"]])))
levels(as.factor(as.character(d[d[["VarRep"]]==2,][["VariantOnTranscript/Haplotype"]]))) # 2 "VariantOnTranscript/Haplotype": "G6PD-Nara" and "G6PD-Santamaria"

nlevels(as.factor(as.character(d[d[["VarRep"]]==2,][["aa.Haplotype"]])))
levels(as.factor(as.character(d[d[["VarRep"]]==2,][["aa.Haplotype"]])))# 4 aa.Haplotype: "p.(Asn126Asp) p.(Asp181Val)", "p.(Asp181Val) p.(Asn126Asp)", "p.(Lys320_Thr327del)" and "p.(Ser189del)"

View(d[d[["VarRep"]]==2,])

# Haplotype correction ----

d[["VariantOnTranscript/Haplotype"]]<-as.character(d[["VariantOnTranscript/Haplotype"]])

# "p.(Arg393His)"<-G6PD-Nashville, Anaheim, Portici (c.1178G>A) | G6PD Nashville (c.1178G>A)
# The correct names is G6PD-Nashville, Anaheim, Portici

d[d[["aa.Haplotype"]]=="p.(Arg393His)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Nashville, Anaheim, Portici"

# "p.(Arg463Cys)"<-G6PD-Kamiube, Keelung (c.1387C>T) | G6PD-Kamiube (c.1387C>T)
# The correct name is G6PD-Kamiube, Keelung

d[d[["aa.Haplotype"]]=="p.(Arg463Cys)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Kamiube, Keelung"

# "p.(Asn165Asp)"<-G6PD-Taipei, Chinese-3 (c.493A>G) | G6PD-Chinese-3 (c.493A>G)
# The correct name is G6PD-Taipei, Chinese-3

d[d[["aa.Haplotype"]]=="p.(Asn165Asp)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Taipei, Chinese-3"

# "p.(Asn363Lys)"<-G6PD-Loma Linda (c.1089C>A) | G6PD-Aachen (c.1089C>G) | G6PD-Loma Linda (c.1089C>A)
# G6PD-Loma Linda and G6PD-Aachen are different Haplotypes defined by their DNA sequence and should be taken into account later
# This code should be used to identify G6PD-Loma Linda (c.1089C>A) and G6PD-Aachen (c.1089C>G) respectively
d[d[["aa.Haplotype"]]=="p.(Asn363Lys)"&d[["DNA.Haplotype"]]=="c.1089C>A",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Loma Linda"
d[d[["aa.Haplotype"]]=="p.(Asn363Lys)"&d[["DNA.Haplotype"]]=="c.1089C>G",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Aachen"

# "p.(Asp350His)"<-G6PD-Mira d\'Aire (c.1048G>C) | G6PD-Mira d' Aire (c.1048G>C)
# The correct name is G6PD-Mira d' Aire

d[d[["aa.Haplotype"]]=="p.(Asp350His)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Mira d' Aire"

# "p.(Cys385Trp)"<-G6PD-Madrid (c.1155C>G) | G6PD-Girona (c.1155C>G)
# G6PD-Madrid and G6PD-Girona are different Haplotypes defined by their Restriction site (no reported in G6PD-Girona) and should be confirmed in the sequence taken into account later. Moreover Girona is nt mentioned in previous databases publications
# If difference in restriction site is confirmed, this code should used to indentify these haplotypes
# According to their "VariantInGenome/DBID" they are the same haplotype
# d[d[["aa.Haplotype"]]=="p.(Cys385Trp)"&d[["VariantOnGenome/Restriction_site"]]=="HpyCH4V-"&!is.na(d[["VariantOnGenome/Restriction_site"]]),][["VariantOnTranscript/Haplotype"]]<-"G6PD-Madrid"
# d[d[["aa.Haplotype"]]=="p.(Cys385Trp)"&is.na(d[["VariantOnGenome/Restriction_site"]]),][["VariantOnTranscript/Haplotype"]]<-"G6PD-Girona"

# else use this:

d[d[["aa.Haplotype"]]=="p.(Cys385Trp)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Madrid, Girona"

# "p.(Glu389Gly)"<-G6PD-Praha (c.1166A>G) | G6PD-Praba (c.1166A>G)
# The correct name is G6PD-Praha

d[d[["aa.Haplotype"]]=="p.(Glu389Gly)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Praha"

# "p.(Glu416Lys)"<-G6PD-Tokyo, Fukushima (c.1246G>A) | G6PD-Tokyo (c.1246G>A)
# The correct name is G6PD-Tokyo, Fukushima

d[d[["aa.Haplotype"]]=="p.(Glu416Lys)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Tokyo, Fukushima"

# "p.(Gly131Val)"<-G6PD-Quing Yan (c.392G>T) | G6PD-Chinese-4 (c.392G>T)
# The correct name is G6PD-Quing Yan, Chinese-4 is not mentioned in previous databases
# According to their "VariantInGenome/DBID" they are the same haplotype

d[d[["aa.Haplotype"]]=="p.(Gly131Val)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Quing Yan, Chinese-4"

# "p.(Gly447Arg)"<-G6PD-Santiago de Cuba, Morioka (c.1339G>A) | G6PD-Santiago de Cuba (c.1339G>A)
# The correct name is G6PD-Santiago de Cuba, Morioka

d[d[["aa.Haplotype"]]=="p.(Gly447Arg)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Santiago de Cuba, Morioka"

# "p.(Phe173Leu)"<-G6PD-Nankang (c.517T>C) | G6PD-Miaoli (c.519C>G)
# G6PD-Nankang and G6PD-Miaoli are different Haplotypes defined by their DNA sequence and should be taken into account later
# This code should be used to identify G6PD-Nankang (c.517T>C) and G6PD-Miaoli (c.519C>G) respectively

d[d[["aa.Haplotype"]]=="p.(Phe173Leu)"&d[["DNA.Haplotype"]]=="c.517T>C",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Nankang"
d[d[["aa.Haplotype"]]=="p.(Phe173Leu)"&d[["DNA.Haplotype"]]=="c.519C>G",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Miaoli"

# "p.(Tyr70Cys)" <-G6PD-Murcia Oristano (c.209A>G) | G6PD-Murcia (c.209A>G)
# The correct name is G6PD-Murcia Oristano

d[d[["aa.Haplotype"]]=="p.(Tyr70Cys)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Murcia Oristano"

# "p.(Tyr70His)" <-G6PD-Namouru (c.208T>C) | G6PD-Namouru (c.208T>C) | G6PD-Namoru (c.208T>C)
# The correct name is G6PD-Namouru

d[d[["aa.Haplotype"]]=="p.(Tyr70His)",][["VariantOnTranscript/Haplotype"]]<-"G6PD-Namouru"

# Classify the others aa.Haplotypes ----

e<-G6PD_LOVD_df[is.na(G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]),]

d$DNA.Haplotype<-as.factor(as.character(d$DNA.Haplotype))
d$aa.Haplotype<-as.factor(as.character(d$aa.Haplotype))
d$`VariantOnTranscript/Haplotype`<-as.factor(as.character(d$`VariantOnTranscript/Haplotype`))

e$DNA.Haplotype<-as.factor(as.character(e$DNA.Haplotype))
e$aa.Haplotype<-as.factor(as.character(e$aa.Haplotype))
e$`VariantOnTranscript/Haplotype`<-as.factor(as.character(e$`VariantOnTranscript/Haplotype`))

nlevels(d$DNA.Haplotype)
nlevels(d$aa.Haplotype)
nlevels(d$`VariantOnTranscript/Haplotype`)

nlevels(e$DNA.Haplotype)
nlevels(e$aa.Haplotype)
nlevels(e$`VariantOnTranscript/Haplotype`)

d$DNA.Haplotype<-(as.character(d$DNA.Haplotype))
d$aa.Haplotype<-(as.character(d$aa.Haplotype))
d$`VariantOnTranscript/Haplotype`<-(as.character(d$`VariantOnTranscript/Haplotype`))

e$DNA.Haplotype<-(as.character(e$DNA.Haplotype))
e$aa.Haplotype<-(as.character(e$aa.Haplotype))
e$`VariantOnTranscript/Haplotype`<-(as.character(e$`VariantOnTranscript/Haplotype`))

for (n in levels(as.factor(as.character(d$aa.Haplotype)))){

  for (m in levels(as.factor(as.character(d[d[["aa.Haplotype"]]==n,][["DNA.Haplotype"]])))){
    if (length(e[e[["aa.Haplotype"]]==n&e[["DNA.Haplotype"]]==m,][["VariantOnTranscript/Haplotype"]])>0){
      e[e[["aa.Haplotype"]]==n&e[["DNA.Haplotype"]]==m,][["VariantOnTranscript/Haplotype"]] <- d[d[["aa.Haplotype"]]==n&d[["DNA.Haplotype"]]==m,][1,"VariantOnTranscript/Haplotype"]    
    }
    }
}

# New Haplotypes----

f<-e[is.na(e$`VariantOnTranscript/Haplotype`),]
nlevels(as.factor(as.character(f$aa.Haplotype))) # 63 aa.Haplotypes
nlevels(as.factor(as.character(f$DNA.Haplotype))) # 73 DNA.Haplotypes

# Synonyms and Intronic  mutations----

n="p.(=)"

f[f[["aa.Haplotype"]]==n,][,c("DNA.Haplotype","VariantOnTranscript/Exon")]

# Manually assing Exon and introns for missing data

f$`VariantOnTranscript/Exon`<-as.character(f$`VariantOnTranscript/Exon`)

f[f[["DNA.Haplotype"]]=="c.1365-13T>C",][["VariantOnTranscript/Exon"]]<-"11i"
f[f[["DNA.Haplotype"]]=="c.486-60C>G",][["VariantOnTranscript/Exon"]]<-"5i"

# doble mutants
n="p.(=) p.(=)"

f[f[["aa.Haplotype"]]==n,][,c("DNA.Haplotype","VariantOnTranscript/Exon")]

# Manually assing Exon and introns for missing data

f$`VariantOnTranscript/Exon`<-as.character(f$`VariantOnTranscript/Exon`)

f[f[["DNA.Haplotype"]]=="c.1311C>T c.1365-13T>C",][["VariantOnTranscript/Exon"]]<-"11 11i"

# Define Synonyms and Intronic Haplotypes

n="p.(=)"

data0<-data.frame(f[f[["aa.Haplotype"]]==n,][,c("DNA.Haplotype","VariantOnTranscript/Exon")])
# Synonyms
data1<-data.frame("DNA.Haplotype"=c("c.822G>A", "c.957C>T", "c.1101C>T", "c.1245C>T", "c.1311C>T"), "VariantOnTranscript/Haplotype"=paste0(rep("Synonym", 5), sep = " ", 1:5))

# Intronic
data2<-data.frame("DNA.Haplotype"=c("c.486-60C>G", "c.1365-13T>C", "c.1458-13C>G", "c.120+3625G>A", "c.120+3754C>T"), "VariantOnTranscript/Haplotype"=paste0(rep("Intronic", 5), sep = " ", 1:5))

data0[["VariantOnTranscript/Haplotype"]]<-NA

data1$VariantOnTranscript.Haplotype<-as.character(data1$VariantOnTranscript.Haplotype)

for (n in levels(as.factor(data1$DNA.Haplotype))) {
  data0[data0$DNA.Haplotype==n,][["VariantOnTranscript/Haplotype"]]<-data1[data1$DNA.Haplotype==n,][["VariantOnTranscript.Haplotype"]]
}

data2$VariantOnTranscript.Haplotype<-as.character(data2$VariantOnTranscript.Haplotype)

for (n in levels(as.factor(data2$DNA.Haplotype))) {
  data0[data0$DNA.Haplotype==n,][["VariantOnTranscript/Haplotype"]]<-data2[data2$DNA.Haplotype==n,][["VariantOnTranscript.Haplotype"]]
}

# Samples with double Syn/Int mutations: Just one Haplotype detected
n="p.(=) p.(=)"

data3<-data.frame(f[f[["aa.Haplotype"]]==n,][,c("DNA.Haplotype","VariantOnTranscript/Exon")],"VariantOnTranscript/Haplotype"="Syn5 Int2")

names(data0)<-c("DNA.Haplotype", "VariantOnTranscript/Exon", "VariantOnTranscript/Haplotype")
names(data3)<-c("DNA.Haplotype", "VariantOnTranscript/Exon", "VariantOnTranscript/Haplotype")

data0<-rbind(data0,data3)

# DNA.Haplotypes with unknown consquences
n="p.?"

data4<-data.frame(f[f[["aa.Haplotype"]]==n,][,c("DNA.Haplotype","VariantOnTranscript/Exon")],"VariantOnTranscript/Haplotype"=c("Intronic 6","Intronic 7"))

names(data4)<-c("DNA.Haplotype", "VariantOnTranscript/Exon", "VariantOnTranscript/Haplotype")

data0<-rbind(data0,data4)

data0$`VariantOnTranscript/Haplotype`<-paste0("G6PD-No name", sep= " ", data0$`VariantOnTranscript/Haplotype`)

# Add new Synonym and Intronic Haplotypes to f data.frame

for (n in levels(as.factor(data0$DNA.Haplotype))){
  f[f[["DNA.Haplotype"]]==n,][["VariantOnTranscript/Exon"]]<-data0[data0[["DNA.Haplotype"]]==n,][1,"VariantOnTranscript/Exon"]
  f[f[["DNA.Haplotype"]]==n,][["VariantOnTranscript/Haplotype"]]<-data0[data0[["DNA.Haplotype"]]==n,][1,"VariantOnTranscript/Haplotype"]
  }


# Non Synonyms  mutations----

# Manually modify the order of the mutations of this DNA.Haplotype "p.(Arg227Leu) p.(Asn126Asp)" in f 

f[f[["aa.Haplotype"]]=="p.(Arg227Leu) p.(Asn126Asp)",][["DNA.Haplotype"]]<-"c.376A>G c.680G>T"
f[f[["aa.Haplotype"]]=="p.(Arg227Leu) p.(Asn126Asp)",][["VariantOnTranscript/Exon"]]<-"5 7"
f[f[["aa.Haplotype"]]=="p.(Arg227Leu) p.(Asn126Asp)",][["aa.Haplotype"]]<-"p.(Asn126Asp) p.(Arg227Leu)"


data1<-(f[is.na(f[["VariantOnTranscript/Haplotype"]]),][,c("position_c_start","aa.Haplotype","DNA.Haplotype","VariantOnTranscript/Exon")])

# Define No Synonyms haplotyes
data2<-data.frame("aa.Haplotype"=levels(as.factor(data1$aa.Haplotype)))
data2$aa.Haplotype<-as.character(data2$aa.Haplotype)

data2$position_c_start<-NA
data2$DNA.Haplotype<-NA

for (n in levels(as.factor(data2$aa.Haplotype))){
  data2[data2$aa.Haplotype==n,][["position_c_start"]]<-data1[data1$aa.Haplotype==n,][1,"position_c_start"]
  data2[data2$aa.Haplotype==n,][["DNA.Haplotype"]]<-data1[data1$aa.Haplotype==n,][1,"DNA.Haplotype"]
  }

data2[data2$aa.Haplotype=="p.(Gly131Val)",][["position_c_start"]]<-392
data2[data2$aa.Haplotype=="p.(Gly131Val)",][["DNA.Haplotype"]]<-"c.392G>T"

data2<-data2[order(data2$position_c_start),]

data2[["VariantOnTranscript/Haplotype"]]<-paste0("G6PD-No name, No Synonym", sep = " ", 1:60)

# Add new No Synonym Haplotypes to f

for (n in levels(as.factor(data2$aa.Haplotype))){
  f[f[["aa.Haplotype"]]==n,][["DNA.Haplotype"]]<-data2[data2[["aa.Haplotype"]]==n,][1,"DNA.Haplotype"]
  f[f[["aa.Haplotype"]]==n,][["VariantOnTranscript/Haplotype"]]<-data2[data2[["aa.Haplotype"]]==n,][1,"VariantOnTranscript/Haplotype"]
}


g<-e[!is.na(e$`VariantOnTranscript/Haplotype`),]

names(d)
names(f)
names(g)

d$VarRep<-NULL

d$Reported<-"old"
g$Reported<-"old"
f$Reported<-"new"

G6PD_LOVD_df<-rbind(d,g,f)

nlevels(as.factor(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`))
nlevels(as.factor(G6PD_LOVD_df[G6PD_LOVD_df[["Reported"]]=="old",][["VariantOnTranscript/Haplotype"]]))
nlevels(as.factor(G6PD_LOVD_df[G6PD_LOVD_df[["Reported"]]=="new",][["VariantOnTranscript/Haplotype"]]))

a<-ls()

rm(list=a[-10:-12])# Modify before to run
rm(a)

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


rm(list = c("a", "b","n"))

sum(table(G6PD_LOVD_df$WHO.Classification)) # 10174 records (180 Haplotypes) has WHO.Classification information

summary(as.factor(G6PD_LOVD_df$WHO.Classification)) # 820 records (74) lacks WHO.Classification information

nlevels(as.factor(as.character(G6PD_LOVD_df[is.na(G6PD_LOVD_df$WHO.Classification),][["VariantOnTranscript/Haplotype"]])))
levels(as.factor(as.character(G6PD_LOVD_df[is.na(G6PD_LOVD_df$WHO.Classification),][["VariantOnTranscript/Haplotype"]])))

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
                              paste0(levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Individual/Origin/Geographic"]]))),collapse=" "), 
                              levels(as.factor(as.character(G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnTranscript/Haplotype"]]==n,][["Individual/Origin/Geographic"]]))))
  
  g6pd.dna.hap_df<-rbind(g6pd.dna.hap_df,temp)
}

g6pd.dna.hap_df

#g6pd.aa.hap_df----

g6pd.aa.hap_df<-NULL


for (n in levels(as.factor(G6PD_LOVD_df$aa.Haplotype))) {
  temp<-data.frame("Haplotype" = n)
  temp[["Haplotype"]] <- ifelse(length(levels(as.factor(as.character(
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
                              G6PD_LOVD_df[G6PD_LOVD_df[["aa.Haplotype"]]==n,][["Individual/Origin/Geographic"]]))),collapse=" "), 
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
                                G6PD_LOVD_df[G6PD_LOVD_df[["VariantOnGenome/DNA"]]==n,][["Individual/Origin/Geographic"]]))),collapse=" "), 
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

