# Haplotypes----

# 191 Known haplotypes
G6PD_LOVD_df$`VariantOnTranscript/Haplotype`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/Haplotype`))
# 221 cDNA Variants
G6PD_LOVD_df$`VariantOnTranscript/DNA`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/DNA`))
# 206 aa Variants
G6PD_LOVD_df$`VariantOnTranscript/Protein`<-as.factor(as.character(G6PD_LOVD_df$`VariantOnTranscript/Protein`))

G6PD_LOVD_df$DNA.Haplotype<-NA
G6PD_LOVD_df$aa.Haplotype<-NA

# Haplotypes from tested individuals----
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