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