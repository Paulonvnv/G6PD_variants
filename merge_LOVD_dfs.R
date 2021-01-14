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