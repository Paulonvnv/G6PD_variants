
# DNA.translate----

# Translate DNA fasta sequence into aa in 1 o 3 digit format
DNA.translate<-function(data=NULL, # vector containing DNA sequence in fasta format
                        start=1, # start position
                        end=NULL, # end position
                        digits=3){ # digit format, only 1 or 3 is allowed
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  
  temp2=NULL
  
  while(length(data1)>0){
    temp<-paste0(data1[1:3],collapse = "")
    temp2<-c(temp2,temp)
    data1=data1[-1:-3]}
  
  temp3<-NULL
  
  Aminoacids <- data.frame("Name" = c("Alanine", "Alanine", "Alanine", "Alanine", "Arginine", "Arginine", "Arginine",
                                      "Arginine", "Arginine", "Arginine", "Asparagine", "Asparagine", "Aspartic acid", "Aspartic acid",
                                      "Cysteine", "Cysteine", "Glutamine", "Glutamine", "Glutamic acid", "Glutamic acid", "Glycine",
                                      "Glycine", "Glycine", "Glycine", "Histidine", "Histidine", "Isoleucine", "Isoleucine", 
                                      "Isoleucine", "Leucine", "Leucine", "Leucine", "Leucine", "Leucine", "Leucine",
                                      "Lysine", "Lysine", "Methionine", "Phenylalanine", "Phenylalanine", "Proline", "Proline",
                                      "Proline", "Proline", "Serine", "Serine", "Serine", "Serine", "Serine", "Serine",
                                      "STOP", "STOP", "STOP", "Threonine", "Threonine", "Threonine", "Threonine", "Tryptophan",
                                      "Tyrosine", "Tyrosine", "Valine", "Valine", "Valine","Valine"),
                           "Name1" = c("A", "A", "A", "A", "R", "R", "R", "R",
                                       "R", "R", "N", "N", "D", "D", "C", "C",
                                       "Q", "Q", "E", "E", "G", "G", "G", "G", 
                                       "H", "H", "I", "I", "I", "L", "L", "L",
                                       "L", "L", "L", "K", "K", "M", "F" ,"F",
                                       "P", "P", "P", "P", "S", "S", "S", "S",
                                       "S", "S", "*", "*", "*", "T", "T", "T",
                                       "T", "W", "Y", "Y", "V", "V", "V", "V"),
                           "Name3" = c("Ala",  "Ala",  "Ala",  "Ala",  "Arg",  "Arg",  "Arg",  "Arg",  "Arg",  "Arg",
                                       "Asn",  "Asn",  "Asp",  "Asp",  "Cys",  "Cys",  "Gln",  "Gln",  "Glu",  "Glu",
                                       "Gly",  "Gly",  "Gly",  "Gly",  "His",  "His",  "Ile",  "Ile",  "Ile",  "Leu",
                                       "Leu",  "Leu",  "Leu",  "Leu",  "Leu",  "Lys",  "Lys",  "Met",  "Phe",  "Phe",
                                       "Pro",  "Pro",  "Pro",  "Pro",  "Ser",  "Ser",  "Ser",  "Ser",  "Ser",  "Ser",
                                       "STOP", "STOP", "STOP", "Thr",  "Thr",  "Thr",  "Thr",  "Trp",  "Tyr",  "Tyr",
                                       "Val",  "Val",  "Val",  "Val"),
                           "CodonRNA" = c("GCU", "GCC", "GCA", "GCG", "CGU", "AGA", "CGC", "CGA", 
                                          "CGG", "AGG", "AAC", "AAU", "GAC", "GAU", "UGU", "UGC",
                                          "CAG", "CAA", "GAA", "GAG", "GGC", "GGU", "GGA", "GGG",
                                          "CAC", "CAU", "AUU", "AUC", "AUA", "CUC", "CUU", "UUG",
                                          "CUA", "CUG", "UUA", "AAA", "AAG", "AUG", "UUC", "UUU",
                                          "CCG", "CCU", "CCC", "CCA", "UCG", "UCU", "UCC", "UCA",
                                          "AGU", "AGC", "UAA", "UGA", "UAG", "ACC", "ACA", "ACU",
                                          "ACG", "UGG", "UAC", "UAU", "GUG", "GUU", "GUC", "GUA"),
                           "CodonDNA" = c("GCT", "GCC", "GCA", "GCG", "CGT", "AGA", "CGC", "CGA",
                                          "CGG", "AGG", "AAC", "AAT", "GAC", "GAT", "TGT", "TGC", 
                                          "CAG", "CAA", "GAA", "GAG", "GGC", "GGT", "GGA", "GGG",
                                          "CAC", "CAT", "ATT", "ATC", "ATA", "CTC", "CTT", "TTG",
                                          "CTA", "CTG", "TTA", "AAA", "AAG", "ATG", "TTC", "TTT",
                                          "CCG", "CCT", "CCC", "CCA", "TCG", "TCT", "TCC", "TCA",
                                          "AGT", "AGC", "TAA", "TGA", "TAG", "ACC", "ACA", "ACT",
                                          "ACG", "TGG", "TAC", "TAT", "GTG", "GTT", "GTC", "GTA"))
  
  name<-paste0("Name",digits)
  
  for (n in 1:length(temp2)){
    temp4<-as.character(Aminoacids[Aminoacids[["CodonDNA"]]==temp2[n],][[name]])
    temp3<-c(temp3,temp4)}
  
  temp3 <- paste0(temp3,collapse = "")
  
  return(temp3)
}


# DNA.codons----

# Split DNA sequence in codons
DNA.codons<-function(data=NULL, # vector containing DNA sequence in fasta format
                     start=NULL, # start position
                     end=NULL){ # end position
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  temp2=NULL
  
  while(length(data1)>0){
    temp<-paste0(data1[1:3],collapse = "")
    temp2<-c(temp2,temp)
    data1=data1[-1:-3]
  }
  
  temp2 <- paste0(temp2, collapse =" ")
  
  return(temp2)
}


# get.aa----
# get the aminoacid for one specific codon

get.aa<-function(data=NULL, # vector containing DNA sequence in fasta format
                 start=NULL, # start cds position
                 end=NULL, # end cds position
                 codon=NULL, # position of the codon 
                 digits=3){ # aminoacid formar, only 1 or 3 allowed
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  temp2=NULL
  
  while(length(data1)>0){
    temp<-paste0(data1[1:3],collapse = "")
    temp2<-c(temp2,temp)
    data1=data1[-1:-3]}
  
  Aminoacids <- data.frame("Name" = c("Alanine", "Alanine", "Alanine", "Alanine", "Arginine", "Arginine", "Arginine",
                                      "Arginine", "Arginine", "Arginine", "Asparagine", "Asparagine", "Aspartic acid", "Aspartic acid",
                                      "Cysteine", "Cysteine", "Glutamine", "Glutamine", "Glutamic acid", "Glutamic acid", "Glycine",
                                      "Glycine", "Glycine", "Glycine", "Histidine", "Histidine", "Isoleucine", "Isoleucine", 
                                      "Isoleucine", "Leucine", "Leucine", "Leucine", "Leucine", "Leucine", "Leucine",
                                      "Lysine", "Lysine", "Methionine", "Phenylalanine", "Phenylalanine", "Proline", "Proline",
                                      "Proline", "Proline", "Serine", "Serine", "Serine", "Serine", "Serine", "Serine",
                                      "STOP", "STOP", "STOP", "Threonine", "Threonine", "Threonine", "Threonine", "Tryptophan",
                                      "Tyrosine", "Tyrosine", "Valine", "Valine", "Valine","Valine"),
                           "Name1" = c("A", "A", "A", "A", "R", "R", "R", "R",
                                       "R", "R", "N", "N", "D", "D", "C", "C",
                                       "Q", "Q", "E", "E", "G", "G", "G", "G", 
                                       "H", "H", "I", "I", "I", "L", "L", "L",
                                       "L", "L", "L", "K", "K", "M", "F" ,"F",
                                       "P", "P", "P", "P", "S", "S", "S", "S",
                                       "S", "S", "*", "*", "*", "T", "T", "T",
                                       "T", "W", "Y", "Y", "V", "V", "V", "V"),
                           "Name3" = c("Ala",  "Ala",  "Ala",  "Ala",  "Arg",  "Arg",  "Arg",  "Arg",  "Arg",  "Arg",
                                       "Asn",  "Asn",  "Asp",  "Asp",  "Cys",  "Cys",  "Gln",  "Gln",  "Glu",  "Glu",
                                       "Gly",  "Gly",  "Gly",  "Gly",  "His",  "His",  "Ile",  "Ile",  "Ile",  "Leu",
                                       "Leu",  "Leu",  "Leu",  "Leu",  "Leu",  "Lys",  "Lys",  "Met",  "Phe",  "Phe",
                                       "Pro",  "Pro",  "Pro",  "Pro",  "Ser",  "Ser",  "Ser",  "Ser",  "Ser",  "Ser",
                                       "STOP", "STOP", "STOP", "Thr",  "Thr",  "Thr",  "Thr",  "Trp",  "Tyr",  "Tyr",
                                       "Val",  "Val",  "Val",  "Val"),
                           "CodonRNA" = c("GCU", "GCC", "GCA", "GCG", "CGU", "AGA", "CGC", "CGA", 
                                          "CGG", "AGG", "AAC", "AAU", "GAC", "GAU", "UGU", "UGC",
                                          "CAG", "CAA", "GAA", "GAG", "GGC", "GGU", "GGA", "GGG",
                                          "CAC", "CAU", "AUU", "AUC", "AUA", "CUC", "CUU", "UUG",
                                          "CUA", "CUG", "UUA", "AAA", "AAG", "AUG", "UUC", "UUU",
                                          "CCG", "CCU", "CCC", "CCA", "UCG", "UCU", "UCC", "UCA",
                                          "AGU", "AGC", "UAA", "UGA", "UAG", "ACC", "ACA", "ACU",
                                          "ACG", "UGG", "UAC", "UAU", "GUG", "GUU", "GUC", "GUA"),
                           "CodonDNA" = c("GCT", "GCC", "GCA", "GCG", "CGT", "AGA", "CGC", "CGA",
                                          "CGG", "AGG", "AAC", "AAT", "GAC", "GAT", "TGT", "TGC", 
                                          "CAG", "CAA", "GAA", "GAG", "GGC", "GGT", "GGA", "GGG",
                                          "CAC", "CAT", "ATT", "ATC", "ATA", "CTC", "CTT", "TTG",
                                          "CTA", "CTG", "TTA", "AAA", "AAG", "ATG", "TTC", "TTT",
                                          "CCG", "CCT", "CCC", "CCA", "TCG", "TCT", "TCC", "TCA",
                                          "AGT", "AGC", "TAA", "TGA", "TAG", "ACC", "ACA", "ACT",
                                          "ACG", "TGG", "TAC", "TAT", "GTG", "GTT", "GTC", "GTA"))
  
  name<-paste0("Name",digits)
  
  temp3<-as.character(Aminoacids[Aminoacids[["CodonDNA"]]==temp2[codon],][[name]])
  
  
  return(temp3)}

# get.codon.seq----
# get the DNA sequence of an specific codon
get.codon.seq<-function(data=NULL, # vector containing DNA sequence in fasta format
                        start=NULL, # start cds position
                        end=NULL, # end cds position
                        codon=NULL){ # position of the codon
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  temp2=NULL
  
  while(length(data1)>0){
    temp<-paste0(data1[1:3],collapse = "")
    temp2<-c(temp2,temp)
    data1=data1[-1:-3]
  }
  
  temp2 <- temp2[codon]
  
  return(temp2)
}

# get.nucleotide----
# get the nucleotide from an specific position
get.nucleotide<-function(data=NULL, # vector containing DNA sequence in fasta format
                         start=NULL, # start cds position
                         end=NULL, # end cds position
                         position=NULL){ # position of the nucleotide
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  temp2 <- data1[position]
  
  return(temp2)
}

# get.exon----
# define location in exons or introns for an specific DNA position

get.exon<-function(position=NULL, # DNA position
                   fasta=NULL, # vector containing DNA sequence in fasta format
                   gff=NULL, # data.frame containing the GFF format (exons positions)
                   start=NULL, # start cds position
                   end=NULL){ # end cds position
  
  data0=fasta
  
  index_df<-data.frame("index"=1:str_length(data0))
  
  index_df$dna.region<-NA
  
  for (n in levels(as.factor(as.character(gff$Type)))){
    index_df[gff[gff[["Type"]]==n,][["Start"]]:gff[gff[["Type"]]==n,][["End"]],][["dna.region"]]<-n
  }
  
  index_df$index<-as.character(index_df$index)
  
  index_df[1:(start-1),][["index"]]<-as.character(as.numeric(index_df[1:(start-1),][["index"]])-start)
  
  index_df[start:end,][["index"]]<-as.character(as.numeric(index_df[start:end,][["index"]])-(start-1))
  
  index_df[(end+1):str_length(data0),][["index"]]<-paste0("+",as.character(as.numeric(index_df[(end+1):str_length(data0),][["index"]])-end))
  
  indexed_gff<-NULL
  
  for(n in levels(as.factor(as.character(gff$Type)))){
    temp<-data.frame("Type"=n,
                     "Start"=index_df[index_df[["dna.region"]]==n,][1,"index"],
                     "End"=index_df[index_df[["dna.region"]]==n,][nrow(index_df[index_df[["dna.region"]]==n,]),"index"])
    
    indexed_gff<-rbind(indexed_gff,temp)
  }
  
  indexed_gff<-indexed_gff[order(as.numeric(as.character(indexed_gff[["Start"]]))),]
  
  indexed_gff[indexed_gff[["Type"]]=="Exon 13",][["End"]]<-end # modify this line to extended to other genes
  
  if(!grepl("(_|-|\\+){1}",position)){
    temp<-indexed_gff[as.numeric(indexed_gff[["Start"]])<=as.numeric(position)&as.numeric(indexed_gff[["End"]])>=as.numeric(position),"Type"]
  }else if(!grepl("(-|\\+){1}",position)){
    temp<-indexed_gff[as.numeric(indexed_gff[["Start"]])<=as.numeric(str_split(position,"_")[[1]][1])&as.numeric(indexed_gff[["End"]])>=as.numeric(str_split(position,"_")[[1]][1]),"Type"]
  }else if (grepl("(\\+){1}",position)){
    temp<-paste0("Intron",
                 sep=" ",
                 str_extract(
                   indexed_gff[as.numeric(indexed_gff[["Start"]])<=as.numeric(str_split(position,"\\+")[[1]][1])&
                                 as.numeric(indexed_gff[["End"]])>=as.numeric(str_split(position,"\\+")[[1]][1]),"Type"],"\\d+"))
  }else if (grepl("-{1}",position)){
    temp<-paste0("Intron",
                 sep=" ",
                 as.numeric(
                   str_extract(
                     indexed_gff[as.numeric(indexed_gff[["Start"]])<=as.numeric(str_split(position,"-")[[1]][1])&
                                   as.numeric(indexed_gff[["End"]])>=as.numeric(str_split(position,"-")[[1]][1]),"Type"],"\\d+"))-1)
  }
  
  return(temp)
  
}
