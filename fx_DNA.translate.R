

DNA.translate<-function(data=NULL,
         start=1,
         end=NULL,
        digits=3){
  
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

  
  


