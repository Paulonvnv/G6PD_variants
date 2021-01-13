
get.exon<-function(position=NULL,
                   fasta=NULL,
                   gff=NULL,
                   start=NULL,
                   end=NULL){
  
  
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



