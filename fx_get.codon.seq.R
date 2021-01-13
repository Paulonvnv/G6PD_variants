
get.codon.seq<-function(data=NULL,
                     start=NULL,
                     end=NULL,
                    codon=NULL){
  
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
