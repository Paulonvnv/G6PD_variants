
get.nucleotide<-function(data=NULL,
                        start=NULL,
                        end=NULL,
                        position=NULL){
  
  data0=data
  data1<-strsplit(data0, split = "")[[1]]
  
  data1<-data1[start:end]
  
  temp2 <- data1[position]
  
  return(temp2)
}
