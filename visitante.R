visitante_picks<- function(a){
  
  id<- data$id== a 
  contador<- data$id== "Visitante"
  x<- numeric(length = 10)
  
  for (i in 1:10){
    
    x[i]<-  sum( data[id,i+1] == data[contador,i+1])
    
  }
  x
}