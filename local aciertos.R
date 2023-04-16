local_aciertos<- function(a){
  
  id<- data$id== a 
  local<- data$id== "Local"
  resultado<- data$id== "Resultado"
  x<- numeric(length = 10)
  
  for (i in 1:10){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[id,i+1] == data[local,i+1])
    
  }
  x
}