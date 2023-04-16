visitante_aciertos<- function(a){
  
  id<- data$id== a 
  visitante<- data$id== "Visitante"
  resultado<- data$id== "Resultado"
  x<- numeric(length = 10)
  
  for (i in 1:10){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[id,i+1] == data[visitante,i+1])
    
  }
  x
}