empate_aciertos<- function(a, b){
  
  id<- data$id== a 
  resultado<- data$id== "Resultado"
  
  x<- numeric(length = b)
  
  for (i in 1:10){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[data$id=="Resultado",i+1]=="EMP" )
    
  }
  x
}