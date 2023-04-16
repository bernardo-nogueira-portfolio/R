puntos<- function(a){
  
id<- data$id== a 
resultados<- data$id== "Resultado"
x<- numeric(length = 10)

for (i in 1:10){
  
 x[i]<-  sum( data[id,i+1] == data[resultados,i+1])
  
}
x
}