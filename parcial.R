parcial_pct<- function (b){
  if (b== "Local"){
    
    suma<- function (a){
      
      x<- sum(local_aciertos(a))/ sum(puntos(a))
      
      x
    }
  }
  
  else if (b=="Visitante"){
    suma<- function (a){
      
      x<- sum(visitante_aciertos(a))/ sum(puntos(a))
      
      x
    }  
  }
  else if (b=="Empate"){
    suma<- function (a){
      
      x<- sum(empate_aciertos(a))/ sum(puntos(a))
      
      x
    } 
  }
  
  else {
    stop()
  }
  
  y<-  lapply(ids, suma)
  names(y)<- ids
  ## y<- tibble(y)
  unlist(y)
}