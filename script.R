library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(stringr)


puntos<- function(a, b){
  
  id<- data$id== a 
  resultados<- data$id== "Resultado"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<-  sum( data[id,i+1] == data[resultados,i+1])
    
  }
  x
}

visitante_picks<- function(a,b){
  
  id<- data$id== a 
  contador<- data$id== "Visitante"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<-  sum( data[id,i+1] == data[contador,i+1])
    
  }
  x
}

visitante_aciertos<- function(a,b){
  
  id<- data$id== a 
  visitante<- data$id== "Visitante"
  resultado<- data$id== "Resultado"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[id,i+1] == data[visitante,i+1])
    
  }
  x
}

local_aciertos<- function(a,b){
  
  id<- data$id== a 
  local<- data$id== "Local"
  resultado<- data$id== "Resultado"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[id,i+1] == data[local,i+1])
    
  }
  x
}

empate_aciertos<- function(a,b){
  
  id<- data$id== a 
  resultado<- data$id== "Resultado"
  
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<- sum (data[id,i+1] == data[resultado,i+1] &  data[data$id=="Resultado",i+1]=="EMP" )
    
  }
  x
}

local_picks<- function(a,b){
  
  id<- data$id== a 
  contador<- data$id== "Local"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<-  sum( data[id,i+1] == data[contador,i+1])
    
  }
  x
}

empate_picks<- function(a,b){
  
  id<- data$id== a 
  contador<- data$id== "Visitante"
  x<- numeric(length = b)
  
  for (i in 1:b){
    
    x[i]<-  sum( data[id,i+1] == "EMP")
    
  }
  x
}

total_pct<- function (b, c){
  if (b== "Local"){
    
    suma<- function (a){
      
      x<- sum(local_aciertos(a,c))/ sum(local_picks(a,c))
      
      x
    }
  }
  
  else if (b=="Visitante"){
    suma<- function (a){
      
      x<- sum(visitante_aciertos(a,c))/ sum(visitante_picks(a,c))
      
      x
    }  
  }
  else if (b=="Empate"){
    suma<- function (a){
      
      x<- sum(empate_aciertos(a,c))/ sum(empate_picks(a,c))
      
      x
    } 
  }
  
  else {
    stop()
  }
  
  y<-  lapply(ID, suma)
  names(y)<- ID
  ## y<- tibble(y)
  unlist(y)
}

puntos_jornada <- function(a){
  ## J1,J2 ...
  
  x<- numeric(length = 11)
  y<- ID
  
  for (i in 1:length(ID)) {
    
    
    x[i]<-  sum( data[data$id== ID[i] ,a] == data[data$id=="Resultado",a] )
    
    
  }
  x
}

resultados<- function(b="Local", a){
  
  
  local<- data$id== b
  resultado<- data$id== "Resultado"
  x<- numeric(length = a)
  
  for (i in 1:a){
    
    x[i]<- sum ( data[local,i+1] == data[resultado,i+1]  )
    
  }
  x
}

suma_de_puntos<-  function(a, b){
  ## needs puntos function
  x<- numeric(length = b)
  y<-  puntos(a,b) 
  
  for ( i in 1:b){
    
    x[i]<- y[1:i] %>%  sum
    
  }
  x
}

ranking<- function (b,a){
  
  ##nombre y jornada
  x<-  sapply( ID, suma_de_puntos, a) %>% as_tibble()%>% add_column( 1:a, .before = T) %>% gather(key,val,-'1:a')%>% spread('1:a', val)
  nombre<- match(b, x$key)
  w<- numeric(length = a)
  
  for (i in 1:a){
    
    
    z<- x[,i+1] %>% unlist() %>%desc() %>% rank( ties.method = "min")
    w[i]<- z[nombre]
    
  }
  w
}

actualizar<- function(a, b){
  ## str_replace( x, regex("^E$"), "EMP")
  ## b is a single vector as tibble with z colname
  
  names(b)<- "z"
  
  w<- c(a, "Visitante", "Local", "Marcador", "Resultado") 
  
  x<-  lapply(w,rep, 9 ) %>% flatten %>% unlist 
  
  b[,1]<- b%>% unlist %>%  str_replace(regex("^E$"), "EMP")
  
  y<- b %>%
    mutate(x)%>% 
    select(x, everything()) %>%
    arrange(x)
  
  y$z
  

  
}


mejores<- function (a,b,c){
  
  x<-  data%>% filter( id== a) %>% select(2:b) %>% unlist
  y<- data %>% filter( id == c) %>% select(2:b)%>% unlist
  z<- x==y 
  w<- x[z] %>% as_tibble %>% group_by(value) %>% tally(sort=T)
  w
  
}