Convertidor_trimestral<- function (a, b=4, c=4){
  
  x<- seq(from =b, to= length(a), by =c)
  y<- a[x]
  y
  ## El default es para series trimestrales que inicien en enero y terminan en diciembre
  ## Regresa un vector numerico
}