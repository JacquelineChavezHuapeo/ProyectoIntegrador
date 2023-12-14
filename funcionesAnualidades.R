print(paste0("ANUALIDADES ANTICIPADAS"))
print(paste0("Para VA conocido:"))

VAAnt=function(A, r, n){
  VAAnt=A*(((1-((1+r)^-n))/r)*(1+r))
  return(VAAnt)
}

pagoVAAnt=function(VA, r, n){
  pagoVAAnt=VA/(((1-((1+r)^-n))/r)*(1+r))
  return(pagoVAAnt)
}

periodosVAAnt=function(VA , A , r){
  periodosVAAnt=(-log(1-((VA*r)/(A*(1+r)))))/log(1+r)
  return(periodosVAAnt)
}

tasaVAAnt=function(VA, A, n){
  r=0.004
  
  izquierda=VA/A
  derecha=(1-((1+r)^-n))/r
  diferencia=izquierda-derecha
  
  while (diferencia>0) {
    r=r/1.01
    
    izquierda=VA/(A)
    derecha=(1-((1+r)^-n))/r
    diferencia=izquierda-derecha
  }
  
  return(r)
}

#DATOS DEL EJERCICO 
A=450
r=0.004583 #11%/24
n=48 #2*24


#Se realizan los cálculos mandando llamar a las funciones

VAAntCal=VAAnt(A, r, n)
cat("EL VA de la anualidad anticipada es:", VAAntCal, "\n")

pagoVAAnt=pagoVAAnt(VAAntCal, r, n)
cat("El monto de la anualidad anticipada conociendo VA es de:", pagoVAAnt,"\n")

periodosVAAnt=periodosVAAnt(VAAntCal, A, r)
cat("El número de periodos que se necesitan para llegar a VA son:", periodosVAAnt,"\n")

tasaVAAnt=tasaVAAnt(VAAntCal, A, n)
cat("La tasa estimada de la anualidad anticipada conociendo VA es de:", tasaVAAnt, "\n")

print(paste0("ANUALIDADES ANTICIPADAS"))
print(paste0("Para VF conocido:"))

VFAnt=function(A, r, n){
  VFAnt=A*(((((1+r)^n)-1)/r)*(1+r))
  return(VFAnt)
}

pagoVFAnt=function(VF, r, n){
  pagoVFAnt=VF/(((((1+r)^n)-1)/r)*(1+r))
  return(pagoVFAnt)
}

periodosVFAnt=function(VF , A , r){
  periodosVFAnt=log(((VF*r)/(A*(1+r)))+1)/log(1+r)
  return(periodosVFAnt)
}


tasaVFAnt=function(VF, A, n){
  r=0.001
  
  izquierda=VF/(A*(1+r))
  derecha=((((1+r)^n)-1)/r)
  diferencia=izquierda-derecha
  
  while (diferencia>0) {
    r=r*1.01
    
    izquierda=VF/(A*(1+r))
    derecha=((((1+r)^n)-1)/r)
    diferencia=izquierda-derecha
  }
  
  return(r)
}

#DATOS DEL EJERCICO 
A=500
r=0.003125 #7.5/24
n=960 #40*24


#Se realizan los cálculos mandando llamar a las funciones

VFAntCal=VFAnt(A, r, n)
cat("EL VF de la anualidad anticipada es:", VFAntCal, "\n")

pagoVFAnt=pagoVFAnt(VFAntCal, r, n)
cat("El monto de la anualidad anticipada conociendo VF es de:", pagoVFAnt,"\n")

periodosVFAnt=periodosVFAnt(VFAntCal, A, r)
print(paste0("Los periodos que se neceistan para llegar a VF son:", periodosVFAnt))

tasaVFAnt=tasaVFAnt(VFAntCal, A, n)
print(paste0("La tasa estimada de la anualidad anticipada conociendo VF es de:",tasaVFAnt))


print(paste0("ANUALIDADES VENCIDAS"))
print(paste0("Para VA conocido:"))

VAVenc=function(A, r, n){
  VAVenc=A*((1-((1+r)^-n))/r)
  return(VAVenc)
}

pagoVAVenc=function(VA, r, n){
  pagoVAVenc=VA/((1-((1+r)^-n))/r)
  return(pagoVAVenc)
}

periodosVAVenc=function(VA , A , r){
  periodosVAVenc=(-log(1-((VA*r)/A)))/(log(1+r))
  return(periodosVAVenc)
}

tasaVAVenc=function(VA, A, n){
  r=0.004
  
  izquierda=VA/A
  derecha=(1-((1+r)^-n))/r
  diferencia=izquierda-derecha
  
  while (diferencia>0) {
    r=r/1.01
    
    izquierda=VA/(A)
    derecha=(1-((1+r)^-n))/r
    diferencia=izquierda-derecha
  }
  
  return(r)
}

#DATOS DEL EJERCICO 
A=450
r=0.004583 #11%/24
n=48 #2*24


#Se realizan los cálculos mandando llamar a las funciones

VAVencCal=VAVenc(A, r, n)
cat("EL VA de la anualidad vencida es:", VAVencCal, "\n")

pagoVAVenc=pagoVAVenc(VAVencCal, r, n)
cat("El monto de la anualidad vencida conociendo VA es de:", pagoVAVenc,"\n")

periodosVAVenc=periodosVAVenc(VAVencCal, A, r)
cat("El número de periodos que se necesitan para llegar a VA son:", periodosVAVenc, "\n")

tasaVAVenc=tasaVAVenc(VAVencCal, A, n)
cat("La tasa estimada de la anualidad vencida conociendo VA es de:", tasaVAVenc, "\n")



print(paste0("ANUALIDADES VENCIDAS"))
print(paste0("Para VF conocido:"))

VFVenc=function(A, r, n){
  VFVenc=A*((((1+r)^n)-1)/r)
  return(VFVenc)
}

pagoVFVenc=function(VF, r, n){
  pagoVFVenc=VF/((((1+r)^n)-1)/r)
  return(pagoVFVenc)
}

periodosVFVenc=function(VF , A , r){
  periodosVFVenc=log(((VF*r)/A)+1)/log(1+r)
  return(periodosVFVenc)
}


tasaVFVenc=function(VF, A, n){
  r=0.001
  
  izquierda=VF/A
  derecha=((((1+r)^n)-1)/r)
  diferencia=izquierda-derecha
  
  while (diferencia>0) {
    r=r*1.01
    
    izquierda=VF/A
    derecha=((((1+r)^n)-1)/r)*(1+r)
    diferencia=izquierda-derecha
  }
  
  return(r)
}





#DATOS DEL EJERCICO 
A=500
r=0.003125 #7.5/24
n=960 #40*24


#Se realizan los cálculos mandando llamar a las funciones

VFVencCal=VFVenc(A, r, n)
cat("EL VF de la anualidad vencida es:", VFVencCal, "\n")

pagoVFVenc=pagoVFVenc(VFVencCal, r, n)
cat("El monto de la anualidad vencida conociendo VF es de:", pagoVFVenc,"\n")

periodosVFVenc=periodosVFVenc(VFVencCal, A, r)
cat("Los periodos que se necesitan para llegar a VF son:", periodosVFVenc, "\n")

tasaVFVenc=tasaVFVenc(VFVencCal, A, n)
cat("La tasa estimada de la anualidad vencida conociendo VF es de:", tasaVFVenc, "\n")



print(paste0("Anualidades Diferidas"))

VADif=function(A,r,n,k){
  VADif=A*((1-((1+r)^-(n-k+1)))/r)
  return(VADif)
}

pagoVADif=function(VA,r,n,k){
  pagoVADif=VA/((1-((1+r)^-(n-k+1)))/r)
  return(pagoVADif)
}

#Datos del ejercicio
A=450
r=0.11/12
n=24
k=3

#Cálculos del ejercicio
VADifCal=VADif(A,r,n,k)
cat("El VA de la anualidad diferida es:", VADifCal, "\n")

pagoVADif=pagoVADif(VADifCal,r,n,k)
cat("El pago que se debe hacer es de:", pagoVADif, "\n")
