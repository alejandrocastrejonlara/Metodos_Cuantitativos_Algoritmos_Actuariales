#GUIA EXAMEN FINAL

#PROBLEMA 1
#(_n^)P_r=n!/(n-r)!
#Crean una función en R con la fórmula anterior y resuelve: Cuando se prueba un nuevo fármaco, 
#la fase I incluye sólo a 8 voluntarios; el objetivo consiste en evaluar la seguridad del fármaco.
#Para ser muy cuidadoso, usted planea tratar a los 8 sujetos en secuencia, de manera que cualquier 
#efecto dañino específico permita detener los tratamientos antes de aplicarlos a otros sujetos. 
#Si se dispone de 10 voluntarios, de los cuales se seleccionarán 8, 
#¿cuántas secuencias diferentes de 8 sujetos son posibles?

voluntarios<-c("Voluntario_1","Voluntario_2","Voluntario_3","Voluntario_4","Voluntario_5","Voluntario_6","Voluntario_7","Voluntario_8","Voluntario_9","Voluntario_10")
nombres<-length(voluntarios)
permutacionn<-function(n,r){
  resultado1<-factorial(n)/(factorial(n-r))
  return(resultado1)
}
permutacionn(nombres,8)



#PROBLEMA 2
#Para una variable aleatoria X~B(n,p) la función de distribución acumulativa está definida por:
#F(x)=P(X≤x)=P(X=0)+P(X=1)+P(X=2)+⋯P(X=x)
#Crea una función en R con los argumentos (n,p,x) para que calcule la probabilidad 
#acumulativa para un valor cualquiera x. 
#Con tu función anterior, resuelve el siguiente problema: 
#Un laboratorio asegura que la dexametasona causa efectos secundarios en una proporción de 3 
#de cada 100 pacientes. Para contrastar esta afirmación, se eligieron al azar 10 pacientes a
#los que se les recetó dexametasona. Calcula la probabilidad de que menos de tres personas 
#tengan efectos secundarios.

options(scipen=999)
funproba<-function(n,x,p){
  if (x<0){
    return(0)
  }
  if (x>=n){
    return(1)
  }
  distri_acu<-0
  for (i in 0:x){
    distri_acu<-distri_acu+choose(n,i)*(p^i)*(1-p)^(n-i)
  }
  return(distri_acu)
}
funproba(10,3,3/100)


#PROBLEMA 3
#Utiliza la librería lpSolve y la función lp() en R para resolver:
#Un agricultor tiene 600 hectáreas en las que puede sembrar maíz o cebada y dispone de
#800 horas de trabajo durante la temporada. Los márgenes de utilidad por hectárea 
#para el maíz son de $120 y para la cebada es de $140. El requerimiento laboral para trabajar 
#en la siembra de maíz es de 1 hora por hectárea y en la siembra de cebada es de 2 horas 
#por hectárea. ¿Cuántas hectáreas de cada cultivo debe sembrar para maximizar su utilidad?, 
#¿Cuál es la utilidad máxima?

install.packages("lpSolve")
library(lpSolve)  

matriz_coeficientes<-matrix(c(1,0,1,1,0,1,2,1),ncol = 2)
matriz_coeficientes
coeficientes_restricciones<-c(0,0,800,600)
solucion_siembra<-lp(direction = "max",            #LA FUNCION QUE SE DESEA MAXIMIZAR
                      objective.in = c(120,140),          #PRECIOS DE CADA. COSA
                      const.mat = matriz_coeficientes,       #COEFICIENTES DE LA MATRIZ
                      const.dir = c(">=",">=","<=","<="),     #LOS SIGNOS DE DESIGUALDAD
                      const.rhs = coeficientes_restricciones)     #EL TOTAL DE LA SUMA DE CADA VARIABLE
solucion_siembra
print(solucion_siembra$solution)     #TE DICE CUANTO DE CADA NECESITAS



#PROBLEMA 4
#Utiliza la librería lpSolve y la función lp() en R para resolver: 
#Una compañía fabrica y venden dos modelos de lámpara L1 y L2. 
#Para su fabricación se necesita un trabajo manual de 20 minutos para el modelo L1 y
#de 30 minutos para el L2; y un trabajo de máquina de 20 minutos para el modelo L1 y 
#de 10 minutos para L2.
#Se dispone para el trabajo manual de 100 horas al mes y para la máquina 80 horas al mes. 
#Sabiendo que el beneficio por unidad es de $300 y $200 para L1 y L2, respectivamente. 
#Planifica la producción para obtener el máximo beneficio.

coefun<-c(300,200)
coematriz<-matrix(c(20,20,1,0,30,10,0,1),ncol = 2)
coerestriccion<-c(6000,4800,0,0)
solucion_lampara<-lp(direction = "max",
                     objective.in = coefun,
                     const.mat = coematriz,
                     const.dir = c("<=","<=",">=",">="),
                     const.rhs = coerestriccion)
solucion_lampara
print(solucion_lampara$solution)




#PROBLEMA 5
#Los bonos con tasa de interés fija, también conocidos como bonos de cupón fijo, son un
#tipo común de bono en el que el emisor paga un interés fijo periódicamente al tenedor del 
#bono, generalmente anual o semestral, más un pago del valor nominal al vencimiento.
#La valuación de este tipo de bono implica determinar su precio actual en el mercado 
#secundario, lo que refleja el valor presente de los flujos de efectivo futuros que se
#generarán a partir de los pagos de cupón y el valor nominal.
#La fórmula para calcular el precio de un bono es:
# P=C/(1+r)^1 +C/(1+r)^2 +⋯+C/(1+r)^(n-1) +C/(1+r)^n +VN/(1+r)^n 
#C=VN*TC
#r=tasa de rendimiento
#n= periodos
#VN= valor nominal
#Crea una función en R con los argumentos: valor nominal, tasa cupón, tasa de rendimiento
#y plazo al vencimiento que calcule el precio de un Bono. Luego, resuelve el siguiente problema
#para poner a prueba tu función.
Valor_Nonimal<-1000
Tasa_Cupon<-0.05
Tasa_rendimiento<-0.04
peridos_n<-5
funcion_bono<-function(vn,tc,r,n){
  cupon<-vn*tc
  resul<-(cupon*(1-(1+r)^-n)/r)+(vn/(1+r)^n)
  return(resul)
  
}
funcion_bono(1000,0.05,0.04,5)
