#----------------------------------------------------------------------------------
######### Diplomado Superior en Economía Financiera #####################
######### Módulo 3. Gestion del Riesgo Financiero   ####################

######### Elaborado por Mtra. Selene Venegas        ####################
######### Sesión 2 Modelo CAPM 
#---------------------------------------------------------------------------------- 


library(quantmod)

# Paso 1. Lectura de datos

startDate = as.Date ("2000-01-01") #fecha de inicio de mis datos
endDate = as.Date ("2021-09-20") #fecha final sera 19 septiembre n-1
getSymbols(c("AAPL", "AMZN", "^IXIC"), from = startDate, to = endDate) 

getSymbols("SAN", from = startDate, to = endDate) 
#Para mostrar los datos
#head(SAN)
#tail(SAN)
#View(SAN)
#str(SAN)
#summary(SAN)

getSymbols("INTGSTUSM193N",src="FRED")
TB_Mensual=INTGSTUSM193N["2000::"]
head(TB_Mensual)

# Paso 2. Cálculo de los retornos

RAA<-periodReturn(AAPL$AAPL.Adjusted,period='daily', type='log')#Ln(t+1/t): (Pt+1/Pt -1)
RAM<-periodReturn(AMZN$AMZN.Adjusted,period='daily', type='log')
RNQ<-periodReturn(IXIC$IXIC.Adjusted,period='daily', type='log')

#2.1 Gráfico sobre los retornos

chartSeries(RAM,
            type = "line",
            subset ="2021::",
            name = "Retornos de AMAZON",
            theme = chartTheme('black',up.col='darkblue',dn.col='darkred'),
            TA = c(addRSI(),addMACD(),addSMA())
)

chartSeries(RNQ,
            type = "line",
            subset ="2021::",
            name = "Retornos de NASDAQ 100",
            theme = chartTheme('black',up.col='darkblue',dn.col='darkred'),
            TA = c(addRSI(),addMACD(),addSMA())
)
#3. Cálculo de la beta de la acción
#Mínimos cuadrados ordinarios
Reg_RAA<- lm(RAA~RNQ,data=RAA)
summary(Reg_RAA)

#Varianzas y Covarianzas
B_RAA<- cov(RNQ, RAA)/var(RNQ);B_RAA
B_RAM<- cov(RNQ, RAM)/var(RNQ);B_RAM

#4. Rendimiento libre de Riesgo Rf
#Rf: Rendimiento diario 
Rf<- (mean(TB_Mensual))/25600;Rf#se divide entre25600 porque son 256 dias habiles en el año y esta en porcentaje por eso los 2 ceros

##5. Premio por riesgo es el promedio de la diferencia 
#entre el rendimiento del mercado y el rendimiento libre de riesgo
#E[RNQ-Rf]

Premio_Riesgo <- mean(RNQ [-1]) -Rf;Premio_Riesgo

#6. Rendimiento esperado de cada acción

Ri.Apple<-Rf+ B_RAA*(Premio_Riesgo);Ri.Apple
Ri.Amazon<-Rf+ B_RAM*(Premio_Riesgo);Ri.Amazon

#Rendimiento anual
Ri.Amazon.Anual<-Ri.Amazon*256; Ri.Amazon.Anual

##6.1 Alternativa Utilizando el ultimo dato
#1. Seleccionar primera columna ultimo registro del TB_mensual
#2. Almacenar en una variable Rff=0.006
Rff<-TB_Mensual[260,];Rff
#3. dividir Rff/25600 256dias operativos*100 tasa
RF<-Rff/25600;RF
#4. Modificar precio al riesgo y rendimiento esperado
#Premio al riesgo 
Premio_Riesgo_RF <- mean(RNQ [-1]) -RF;Premio_Riesgo_RF
#Rendimiento esperado de cada accion
Rii.Amazon<-RF+ B_RAM*(Premio_Riesgo_RF);Rii.Amazon
Rii.Apple<-RF+ B_RAA*(Premio_Riesgo_RF);Rii.Apple
#comparamos con una tasa libre de riesgo actual, quiero tener el ultimo panomarama del tipo de cambio de interes 

#Rendimiento anual
Rii.Amazon.Anual<-Rii.Amazon*256; Rii.Amazon.Anual


#7. Guardar resultados

Cartera<-xts()
Cartera<-data.frame(AAPL$AAPL.Adjusted,AMZN$AMZN.Adjusted,IXIC$IXIC.Adjusted,RAA,RAM,RNQ)
head(Cartera)
colnames(Cartera)<-c("Apple","Amazon","Nasdaq100","R.Apple","R.Amazon","R.Nasdaq 100")
head(Cartera)


CAPM.diario<-xts()
CAPM.diario<-data.frame(Rf,mean(RAA),mean(RAM),mean(RNQ),Premio_Riesgo,B_RAA,B_RAM,Ri.Apple,Ri.Amazon)
head(CAPM.diario)
colnames(CAPM.diario)<-c("ti Libre de Riesgo","Rendimiento promedio Apple","Rendimiento promedio Amazon","Rendimiento promedio Nasdaq 100", "Premio al riesgo","Beta Apple","Beta Amazon","Rendimiento esperado Apple","Rendimiento esperado Amazon")
head(CAPM.diario)

TB<-xts()
TB<-data.frame(TB_Mensual)
colnames(TB)<-"T Bills"

write.csv(Cartera, "Cartera_Nasdaq.csv")
write.csv(CAPM.diario, "CAPM_diario.csv")
write.csv(TB, "TB.csv")
