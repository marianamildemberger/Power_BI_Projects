consumo_cerveja_final <- read.csv("C:/Users/maria/Desktop/Pós - Data Science/Estatística/Trabalho/consumo_cerveja_final.csv", sep=";")
attach(consumo_cerveja_final)

#A temperatura interfere no consumo de cerveja?
  
  #H0 a temperatura nao interfere no consumo
  #H1 a temperatura interfere no consumo
  
#Descritivas

#Resumo do DB
summary(consumo_cerveja_final)
  
#CONSUMO
  #media consumo
  media_consumo <- mean(consumo_cerveja_final$Consumo.de.cerveja..litros.) #22934.63
  
  #variancia consumo
  var(consumo_cerveja_final$Consumo.de.cerveja..litros.) #66264517

  #desvio padrao consumo
  sd(consumo_cerveja_final$Consumo.de.cerveja..litros.) #8140.302

  #quartil 2nd quartil = mediana
  quantile(consumo_cerveja_final$Consumo.de.cerveja..litros.) #  0%   25%   50%   75%  100% 
                                                              # 203 21092 24304 28034 37937 
  boxplot(consumo_cerveja_final$Consumo.de.cerveja..litros.)

  #distancia interquartil
  IQR(consumo_cerveja_final$Consumo.de.cerveja..litros.) #6942

  #maximo consumo
  max(consumo_cerveja_final$Consumo.de.cerveja..litros.) #37937

  #minimo consumo
  min(consumo_cerveja_final$Consumo.de.cerveja..litros.) #203
  
  #Registros de consumo acima da media 22934.63
  acima_media <- (consumo_cerveja_final$Consumo.de.cerveja..litros.)[ consumo_cerveja_final$Consumo.de.cerveja..litros. > media_consumo]
  acima_media
  
  #consumo medio por dia da semana
  consumo_dia_semana <- tapply(consumo_cerveja_final$Consumo.de.cerveja..litros.,consumo_cerveja_final$Nome_Dia.da.Semana,mean)
  consumo_dia_semana # Domingo  Quarta-feira  Quinta-feira        Sábado Segunda-feira   Sexta-feira   Terça-feira 
                    #  27511.10      22948.29      21090.42      23634.19      19736.17      22508.88      23148.81 
  barplot(consumo_dia_semana, ylab = "Média diária de consumo")
  
  #consumo medio se é dia de semana ou se é fim de semana
  consumo_se_fds <- tapply(consumo_cerveja_final$Consumo.de.cerveja..litros.,consumo_cerveja_final$Final.de.Semana,mean)
  consumo_se_fds #       0        1    -> em que 0 e dia de semana e 1 fds
                    #21883.46 25572.64
  pie(consumo_se_fds)#zoeira profs
  barplot(consumo_se_fds, names.arg = c("dia útil", "fim de semana"))
  
  #consumo medio por estacao do ano
  consumo_estacao <- tapply(consumo_cerveja_final$Consumo.de.cerveja..litros.,consumo_cerveja_final$Estação.do.ano,mean)
  consumo_estacao #Inverno    Outono Primavera     Verão 
                # 21420.59  21387.81  25137.83  23933.33
  barplot(consumo_estacao, horiz = TRUE)
  
  #total de litros no ano 2015
  sum(consumo_cerveja_final$Consumo.de.cerveja..litros) #  8371139
  
  #media diaria litros ano 2015
  mean(consumo_cerveja_final$Consumo.de.cerveja..litros.) # 22934.63
  
  #media calculada sem formula
  8371139/365 # 22934.63
  
  #Teste de Normalidade
  shapiro.test(consumo_cerveja_final$Consumo.de.cerveja..litros.)
  #p-value < 2.2e-16 ou seja < alfa, logo rejeita H0, não é normalmente distribuido
  
  #TEMPERATURA 
  
  #media temperatura
  mean(consumo_cerveja_final$Temperatura.Media..C.) # 21.22636

  #variancia temperatura
  var(consumo_cerveja_final$Temperatura.Media..C.) # 10.11308

  #desvio padrao temperatura
  sd(consumo_cerveja_final$Temperatura.Media..C.) # 3.180108

  #Quantas vezes foram registrados uma precipitaÃ§Ã£o abaixo de 3mm
  precipitacao_baixa <- (consumo_cerveja_final$Precipitacao..mm.)[ consumo_cerveja_finalPrecipitacao..mm. < 3]
  precipitacao_baixa
  
  ##Quantas vezes não choveu
  sem_chuva <- (consumo_cerveja_final$Precipitacao..mm.)[ consumo_cerveja_final$Precipitacao..mm. == 0]
  sem_chuva
  
#REGRESSAO LINEAR SIMPLES
  
  #Temperatura.Media..C.
  modelo<- lm(Consumo.de.cerveja..litros.~Temperatura.Media..C., data= consumo_cerveja_final)
  summary(modelo) # Multiple R-squared:  0.06819 ***
  
  #Temperatura.Minima..C.
  modelo2<- lm(Consumo.de.cerveja..litros.~Temperatura.Minima..C., data= consumo_cerveja_final)
  summary(modelo2) #Multiple R-squared:  0.02631 **
  
  #Temperatura.Maxima..C.
  modelo3<- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C., data= consumo_cerveja_final)
  summary(modelo3) #Multiple R-squared:  0.1016 ***
  
  #Precipitacao..mm.
  modelo4<- lm(Consumo.de.cerveja..litros.~Precipitacao..mm., data= consumo_cerveja_final)
  summary(modelo4) #Multiple R-squared:  0.00157 nenhum
  
  #Final.de.Semana
  modelo5<- lm(Consumo.de.cerveja..litros.~ as.factor(Final.de.Semana), data= consumo_cerveja_final) #variavel categorica
  s <- summary(modelo5) #Multiple R-squared:  0.04196 ***
  #correlação:
  sqrt(s$r.squared) #0.2048468
  
  #Mês
  modelo6<- lm(Consumo.de.cerveja..litros.~ as.factor(Mês), data= consumo_cerveja_final) #variavel categorica
  s1 <- summary(modelo6) #Multiple R-squared:  0.05373 nenhum
  #correlação:
  sqrt(s1$r.squared) #0.2318001
  
  #Dia.da.Semana
  modelo7<- lm(Consumo.de.cerveja..litros.~ as.factor(Dia.da.Semana), data= consumo_cerveja_final) #variavel categorica
  s2 <- summary(modelo7) #Multiple R-squared:  0.07623 ***
  #correlação:
  sqrt(s2$r.squared) #0.27609
  
  #Estação.do.ano
  modelo8<- lm(Consumo.de.cerveja..litros.~ as.factor(Estação.do.ano), data= consumo_cerveja_final) #variavel categorica
  s3 <- summary(modelo8) # Multiple R-squared:  0.03991 nenhum
  #correlação:
  sqrt(s3$r.squared) #0.1997746
  
  #Nome_Dia.da.Semana
  modelo9<- lm(Consumo.de.cerveja..litros.~ as.factor(Nome_Dia.da.Semana), data= consumo_cerveja_final) #variavel categorica
  s4 <- summary(modelo9) #Multiple R-squared:  0.07623 ***
  #correlação:
  sqrt(s4$r.squared) #0.27609
  
  #Período
  modelo10<- lm(Consumo.de.cerveja..litros.~ as.factor(Período), data= consumo_cerveja_final) #variavel categorica
  s5 <- summary(modelo10) #Multiple R-squared:  0.001359 nenhum
  #correlação:
  sqrt(s5$r.squared) #0.03686949
  
  #Nome_Mês
  modelo11<- lm(Consumo.de.cerveja..litros.~ as.factor(Nome_Mês), data= consumo_cerveja_final) #variavel categorica
  s6 <- summary(modelo11) #Multiple R-squared:  0.05373 nenhum
  #correlação:
  sqrt(s6$r.squared) #0.2318001

  #grafico consumoxtemperatura maxima 
  plot(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Temperatura.Maxima..C., col= 'yellow')
  abline(lm(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Temperatura.Maxima..C.))
  
  #grafico consumoxtemperatura media 
  plot(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Temperatura.Media..C., col= 'green')
  abline(lm(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Temperatura.Media..C.))
  
  #grafico consumoxdia de semana 
  plot(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Dia.da.Semana, col= 'blue')
  abline(lm(consumo_cerveja_final$Consumo.de.cerveja..litros.~consumo_cerveja_final$Dia.da.Semana))


#REGRESSAO LINEAR MULTIPLA
  
  #Correlação
  cor(consumo_cerveja_final) #deu ruim, bora individual
  cor(Temperatura.Maxima..C., Consumo.de.cerveja..litros.)####0.3188069
  cor(Temperatura.Media..C., Consumo.de.cerveja..litros.)####0.2611241
  cor(Temperatura.Minima..C., Consumo.de.cerveja..litros.)####0.1621955
  cor(Precipitacao..mm., Consumo.de.cerveja..litros.)####-0,03962609
  #o restante das variáveis são categoricas, regressão linear simples acima 
  
  #Ordem de variáveis:
  #Temperatura.Maxima..C., Dia.da.Semana, Temperatura.Media..C., Final.de.Semana, Temperatura.Minima..C.,Precipitacao..mm.
  
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C., data= consumo_cerveja_final)
  summary(reg_lin_multi)   #Multiple R-squared:  0.1016
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana), data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1686
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana) + Temperatura.Media..C., data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1737
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana) + Temperatura.Media..C. + as.factor(Final.de.Semana), data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1737
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana) + Temperatura.Media..C. + Temperatura.Minima..C., data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1714
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana) + Temperatura.Media..C. + Precipitacao..mm., data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1715
  
  #Portanto, reg linear multipla que mais explica:
  reg_lin_multi <- lm(Consumo.de.cerveja..litros.~Temperatura.Maxima..C. + as.factor(Dia.da.Semana) + Temperatura.Media..C., data= consumo_cerveja_final)
  summary(reg_lin_multi) #Adjusted R-squared:  0.1737
  
  #Grafico reg linear multipla
  plot(reg_lin_multi)
  abline(reg_lin_multi)

#ANALISE DOS RESIDUOS
  summary(reg_lin_multi)
  residuals(reg_lin_multi)
  shapiro.test(residuals(reg_lin_multi))
  #p-value < 2.2e-16 ou seja < alfa, logo rejeita H0, não é normalmente distribuido
  
#Considerando que os dados não são normais para variaveis categoricas
  #Considerando categoricas com mais de 3 categorias:
  #Mês, Dia.da.Semana, Estação.do.ano
  
#Kruskal-Wallis test
  
  a1 <- kruskal.test(Consumo.de.cerveja..litros. ~ as.factor(Mês), data = consumo_cerveja_final)
  a1
  install.packages("FSA")
  library("FSA")
  dunnTest(Consumo.de.cerveja..litros. ~ as.factor(Mês), data = consumo_cerveja_final, method = "bonferroni")
  #rejeitamos H0, logo há diferença entre as médias
  tapply(Consumo.de.cerveja..litros., Mês, mean)
  
  a2 <- kruskal.test(Consumo.de.cerveja..litros. ~ as.factor(Dia.da.Semana), data = consumo_cerveja_final)
  a2
  library("FSA")
  dunnTest(Consumo.de.cerveja..litros. ~ as.factor(Dia.da.Semana), data = consumo_cerveja_final, method = "bonferroni")
  #rejeitamos H0, logo há diferença entre as médias
  tapply(Consumo.de.cerveja..litros., Dia.da.Semana, mean)
  
  a3 <- kruskal.test(Consumo.de.cerveja..litros. ~ as.factor(Estação.do.ano), data = consumo_cerveja_final)
  a3
  library("FSA")
  dunnTest(Consumo.de.cerveja..litros. ~ as.factor(Estação.do.ano), data = consumo_cerveja_final, method = "bonferroni")
  #rejeitamos H0, logo há diferença entre as médias
  tapply(Consumo.de.cerveja..litros., Estação.do.ano, mean)
  
