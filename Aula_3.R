# Pacotes utilizados --------------------------------------------------------- #

# install.packages("ggplot2")
library(ggplot2)

# Pacotes referenciados no codigo por "::", exp: readxl::read_excel()

# install.packages("readxl")
# install.packages("forecast")
# install.packages("randtests")
# install.packages("trend")
# install.packages("Kendall")
# install.packages("tseries")
# install.packages("seastests")
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("ggpubr")
# install.packages("cowplot")


# Link com series temporais:
# http://www.ipeadata.gov.br/Default.aspx

# Funcoes ----------------------------------------------------------------------

# Tendencia deterministica:
tend_determ<-function(ts){
  CS<-suppressWarnings(randtests::cox.stuart.test(ts,c("two.sided"))) #H0: NAO existe tendencia
  CeST<-suppressWarnings(trend::cs.test(ts)) #H0: NAO existe tendencia
  # Runs<-suppressWarnings(randtests::runs.test(ts)) #H0: NAO existe tendencia
  # WaldW<-suppressWarnings(trend::ww.test(ts)) #H0: NAO existe tendencia
  MannKT<-suppressWarnings(trend::mk.test(ts,continuity = TRUE)) #H0: a serie eh i.i.d. / NAO existe tendencia
  MannK<-suppressWarnings(Kendall::MannKendall(ts)) #H0: NAO existe tendencia
  KPSST<-suppressWarnings(tseries::kpss.test(ts, null = c("Trend"))) #H0: NAO existe tendencia
  #
  p_value<-c(CS$p.value,CeST$p.value,MannKT$p.value,MannK$sl,KPSST$p.value)
  p_value1<-p_value
  p_value1[p_value>=0.05]<-"NAO tendencia"
  p_value1[p_value<0.05]<-"Tendencia"
  tabela<-data.frame(Testes=c("Cox Stuart","Cox and Stuart Trend",
                              "Mann-Kendall Trend","Mann-Kendall","KPSS Test for Trend"),
                     H0=c(rep("NAO tendencia",5)),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1))
  list(CS=CS,CeST=CeST,MannKT=MannKT,MannK=MannK,KPSST=KPSST,Tabela=tabela)
}

# Raiz unitaria:
raiz_unit<-function(ts){
  ADF<-suppressWarnings(tseries::adf.test(ts,alternative = c("stationary"))) #H0: raiz unitaria
  PP<-suppressWarnings(tseries::pp.test(ts,alternative = c("stationary"))) #H0: raiz unitaria
  KPSSL<-suppressWarnings(tseries::kpss.test(ts, null = c("Level"))) #H0: nao existe tendencia
  #
  p_value<-c(ADF$p.value,PP$p.value,KPSSL$p.value)
  p_value1<-p_value[1:2]
  p_value1[p_value[1:2]>=0.05]<-"Tendencia"
  p_value1[p_value[1:2]<0.05]<-"NAO tendencia"
  p_value2<-p_value[3]
  p_value2[p_value[3]>=0.05]<-"NAO tendencia"
  p_value2[p_value[3]<0.05]<-"Tendencia"
  tabela<-data.frame(Testes=c("Augmented Dickey-Fuller","Phillips-Perron Unit Root","KPSS Test for Level"),
                     H0=c(rep("Tendencia",2),"NAO tendencia"),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1,p_value2))
  list(ADF=ADF,PP=PP,KPSSL=KPSSL,Tabela=tabela)
}

# Sazonalidade:
sazonalidade<-function(ts,diff=0,freq){
  KrusW<-suppressWarnings(seastests::kw((ts),diff = diff, freq=12)) #H0: NAO Sazonal
  Fried<-suppressWarnings(seastests::fried((ts),diff = diff, freq=12)) #H0: NAO Sazonal
  #
  p_value<-c(KrusW$Pval,Fried$Pval)
  p_value1<-p_value
  p_value1[p_value>=0.05]<-"NAO Sazonal"
  p_value1[p_value<0.05]<-"Sazonal"
  tabela<-data.frame(Testes=c("Kruskall Wallis","Friedman rank"),
                     H0=c(rep("NAO Sazonal",2)),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1))
  list(KrusW=KrusW,Fried=Fried,Tabela=tabela)
}

# Grafico para a previsao:
graf_prev<-function(Tempo,serie,ajuste,y_limits=c(NA,NA),h,xlab="Tempo",ylab="Dados",
                    previsao,l95 = previsao , u95=previsao, leg = T){
  t3<-data.frame(Tempo=Tempo,serie=c(serie,rep(NA,h)),ajuste=c(ajuste,rep(NA,h)),
                 previsao=c(rep(NA,length(serie)),previsao),
                 l95a=c(rep(NA,length(serie)),l95),u95a=c(rep(NA,length(serie)),u95))
  dt1<- t3 |> 
    tidyr::pivot_longer(`serie`:`previsao`,
                        names_to = "series", values_to = "valor")
  ggplot(dt1) +
    aes(x = Tempo, y = valor, colour = series) +
    geom_ribbon(aes(ymin = l95a, ymax = u95a),
                fill = "blue",color=NA, alpha = 0.2,show.legend = F)+
    geom_line(size = 0.8,show.legend = leg) +
    scale_color_manual(
      values = c(serie = "black",
                 ajuste = "red",
                 previsao = "blue"),
      labels = c("serie"="Real","ajuste"="Ajustado","previsao"="Previsão")
    ) +
    labs(x = xlab, y = ylab, color= "") +
    theme_minimal()+ 
    if(any(!is.na(y_limits))){
      scale_y_continuous(
      limits = y_limits)}
}

# Dados2 Trimestral ============================================================
# Leitura do banco de dados
dados2<-readxl::read_excel("ipeadata[03-04-2024-02-35].xls")
y2<-dados2$`PIB - indústria - R$ (milhões)  - Instituto Brasileiro de Geografia e Estatística, Sistema de Contas Nacionais Trimestrais (IBGE/SCN Trimestral) - SCN104_PIBINDV104 -`
min(dados2$Data) # Data inicial
d2<-ts(y2,start = c(1996,1),frequency = 4)# Trimestral frequencia = 4

# Gráfico da Série
forecast::autoplot(d2,xlab = "Tempo (Trimestre)",ylab = "PIB - Industria")+theme_minimal()

# Gráfico da Função de Autocorrelação (FAC)
forecast::ggAcf(d2,lag.max = 60, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

# Gráfico da Função de Autocorrelação Parcial (FACP)
forecast::ggAcf(d2,lag.max = 60,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

# analise descritiva 
summary(d2)
plot(decompose(d2))
plot(decompose(d2,type = "multiplicative"))


# Analise de tendencia deterministica: --------------------------------------- #

# Tendencia deterministica: Refere-se a mudanças de longo prazo na serie 
# temporal que seguem um padrao ou comportamento especifico e previsivel.
# Como por exemplo, uma tendencia crescente ou descrecente na serie.

tend_determ(ts = d2)$Tabela

# Os 5 testes apresentaram tendencia deterministica, a serie nao eh estacionária.

# Teste para raiz unitaria: -------------------------------------------------- #


# Tendencia estocastica (raiz unitária): Refere-se a mudanças de longo prazo na 
# serie temporal que nao seguem um padrao previsivel ou deterministico.
# Aqui nao eh possivel identificar um comportamento padrao que nem no deterministico
# A serie apresenta ocilaçoes que nao conseguimos descrever, por seguirem de forma 
# aleatoria.

raiz_unit(ts=d2)$Tabela

# Os tres testes apresentaram tendencia estocastica (raiz unitaria).

# Diferenciacao -------------------------------------------------------------- #

diff_demostrativo<-diff((d2), differences = 1)
forecast::autoplot(diff_demostrativo)+theme_minimal()

diff_d2<-diff(log(d2), differences = 1)
forecast::autoplot(diff_d2)+theme_minimal()

forecast::ggAcf(diff_d2,lag.max = 100,type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

forecast::ggAcf(diff_d2,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

# Analise de tendencia deterministica (DIFF): -------------------------------- #

tend_determ(ts = diff_d2)$Tabela

## nao apresenta tendencia deterministica

# Teste para raiz unitaria (DIFF): ------------------------------------------- #

raiz_unit(ts= diff_d2)$Tabela

## nao apresenta raiz unitaria

# Sazonalidade --------------------------------------------------------------- #

# sazonalidade(ts = d1,diff = 1,freq = 4)
sazonalidade(ts = log(d2),diff = 1,freq = 4)$Tabela

# A serie apresenta sazonalidade.

# Modelo ARIMA --------------------------------------------------------------- #

mod<-forecast::auto.arima(log(d2))
summary(mod) # Conseguimos ver alguns criterios do modelo AIC, erros, os coeficientes.
lmtest::coeftest(mod) # Coeficientes do modelo
confint(mod,level=0.95) # intervalo de confianca

#Verificacao das Suposições do Modelo ---------------------------------------- #
forecast::autoplot(mod)+ggtitle("Dados")+ xlab("Time") + ylab("Valores $")+theme_minimal()
# Inverso do circulo unitario do modelo, esperamos que estejam dentro do circulo
# por ser o inverso.

#Analise de Residuos: -------------------------------------------------------- #
res.mod<-mod$residuals
forecast::ggtsdisplay(res.mod)
## Tanto o FAC(ACF) e o FACP(PACF) apresentam legs dentro das bandas.

Box.test(res.mod,lag = 15, type="Ljung") # H0: NAO Autocorrelacionado 
# P-valor maior do que 0.05, portanto nao rejeitamos a hipotese nula
# Os residuos nao apresentam autocorrelacao

nortest::ad.test(res.mod) # H0: Normalidade
# P-valor maior do que 0.05, portanto NAO rejeitamos a hipotese nula
# Os residuos apresentam normalidade

raiz_unit(ts = res.mod)$Tabela
# Os residuos nao apresentam raiz unitaria, sao estacionarios

ggpubr::ggqqplot(res.mod)

# Previsao ------------------------------------------------------------------- #
h=12
predict.mod<-forecast::forecast(mod,h=h)

min(dados2$Data)
Tempo<-seq(as.Date("1996/01/01"), by = "3 months", length.out = length(d2)+h)
ajuste<-predict.mod$fitted # ajuste do modelo
previsao<-predict.mod$mean # previsao do modelo
l95<-predict.mod$lower[,2] # limite inferior 95%
u95<-predict.mod$upper[,2] # limite superior 95%
g1<-graf_prev(Tempo = Tempo,serie = log(d2),ajuste = ajuste,previsao = previsao,
              l95 = l95,u95 = u95,h = h,ylab = "log(PIB - Industria)",leg = T)
g1

# Voltando para a escala original -------------------------------------------- #

# Como foi aplicado log() na serie, agora aplicamos exp() nas saidas do modelo
# ajustado, assim retornando a escala original

ajuste<-exp(predict.mod$fitted)
previsao<-exp(predict.mod$mean)
l95<-exp(predict.mod$lower[,2])
u95<-exp(predict.mod$upper[,2])
g2<-graf_prev(Tempo = Tempo,serie = d2,ajuste = ajuste,previsao = previsao,
          l95 = l95,u95 = u95,h = h,ylab = "PIB - Industria",leg = F)
g2

# Modelo ARIMA --------------------------------------------------------------- #

mod2<-forecast::auto.arima((d2),allowdrift = F)
summary(mod2) # Conseguimos ver alguns criterios do modelo AIC, erros, os coefifientes.
lmtest::coeftest(mod2) #Coeficientes do modelo

#Verificacao das Suposições do Modelo ---------------------------------------- #
forecast::autoplot(mod2)+ggtitle("Dados")+ xlab("Time") + ylab("Valores $")+theme_minimal()
# Inverso do circulo unitario do modelo, esperamos que estejam dentro do circulo
# por ser o inverso.

#Analise de Residuos: -------------------------------------------------------- #
res.mod2<-mod2$residuals
forecast::ggtsdisplay(res.mod2)
## Tanto o FAC(ACF) e o FACP(PACF) apresentam legs dentro das bandas.

Box.test(res.mod2,lag = 15, type="Ljung") # H0: NAO Autocorrelacionado 
# P-valor maior do que 0.05, portanto nao rejeitamos a hipotese nula
# Os residuos nao apresentam autocorrelacao

nortest::ad.test(res.mod2) # H0: Normalidade
# P-valor menor do que 0.05, portanto NAO rejeitamos a hipotese nula
# Os residuos NAO apresentam normalidade

raiz_unit(ts = res.mod2)$Tabela
# Os residuos nao apresentam raiz unitaria, sao estacionarios

ggpubr::ggqqplot(res.mod2)

# Previsao ------------------------------------------------------------------- #
h=12
predict.mod2<-forecast::forecast(mod2,h=h)

ajuste2<-predict.mod2$fitted # ajuste do modelo
previsao2<-predict.mod2$mean # previsao do modelo
l95_2<-predict.mod2$lower[,2] # limite inferior 95%
u95_2<-predict.mod2$upper[,2] # limite superior 95%
g3<-graf_prev(Tempo = Tempo,serie = d2,ajuste = ajuste2,previsao = previsao2,
              l95 = l95_2,u95 = u95_2,h = h,ylab = "PIB - Industria (Sem tranformar)",
              leg = F,y_limits = c(0,1150000))
              
g3

# Comparacao entre os dois modelos: 
summary(mod)
summary(mod2)
cowplot::plot_grid(g2,g3,nrow = 2)



