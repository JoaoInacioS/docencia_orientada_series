
# Pacotes utilizados --------------------------------------------------------- #

# install.packages("readr")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("dygraphs")
 
library(readxl)
library(forecast)
library(ggplot2)
library(dygraphs)

# Link com series temporais:
# http://www.ipeadata.gov.br/Default.aspx

# Dados mensais ================================================================
# Leitura do banco de dados
dados<-read_excel("ipeadata[03-04-2024-01-55].xls")
y<-dados$`Exportações - preços - índice (média 2018 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_XPT12 -`
min(dados$Data) # Data inicial
max(dados$Data) # Data final
d1<-ts(y,start = c(1977,01),end = c(2024,01),frequency = 12)# Mensal frequencia = 12

# Gráfico da Série
autoplot(d1)+labs(x="Tempo (meses)",y="Preços das exportações")+theme_minimal()
# ou
dygraph(d1,xlab = "Tempo (meses)",ylab = "Preços das exportações") %>% dyRangeSelector()

# Gráfico da Função de Autocorrelação (FAC)
ggAcf(d1,lag.max = 100, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

# Gráfico da Função de Autocorrelação Parcial (FACP)
ggAcf(d1,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

# Dados2 Trimestral ============================================================
# Leitura do banco de dados
dados2<-read_excel("ipeadata[03-04-2024-02-35].xls")
y2<-dados2$`PIB - indústria - R$ (milhões)  - Instituto Brasileiro de Geografia e Estatística, Sistema de Contas Nacionais Trimestrais (IBGE/SCN Trimestral) - SCN104_PIBINDV104 -`
min(dados2$Data) # Data inicial
max(dados2$Data) # Data final
d2<-ts(y2,start = c(1996,1),end = c(2023,4),frequency = 4)# Trimestral frequencia = 4

# Gráfico da Série
autoplot(d2,xlab = "Tempo (Trimestre)",ylab = "PIB - Industria")+theme_minimal()
# ou
dygraph(d2,xlab = "Tempo (Trimestre)",ylab = "PIB - Industria") %>% dyRangeSelector()

# Gráfico da Função de Autocorrelação (FAC)
ggAcf(d2,lag.max = 60, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

# Gráfico da Função de Autocorrelação Parcial (FACP)
ggAcf(d2,lag.max = 60,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

# Dados3 Anual =================================================================
# Leitura do banco de dados
dados3<-read_excel("ipeadata[03-04-2024-02-53].xls")
y3<-dados3$`Taxa de fecundidade - (%) - Instituto Brasileiro de Geografia e Estatística, Departamento de População e Indicadores Sociais. Divisão de Estudos e Análises da Dinâmica Demográfica (IBGE/Pop) - DEPIS_TFEC -`
min(dados3$Data) # Data inicial
max(dados3$Data) # Data final
d3<-ts(y3,start = c(2000),end = c(2022),frequency = 1)# Anual frequencia = 1

# Gráfico da Série
autoplot(d3,xlab = "Tempo (Anos)",ylab = "Taxa de Fecundidade")+theme_minimal()
# ou
dygraph(d3,xlab = "Tempo (Anos)",ylab = "Taxa de Fecundidade") %>% dyRangeSelector()

# Gráfico da Função de Autocorrelação (FAC)
ggAcf(d3,lag.max = 12, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

# Gráfico da Função de Autocorrelação Parcial (FACP)
ggAcf(d3,lag.max = 12,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()
