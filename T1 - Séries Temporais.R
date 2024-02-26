# TRABALHO 1 - SÉRIES TEMPORAIS - Grupo 17


# PACOTES UTILIZADOS E SÉRIE TEMPORAL ESCOLHIDA ----

library(Mcomp)
library(forecast)
library(tidyverse)

data(M3)
id=2334 # id da série escolhida

horizonte <- M3[[id]]$h
serie_temp <- M3[[id]]$x
out_sample <- M3[[id]]$xx

M3[[id]]$description # descrição da série

M3[[id]]$type
M3[[id]]$period
M3[[id]]$n


# Gráfico da Série
serie_temp %>% autoplot() + 
  geom_line(color = "#1f0866", linewidth = 0.8) +
  xlab("Anos") +
  ylab("Vendas Totais (milhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  scale_y_continuous(breaks = seq(from = 4000,to = 10000,by = 1000), 
                     limits = c(4500,10000)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()
  
serie_temp %>% plot()  # opção mais simples



# DECOMPOSIÇÃO DA SÉRIE TEMPORAL VIA STL (OU MSTL) ----

stl(serie_temp, s.window=12) %>% plot( main='s.window=12')
mstl(serie_temp, s.window = "periodic") %>% plot()

mstl(serie_temp, s.window = "periodic") %>% autoplot() +
  geom_line(color = c("#1f0866"), linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  theme_minimal()



# MODELO ARIMA - FORMA MANUAL ----

# Operador de diferença (tendencia e sazonalidade)
ndiffs(serie_temp) # 1 diferença   d = 1
diff(serie_temp) %>% nsdiffs() # 1 diferença  D = 1


# SARIMA(p,1,q)x(P,1,Q)
x <- diff(serie_temp) %>% diff(lag=12) #Série estacionária
par(mfrow=c(1,3))
plot(x);acf(x,lag.max = 12*5);pacf(x, lag.max = 12*5)


x %>% autoplot() + 
  geom_line(color = "#1f0866", linewidth = 0.8) +
  xlab("Anos") +
  ylab("Valores da Série") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  scale_y_continuous(breaks = seq(from = -800,to = 600,by = 100), 
                     limits = c(-800,800)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()




melhor_AICc = Inf
for(p in 0:3){
  for(q in 0:3){
    fit = Arima(serie_temp, order = c(p,1,q), seasonal = c(0,1,1))
    if(fit$aicc < melhor_AICc){
      melhor_AICc = fit$aicc
      cat("p =",p,",q =",q,",AICc =",fit$aicc, "\n")
    }
  }
}


############################################################
# FUNÇÃO PARA VALIDAÇÃO, EXCLUIR QUANDO ENVIAR PRO PROFESSOR
auto.arima(serie_temp, stepwise = T, stationary = F)
auto.arima(serie_temp, stepwise = F, stationary = F)
############################################################



# ANÁLISE DE RESÍDUOS ----

# Modelo SARIMA(0,1,3)x(1,1,1)
fit = Arima(serie_temp, order=c(0,1,3), seasonal=c(0,1,1))
fit

# Resíduos
par(mfrow=c(1,2))
E_1 <- fit$residuals
plot(E_1)
E <- fit$residuals %>% window(start=c(1984,2))
plot(E)

#
par(mfrow=c(1,3))
plot(E)
qqnorm(E); qqline(E)
acf(E, lag.max = 12*5)
hist(E)

# verificando estacionariedade

tseries::kpss.test(E)


# verificando independência
Box.test(E,lag = 15, type = "Ljung-Box")
Box.test(E,lag = 20, type = "Ljung-Box")

# Normalidade dos resíduos
shapiro.test(E)

# Horizonte de previsão
par(mfrow=c(1,1))
f = forecast(fit, horizonte)
f
f %>% autoplot()


mean(E)
sd(E)























