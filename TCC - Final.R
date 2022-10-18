#### 1) Instalar e Carregar os pacotes a serem utilizados ####

pacotes <- c("readr", "readxl", "plotly", "tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3", "lubridate",
             "urca", "dygraphs", "quantmod", "BETS", "tseries", "FinTS",
             "scales", "caret", "xtable", "tsutils", "GetBCBData", "dgof",
             "seasonal", "xlsx", "kableExtra", "rnn", "neuralnet", "keras",
             "tensorflow", "nnfor", "MLmetrics")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#--------------------------------------------------------------------#

#### 2) Carregar a base de dados ####

BRF <- read_excel("BRF Teste.xlsx", 
                  col_types = c("text", "numeric"))
View(BRF)

# Foi escolhido apenas a serie de dados (Receita Operacional Bruta do MERCADO INTERNO).
# O motivo é uma maior quantidade de valores. Nos dados do mercado externo algumas 
# operações foram descontinuadas em 2015/2016, assim trazendo variações para a serie.
# Também foi desconsiderado o 1 e 2 trimentes de 2009 por não conter a "Sadia"

#--------------------------------------------------------------------#

#### 3) Transformando em Time Series e Separação Treino/Teste ####

brf_ts <- ts(data = BRF[2], 
             start = c(2009, 3), 
             end = c(2022,1), 
             frequency = 4 )

length(brf_ts)

## Separando entre serie teste / treino

brf_treino <- window(brf_ts, 
                     start = c(2009,3), 
                     end = c(2020,3))
length(brf_treino)

brf_teste <- window(brf_ts, 
                    start = c(2020,4), 
                    end = c(2022,1))
length(brf_teste)
# Para poder validar o modelo, assim evitamos problemas de overfitting

## Visualização Serie Treino e Serie Teste

autoplot(brf_ts) +
  autolayer(brf_treino, series = "Treino") +
  autolayer(brf_teste, series = "Teste") +
  scale_color_viridis_d() +
  theme_bw()

#--------------------------------------------------------------------#

#### 4) ANALISE EXPLORATORIA ####

## 4.1) Visualizando a Time Series como um Todo

options(scipen = 999)
plot(brf_ts, main="Receita Operacional Bruta (Mercado Nacional) - 3T/2009 ao 1T/2022",
     xlab="Trimestres - jul/2009 a mar/2022", ylab="R$ mil")

# Verificamos uma tendencia de crescimento e sazonalidade

## 4.2) Decompondo e Visualizando (TS Patterns)
{
  # 4.2.1) Visualizando a Sazonalidade
  
  #Separacao e media por Trimeste
  ggsubseriesplot( x = brf_ts )
  
  # Q1 e Q2 Sao muito proximos
  # Q3 Existe um leve aumento em relacao a Q1 e Q2
  # Q4 Existe um aumento grande em relacao a Q3
  
  # 4.2.2) Automatico pelo R (Tendencia, Sazonalidade e "Erro")
  
  ## Decomposicao Classica
  plot_analise <- decompose(brf_ts, type = "multiplicative")
  
  #multiplicativo - Por causa dos efeitos sazonais aumentando com o tempo
  
  autoplot(plot_analise$trend)
  autoplot(plot_analise$seasonal)
  autoplot(plot_analise$random)
  autoplot(plot_analise)
  
  ## Decomposicao X-13ARIMA-SEATS
  
  plot_X13 <- seas(brf_ts)
  autoplot(plot_X13)
  summary(plot_X13)
}

## 4.3) AutoCorrelacao
{
  # 4.3.1) AutoCorrelacao Total
  
  ACF <- acf(brf_treino, lag.max = 20, type = "correlation")
  
  # Decaimento devagar -> Apresenta Tendencia e Serie não aparenta ser Estacionaria
  # Aumento a cada 4 Medicoes -> Sazonalidade a cada 4 colunas.
  
  # 4.3.2) AutoCorrelacao Parcial 
  
  PACF <- pacf(brf_treino, lag.max = 20)
  
  # Alguns valores significativos, interessante verificar
  
}

#--------------------------------------------------------------------#

#### 5) MODELO ETS (SUAVIZACAO EXPONENCIAL) ####

## 5.1) SES - Suavizacao Exponencial Simples (L)
{
  # Y*[T+1] = Alpha * Y[t] 
  #           + Alpha(1- Alpha) * Y[t-1] 
  #           + Alpha(1-Alpha)^2 * Y[t-2]...
  
  modeloSES <- ses(brf_treino, h = 6)
  
  modeloSES
  modeloSES$model
  modeloSES$fitted
  modeloSES$residuals
  autoplot(modeloSES)
  
  # ForeCast
  brf_ses_forecast <- forecast::forecast(modeloSES, h = 6)
  
  # Acuracia
  ac_ses <- forecast::accuracy(brf_ses_forecast, brf_teste)
  ac_ses
  
  # Ajustando DataFrame com modelos
  
  Resultados_exp <- rbind(    data.frame( 'Nome do Modelo' = brf_ses_forecast$model$method, 
                                          'AIC' = brf_ses_forecast$model$aic,
                                          'MAPE - Treino' = ac_ses[9],
                                          'MAPE - Teste' = ac_ses[10]))
}
## 5.2) Holt - Com Tendencida e Sem Sazonalidade (L // T)
{
  # Y*[t+h] = L[t]+ h*T[t]
  # L[t] = Alpha * Y[t] + (1 - Alpha)*(L[t-1]+T[t-1])
  # T[t] = Beta * (L[t]-L[t-1]) + (1 - Beta)T[t-1] 
  
  modeloholt <- holt(brf_treino, h = 6) 
  
  modeloholt
  modeloholt$model
  modeloholt$fitted
  modeloholt$residuals
  autoplot(modeloholt)
  
  # ForeCast
  brf_holt_forecast <- forecast::forecast(modeloholt, h = 6)
  
  # Acuracia
  ac_ht <- forecast::accuracy(brf_holt_forecast, brf_teste)
  ac_ht
  
  # Ajustando DataFrame com modelos
  
  Resultados_exp <- rbind(    Resultados_exp,
                              data.frame( 'Nome do Modelo' = brf_holt_forecast$model$method, 
                                          'AIC' = brf_holt_forecast$model$aic,
                                          'MAPE - Treino' = ac_ht[9],
                                          'MAPE - Teste' = ac_ht[10]))
}
## 5.3) SEH - Tendencia e Sazonalidade (L // T // S)
{
  ## HoltWinter Aditivo
  # Y[T+H|T] = L[T] + h * B[T] + S[T + h - m * (k+1)]
  # L[T] = Alpha * (Y[Y] - S[T-m]) + (1-Alpha) * (L[T-1]+B[T-1])
  # B[T] = Beta * (L[T] - L[T-1]) + (1 - Beta) * B[T-1]
  # S[T] = Gama * (Y[T] - L[T-1] - B[T-1]) + (1 - Gama) * S[T-m]
  
  modelohwa <- hw(brf_treino, h = 6, seasonal = "additive")
  
  modelohwa  
  modelohwa$model
  modelohwa$fitted
  modelohwa$residuals
  autoplot(modelohwa)
  
  # ForeCast
  brf_hwa_forecast <- forecast::forecast(modelohwa, h = 6)
  
  # Acuracia
  ac_hwa <- forecast::accuracy(brf_hwa_forecast, brf_teste)
  ac_hwa
  
  # Ajustando DataFrame com modelos
  Resultados_exp <- rbind(    Resultados_exp,
                              data.frame( 'Nome do Modelo' = brf_hwa_forecast$model$method, 
                                          'AIC' = brf_hwa_forecast$model$aic,
                                          'MAPE - Treino' = ac_hwa[9],
                                          'MAPE - Teste' = ac_hwa[10]))
  
  ## HoltWinter Multiplicativo
  # Y[T+H|T] = ( L[T] + h * B[T] ) * S[T + h - m * (k+1)]
  # L[T] = Alpha * (Y[Y] / S[T-m]) + (1-Alpha) * (L[T-1]+B[T-1])
  # B[T] = Beta * (L[T] - L[T-1]) + (1 - Beta) * B[T-1]
  # S[T] = Gama * (Y[T] / ( L[T-1] + B[T-1] ) ) + (1 - Gama) * S[T-m]
  
  modelohwm <- hw(brf_treino, h = 6, seasonal = "multiplicative")
  
  modelohwm  
  modelohwm$model
  modelohwm$fitted
  modelohwm$residuals
  autoplot(modelohwm)
  
  # ForeCast
  brf_hwm_forecast <- forecast::forecast(modelohwm, h = 6)
  
  # Acuracia
  ac_hwm <- forecast::accuracy(brf_hwm_forecast, brf_teste)
  ac_hwm
  
  # Ajustando DataFrame com modelos
  Resultados_exp <- rbind(    Resultados_exp,
                              data.frame( 'Nome do Modelo' = brf_hwm_forecast$model$method, 
                                          'AIC' = brf_hwm_forecast$model$aic,
                                          'MAPE - Treino' = ac_hwm[9],
                                          'MAPE - Teste' = ac_hwm[10]))
}
## 5.4) ETS - "Automatica R" - Modela individualmente cada comportamento
{
  modeloets <- ets(brf_treino, model = "ZZZ") 
  
  modeloets
  summary(modeloets)
  # ETS -> Error -> Multiplicative / Trend -> Additive / Season -> Multiplicative
  
  # Forecast
  brf_ets_forecast <- forecast.ets(modeloets, h = 6)
  autoplot(brf_ets_forecast)
  
  # Acuracia
  ac_ets <- forecast::accuracy(brf_ets_forecast, brf_teste)
  ac_ets
  
  # Ajustando DataFrame com modelos
  Resultados_exp <- rbind(    Resultados_exp,
                              data.frame( 'Nome do Modelo' = brf_ets_forecast$model$method, 
                                          'AIC' = brf_ets_forecast$model$aic,
                                          'MAPE - Treino' = ac_ets[9],
                                          'MAPE - Teste' = ac_ets[10]))
}
## 5.5) Comparacao dos modelos 
{
  Resultados_exp %>% 
    kable() %>%
    kable_styling(bootstrap_options = "striped", 
                  full_width = TRUE, 
                  font_size = 12)
  
  # Foi escolhido o modelo ETS(M,A,M), por ter o menor AIC.
  # E menor RSME e MAPE na base de Validacao.
}
## 5.6) Verificacao dos Residuos
{
  checkresiduals(modelohwm)
  
  #Ljung-Box p-value = 0.1225 > 0.05 -> Aceitamos H0, residuos nao sao 
  #correlacionados
  
  #Normalidade dos residuos
  ks.test(modelohwm$residuals, "pnorm", mean(modelohwm$residuals),
          sd(modelohwm$residuals))
  #p-value = 0.7457 > 0.05. Aceito H0, residuos são Normais
  
  ArchTest(modelohwm$residuals)
  #p-value = 0.5630 > 0.05 -> Nao existe efeitos ARCH
}

#--------------------------------------------------------------------#

#### 6) MODELO ARRIMA ####

# Parametros ARIMA / SARIMA
# AR (AutoRegressivo) - p - PACF          / P - SAR //
# I (Integracao)      - d - Diferenciacao / D - SI  // T - periodo saz.  
# MA (Medias Moveis)  - q - ACF           / Q - SMA //

## 6.1) MODELO BASEADOS NOS GRAFICOS
{
  # 6.1.1) IDENTIFICACAO DO MODELO
  
  ACF <- acf(brf_ts, lag.max = 20, type = "correlation")
  
  #Decaimento devagar -> Apresenta Tendencia
  #Aumento a cada 4 Medicoes -> Sazonalidade a cada 4 colunas.
  
  PACF <- pacf(brf_ts, lag.max = 20)
  
  #Serie nao Aparenta ser estacionaria
  
  # 6.1.2) Teste de Estacionariedade
  
  # Teste de Dickey-Fuller (H0 = Nao e Estacionaria / H1 = Estacionaria)
  
  testeDF <- ur.df(brf_treino)
  testeDF
  summary(testeDF)
  #Serie NaO e estacionaria P-Value = 04177 > 0,05
  #Necessario uma transformacao - realizar a diferenciacao
  
  # 6.1.3) DIFERENCIACAO
  
  # Busca a quantidade de diferenciacao
  ndiffs(brf_treino)
  
  # Diferenciacao I = 1
  brf_treino_dif <- diff(brf_treino)
  
  # Verificacao da Estacionariedade da Serie Diferenciada
  
  testeDF_diff <- ur.df(brf_treino_dif)
  summary(testeDF_diff)
  #Serie E Estacionaria P-Value = 3,71x10^-9 < 0,05
  
  # 6.1.4) ESTIMACAO DO MODELO - (PARAMETROS)
  
  # 6.1.4.1) I - Integracao
  
  ndiffs(brf_treino)
  
  # d = 1 / D = 1
  
  # 6.1.4.2) Q - AutoRegressao - ACF
  
  ACF_dif <- acf(brf_treino_dif, 
                 lag.max = 20, 
                 type = "correlation")
  #T = [4] -> Sazonalidade
  #Verificacao Sazonal (lag = 4/8/12/16/20)
  #Hipotese 1 -> Q = 1 (Lag estao decaindo com o tempo)
  #Hipotese 2 -> Q = 1 (Lag estao decaindo com o tempo)
  #Verificacao Entre Sazonalidades (lag = [1:3]/[5:7]/[9:11])
  #Hipotese 1 -> q > 0 (Lag esta acompanhando sazonalidade / insignificativo)
  #Hipotese 2 -> q = 0 (Lag esta acompanhando sazonalidade / insignificativo)
  
  # 6.1.4.3) P - Medias Moveis - PACF
  
  PACF_dif <- pacf(brf_treino_dif, 
                   lag.max = 20)
  
  #Verificacao Sazonal (lag = 4/8/12/16/20)
  #Hipotese 1 -> P = 0 (Sem Lags significativos)
  #Hipotese 2 -> P = 1 (Spike no lag 1 e depois insignificativos)
  #Verificacao Entre Sazonalidades (lag = [1:3]/[5:7]/[9:11])
  #Hipotese 1 -> p = 0 (Lag esta acompanhando sazonalidade / insignificativo)
  #Hipotese 2 -> p = 3 (Spike no lag 3 e depois insignificativos)
  
  # 6.1.4.4) Analise dos Modelos SARIMA
  
  # Teoria 1 - (0,1,0)x(0,1,1)
  mod_arima_s1 <- arima( x = brf_treino, 
                         order = c(0,1,0), 
                         seasonal = list(order = c(0,1,1),
                                         period = 4)
  )
  # Teoria 2 - (0,1,0)x(1,1,1)
  mod_arima_s2 <- arima( x = brf_treino, 
                         order = c(0,1,0), 
                         seasonal = list(order = c(1,1,1),
                                         period = 4)
  )
  # Teoria 3 - (0,1,1)x(0,1,1)
  mod_arima_s3 <- arima( x = brf_treino, 
                         order = c(0,1,1), 
                         seasonal = list(order = c(0,1,1),
                                         period = 4)
  )
  # Teoria 4 - (0,1,1)x(1,1,1)
  mod_arima_s4 <- arima( x = brf_treino, 
                         order = c(0,1,1), 
                         seasonal = list(order = c(1,1,1),
                                         period = 4)
  )
  # Teoria 5 - (3,1,0)x(0,1,1)
  mod_arima_s5 <- arima( x = brf_treino, 
                         order = c(3,1,0), 
                         seasonal = list(order = c(0,1,1),
                                         period = 4)
  )
  # Teoria 6 - (3,1,0)x(1,1,1)
  mod_arima_s6 <- arima( x = brf_treino, 
                         order = c(3,1,0), 
                         seasonal = list(order = c(1,1,1),
                                         period = 4)
  )
  # Teoria 7 - (3,1,1)x(0,1,1)
  mod_arima_s7 <- arima( x = brf_treino, 
                         order = c(3,1,1), 
                         seasonal = list(order = c(0,1,1),
                                         period = 4)
  )
  
  # Teoria 8 - (3,1,1)x(1,1,1)
  mod_arima_s8 <- arima( x = brf_treino, 
                         order = c(3,1,1), 
                         seasonal = list(order = c(1,1,1),
                                         period = 4)
  )
  
  ## 6.1.5) Previsao (H = 5)
  
  prev_s1_brf <- forecast::forecast(mod_arima_s1, h = 6)
  prev_s2_brf <- forecast::forecast(mod_arima_s2, h = 6)
  prev_s3_brf <- forecast::forecast(mod_arima_s3, h = 6)
  prev_s4_brf <- forecast::forecast(mod_arima_s4, h = 6)
  prev_s5_brf <- forecast::forecast(mod_arima_s5, h = 6)
  prev_s6_brf <- forecast::forecast(mod_arima_s6, h = 6)
  prev_s7_brf <- forecast::forecast(mod_arima_s7, h = 6)
  prev_s8_brf <- forecast::forecast(mod_arima_s8, h = 6)
  
  ## 6.1.6) Acuraria (Comparacao brf_teste)
  
  ac_s1 <- as.data.frame(forecast::accuracy(prev_s1_brf, brf_teste))
  ac_s2 <- as.data.frame(forecast::accuracy(prev_s2_brf, brf_teste))
  ac_s3 <- as.data.frame(forecast::accuracy(prev_s3_brf, brf_teste))
  ac_s4 <- as.data.frame(forecast::accuracy(prev_s4_brf, brf_teste))
  ac_s5 <- as.data.frame(forecast::accuracy(prev_s5_brf, brf_teste))
  ac_s6 <- as.data.frame(forecast::accuracy(prev_s6_brf, brf_teste))
  ac_s7 <- as.data.frame(forecast::accuracy(prev_s7_brf, brf_teste))
  ac_s8 <- as.data.frame(forecast::accuracy(prev_s8_brf, brf_teste))
  
  ## 6.1.7) Ajustando DataFrame com modelos
  
  Resultados_arima <- rbind(
    data.frame('Nome do Modelo' = c(prev_s1_brf$method,
                                    prev_s2_brf$method,
                                    prev_s3_brf$method,
                                    prev_s4_brf$method,
                                    prev_s5_brf$method,
                                    prev_s6_brf$method,
                                    prev_s7_brf$method,
                                    prev_s8_brf$method), 
               'AIC' = c(mod_arima_s1$aic,
                         mod_arima_s2$aic,
                         mod_arima_s3$aic,
                         mod_arima_s4$aic,
                         mod_arima_s5$aic,
                         mod_arima_s6$aic,
                         mod_arima_s7$aic,
                         mod_arima_s8$aic),
               'MAPE - Treino' = c(ac_s1$MAPE[1],
                                   ac_s2$MAPE[1],
                                   ac_s3$MAPE[1],
                                   ac_s4$MAPE[1],
                                   ac_s5$MAPE[1],
                                   ac_s6$MAPE[1],
                                   ac_s7$MAPE[1],
                                   ac_s8$MAPE[1]),
               'MAPE - Teste' = c(ac_s1$MAPE[2],
                                  ac_s2$MAPE[2],
                                  ac_s3$MAPE[2],
                                  ac_s4$MAPE[2],
                                  ac_s5$MAPE[2],
                                  ac_s6$MAPE[2],
                                  ac_s7$MAPE[2],
                                  ac_s8$MAPE[2])
    )
    
  )
}
## 6.2) MODELO AUTO ARIMA
{
  # 6.2.1) Estimacao do Modelo
  
  mod_auto_arima <- auto.arima(y = brf_treino, trace = TRUE)
  
  # 6.2.2) Previsao (H = 5)
  
  prev_AA_brf <- forecast::forecast(mod_auto_arima, h = 6 )
  
  autoplot(prev_AA_brf)+
    theme_bw()
  
  # 6.2.3) Acuraria (Comparacao brf_teste)
  
  ac_aa <- as.data.frame(forecast::accuracy(prev_AA_brf, brf_teste))
  
  # 6.2.4) Ajustando DataFrame com modelos
  
  Resultados_arima <- rbind( Resultados_arima,
                             data.frame( 'Nome do Modelo' = prev_AA_brf$method, 
                                         'AIC' = mod_auto_arima$aic,
                                         'MAPE - Treino' = ac_aa$MAPE[1],
                                         'MAPE - Teste' = ac_aa$MAPE[2]))
  
  # Resultado diferente do Arima realizado pelos graficos ACF e PACF.
  # Porem valores de AIC são maiores.
}
## 6.3) ITERACAO DE MODELOS (TESTE DE OUTROS MODELOS)
{
  # I - Integracao // T - Tempo Sazonalidade
  
  ndiffs(brf_treino)
  
  # d = D <= 1
  # T = 4
  
  # Q - AutoRegressao - ACF
  
  ACF_dif <- acf(brf_treino_dif, 
                 lag.max = 20, 
                 type = "correlation")
  # q + 4Q <= 5 
  # q -> Poderia variar de 0 a 3 porapresentar significancia no lag 3
  # Q -> Apresenta sazonalidade e decaindo = 1
  
  # P - Medias Moveis - PACF
  
  PACF_dif <- pacf(brf_treino_dif, 
                   lag.max = 20)
  
  # p + 4P <= 5
  # p -> Poderia variar de 0 a 3 porapresentar significancia no lag 3
  # P -> Apresenta 1 lag significativo na 1 sazonalidade 
  # Verificar caso estatistico de [+-2/(N)^(1/2)] apresentar lag significativo
  # por casualidade
  
  # Matrix com os Parametros
  
  qQ <- list()
  for(i in 1:6) {qQ[[i]]=c(i-1,0)}
  qQ[[7]]=c(0,1)
  qQ[[8]]=c(1,1)
  pP=qQ
  qQ
  pP
  dt_params <- c()
  for (i in 1:8) {
    for (j in 1:8) {
      temp <- c(pP[[i]][1],1,qQ[[j]][1],pP[[i]][2],1,qQ[[j]][2],4)
      ##        (   p     ,d,    q     ,    P     ,D,    Q     ,T)
      dt_params <- rbind(temp,dt_params)
    }
    
  }
  dt_params
  colnames(dt_params) <- c("p","d","q","P","D","Q","T")
  rownames(dt_params) <- 1:64
  
  # Estimando os Modelos
  
  models <- vector("list", 64)
  for (i in 1:64) {
    try(models[[i]] <- arima(x = brf_treino,
                             order = dt_params[i,1:3],
                             seasonal = list(order=dt_params[i,4:6],
                                             period=4)
    ))
  }
  
  # Previsao (H = 5)
  
  prev_models <- vector("list", 64)
  for (i in 1:64) {
    try(prev_models[[i]] <- forecast::forecast(models[[i]], h = 6 )
    )
  }
  
  # Acuraria (Comparacao brf_teste)
  
  ac_models <- vector("list", 64)
  for (i in 1:64) {
    try(ac_models[[i]] <- forecast::accuracy(prev_models[[i]], brf_teste)
    )
  }
  
  # Ajustando DataFrame com modelos
  
  Resultados2 <- rbind(           data.frame(
    'Nome do Modelo' = NA, 
    'AIC' = NA,
    'MAPE - Treino' = NA,
    'MAPE - Teste' = NA))
  
  for (i in 1:64) {
    try(Resultados2 <- rbind(Resultados2, 
                             data.frame('Nome do Modelo' = prev_models[[i]]$method, 
                                        'AIC' = prev_models[[i]]$model$aic,
                                        'MAPE - Treino' = ac_models[[i]][9],
                                        'MAPE - Teste' = ac_models[[i]][10]))
    )
    
  }
  
  Resultados2 %>% 
    kable() %>%
    kable_styling(bootstrap_options = "striped", 
                  full_width = TRUE, 
                  font_size = 12)
  
  # Menores valores de AIC são compativeis com os Modelos 
  # com base em ACF e PACF
  # Melhor Modelo SARIMA(0,1,0)(0,1,1)[4]
  
}

## 6.4) Verificacao dos Residuos do Melhor Modelo
{
  checkresiduals(mod_arima_s1)
  
  #Ljung-Box p-value = 0.9804 > 0.05 -> Aceitamos H0, residuos nao sao 
  #correlacionados
  
  #Normalidade dos residuos
  ks.test(mod_arima_s1$residuals, "pnorm", mean(mod_arima_s1$residuals),
          sd(mod_arima_s1$residuals))
  #p-value = 0.2296 > 0.05. Aceito H0, residuos são Normais
  
  ArchTest(mod_arima_s1$residuals)
  #p-value = 0.9281 > 0.05 -> Nao existe efeitos ARCH
}

#--------------------------------------------------------------------#

#### 7) REDES NEURAIS ####

## 7.1) Pre-processamento do dados
{
  # Input (x) e Output (y) dos dados
  # faturamento (t) = f(faturamento (t-1))
  
  data_used <- BRF
  
  # 7.1.1) Defasar o Faturamento
  
  periodos_anteriores = 4
  
  n_col = ncol(data_used)
  
  for (i in 1:periodos_anteriores) {
    for (j in 1:nrow(data_used)) {
      if (j - periodos_anteriores <= 0) {
        
      } else {
        data_used[j, n_col + i] = data_used[j - i, 2]
      }
      
    }
  }  
  
  names(data_used) = c("data", "Fat", "Fat_M1","Fat_M2","Fat_M3","Fat_M4")
  
  # Excluir o valor NA (inicial)
  
  data_analise <- data_used[5:(nrow(data_used)),-1]
  
  # 7.1.2) Separando Conjunto de Treinamento e Teste
  ## Treino = 1:41
  ## Teste = 42:47
  
  data_treino <- data_analise[1:41,]
  data_teste <- data_analise[42:47,]
  
  X_T_1 <- data_treino[,2]
  X_T_2 <- data_treino[,3]
  X_T_3 <- data_treino[,4]
  X_T_4 <- data_treino[,5]
  Y_T <- data_treino[,1]
  
  X_Teste_1 <- data_teste[,2]
  X_Teste_2 <- data_teste[,3]
  X_Teste_3 <- data_teste[,4]
  X_Teste_4 <- data_teste[,5]
  Y_Teste <- data_teste[,1]
  
  
  # Normalizando os dados
  
  Ys_T <- linscale(Y_T$Fat, minmax = list(mn = 0, mx = 1)) 
  Xs_T_1 <- linscale(X_T_1$Fat_M1, minmax = list(mn = 0, mx = 1))
  Xs_T_2 <- linscale(X_T_2$Fat_M2, minmax = list(mn = 0, mx = 1))
  Xs_T_3 <- linscale(X_T_3$Fat_M3, minmax = list(mn = 0, mx = 1))
  Xs_T_4 <- linscale(X_T_4$Fat_M4, minmax = list(mn = 0, mx = 1))
  
  Xs_Teste_1 <- linscale(X_Teste_1$Fat_M1, minmax = list(mn = 0, mx = 1))
  Xs_Teste_2 <- linscale(X_Teste_2$Fat_M2, minmax = list(mn = 0, mx = 1))
  Xs_Teste_3 <- linscale(X_Teste_3$Fat_M3, minmax = list(mn = 0, mx = 1))
  Xs_Teste_4 <- linscale(X_Teste_4$Fat_M4, minmax = list(mn = 0, mx = 1))
  
  # Criando o df de treino
  
  Y_treino <- array(Ys_T$x, dim = c(nrow(data_treino),1))
  X_treino <- array(c(Xs_T_1$x,
                      Xs_T_2$x,
                      Xs_T_3$x,
                      Xs_T_4$x), dim = c(nrow(data_treino), 1, 4))
  X_treino_1 <- array(c(Xs_T_1$x), dim = c(nrow(data_treino), 1, 1))
  
  # Criando o df de teste
  
  X_teste <- array(c(Xs_Teste_1$x,
                     Xs_Teste_2$x,
                     Xs_Teste_3$x,
                     Xs_Teste_4$x), dim = c(nrow(data_teste), 1, 4))
  X_teste_1 <- array(c(Xs_Teste_1$x), dim = c(nrow(data_teste), 1, 1))
  
  
}

## 7.2) Arquiteturas das Redes Neurais
{
  
  # Camada de Entrada / Camada Escondida / Camada de Saida
  # Camada de Entrada = Serão executadas duas possibilidades 
  #com 1 e 4 neuronios
  # Camada Escondida = Serão executadas três possibilidades 
  #com 5, 10 e 20 neuronios
  # Camada de Saida = Será executadas uma possibilidade com 1 neuronio
  
  #Visualização da rede neural com 1/10/1
  
  n_1_10_1 <- neuralnet(Fat ~ Fat_M1,
                        data = data_analise,
                        hidden = 10,
                        linear.output = F,
                        lifesign = 'full',
                        rep = 1)
  plot(n_1_10_1,
       col.hidden = 'darkgreen',
       col.hidden.synapse = 'darkgreen',
       show.weights = F,
       information = F,
       fill = 'lightblue')
  
  #Visualização da rede neural com 4/5/1
  
  n_4_5_1 <- neuralnet(Fat ~ .,
                       data = data_analise,
                       hidden = 5,
                       linear.output = F,
                       lifesign = 'full',
                       rep = 1)
  plot(n_4_5_1,
       col.hidden = 'darkgreen',
       col.hidden.synapse = 'darkgreen',
       show.weights = F,
       information = F,
       fill = 'lightblue')
  
}

## 7.3) RNN

## 7.3.1) RNN 1_5_1
{
  # Criando Modelo
  
  rnn_151 <- trainr( Y = Y_treino,
                     X = X_treino_1,
                     learningrate = 0.1,
                     hidden_dim = 5,
                     numepochs = 100,
                     network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_151 <- as.data.frame(predictr(rnn_151, X = X_treino_1))
  Pred_desnorm_rnn_151 <- linscale(Pred_T_151$V1,
                                   minmax = Xs_T_2$minmax,
                                   rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_151 <- as.data.frame(predictr(rnn_151, X = X_teste_1))
  Pred_desnorm_teste_rnn_151 <- linscale(Pred_Teste_151$V1,
                                         minmax = Xs_Teste_1$minmax,
                                         rev = TRUE)
  
  #Acuracia
  
  MapeTreino <- MAPE(Pred_desnorm_rnn_151$x,Y_T$Fat) *100
  MapeTeste <- MAPE(Pred_desnorm_teste_rnn_151$x,Y_Teste$Fat) *100
  SSE <- sum((Pred_desnorm_rnn_151$x - Y_T$Fat)^2)
  AIC <- (2*rnn_151$input_dim)+length(X_treino_1)*log(SSE/length(X_treino_1)) 
  
  
  #Data Frame Resultado
  Resultados_RNA <- rbind(                data.frame( 'Nome do Modelo' = 'RNN - 1/5/1', 
                                                      'AIC' = AIC,
                                                      'MAPE - Treino' = MapeTreino,
                                                      'MAPE - Teste' = MapeTeste))
  
  
  
  
}

## 7.3.2) RNN 1_10_1
{
  # Criando Modelo
  
  rnn_1101 <- trainr( Y = Y_treino,
                      X = X_treino_1,
                      learningrate = 0.1,
                      hidden_dim = 10,
                      numepochs = 100,
                      network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_1101 <- as.data.frame(predictr(rnn_1101, X = X_treino_1))
  Pred_desnorm_rnn_1101 <- linscale(Pred_T_1101$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_1101 <- as.data.frame(predictr(rnn_1101, X = X_teste_1))
  Pred_desnorm_teste_rnn_1101 <- linscale(Pred_Teste_1101$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  MapeTreino2 <- MAPE(Pred_desnorm_rnn_1101$x,Y_T$Fat) *100
  MapeTeste2 <- MAPE(Pred_desnorm_teste_rnn_1101$x,Y_Teste$Fat) *100
  SSE2 <- sum((Pred_desnorm_rnn_1101$x - Y_T$Fat)^2)
  AIC2 <- (2*rnn_1101$input_dim)+length(X_treino_1)*log(SSE2/length(X_treino_1))
  
  
  #Data Frame Resultado
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'RNN - 1/10/1', 
                                       'AIC' = AIC2,
                                       'MAPE - Treino' = MapeTreino2,
                                       'MAPE - Teste' = MapeTeste2))
}

## 7.3.3) RNN 1_20_1
{
  # Criando Modelo
  
  rnn_1201 <- trainr( Y = Y_treino,
                      X = X_treino_1,
                      learningrate = 0.1,
                      hidden_dim = 20,
                      numepochs = 100,
                      network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_1201 <- as.data.frame(predictr(rnn_1201, X = X_treino_1))
  Pred_desnorm_rnn_1201 <- linscale(Pred_T_1201$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_1201 <- as.data.frame(predictr(rnn_1201, X = X_teste_1))
  Pred_desnorm_teste_rnn_1201 <- linscale(Pred_Teste_1201$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  MapeTreino3 <- MAPE(Pred_desnorm_rnn_1201$x,Y_T$Fat) *100
  MapeTeste3 <- MAPE(Pred_desnorm_teste_rnn_1201$x,Y_Teste$Fat) *100
  SSE3 <- sum((Pred_desnorm_rnn_1201$x - Y_T$Fat)^2)
  AIC3 <- (2*rnn_1201$input_dim)+length(X_treino_1)*log(SSE3/length(X_treino_1))
  
  
  #Data Frame Resultado
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'RNN - 1/20/1', 
                                       'AIC' = AIC3,
                                       'MAPE - Treino' = MapeTreino3,
                                       'MAPE - Teste' = MapeTeste3))
}

## 7.3.4) RNN 4_5_1
{
  # Criando Modelo
  
  rnn_451 <- trainr( Y = Y_treino,
                     X = X_treino,
                     learningrate = 0.1,
                     hidden_dim = 5,
                     numepochs = 100,
                     network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_451 <- as.data.frame(predictr(rnn_451, X = X_treino))
  Pred_desnorm_rnn_451 <- linscale(Pred_T_451$V1,
                                   minmax = Xs_T_2$minmax,
                                   rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_451 <- as.data.frame(predictr(rnn_451, X = X_teste))
  Pred_desnorm_teste_rnn_451 <- linscale(Pred_Teste_451$V1,
                                         minmax = Xs_Teste_1$minmax,
                                         rev = TRUE)
  
  #Acuracia
  
  MapeTreino4 <- MAPE(Pred_desnorm_rnn_451$x,Y_T$Fat) *100
  MapeTeste4 <- MAPE(Pred_desnorm_teste_rnn_451$x,Y_Teste$Fat) *100
  SSE4 <- sum((Pred_desnorm_rnn_451$x - Y_T$Fat)^2)
  AIC4 <- (2*rnn_451$input_dim)+length(X_treino_1)*log(SSE4/length(X_treino_1))
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'RNN - 4/5/1', 
                                       'AIC' = AIC4,
                                       'MAPE - Treino' = MapeTreino4,
                                       'MAPE - Teste' = MapeTeste4))
  
}

## 7.3.5) RNN 4_10_1
{
  # Criando Modelo
  
  rnn_4101 <- trainr( Y = Y_treino,
                      X = X_treino,
                      learningrate = 0.1,
                      hidden_dim = 10,
                      numepochs = 100,
                      network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_4101 <- as.data.frame(predictr(rnn_4101, X = X_treino))
  Pred_desnorm_rnn_4101 <- linscale(Pred_T_4101$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_4101 <- as.data.frame(predictr(rnn_4101, X = X_teste))
  Pred_desnorm_teste_rnn_4101 <- linscale(Pred_Teste_4101$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  
  MapeTreino5 <- MAPE(Pred_desnorm_rnn_4101$x,Y_T$Fat) *100
  MapeTeste5 <- MAPE(Pred_desnorm_teste_rnn_4101$x,Y_Teste$Fat) *100
  SSE5 <- sum((Pred_desnorm_rnn_4101$x - Y_T$Fat)^2)
  AIC5 <- (2*rnn_4101$input_dim)+length(X_treino_1)*log(SSE5/length(X_treino_1))
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'RNN - 4/10/1', 
                                       'AIC' = AIC5,
                                       'MAPE - Treino' = MapeTreino5,
                                       'MAPE - Teste' = MapeTeste5))
  
}

## 7.3.4) RNN 4_20_1
{
  # Criando Modelo
  
  rnn_4201 <- trainr( Y = Y_treino,
                      X = X_treino,
                      learningrate = 0.1,
                      hidden_dim = 20,
                      numepochs = 100,
                      network_type = "rnn")
  
  # Previsão Serie Treino
  Pred_T_4201 <- as.data.frame(predictr(rnn_4201, X = X_treino))
  Pred_desnorm_rnn_4201 <- linscale(Pred_T_4201$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_4201 <- as.data.frame(predictr(rnn_4201, X = X_teste))
  Pred_desnorm_teste_rnn_4201 <- linscale(Pred_Teste_4201$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  
  MapeTreino6 <- MAPE(Pred_desnorm_rnn_4201$x,Y_T$Fat) *100
  MapeTeste6 <- MAPE(Pred_desnorm_teste_rnn_4201$x,Y_Teste$Fat) *100
  SSE6 <- sum((Pred_desnorm_rnn_4201$x - Y_T$Fat)^2)
  AIC6 <- (2*rnn_4201$input_dim)+length(X_treino_1)*log(SSE6/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'RNN - 4/20/1', 
                                       'AIC' = AIC6,
                                       'MAPE - Treino' = MapeTreino6,
                                       'MAPE - Teste' = MapeTeste6))
  
}

## 7.3) LSTM

## 7.3.1) LSTM 1_5_1
{
  # Criando Modelo
  
  lstm_151 <- trainr( Y = Y_treino,
                      X = X_treino_1,
                      learningrate = 0.1,
                      hidden_dim = 5,
                      numepochs = 100,
                      network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_151 <- as.data.frame(predictr(lstm_151, X = X_treino_1))
  Pred_desnorm_lstm_151 <- linscale(Pred_T_lstm_151$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_151 <- as.data.frame(predictr(lstm_151, X = X_teste_1))
  Pred_desnorm_teste_lstm_151 <- linscale(Pred_Teste_lstm_151$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  
  MapeTreino7 <- MAPE(Pred_desnorm_lstm_151$x,Y_T$Fat) *100
  MapeTeste7 <- MAPE(Pred_desnorm_teste_lstm_151$x,Y_Teste$Fat) *100
  SSE7 <- sum((Pred_desnorm_lstm_151$x - Y_T$Fat)^2)
  AIC7 <- (2*lstm_151$input_dim)+length(X_treino_1)*log(SSE7/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 1/5/1', 
                                       'AIC' = AIC7,
                                       'MAPE - Treino' = MapeTreino7,
                                       'MAPE - Teste' = MapeTeste7))
  
}

## 7.3.2) LSTM 1_10_1
{
  # Criando Modelo
  
  lstm_1101 <- trainr( Y = Y_treino,
                       X = X_treino_1,
                       learningrate = 0.1,
                       hidden_dim = 10,
                       numepochs = 100,
                       network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_1101 <- as.data.frame(predictr(lstm_1101, X = X_treino_1))
  Pred_desnorm_lstm_1101 <- linscale(Pred_T_lstm_1101$V1,
                                     minmax = Xs_T_2$minmax,
                                     rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_1101 <- as.data.frame(predictr(lstm_1101, X = X_teste_1))
  Pred_desnorm_teste_lstm_1101 <- linscale(Pred_Teste_lstm_1101$V1,
                                           minmax = Xs_Teste_1$minmax,
                                           rev = TRUE)
  
  #Acuracia
  
  MapeTreino8 <- MAPE(Pred_desnorm_lstm_1101$x,Y_T$Fat) *100
  MapeTeste8 <- MAPE(Pred_desnorm_teste_lstm_1101$x,Y_Teste$Fat) *100
  SSE8 <- sum((Pred_desnorm_lstm_1101$x - Y_T$Fat)^2)
  AIC8 <- (2*lstm_1101$input_dim)+length(X_treino_1)*log(SSE8/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 1/10/1', 
                                       'AIC' = AIC8,
                                       'MAPE - Treino' = MapeTreino8,
                                       'MAPE - Teste' = MapeTeste8))
}

## 7.3.3) LSTM 1_20_1
{
  # Criando Modelo
  
  lstm_1201 <- trainr( Y = Y_treino,
                       X = X_treino_1,
                       learningrate = 0.1,
                       hidden_dim = 20,
                       numepochs = 100,
                       network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_1201 <- as.data.frame(predictr(lstm_1201, X = X_treino_1))
  Pred_desnorm_lstm_1201 <- linscale(Pred_T_lstm_1201$V1,
                                     minmax = Xs_T_2$minmax,
                                     rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_1201 <- as.data.frame(predictr(lstm_1201, X = X_teste_1))
  Pred_desnorm_teste_lstm_1201 <- linscale(Pred_Teste_lstm_1201$V1,
                                           minmax = Xs_Teste_1$minmax,
                                           rev = TRUE)
  
  #Acuracia
  
  MapeTreino9 <- MAPE(Pred_desnorm_lstm_1201$x,Y_T$Fat) *100
  MapeTeste9 <- MAPE(Pred_desnorm_teste_lstm_1201$x,Y_Teste$Fat) *100
  SSE9 <- sum((Pred_desnorm_lstm_1201$x - Y_T$Fat)^2)
  AIC9 <- (2*lstm_1201$input_dim)+length(X_treino_1)*log(SSE9/length(X_treino_1))
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 1/20/1', 
                                       'AIC' = AIC9,
                                       'MAPE - Treino' = MapeTreino9,
                                       'MAPE - Teste' = MapeTeste9))
  
}

## 7.3.4) LSTM 4_5_1
{
  # Criando Modelo
  
  lstm_451 <- trainr( Y = Y_treino,
                      X = X_treino,
                      learningrate = 0.1,
                      hidden_dim = 5,
                      numepochs = 100,
                      network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_451 <- as.data.frame(predictr(lstm_451, X = X_treino))
  Pred_desnorm_lstm_451 <- linscale(Pred_T_lstm_451$V1,
                                    minmax = Xs_T_2$minmax,
                                    rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_451 <- as.data.frame(predictr(lstm_451, X = X_teste))
  Pred_desnorm_teste_lstm_451 <- linscale(Pred_Teste_lstm_451$V1,
                                          minmax = Xs_Teste_1$minmax,
                                          rev = TRUE)
  
  #Acuracia
  
  MapeTreino10 <- MAPE(Pred_desnorm_lstm_451$x,Y_T$Fat) *100
  MapeTeste10 <- MAPE(Pred_desnorm_teste_lstm_451$x,Y_Teste$Fat) *100
  SSE10 <- sum((Pred_desnorm_lstm_451$x - Y_T$Fat)^2)
  AIC10 <- (2*lstm_451$input_dim)+length(X_treino_1)*log(SSE10/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 4/5/1', 
                                       'AIC' = AIC10,
                                       'MAPE - Treino' = MapeTreino10,
                                       'MAPE - Teste' = MapeTeste10))
}

## 7.3.5) LSTM 4_10_1
{
  # Criando Modelo
  
  lstm_4101 <- trainr( Y = Y_treino,
                       X = X_treino,
                       learningrate = 0.1,
                       hidden_dim = 10,
                       numepochs = 100,
                       network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_4101 <- as.data.frame(predictr(lstm_4101, X = X_treino))
  Pred_desnorm_lstm_4101 <- linscale(Pred_T_lstm_4101$V1,
                                     minmax = Xs_T_2$minmax,
                                     rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_4101 <- as.data.frame(predictr(lstm_4101, X = X_teste))
  Pred_desnorm_teste_lstm_4101 <- linscale(Pred_Teste_lstm_4101$V1,
                                           minmax = Xs_Teste_1$minmax,
                                           rev = TRUE)
  
  #Acuracia
  
  MapeTreino11 <- MAPE(Pred_desnorm_lstm_4101$x,Y_T$Fat) *100
  MapeTeste11 <- MAPE(Pred_desnorm_teste_lstm_4101$x,Y_Teste$Fat) *100
  SSE11 <- sum((Pred_desnorm_lstm_4101$x - Y_T$Fat)^2)
  AIC11 <- (2*lstm_4101$input_dim)+length(X_treino_1)*log(SSE11/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 4/10/1', 
                                       'AIC' = AIC11,
                                       'MAPE - Treino' = MapeTreino11,
                                       'MAPE - Teste' = MapeTeste11))
}

## 7.3.4) RNN 4_20_1
{
  # Criando Modelo
  
  lstm_4201 <- trainr( Y = Y_treino,
                       X = X_treino,
                       learningrate = 0.1,
                       hidden_dim = 20,
                       numepochs = 100,
                       network_type = "lstm")
  
  # Previsão Serie Treino
  Pred_T_lstm_4201 <- as.data.frame(predictr(lstm_4201, X = X_treino))
  Pred_desnorm_lstm_4201 <- linscale(Pred_T_lstm_4201$V1,
                                     minmax = Xs_T_2$minmax,
                                     rev = TRUE)
  
  #Previsão Serie Teste
  Pred_Teste_lstm_4201 <- as.data.frame(predictr(lstm_4201, X = X_teste))
  Pred_desnorm_teste_lstm_4201 <- linscale(Pred_Teste_lstm_4201$V1,
                                           minmax = Xs_Teste_1$minmax,
                                           rev = TRUE)
  
  #Acuracia
  
  MapeTreino12 <- MAPE(Pred_desnorm_lstm_4201$x,Y_T$Fat) *100
  MapeTeste12 <- MAPE(Pred_desnorm_teste_lstm_4201$x,Y_Teste$Fat) *100
  SSE12 <- sum((Pred_desnorm_lstm_4201$x - Y_T$Fat)^2)
  AIC12 <- (2*lstm_4201$input_dim)+length(X_treino_1)*log(SSE12/length(X_treino_1))
  
  
  #Data Frame Resultado
  
  Resultados_RNA <- rbind( Resultados_RNA,
                           data.frame( 'Nome do Modelo' = 'LSTM - 4/20/1', 
                                       'AIC' = AIC12,
                                       'MAPE - Treino' = MapeTreino12,
                                       'MAPE - Teste' = MapeTeste12))
  
}

