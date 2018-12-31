#criando e retornando uma dt com os limites das classes, o pto medio, e as frequencias 
#absolutas e realtivas
criando_dt_freq <- function(numbers, c_lass = 10){
  numbers <- sort(numbers)
  amplitude <- max(numbers)-min(numbers)
  cl <- ceiling(amplitude/c_lass)
  limites_inf <- seq(min(numbers), min(numbers)+(c_lass-1)*cl,cl)
  limites_sup <- seq(min(numbers)+cl-1, min(numbers)+cl-1+(c_lass-1)*cl,cl)
  pto_medio <- (limites_inf+limites_sup)/2
  freq_abs <- c()
  k <- 1
  while(k <= length(limites_sup)){
    freq_abs <- append(freq_abs, length(numbers[(numbers <= limites_sup[k])]) -
                         length(numbers[(numbers <= limites_sup[k-1])]))
    k <- k+1
  }
  freq_rel <- freq_abs/length(numbers)
  return(data.frame(limites_inf, limites_sup, pto_medio, freq_abs, freq_rel))
}
#salvando
save(criando_dt_freq, file = "dt_classes_e_freq.RData")


##adicionando coluna de frequencia acumulada em um dataframe
add_freq_acu <- function(dt, freq_abs = 4){
  #requer o dt e a coluna onde estão as frequencias absolutas
  freq_acu <- c(dt[1, freq_abs])
  k<-2
  while (k <= length(dt[,freq_abs])){
    freq_acu <- append(freq_acu, dt[k,freq_abs] + freq_acu[k-1])
    k <- k+1
  }
  dt$freq_acu <- freq_acu
  return(dt)
}
#salvando
save(add_freq_acu, file = "dt_freq_acu.RData")


#outliers
outliers <- function(lista){
  lista <- sort(lista)
  return(c(lista[lista < (quantile(lista, 1/4)[[1]] - IQR(lista)*1.5)], 
           lista[lista > (quantile(lista, 3/4)[[1]] + IQR(lista)*1.5)]))
}
#salvando a função outlier()
save(outliers, file = "outliers.RData")

#Calculando médias e desvio padrões
calc_m_v_dp <- function(dados, peso = NULL){
  if(length(peso) == length(dados)){
    require(Hmisc)
    cat(
      "Amplitude: ", max(dados)-min(dados), "\n",
      "Média Ponderada: ", weighted.mean(dados, peso), "\n",
      "Variancia Ponderada: ", wtd.var(dados, peso, normwt = T), "\n",
      "DP Ponderado: ", sqrt(wtd.var(dados, peso, normwt = T)), "\n",
      "Mediana Ponderada: ", wtd.quantile(sort(dados), peso, 1/2, normwt = T), "\n\n"
    )
  }
  else{
    cat(
      "Peso inexistente ou de tamanho diferente do tamanho do vetor de dados\n",
      "Amplitude: ", max(dados)-min(dados), "\n",
      "Média: ", mean(dados), "\n",
      "Variancia: ", var(dados), "\n",
      "DP: ", sd(dados), "\n",
      "Mediana: ", median(sort(dados)), "\n\n"
    )
  }
}
#salvando a função calc_m_v_dp()
save(calc_m_v_dp, file = "calculo_mean_dp.RData")
