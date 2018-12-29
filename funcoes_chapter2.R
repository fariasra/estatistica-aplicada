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
ex_2_1 <- function(dt, freq_abs = 4){
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
save(ex_2_1, file = "dt_freq_acu.RData")


#outliers
outliers <- function(lista){
  lista <- sort(lista)
  return(c(lista[lista < (quantile(lista, 1/4)[[1]] - IQR(lista)*1.5)], 
           lista[lista > (quantile(lista, 3/4)[[1]] + IQR(lista)*1.5)]))
}
save(outliers, file = "outliers.RData")