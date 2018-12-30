###Exercícios de revisão

##2.1
#datasets usados
exe_1 <- c(13,15,15,8,16,20,28,19,18,15, #razão entre aluno e professores em 20 faculdade
           21,23,30,17,10,16,15,16,20,15) #publicas

exe_3 <- c(11.95,11.93,11.99,11.94,11.98,11.91,12,11.94,11.92,11.92, #volumes (em onças)
           11.86,11.94,11.89,11.98,11.95,11.94,12.1,12.01,11.88,11.93,
           12,11.95,11.99,11.94)

exe_5 <- c(153,104,118,166,89,104,100,79,93,96,116, #número de quartos reservados em
           94,140,84,81,96,108,111,87,126,101,111, #uma amostra de hoteis
           122,108,126,93,108,87,103,95,129,93)

#Ex 1
load("dt_classes_e_freq.RData")
load("dt_freq_acu.RData")

resposta_1 <- criando_dt_freq(exe_1, 5)
resposta_1 <- ex_2_1(resposta_1, 4)

#Ex 2
resposta_2 <- plot(resposta_1$limites_inf, resposta_1$freq_rel, type = "h")
resposta_2

#Ex 3
#ajeitando a função
criando_dt_freq <- function(numbers, c_lass = 10){
  numbers <- sort(numbers)
  amplitude <- max(numbers)-min(numbers)
  cl <- ceiling((amplitude/c_lass)*100)/100
  limites_inf <- seq(min(numbers), min(numbers)+(c_lass-0.01)*cl,cl)
  limites_sup <- seq(min(numbers)+cl-0.01, min(numbers)+cl-0.01+(c_lass-1)*cl,cl)
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

#Obtendo a resposta e fazendo o plot
resposta_3 <- criando_dt_freq(exe_3, 7)
plot(resposta_3$limites_inf, resposta_3$freq_abs, type = "h")

#Ex 4
resposta_4 <- resposta_3
plot(resposta_4$limites_inf, resposta_4$freq_rel, type = "h")

#Ex 5
resposta_5 <- criando_dt_freq(exe_5, 6)
x <- c(resposta_5[1,3]-5, resposta_5$pto_medio, resposta_5[6,3]+5)
y <- c(0, resposta_5$freq_abs, 0)
plot(x,y, type = "l")

#Ex 6
resposta_6 <- criando_dt_freq(exe_5, 6)
resposta_6 <- add_freq_acu(resposta_6)
x <- c(resposta_6[1,3]-(resposta_6[2,3]-resposta_6[1,3]), resposta_6$pto_medio)
y <- c(0, resposta_6$freq_acu)
plot(x,y, type = "l")
