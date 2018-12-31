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

##2.2
#datasets usados

exe_7 <- c(25,35,20,75,10,10,61,89,44,22,34,33,38,30,47, #índice de qualidade do ar para 30
           53,44,57,71,20,42,52,48,41,35,59,53,61,65,25) #cidades americanas

exe_9 <- data.frame(resposta = c("Em casa", "Na casa de um amigo", "Em um bar", 
                       "Em outro lugar", "Nao sabe"), numero = c(620,110,50,100,130), 
                    stringsAsFactors = F)

exe_11 <- data.frame(altura = c(992,780,762,756,741,732,714,662,579),
                     num_andares = c(71,56,53,55,47,53,50,49,40))

exe_12 <- data.frame(ano = seq(2001,2012,1), ind_desemprego = c(4.7,5.8,6.0,5.5,5.1,4.6,
                     4.6,5.8,9.3,9.6,8.9,8.1))


#Ex 7
resposta_7 <- stem(exe_7)
resposta_7
# os dados parecem ser simétricos e em forma de sino, a maioria das observações
#está entre 30 e 49

#Ex 8
resposta_8 <- dotchart(sort(exe_7))
resposta_8
#Não sei alinhar as bolinhas com o eixo X, mas não observei nenhum comportamento relevante

#Ex 9
resposta_9 <- pie(exe_9$numero, labels = exe_9$resposta)
resposta_9
#A grande maioria (ou mais da metade) dos americanos vai passar o reveillon em casa

#Ex 10
library(ggplot2)
resposta_10 <- ggplot(data = exe_9, aes(x = resposta, y = numero)) +
  geom_col() 
resposta_10

#Ex 11
library(ggplot2)
resposta_11 <- ggplot(data = exe_11, aes(x = altura, y = num_andares)) +
  geom_point()
resposta_11

#Ex 12
library(ggplot2)
resposta_12 <- ggplot(data = exe_12, aes(x = ano, y = ind_desemprego)) +
  geom_line()
resposta_12


##2.3
#datasets usados

exe_13 <- c(24.5,29.5,32.5,28,28.5,25.5,34,24.5,30,31)

exe_14 <- data.frame(resposta = c("favor", "contra", "não_op"), n_obs = c(734,255,20))

exe_15 <- data.frame(nota = c(78,72,86,91,87,80), peso = c(rep(.15,5),.25))

exe_16 <- data.frame(nota = c(96,85,91,86), peso = c(rep(.2,3), .4))

exe_18 <- data.frame(n_revista = seq(0,6,1), freq = c(13,9,19,8,5,2,4))

#Ex 13
cat(
  "Média = ", mean(exe_13), "\n",
  "Mediana = ", median(exe_13), "\n",
  "Moda = ", mode(exe_13), "\n\n"
) # O modelo é amodal pois não tem observações de valores repetidos

#Ex 14
#A média não pode ser calculada pois os dados não são quantitativos
#A mediana é 'A Favor' pois essa característica foi observada em mais da metade dos dados
#A moda também é 'A Favor' pois foi a resposta mais observada

#Ex 15
library(Hmisc)
cat(
  "Média Ponderada = ", weighted.mean(exe_15$nota, weights = exe_15$peso), "\n",
  "Mediana Ponderada = ", wtd.quantile(exe_15$nota, weights = exe_15$peso, probs = .5), "\n"
) # A moda é a nota de maior peso, isto é, á última, que é igual 80


#Ex 16
library(Hmisc)
cat(
  "Média Ponderada = ", weighted.mean(exe_16$nota, weights = exe_16$peso), "\n",
  "Mediana Ponderada = ", wtd.quantile(exe_16$nota, weights = exe_16$peso, probs = .5), "\n"
) # A moda é a nota de maior peso, isto é, á última, que é igual 86

#Ex 17
#Poderiamos calcular a média facilmente com mean(exe_1), 
#mas como pede-se para usar a distribuição de frequência, então vamos usar o seguinte:
resposta_17 <- criando_dt_freq(exe_1, 5)
weighted.mean(resposta_17$pto_medio, resposta_17$freq_abs)
#observe que o valor encontrado é bem próximo ao que obtemos ao usar a função
mean(exe_1)

#Ex 18
weighted.mean(exe_18$n_revista, exe_18$freq)

#Ex 19
resposta_3
#assimétrica a direita, talvez se 12.1 for um outlier, se removermos ele, ficamos com
#uma base de dados simétricas

#Ex 20
resposta_4
#assimétrica a direita o gráfico é praticamente igual ao gráfico de frequencia absoluta

#Ex 21
#Assimétrica a esquerda, pois a maioria dos dados está a direita do gráfico

#Ex 22
#Assimétrica a direita, pois a maioria das observações está a esquerda do gráfico

#Ex 23
#a mediana, pois ela está a direita da média no gráfico

#Ex 24
# a média, pois ela está a direita da mediana no gráfico

##2.3
#datasets usados
exe_25 <- c(4,2,9,12,15,3,6,8,1,4,14,12,3,3) #distância percorrida

exe_26 <- c(58,52,76,76,64,79,74,62,58) #idades membros da suprema corte

exe_27 <- c(5306,6444,5304,4218,5159,6342,57134859,5365,5078,
            4334,5262,5905,6099,5113) #amostra preços dormitorios

exe_28 <- c(49632,54619,58298,48250,51842,50875,53219,49924) #amostra salario professores

#funções usadas
load("calculo_mean_dp.RData") #chama a função calc_m_v_dp()

#Ex 25
resposta_25 <- calc_m_v_dp(exe_25)

#Ex 26
resposta_26 <- calc_m_v_dp(exe_26)

#Ex 27
resposta_27 <- calc_m_v_dp(exe_27)

#Ex 28
resposta_28 <- calc_m_v_dp(exe_28)

#Ex 29
#Média = 70, DP = 14.5
#99.7% dos dados, em uma distribuição normal, estão dentro de 3 dp da média
#então 70 +- 3x14.5
c(70-3*14.5,70+3*14.5)

#Ex 30
#Média = 72.5, DP = 12.5
#Intervalo = 60-85
#72 - X = 60 portanto X = 12.5
#72 + Y = 85 portanto Y = 12.5
#O intervalo equivale a +- 1 dp da média, então contém 68% dos dados

#Ex 31
#Média = 36, DP = 8
#Intervalo = 20-52
#36 - X = 20 portanto X = 16 = -2DP
#36 + Y = 52 portanto Y = 16 = +2DP
#O intervalo equivale a +- 2 dp da média, então pelo teorema de Chebyshev,
#o intervalo contém ao menos: 1 - 1/(k^2), como k = 2
1-1/(2^2)
#75% dos dados estão dentro do intervalo 20-52

#Ex 32
#Média = 7, DP = 2
#Intervalo = 3-11
#7 - X = 3 portanto X = 4 = -2DP
#7 + Y = 11 portanto Y = 4 = +2DP
#O intervalo equivale a +- 2 dp da média, então pelo teorema de Chebyshev,
#o intervalo contém ao menos: 1 - 1/(k^2), como k = 2
1-1/(2^2)
#75% dos dados estão dentro do intervalo 3-11

#Ex 33
resposta_33 <- calc_m_v_dp(exe_33$televisores,exe_33$qtde_dom)
#Em média, existem 2.5 televisores por casa

#Ex 34
resposta_34 <- calc_m_v_dp(exe_34$defeitos,exe_34$avioes)
#Em média, cada avião apresenta 2.4 defeitos na fuselagem

#Ex 35
#Mudando a função calc_m_v_dp para calcular o coeficiente de variação, 
#que é o DP dividido pela média
calc_cv <- function(dados, peso = NULL){
  if(length(peso) == length(dados)){
    require(Hmisc)
    return(((sqrt(wtd.var(dados,peso, normwt = T)))/wtd.mean(dados, peso))*100)
  }
  else{
    return((sd(dados)/mean(dados))*100)
  }
}

calc_cv(exe_35$calouros)
calc_cv(exe_35$veteranos)
#o desempenho dos calouros é mais variável que o desempenho dos veteranos

#Ex36
calc_cv(exe_36$idades)
calc_cv(exe_36$anos_xp)
#os anos de experiência variam mais que a idade


##2.5
#datasets usados

exe_37 <- c(53,57,60,57,54,53,54,53,54,42,48,
            53,47,47,50,48,42,42,54,54,60)

exe_41 <- exe_13

exe_42 <- c(173,145,205,192,197,227,156,240,172,185,208,185,190,167,212,228,190,184,195)

#Ex 37
#Os elementos são: min, q1, q2, q3 e max
exe_37 <- sort(exe_37)
cat(
  "Mínimo = ", min(exe_37), "\n",
  "Q1 = ", quantile(exe_37, 1/4), "\n",
  "Q2 = ", quantile(exe_37, 2/4), "\n",
  "Q3 = ", quantile(exe_37, 3/4), "\n",
  "Máximo = ", max(exe_37), "\n\n"
)

#Ex 38
#Amplitude interquartil é igual a IQR (interquartil range), i.e, q3-q1
IQR(exe_37)

#Ex 39
boxplot(exe_37)

#Ex 40
length(exe_37[exe_37 < quantile(exe_37, 3/4)])

#Ex 41
IQR(exe_41)

#Ex 42
boxplot(exe_42)
#Os dados parecem tem uma distribuição normal (simétrica)

#Ex 43
#se 65% dos estudantes tiraram 75 ou menos, então 35% tiraram mais que 75

#Ex 44
#Existem 665 estações, sendo 106 com maior audiencia, vamos supor q os dados estão
#ordenados em ordem crescente, isso é, do de menor audiência para o maior, podemos achar o 
#percentil fazendo
round((1-(106/665))*100, digits = 0)
#Isto é, 84º percentil

#Ex 45-48
#Média = 11830, DP = 2370

#45a
(16500-11830)/2370
#45b
#A camionete do exercício 45 tem 1,97dp a mais de capacidade de carga que a média das camionetes
#dessa concessionária
#45c
#Incomum

#46a
(18000-11830)/2370
#46b
#A camionete do exercício 46 tem 2,60dp a mais de capacidade de carga que a média das camionetes
#dessa concessionária
#46c
#Muito Incomum

#47a
(5500-11830)/2370
#47b
#A camionete do exercício 47 tem 2,67dp a menos de capacidade de carga que a média das camionetes
#dessa concessionária
#47c
#Muito Incomum

#48a
(11300-11830)/2370
#48b
#A camionete do exercício 45 tem 0,22dp a menos de capacidade de carga que a média das camionetes
#dessa concessionária
#48c
#Comum
