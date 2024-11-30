library(dplyr)
library(pracma)
clear()
equip <- c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5 
)

tempo_i <- c(
  0, 9.197, 14.761, 19.246, 24.597, 25.302, 30.948, 31.602, 34.574, 41.059,
  44.114, 44.279, 46.381, 54.519, 56.355, 74.44, 77.236, 80.261, 82.815, 84.772,
  88.901, 93.727, 98.149, 106.429, 0, 3.073, 4.685, 5.125, 10.867, 11.199,
  12.046, 20.275, 21.721, 24.944, 32.604, 34.43, 36.343, 36.899, 49.009, 49.27,
  51.357, 53.56, 57.321, 58.775, 62.879, 65.651, 68.605, 71.925, 72.362, 80.282,
  89.826, 89.911, 91.807, 96.57, 97.373, 99.731, 103.386, 0, 3.814, 18.336,
  20.887, 24.792, 28.197, 30.747, 35.269, 39.679, 43.93, 44.995, 54.552, 58.139,
  63.358, 63.816, 65.727, 69.452, 75.205, 75.401, 82.888, 83.607, 93.903,
  102.791, 103.602, 0, 3.108, 11.976, 14.593, 23.326, 24.15, 25.627, 34.732,
  36.546, 37.267, 42.407, 46.347, 48.593, 49.13, 52.783, 56.483, 63.891, 71.913,
  72.627, 73.248, 77.949, 80.195, 81.409, 87.642, 87.782, 87.82, 91.752,
  101.995, 104.54, 0, 3.346, 8.527, 10.942, 22.723, 25.885, 27.479, 32.869,
  33.975, 35.194, 37.683, 50.615, 53.079, 54.31, 56.651, 62.984, 64.305, 67.309,
  77.525, 85.587, 87.362, 91.461, 92.839, 99.475
)

tempo_f <- c(
  9.197, 14.761, 19.246, 24.597, 25.302, 30.948, 31.602, 34.574, 41.059, 44.114,
  44.279, 46.381, 54.519, 56.355, 74.44, 77.236, 80.261, 82.815, 84.772, 88.901,
  93.727, 98.149, 106.429, 106.429, 3.073, 4.685, 5.125, 10.867, 11.199, 12.046,
  20.275, 21.721, 24.944, 32.604, 34.43, 36.343, 36.899, 49.009, 49.27, 51.357,
  53.56, 57.321, 58.775, 62.879, 65.651, 68.605, 71.925, 72.362, 80.282, 89.826,
  89.911, 91.807, 96.57, 97.373, 99.731, 103.386, 103.386, 3.814, 18.336,
  20.887, 24.792, 28.197, 30.747, 35.269, 39.679, 43.93, 44.995, 54.552, 58.139,
  63.358, 63.816, 65.727, 69.452, 75.205, 75.401, 82.888, 83.607, 93.903,
  102.791, 103.602, 103.602, 3.108, 11.976, 14.593, 23.326, 24.15, 25.627,
  34.732, 36.546, 37.267, 42.407, 46.347, 48.593, 49.13, 52.783, 56.483, 63.891,
  71.913, 72.627, 73.248, 77.949, 80.195, 81.409, 87.642, 87.782, 87.82, 91.752,
  101.995, 104.54, 104.54, 3.346, 8.527, 10.942, 22.723, 25.885, 27.479, 32.869,
  33.975, 35.194, 37.683, 50.615, 53.079, 54.31, 56.651, 62.984, 64.305, 67.309,
  77.525, 85.587, 87.362, 91.461, 92.839, 99.475, 99.475
)

censura <- c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 0
)

mcf_nao_parametrica <- c(
  0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0,
  2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4.0,
  4.2, 4.4, 4.6, 4.8, 5.0, 5.2, 5.4, 5.6, 5.8, 6.0,
  6.2, 6.4, 6.6, 6.8, 7.0, 7.2, 7.4, 7.6, 7.8, 8.0,
  8.2, 8.4, 8.6, 8.8, 9.0, 9.2, 9.4, 9.6, 9.8, 10.0,
  10.2, 10.4, 10.6, 10.8, 11.0, 11.2, 11.4, 11.6, 11.8, 12.0,
  12.2, 12.4, 12.6, 12.8, 13.0, 13.2, 13.4, 13.6, 13.8, 14.0,
  14.2, 14.4, 14.6, 14.8, 15.0, 15.2, 15.4, 15.6, 15.8, 16.0,
  16.2, 16.4, 16.6, 16.8, 17.0, 17.2, 17.4, 17.6, 17.8, 18.0,
  18.2, 18.4, 18.6, 18.8, 19.0, 19.2, 19.4, 19.6, 19.8, 20.0,
  20.2, 20.4, 20.6, 20.8, 21.0, 21.2, 21.4, 21.6, 21.8, 22.0,
  22.2, 22.4, 22.6, 22.8, 23.0, 23.2, 23.4, 23.6, 23.8, 24.0,
  24.2, 24.4, 24.65, 24.9, 25.15, 25.4, 25.7333, 26.2333, 27.2333)



tempos_mcf <- c(3.073, 3.108, 3.346, 3.814, 4.685, 5.125, 8.527, 9.197, 10.867, 10.942,
                11.199, 11.976, 12.046, 14.593, 14.761, 18.336, 19.246, 20.275, 20.887, 
                21.721, 22.723, 23.326, 24.15, 24.597, 24.792, 24.944, 25.302, 25.627, 
                25.885, 27.479, 28.197, 30.747, 30.948, 31.602, 32.604, 32.869, 33.975, 
                34.43, 34.574, 34.732, 35.194, 35.269, 36.343, 36.546, 36.899, 37.267, 
                37.683, 39.679, 41.059, 42.407, 43.93, 44.114, 44.279, 44.995, 46.347, 
                46.381, 48.593, 49.009, 49.13, 49.27, 50.615, 51.357, 52.783, 53.079, 
                53.56, 54.31, 54.519, 54.552, 56.355, 56.483, 56.651, 57.321, 58.139, 
                58.775, 62.879, 62.984, 63.358, 63.816, 63.891, 64.305, 65.651, 65.727, 
                67.309, 68.605, 69.452, 71.913, 71.925, 72.362, 72.627, 73.248, 74.44, 
                75.205, 75.401, 77.236, 77.525, 77.949, 80.195, 80.261, 80.282, 81.409, 
                82.815, 82.888, 83.607, 84.772, 85.587, 87.362, 87.642, 87.782, 87.82, 
                88.901, 89.826, 89.911, 91.461, 91.752, 91.807, 92.839, 93.727, 93.903, 
                96.57, 97.373, 98.149, 99.475, 99.731, 101.995, 102.791, 103.386, 
                103.602, 104.54, 106.429)

dados <- data.frame(equip, tempo_i, tempo_f, censura)



################################################################################
#ARA_m MCF
#Ajuste inicial dos dados

m <- 20
par <- c(1.806,7.594,0.598)
mcfs_ara <- matrix(nrow = length(tempos_mcf), ncol = max(dados$equip) + 1)

dados_ara <- dados %>% mutate(tempo_ent = tempo_f-tempo_i) %>%
  filter(tempo_ent > 0) %>% select(-tempo_ent) %>%
  group_by(equip) %>% mutate(num_falha = seq(0,(n()-1))) %>%
  mutate(min_sum = ifelse(num_falha != 0, pmin((m-1), (num_falha-1)), 0)) %>%
  mutate(desc = 0) %>% mutate(acum = 0)

for (i in 1:nrow(dados_ara)){
  if  (dados_ara$num_falha[i] == 0){
    dados_ara$desc[i] <- 0
  } else {
    x <- seq(from =0, to = dados_ara$min_sum[i])
    min_sum <- dados_ara$min_sum[i]
    dados_ara$desc[i] <- sum((par[3]^x)*dados_ara$tempo_f[(i-1):(i-1-min_sum)]*(1-par[3]))
  }
  
}

f <- function(t,desc) {
  par[1]/par[2]*((t-desc)/par[2])^(par[1]-1)
}

for (i in 1:nrow(dados_ara)){
  dados_ara$acum[i] <- unlist(integrate(f, lower = dados_ara$tempo_i[i], 
                                 upper = dados_ara$tempo_f[i], desc = dados_ara$desc[i])[1])
}



max_equip <- max(dados_ara$equip)
for (j in 1:max_equip){
  dados_equip <- dados_ara[dados_ara$equip == j, ]
  for (l in 1:length(tempos_mcf)){
    tempo_atual <- tempos_mcf[l]
    dados_parc <- dados_equip[tempo_atual > dados_equip$tempo_i,]
    if (tempo_atual == dados_parc$tempo_f[nrow(dados_parc)]){
      acumul <- sum(dados_parc$acum)
      mcfs_ara[l,j] <- acumul
    } else {
      int_interv <- unlist(integrate(f, lower = dados_parc$tempo_i[nrow(dados_parc)],
                              upper = tempo_atual,
                              desc = dados_parc$desc[nrow(dados_parc)])[1])
      mcfs_ara[l,j] <- sum(dados_parc$acum[1:(nrow(dados_parc-1)-1)]) + int_interv
    }
  }
}

mcfs_ara[,ncol(mcfs_ara)] <- rowMeans(mcfs_ara[,1:5])

plot(y = mcfs_ara[, 6], x = tempos_mcf, type = "l", main = "MCF Plot",
     xlab = "Tempo", ylab = "MCF", col = "blue")

# Adicionando mcf_nao_parametrica como pontos pontilhados
points(x = tempos_mcf, y = mcf_nao_parametrica, pch = 16, col = "red")


################################################################################
#ARI_m MCF
#Ajuste inicial dos dados

m <- 20
par <- c(1.898,7.652,0.639)
mcfs_ari <- matrix(nrow = length(tempos_mcf), ncol = max(dados$equip) + 1)

dados_ari <- dados %>% mutate(tempo_ent = tempo_f-tempo_i) %>%
  filter(tempo_ent > 0) %>% select(-tempo_ent) %>%
  group_by(equip) %>% mutate(num_falha = seq(0,(n()-1))) %>%
  mutate(min_sum = ifelse(num_falha != 0, pmin((m-1), (num_falha-1)), 0)) %>%
  mutate(desc = 0) %>% mutate(acum = 0)

for (i in 1:nrow(dados_ari)){
  if  (dados_ari$num_falha[i] == 0){
    dados_ari$desc[i] <- 0
  } else {
    x <- seq(from =0, to = dados_ari$min_sum[i])
    min_sum <- dados_ari$min_sum[i]
    dados_ari$desc[i] <- sum((par[3]^x)*(par[1]/par[2]*(dados_ari$tempo_f[(i-1):(i-1-min_sum)]/par[2])^(par[1]-1))
                             *(1-par[3]))
  }
  
}

f <- function(t,desc) {
  par[1]/par[2]*((t)/par[2])^(par[1]-1) - desc
}

for (i in 1:nrow(dados_ari)){
  dados_ari$acum[i] <- unlist(integrate(f, lower = dados_ari$tempo_i[i], 
                                        upper = dados_ari$tempo_f[i], desc = dados_ari$desc[i])[1])
}



max_equip <- max(dados_ari$equip)
for (j in 1:max_equip){
  dados_equip <- dados_ari[dados_ari$equip == j, ]
  for (l in 1:length(tempos_mcf)){
    tempo_atual <- tempos_mcf[l]
    dados_parc <- dados_equip[tempo_atual > dados_equip$tempo_i,]
    if (tempo_atual == dados_parc$tempo_f[nrow(dados_parc)]){
      acumul <- sum(dados_parc$acum)
      mcfs_ari[l,j] <- acumul
    } else {
      int_interv <- unlist(integrate(f, lower = dados_parc$tempo_i[nrow(dados_parc)],
                                     upper = tempo_atual,
                                     desc = dados_parc$desc[nrow(dados_parc)])[1])
      mcfs_ari[l,j] <- sum(dados_parc$acum[1:(nrow(dados_parc-1)-1)]) + int_interv
    }
  }
}

mcfs_ari[,ncol(mcfs_ari)] <- rowMeans(mcfs_ari[,1:5])



mcf_nao_parametrica <- c(
  0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0,
  2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4.0,
  4.2, 4.4, 4.6, 4.8, 5.0, 5.2, 5.4, 5.6, 5.8, 6.0,
  6.2, 6.4, 6.6, 6.8, 7.0, 7.2, 7.4, 7.6, 7.8, 8.0,
  8.2, 8.4, 8.6, 8.8, 9.0, 9.2, 9.4, 9.6, 9.8, 10.0,
  10.2, 10.4, 10.6, 10.8, 11.0, 11.2, 11.4, 11.6, 11.8, 12.0,
  12.2, 12.4, 12.6, 12.8, 13.0, 13.2, 13.4, 13.6, 13.8, 14.0,
  14.2, 14.4, 14.6, 14.8, 15.0, 15.2, 15.4, 15.6, 15.8, 16.0,
  16.2, 16.4, 16.6, 16.8, 17.0, 17.2, 17.4, 17.6, 17.8, 18.0,
  18.2, 18.4, 18.6, 18.8, 19.0, 19.2, 19.4, 19.6, 19.8, 20.0,
  20.2, 20.4, 20.6, 20.8, 21.0, 21.2, 21.4, 21.6, 21.8, 22.0,
  22.2, 22.4, 22.6, 22.8, 23.0, 23.2, 23.4, 23.6, 23.8, 24.0,
  24.2, 24.4, 24.65, 24.9, 25.15, 25.4, 25.7333, 26.2333, 27.2333
)

plot(y = mcfs_ari[, 6], x = tempos_mcf, type = "l", main = "MCF Plot",
     xlab = "Tempo", ylab = "MCF", col = "blue")

# Adicionando mcf_nao_parametrica como pontos pontilhados
points(x = tempos_mcf, y = mcf_nao_parametrica, pch = 16, col = "red")


library(writexl)
ARA=data.frame(mcfs_ara[, 6])
ARI=data.frame(mcfs_ari[, 6])
Result<-cbind(ARA,ARI,mcf_nao_parametrica,tempos_mcf)
Result=data.frame(Result)
writexl::write_xlsx(Result,"Result.xlsx")


