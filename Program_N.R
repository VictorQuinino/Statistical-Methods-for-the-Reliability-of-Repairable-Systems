library(pracma)
library(dplyr)
clear()
ref_ara=20 #memory
ref_ari=20 #memory
LI=0 #Lower Bound Repair Effect
LS=1 #Upper Bound Repair Effect
LIbeta=1
LSbeta=3
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

dados <- data.frame(equip, tempo_i, tempo_f, censura)

################################################################################
#ARA_m

#Ajuste inicial dos dados
dados_ara <- dados %>% mutate(tempo_ent = tempo_f-tempo_i) %>%
  filter(tempo_ent > 0) %>% select(-tempo_ent) %>%
  group_by(equip) %>% mutate(num_falha = seq(0,(n()-1)))



num_equip <- as.integer(max(dados_ara$equip))
verossimilhanca <- 0


vero_ara <- function(epsilon){
  
  for (k in 1:num_equip){
    
    dados_parc <- dados_ara[dados_ara$equip==k, ]
    
    
    for (i in 1:nrow(dados_parc)){
      num_falha <- as.integer(dados_parc[i,'num_falha'])
      censura <- as.integer(dados_parc[i, 'censura'])
      tempo_i <- as.numeric(dados_parc[i, 'tempo_i'])
      tempo_f <- as.numeric(dados_parc[i, 'tempo_f'])
      
      if (num_falha == 0){
        
        vero_parc <- log(epsilon[1]) - epsilon[1]*log(epsilon[2]) + (epsilon[1]-1)*log(tempo_f) - 
          (tempo_f/epsilon[2])^epsilon[1]
        
        
      } else if (num_falha > 0 & censura == 1) {
        
        j <- min((ara - 1), num_falha-1)
        l <- seq(0,j)
        
        desc <- sum((epsilon[3]^l) * dados_parc[(i-1):(i-1-j),'tempo_f'] * (1-epsilon[3]))
        tempo_vir_i <- tempo_i - desc
        tempo_vir_f <- tempo_f - desc
        
        
        vero_parc <- log(epsilon[1]) - epsilon[1]*log(epsilon[2]) + (epsilon[1]-1)*log(tempo_vir_f) -
          (tempo_vir_f/epsilon[2])^epsilon[1] + (tempo_vir_i/epsilon[2])^epsilon[1]
        
        
      } else {
        
        j <- min((ara - 1), (num_falha-1))
        l <- seq(0,j)
        
        desc <- sum((epsilon[3]^l)*dados_parc[(i-1):(i-1-j),'tempo_f']*(1-epsilon[3]))
        tempo_vir_i <- tempo_i - desc
        tempo_vir_f <- tempo_f - desc
        
        
        vero_parc <- -(tempo_vir_f/epsilon[2])^epsilon[1] + (tempo_vir_i/epsilon[2])^epsilon[1]
        
        
      } ##fim do if
      
      verossimilhanca <- verossimilhanca + vero_parc
      
    } ## fim do 'for i = falhas'
    
  }## fim do 'for para k'
  return(-verossimilhanca)
}##fim da funcao 'vero_ara'

var_inic <- c((LIbeta+LSbeta)/2,6,(LI+LS)/2)
ara <- ref_ara
ara_m <- nlminb(var_inic, vero_ara, lower=c(LIbeta,1,LI), upper=c(LSbeta,10,LS))
log_likelihood <- -ara_m$objective
AIC_ara <- 2 * 4 - 2 * log_likelihood

################################################################################
#ARI_m

dados_ari <- dados %>% mutate(tempo_ent = tempo_f-tempo_i) %>%
  filter(tempo_ent > 0) %>% select(-tempo_ent) %>%
  group_by(equip) %>% mutate(num_falha = seq(0,(n()-1)))

num_equip <- as.integer(max(dados_ari$equip))
verossimilhanca <- 0

vero_ari <- function(alpha){
  
  for (k in 1:num_equip){
    
    dados_parc <- dados_ari[dados_ari$equip==k, ]
    
    for (i in 1:nrow(dados_parc)){
      
      num_falha <- as.integer(dados_parc[i,'num_falha'])
      tempo_i <- as.numeric(dados_parc[i, 'tempo_i'])
      tempo_f <- as.numeric(dados_parc[i, 'tempo_f'])
      censura <- as.integer(dados_parc[i, 'censura'])
      
      if (num_falha == 0){
        
        vero_parc <- log(alpha[1]) - alpha[1]*log(alpha[2]) + (alpha[1]-1)*log(tempo_f) - 
          (tempo_f/alpha[2])^alpha[1]
        
      }else if (num_falha > 0 & censura == 1){
        
        j <- min((ari - 1),num_falha-1)
        p <- seq(0,j)
        
        desc_taxa <- sum((1-alpha[3])*(alpha[3]^p)*(alpha[1]/(alpha[2]^alpha[1]))*(dados_parc[(i-1):(i-1-j),'tempo_f'])^(alpha[1]-1))
        taxa_falha <- (alpha[1]/(alpha[2]^alpha[1]))*(tempo_f)^(alpha[1]-1) - desc_taxa
        
        vero_parc <- log(taxa_falha) - (tempo_f/alpha[2])^alpha[1] + (tempo_i/alpha[2])^alpha[1] + desc_taxa*(tempo_f-tempo_i)
        
      }else{
        
        j <- min((ari - 1), (num_falha-1))
        p <- seq(0,j)
        
        desc_taxa <- sum((1-alpha[3])*(alpha[3]^p)*(alpha[1]/(alpha[2]^alpha[1]))*(dados_parc[(i-1):(i-1-j),'tempo_f'])^(alpha[1]-1))
        taxa_falha <- alpha[1]/(alpha[2]^alpha[1])*(tempo_f)^(alpha[1]-1) - desc_taxa
        
        vero_parc <-  - (tempo_f/alpha[2])^alpha[1] + (tempo_i/alpha[2])^alpha[1] + desc_taxa*(tempo_f-tempo_i)
      }
      verossimilhanca <- verossimilhanca + vero_parc
    } #fim do "for 'i'"
    
  } #fim do "for 'k'"
  return(-verossimilhanca)
}

ari <- ref_ari
var_inic <- c((LIbeta+LSbeta)/2,6,(LI+LS)/2)
par_ari <- nlminb(start = var_inic, objective = vero_ari, lower=c(LIbeta,1,LI), upper=c(LSbeta,10,LS))
log_likelihood <- -par_ari$objective
AIC_ari <- 2 * 4 - 2 * log_likelihood

cat('Est ARA=',ara_m$par,"\n")
cat('Vero_ARA=',-ara_m$objective,"\n")
cat('AIC_ARA=',AIC_ara,"\n")

cat('Est ARI=',par_ari$par,"\n")
cat('Vero_ARI=',-par_ari$objective,"\n")
cat('AIC_ARI=',AIC_ari,"\n")







