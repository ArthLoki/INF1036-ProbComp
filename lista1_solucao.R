## Funcoes Auxiliares
LCG <- function(seed, a, c, M, nsamples){
  x = seed
  u = c(x)
  
  for (i in 1:nsamples){
    xk = (a * x + c) %% M
    u = c(u, as.double(xk) / as.double(M))
    x = xk
  }
  return (u)
}

MT <- function(nsamples){
  return (runif(nsamples))  # retorna nsamples valores no intervalo [0.0, 1.0)
}

CARA_COROA <- function(U, p){
  cc = NULL
  for (i in 1:length(U)){
    if (U[i] < (1.0 - p))
      CC = c(CC, 0)  # cara
    else
      CC = c(CC, 1)  # coroa
  }
  
  return (CC)
}

DADO <- function(U){
  dado = NULL
  for (i in 1:length(U)){
    dado = c(dado, as.integer(U[i] * 6.0) + 1)
  }
}


## Parte 1

## Parte 2
# Questão 4
Q4 <- function(nsamples){
  evento.a = numeric(nsamples)  # inicializados com 0
  evento.b = numeric(nsamples)  # inicializados com 0
  evento.c = numeric(nsamples)  # inicializados com 0
  
  for (i in 1:nsamples){
    v1 = cod = sample(c("A", "B", "C", "D", "E", "F"), 3, replace=T)  # cupom 1
    v2 = cod = sample(c("A", "B", "C", "D", "E", "F"), 3, replace=T)  # cupom 2
    
    while ((v1[1] == v2[1]) & (v1[2] == v2[2]) & (v1[3] == v2[3]))
      v2 = cod = sample(c("A", "B", "C", "D", "E", "F"), 3, replace=T)  
      # pega um novo v2 ate v2 != v1
    
    # item a
    if (!(v1[1] %in% v2) & !(v1[2] %in% v2) & !(v1[3] %in% v2)){
      evento.a[i] = 1  # como atende evento A coloca 1
    }
    
    # item b
    cond.b1 = ((v1[1] %in% v2) & (v1[2] %in% v2) & (v1[3] %in% v2))
    cond.b2 = ((v1[1] != v2[1]) & (v1[2] != v2[2]) & (v1[3] != v2[3]))
    
    cond.b3a = (v1[1] == v2[2] | v1[1] == v2[3])
    cond.b3b = (v1[2] == v2[1] | v1[2] == v2[3])
    cond.b3c = (v1[3] == v2[1] | v1[3] == v2[2])
    cond.b3 = cond.b3a & cond.b3b & cond.b3c
    
    cond.b4a = (v2[1] == v1[2] | v2[1] == v1[3])
    cond.b4b = (v2[2] == v1[1] | v2[2] == v1[3])
    cond.b4c = (v2[3] == v1[1] | v2[3] == v1[2])
    cond.b4 = cond.b4a & cond.b4b & cond.b4c
    
    if (cond.b1 & cond.b2 & cond.b3){
      evento.b[i] = 1  # como atende evento B coloca 1
    }
    
    # item c
    if (v1[1] %in% c("A", "E") & v2[1] %in% c("A", "E")){
      evento.c[i] = 1  # como atende evento C coloca 1
    }
    
  }
  
  PA = sum(evento.a) / nsamples
  PB = sum(evento.b) / nsamples
  PC = sum(evento.c) / nsamples
  
  print(paste("evento A: ", sum(evento.a), "; evento B:", sum(evento.b), "; evento C:", sum(evento.c)))
  
  print(paste("prob A: ", PA, "; prob B:", PB, "; prob C:", PC))
}

Q4(100000)

# Questão 5
Q5 <- function(nsamples){
  
  # Inicializacao das variaveis
  tipos.defeito = c('C1', 'C2', 'C3', 'C4')
  prob.defeitos.sem.manu = c(0.04, 0.04, 0.06, 0.06)
  prob.defeitos.com.manu = c(0.04, 0.04, 0.03, 0.03)
  
  perc.equip.com.manu = 0.4
  
  cont.equip.com.defeito = 0
  cont.equip.sem.defeito = 0
  
  cont.equip.com.manu = rep(0, nsamples)
  cont.equip.sem.manu = rep(0, nsamples)
  
  cont.equip.com.defeito.sem.manu = 0
  cont.equip.com.defeito.com.manu = 0
  cont.equip.sem.defeito.sem.manu = 0
  cont.equip.sem.defeito.com.manu = 0
  
  cont.equip.com.manu.com.defeito.c1 = 0
  cont.equip.sem.manu.com.defeito.c1 = 0
  
  cont.equip.com.manu.com.defeito.c3 = 0
  cont.equip.sem.manu.com.defeito.c3 = 0
  
  # loop que vai de 1 a nsamples
  for (i in 1:nsamples){
    # tira uma amostra para ver se o equipamento tem manuetncao ou nao
    efetua.manutencao = sample(c(T, F), 1, prob = c(perc.equip.com.manu, 1-perc.equip.com.manu))
    
    # inicializa uma variavel para contar os defeitos de acordo com seu tipo
    cont.defeitos = rep(0, length(tipos.defeito))

    if (efetua.manutencao == T){  # caso 1: tem manutencao
      cont.equip.com.manu[i] = 1 # coloca 1 na posicao i do vetor cont.equip.com.manu.v2
      
      # contagem dos defeitos
      for (j in 1:length(tipos.defeito)){
        # Na linha abaixo, no vetor C(1, 0), os valores 1 e 0 representam, respectivamente, a existencia e a nao existencia do defeito j
        cont.defeitos[j] = cont.defeitos[j] + sample(c(1, 0), 1, prob=c(prob.defeitos.com.manu[j], 1-prob.defeitos.com.manu[j]))
      }
      
      # elaboracao letra a
      if (sum(cont.defeitos) > 0){
        cont.equip.com.defeito.com.manu = cont.equip.com.defeito.com.manu + 1
      } else {
        cont.equip.sem.defeito.com.manu = cont.equip.sem.defeito.com.manu + 1
      }
      
      # elaboracao letra b
      if (cont.defeitos[1] > 0 & sum(cont.defeitos[2:4]) == 0){
        cont.equip.com.manu.com.defeito.c1 = cont.equip.com.manu.com.defeito.c1 + 1
      }
      
      if (cont.defeitos[3] > 0 & sum(cont.defeitos[1:2]) == 0 & cont.defeitos[4] == 0){
        cont.equip.com.manu.com.defeito.c3 = cont.equip.com.manu.com.defeito.c3 + 1
      }

    } else {  # caso 2: nao tem manutencao
      cont.equip.sem.manu[i] = 1 # coloca 1 na posicao i do vetor cont.equip.com.manu.v2
      
      # contagem dos defeitos
      for (k in 1:length(tipos.defeito)){
        # Na linha abaixo, no vetor C(1, 0), os valores 1 e 0 representam, respectivamente, a existencia e a nao existencia do defeito k
        cont.defeitos[k] = cont.defeitos[k] + sample(c(1, 0), 1, prob=c(prob.defeitos.sem.manu[k], 1-prob.defeitos.sem.manu[k]))
      }
      
      # elaboracao letra a
      if (sum(cont.defeitos) > 0){
        cont.equip.com.defeito.sem.manu = cont.equip.com.defeito.sem.manu + 1
      } else {
        cont.equip.sem.defeito.sem.manu = cont.equip.sem.defeito.sem.manu + 1
      }
      
      # elaboracao letra b
      if (cont.defeitos[1] > 0 & sum(cont.defeitos[2:4]) == 0){
        cont.equip.sem.manu.com.defeito.c1 = cont.equip.sem.manu.com.defeito.c1 + 1
      }
      
      if (cont.defeitos[3] > 0 & sum(cont.defeitos[1:2]) == 0 & cont.defeitos[4] == 0){
        cont.equip.sem.manu.com.defeito.c3 = cont.equip.sem.manu.com.defeito.c3 + 1
      }

    }
    
    # Letra A
    cont.equip.com.defeito = cont.equip.com.defeito.sem.manu + cont.equip.com.defeito.com.manu
    cont.equip.sem.defeito = cont.equip.sem.defeito.sem.manu + cont.equip.sem.defeito.com.manu
    
    # Letra B
    total.com.defeito.c1 = cont.equip.sem.manu.com.defeito.c1 + cont.equip.com.manu.com.defeito.c1
    total.com.defeito.c3 = cont.equip.sem.manu.com.defeito.c3 + cont.equip.com.manu.com.defeito.c3
  }
  
  ### Respostas
  ### Enunciado: Calcule, usando simulação, a probabilidade de que entre dois e três anos
  
  ## A) o equipamento
  # i) apresente defeito (OBS: cont.equip.com.defeito / nsamples)
  Pai = cont.equip.com.defeito / nsamples
  print(paste("Probabilidade de um equipamento apresentar defeito: ", Pai))
  
  # ii) apresente algum defeito se não for feita manutenção preventiva  (OBS: cont.equip.com.defeito.sem.manu / sum(cont.equip.sem.manu))
  Paii = cont.equip.com.defeito.sem.manu / sum(cont.equip.sem.manu)
  print(paste("Probabilidade de um equipamento apresentar defeito sem manutencao: ", Paii))
  
  # iii) apresente algum defeito se for feita a manutenção preventiva  (OBS: cont.equip.com.defeito.com.manu / sum(cont.equip.com.manu))
  Paiii = cont.equip.com.defeito.com.manu / sum(cont.equip.com.manu)
  print(paste("Probabilidade de um equipamento apresentar defeito com manutencao: ", Paiii))
  
  ## B) tenha sido feita manutenção preventiva dado que o equipamento apresentou apenas o defeito
  # i) C1  (OBS: cont.equip.com.manu.com.defeito.c1 / total.com.defeito.c1)
  Pbi = cont.equip.com.manu.com.defeito.c1 / total.com.defeito.c1
  print(paste("Probabilidade de um equipamento apresentar defeito C1 com manutencao: ", Pbi))

  # ii) C3  (OBS: cont.equip.com.manu.com.defeito.c3 / total.com.defeito.c3)
  Pbii = cont.equip.com.manu.com.defeito.c3 / total.com.defeito.c3
  print(paste("Probabilidade de um equipamento apresentar defeito C3 com manutencao: ", Pbii))
  
}

Q5(1000000)
