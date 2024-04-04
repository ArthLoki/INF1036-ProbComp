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

CARA_COROA <- function(U, p){  # p  probabilidade de coroa
  CC = NULL
  for (i in 1:length(U)){
    if (U[i] < (1.0 - p))
      CC = c(CC, 0)  # cara
    else
      CC = c(CC, 1)  # coroa
  }
  
  return (CC)
}

DADO <- function(U, n.faces){
  dado = NULL
  for (i in 1:length(U)){
    dado = c(dado, as.integer(U[i] * n.faces) + 1)
  }
  return (dado)
}

fatorial_rec <- function(n){
  if (n == 1){
    return (1)
  } else {
    return (n * fatorial_rec(n-1))
  }
}


## Parte 1
# Questão 1
Q11 <- function(){
  n.transp = 3
  resp = fatorial_rec(n.transp)
  print(resp)
}

# Q11()

# Questão 2
Q12 <- function(){
  
  prim.alg = 3
  ult.alg = 3
  outros.alg = 4  # para cada algarismo
  
  resp = prim.alg * ult.alg * (outros.alg**2)
  print(resp)
}

# Q12()

# Questão 3
Q13 <- function(){
  # n.naturais = 10
  # n.impares = 5
  # n.pares = 5
  
  prim.alg = 9
  sec.alg = 8
  last.alg = 5
  
  resp = prim.alg * sec.alg * last.alg
  print(resp)
}

# Q13()

# Questão 4  (???)
Q14 <- function(){
  n.mocas = 5
  n.rapazes = 5
  
  resp = fatorial_rec(n.mocas) * fatorial_rec(n.rapazes)
  print(resp)
}

# Q14()

# Questão 5

# Questão 6

# Questão 7

# Questão 8

# Questão 9

# Questão 10

# Questão 11

# Questão 12

# Questão 13

# Questão 14

# Questão 15

# Questão 16

# Questão 17

# Questão 18


## Parte 2
# Questão 1
Q21 <- function(){
  nsamples = 10000
  
  qtd.duas.caras = 0
  
  for (i in 1:nsamples){
    moeda1 = sample(c('ca', 'co'), 1, replace=T, prob=c(0.5, 0.5))  # 0 = cara, 1 = coroa
    moeda2 = sample(c('ca', 'co'), 1, replace=T, prob=c(0.5, 0.5))  # 0 = cara, 1 = coroa
    
    if (moeda1 == 'ca' & moeda2 == 'ca'){
      qtd.duas.caras = qtd.duas.caras + 1
    }
  }
  
  print(paste("Em", nsamples, "lancamentos, tiveram duas caras", qtd.duas.caras, "vezes"))
  
}

# Q21()

# Questão 2
Q22 <- function(){
  
  cont.dados = rep(0, 11)
  
  while (any(cont.dados == 0)){
    face1 = sample(1:6, 1, replace=T)
    face2 = sample(1:6, 1, replace=T)
    
    soma = face1 + face2
    
    print(paste("Face 1:", face1, "+ Face 2:", face2, "= Soma: ", soma))
    
    cont.dados[soma-1] = cont.dados[soma-1] + 1
  }
  
  print(sum(cont.dados))
}

# Q22()

# Questão 3
Q23 <- function(){
  nsamples = 10000
  
  faces.moeda = c('ca', 'co') 
  prob.moeda = c(2/5, 3/5)  # c(cara, coroa)
  
  evento.a = 0
  evento.b = 0
  
  for (i in 1:nsamples){
    face1 = sample(faces.moeda, 1, replace=T, prob=prob.moeda)
    face2 = sample(faces.moeda, 1, replace=T, prob=prob.moeda)
    
    # Evento A
    if ((face1 == 'ca' & face2 == 'co') | (face2 == 'ca' & face1 == 'co')){
      evento.a = evento.a + 1
    }
    
    # Evento B
    if (face1 == 'co' & face2 == 'co'){
      evento.b = evento.b + 1
    }
  }
  
  PA = evento.a / nsamples
  PB = evento.b / nsamples
  
  print(paste("Item a) Probabilidade de A:", PA, "| Probabilidade de B:", PB))
}

# Q23()

# Questão 4
Q24 <- function(){
  nsamples = 100000
  
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

# Q24()

# Questão 5
Q25 <- function(){
  
  # Inicializacao das variaveis
  nsamples = 1000000
  
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
      
      # if (cont.defeitos[3] > 0 & sum(cont.defeitos[1:2]) == 0 & cont.defeitos[4] == 0){
      # cont.equip.sem.manu.com.defeito.c3 = cont.equip.sem.manu.com.defeito.c3 + 1
      # }
      
      if (cont.defeitos[3] > 0 & sum(cont.defeitos[c(1, 2, 4)]) == 0){
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

# Q25()

# Questão 6
Q26 <- function(){
  nsamples = 100000
  
  cont.dentro.area.cultivada = 0
  
  for (i in 1:nsamples){
    x = runif(1)
    y = runif(1)
    
    distancia1 = sqrt((x-0)^2 + (y-0)^2)
    distancia2 = sqrt((x-1)^2 + (y-1)^2)
    
    if (distancia1 <= 1 & distancia2 <= 1) cont.dentro.area.cultivada = cont.dentro.area.cultivada + 1
  }
  
  prob.dentro.area.cultivada = cont.dentro.area.cultivada / nsamples  # probabilidade ou proporcao de pontos dentro da area
  area.total = 1
  
  area.cultivada = prob.dentro.area.cultivada * area.total
  
  adubo.por.km = 100
  meses = 12
  qtd.adubo = area.cultivada * adubo.por.km * meses
  
  print(paste("Quantidade (Kg) = ", qtd.adubo))
}

# Q26()

# Questão 7
Q27A <- function(){
  x0 = 3
  a = 39373
  c = 0
  M = 2147483647
  nsamples = 10000

  U = LCG(x0, a, c, M, nsamples)
  dado = DADO(U, 10)
  
  cont.face5 = rep(0, length(dado))
  
  for (i in 1:length(dado)){
    if (dado[i] == 5)
      cont.face5[i] = 1
  }
  
  print(paste("Qtd de faces 5:", sum(cont.face5)))
}

# Q27A()

Q27B <- function(){
  nsamples = 10000
  
  # LCG
  x0 = 3
  a = 39373
  c = 0
  M = 2147483647
  U1 = LCG(x0, a, c, M, nsamples)
  
  # MT
  U2 = MT(nsamples)
  
  # dado viciado -> MT
  n.faces = 10
  dado = DADO(U1, n.faces-1)
  
  # moeda viciada -> LCG
  p.coroa = 0.45
  moeda = CARA_COROA(U2, p.coroa)  # cara = 0, coroa = 1
  
  # contadores
  count.cara.face6 = 0
  count.coroa.face9 = 0
  count.face8 = 0
  
  for (i in 1:nsamples){
    if (dado[i] == 8) count.face8 = count.face8 + 1
    
    if (moeda[i] == 0 && dado[i] == 6) count.cara.face6 = count.cara.face6 + 1
    
    if (moeda[i] == 1 && dado[i] == 9) count.coroa.face9 =  count.coroa.face9 + 1
  }
  
  print(paste("Probabilidade de se obter cara e face 6:", count.cara.face6 / nsamples))
  print(paste("Probabilidade de se obter coroa e face 9:", count.coroa.face9 / nsamples))
  print(paste("Probabilidade de se obter face 8:", count.face8 / nsamples))
}

# Q27B()

# Questão 8
simulacaoEvento <- function(n, nsamples){  # n >= 4
  
  count.a = rep(0, 4)
  Pa = NULL
  
  pessoas = NULL
  for (i in 1:n){
    pessoas = c(pessoas, c('L', 'T'))
  }
  
  for (i in 1:nsamples){
    v = sample(pessoas, 2*n, replace=F)
    
    
  }

  Pa = c(count.a[1] / nsamples, count.a[2] / nsamples, count.a[3] / nsamples, count.a[4] / nsamples)
  
  return (Pa)
}
