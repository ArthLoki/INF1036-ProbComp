# Teste 1 (G1) 2023.1


# Questão 1
LCG <- function(seed, a, c, M, nsamples){
  x = seed
  u = c(x)
  
  for (i in 1:nsamples){
    xk = (a * x + c) %% M
    u = c(u, as.double(xk)/as.double(M))
    x = xk
  }
  
  return (u)
}

MT <- function(nsamples){
  return (runif(nsamples))
}

DADO <- function(U, n.faces){
  dado = NULL
  for (i in 1:length(U)){
    dado = c(dado, as.integer(U[i] * n.faces) + 1)
  }
  
  return (dado)
}

CARA_COROA <- function(U, p){  # probabilidade coroa
  cc = NULL
  
  for (i in 1:length(U)){
    if (U[i] < (1.0 - p))
      cc = c(cc, 0)  # cara
    else
      cc = c(cc, 1)  # coroa
  }
  
  return (cc)
}


Q1A.v1 <- function(){
  
  nsamples = 10000
  n.faces = 10
  
  count.face.5 = 0
  
  U = LCG(3, 39373, 0, 2147483647, nsamples)
  dado = DADO(U, n.faces)
  
  for (i in 1:nsamples){  # ou length(dado) == nsamples
    if (dado[i] == 5) {
      count.face.5 = count.face.5 + 1
    }
  }
  
  print(paste("v1) Qtd faces 5:", count.face.5))
}

Q1A.v2 <- function(){
  nsamples = 10000
  faces = 1:10
  
  count.face.5 = 0
  
  for (i in 1:nsamples){
    face = sample(faces, 1, replace=T)
    
    if (face == 5) count.face.5 = count.face.5 + 1
  }
  
  print(paste("v2) Qtd faces 5:", count.face.5))
}

Q1B <- function(){
  nsamples = 10000
  
  # dado viciado -> MT
  n.faces = 10
  
  Ud = MT(nsamples)
  dado = DADO(Ud, n.faces-1)
  
  # moeda viciada -> LCG
  p.coroa = 0.45

  Um = LCG(3, 39373, 0, 2147483647, nsamples)
  cc = CARA_COROA(Um, p.coroa)
  
  # counters
  count = 0
  
  for (i in 1:nsamples){
    if ((dado[i] == 6 && cc == 0) | (dado[i] == 9 && cc == 1) | (dado[i] == 8))
      count = count + 1
  }
  
  prob = count / nsamples
  
  print(paste("Probabilidade de ocorrer uma das condicoes:", prob))
}

Q1 <- function(){
  print("----> QUESTAO 1")
  Q1A.v1()
  Q1A.v2()
  
  Q1B()
}

Q1()
