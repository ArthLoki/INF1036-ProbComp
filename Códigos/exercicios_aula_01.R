# Exemplo 1: Lancamento de dado

exemplo1 <- function(){
  nsamples = 12
  
  dado = c(1:6)
  nFaces = length(dado)
  
  totalFaces = rep(0, nFaces)
  
  for (i in 1:nsamples){
    face = sample(dado, 1)
    totalFaces[face] = totalFaces[face] + 1
  }
  
  print("Exemplo 1: Lançamento de dado")
  for (i in 1:nFaces){
    print(paste("Total da face [", i, "] = ", totalFaces[i]))
  }
  
  for (i in 1:nFaces){
    print(paste("Probabilidade da face [", i, "] = ", totalFaces[i]/nsamples))
  }
}

# Exemplo 2: Lancamento de moeda
exemplo2 <- function(){
  nsamples = 10000
  
  moeda = c('ca', 'co')
  nFaces = length(moeda)
  
  cont.evento.a = 0
  cont.evento.b = 0
  
  for (i in 1:nsamples){
    face1 = sample(moeda, 1, prob=c(2/5, 3/5))
    face2 = sample(moeda, 1, prob=c(2/5, 3/5))
    face3 = sample(moeda, 1, prob=c(2/5, 3/5))
    
    # Evento A
    if (((face1 == 'co') & (face2 == 'ca')) | ((face1 == 'ca') & (face2 == 'co')))
      cont.evento.a = cont.evento.a + 1
    
    # Evento B
    if (face2 == 'co' & face3 == 'ca')
      cont.evento.b = cont.evento.b + 1
  }
  
  PA = cont.evento.a / nsamples
  PB = cont.evento.b / nsamples
  
  print(PA)
  print(PB)
}

exemplo2()

# Exemplo 3: Adubo
