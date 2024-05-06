### LISTA 1

## FUNCOES AUXILIARES

# Permutacao Simples: P(n) = n!
fatorial_rec <- function(n){
  if (n == 1){
    return (n)
  } else {
    return (n * fatorial_rec(n-1))
  }
}

# Permutacao com Repeticao: Pr(n, [k1, ..., kn]) = n! / (k1! * ... * kn!)
perm.com.repeticao <- function(n, K){
  repeticoes = 1
  for (i in 1:length(K)){
    repeticoes = repeticoes * fatorial_rec(K[i])
  }
  
  return (fatorial_rec(n) / repeticoes)
}

# Permutacao Circular: PC(n) = n! / n = (n-1)!
perm.circular <- function(n){
  return(fatorial_rec(n-1))
}

# Arranjo.simples: A(n, p) = n! / (n-p)!
arranjo.simples <- function(n, p){
  return (fatorial_rec(n) / fatorial_rec(n-p))
}

# Arranjo com Repeticao: AP(n, p) = n^p
arranjo.com.repeticao <- function(n, p){
  return (n**p)
}

# Combinação Simples: C(n, p) = n! / (p! * (n-p)!)
comb.simples <- function(n, p){
  return (fatorial_rec(n) / (fatorial_rec(p) * fatorial_rec(n-p)))
}

# Combinacao Complementar: C(n, p) = C(n, n-p)
comb.complementar <- function(n, p){
  return (comb.simples(n, n-p))
}

# Combinacao Completa (com Repeticao): CR(n, p) = C(n+p-1, p)
comb.com.repeticao <- function(n, p){
  return (comb.simples(n+p-1, p))
}

# LCG
LCG <- function(seed, a, c, M, nsamples){
  x = seed
  u = c(x)
  
  for (i in 1:nsamples){
    xk = (a * x + c) %% M
    u = c(u, xk)
    x = xk
  }
  
  return (u)
}

# MT
MT <- function(nsamples){
  return (runif(nsamples))
}

# Dado
DADO <- function(U, n.faces){
  dado = NULL
  for (i in 1:length(U)){
    dado = c(dado, as.integer(U[i] * n.faces) + 1)
  }
  
  return (dado)
}

# Moeda
MOEDA <- function(U, p){
  CC = NULL
  for (i in 1:length(U)){
    if (U[i] < (1 - p)){
      CC = c(CC, 'coroa')
    } else {
      CC = c(CC, 'cara')
    }
  }
  
  return (CC)
}

# Probabilidade da Intersecao
prob.intersecao <- function(A, B, nsamples){
  intersecao = rep(0, nsamples)
  for (i in 1:nsamples){
    if (B[i] == 1 & A[i] == 1){
      intersecao[i] = 1
    }
  }
  prob.inter = sum(intersecao) / nsamples
  return (prob.inter)
}

# Probabilidade Condicional
prob.condicional <- function(A, B, nsamples){
  # 1) Achar probabilidade da intersecao
  prob.inter = prob.intersecao(A, B, nsamples)
  
  # 2) Achar probabilidade de B
  prob.B = sum(B) / nsamples
  
  # 3) Achar probabilidade condicional
  prob.cond = prob.intersecao / prob.B
  return (prob.cond)
}

# Eventos Independentes: P(A intersecao B) = P(A)*P(B)
eventos.independentes <- function(PA, PB, P.intersecao){
  if (P.intersecao != PA * PB){
    print(paste("Os eventos não são independentes, pois", P.intersecao, "!=", PA * PB))
    # return (FALSE)
  } else {
    print(paste("Os eventos são independentes, pois", P.intersecao, "!=", PA * PB))
    # return (TRUE)
  }
}

## PARTE 1

# Questão 1
Q1.P1 <- function(){
  n = 3 # n meios de transporte
  ida = n # transportes
  volta = n-1
  print(ida * volta)
}

# Questão 2
Q2.P1 <- function(){
  # numeros naturais de 4 algarismos (base 10)
  # menores do que 5000 e divisiveis por 5
  # algarismos disponiveis: 2, 3, 4 e 5
  
  # divisivel por 5 => ult alg so pode ser 5, logo:
  ult_alg = 1
  
  # menores do que 5000 => prim alg pode ser 2, 3 e 4, logo:
  prim_alg = 3
  
  # outros algs:
  resto = 4*3
  
  print(resto*ult_alg*prim_alg)
}

# Questão 3
Q3.P1 <- function(){
  n = 10
  npares = 5
  nimpares = 5
  
  prim.alg = 9
  sec.alg = 8
  ult.alg = 5
  
  print(prim.alg*sec.alg*ult.alg)
}

# Questão 4
Q4.P1 <- function(){
  # _|_ _|_ _|_ _|_ _|_ = 10 lugares
  # 5 rapazes e 5 mocas
  rapazes = 10 * 8 * 6 * 4 * 2
  mocas = fatorial_rec(5)
  
  print(rapazes * mocas)
}

# Questão 5
Q5.P1 <- function(){
  n = 5
  print(perm.circular(n))
}

# Questão 6
Q6.P1 <- function(){
  joao.resolve = 2/3
  maria.resolve = 3/4
  
  joao.nao.resolve = 1 - joao.resolve
  maria.nao.resolve = 1 - maria.resolve
  
  # Caso 1: Joao e Maria resolvem
  prob1 = joao.resolve * maria.resolve
  
  # Caso 2: Joao resolve e Maria não resolve
  prob2 = joao.resolve * maria.nao.resolve
  
  # Caso 3: Joao não resolve e Maria resolve
  prob3 = joao.nao.resolve * maria.resolve
  
  # Caso 4: Joao e Maria nao resolvem
  prob4 = joao.nao.resolve * maria.nao.resolve
  
  # Resposta
  prob.resolve = prob1 + prob2 + prob3
  print(prob.resolve)
}

# Questão 7 (??? + ??? + ??? + ???)
Q7.P1 <- function(){
  nbolinhas = 6
  nurnas = 3
}

# Questão 8
Q8.P1 <- function(){
  nsamples = 10000
  
  mult.3.ou.5 = 0
  
  for (i in 1:nsamples){
    num = sample(1:300, 1, replace=T)
    
    if (num %% 3 == 0 | num %% 5 == 0){
      mult.3.ou.5 = mult.3.ou.5 + 1
    }
  }
  
  print(mult.3.ou.5 / nsamples)
}

# Questão 9
Q9.P1 <- function(){
  # 80% dos pênaltis marcados a favor do Brasil são cobrados por jogadores do Botafogo

  # probabilidade de um pênalti ser convertido é de 40% se o cobrador for do Botafogo 
  # e de 70% caso contrário
  
  # a) probabilidade do pênalti ser cobrado por um jogador do Botafogo e ser convertido
  evento.a = 0.8 * 0.4
  
  # b) probabilidade do pênalti ser convertido
  evento.b = 0.8 * 0.4 + (1 - 0.8) * 0.7
  
  # c) Este pênalti acabou de ser desperdiçado. Qual é a probabilidade de que o cobrador tenha sido
  # um jogador do Botafogo?
  evento.c = 0.8 * (1 - 0.4)
  
  print(paste("Evento A:", evento.a, "; Evento B:", evento.b, "; Evento C:", evento.c))
}

# Questão 10
Q10.P1 <- function(){
  n.faces = 6

  # a) obter ao menos um 6 em 4 lances de um dado
  evento.a = 1 - (1 - (1/n.faces))**4
  
  # b) Obter ao menos um duplo 6 em 24 lançamentos de um par de dados
  evento.b = 1 - (1 - ((1/n.faces) * (1/n.faces)))**24
  
  print(paste("Evento A:", evento.a, "; Evento B:", evento.b))
}

# Questão 11 (???)
Q11.P1 <- function(){
  n.alunos = 30
  dias.ano = 365
}

# Questão 12 (???)
Q12.P1 <- function(){
  marina.escreve = 8/10
  correio.perde = 9/10
  carteiro.entrega = 9/10
  
  marina.nao.escreve = 1 - marina.escreve
  correio.nao.perde = 1 - correio.perde
  carteiro.nao.entrega = 1 - carteiro.entrega
}

# Questão 13 (???)

# Questão 14 (???)

# Questão 15 (???)

# Questão 16 (???)

# Questão 17 (???)

# Questão 18 (???)

# Questão 19 (???)

# Questão 20 (???)

# Questão 21 (???)


## PARTE 2

# Questão 1
Q1.P2 <- function(){
  nsamples = 10000
  
  # seed = 3
  # a = 39373
  # c = 0
  # M = (2**31) - 1
  
  duas.caras = 0
  
  for (i in 1:nsamples){
    face1 = sample(c('cara', 'coroa'), 1, replace=T)
    face2 = sample(c('cara', 'coroa'), 1, replace=T)
    
    if (face1 == 'cara' & face2 == 'cara'){
      duas.caras = duas.caras + 1
    }
  }
  
  print(paste("Foram encontradas duas caras em ", duas.caras, "lançamentos."))
}

# Questão 2
Q2.P2 <- function(){
  nsamples = 10000
  
  counter.somas = rep(0, 11)
  
  # A condição any(counter.somas == 0) verifica se há algum elemento em counter.somas que seja 
  # igual a zero. Se houver pelo menos um elemento igual a zero, o loop continuará a ser executado.
  while (any(counter.somas == 0)){
    dado1 = sample(1:6, 1, replace=T)
    dado2 = sample(1:6, 1, replace=T)
    
    soma = dado1 + dado2
    counter.somas[soma-1] = soma
  }
  
  print(paste("A quantidade de rolagens necessárias foi de", sum(counter.somas), 'vezes.'))
}

# Questão 3
Q3.P2 <- function(){
  nsamples = 10000
  
  moeda = c('coroa', 'cara')
  prob.moeda = c(3/5, 2/5)  # c('coroa', 'cara')
  
  evento.a = rep(0, nsamples)
  evento.b = rep(0, nsamples)
  
  for (i in 1:nsamples){
    face1 = sample(moeda, 1, replace=T, prob=prob.moeda)
    face2 = sample(moeda, 1, replace=T, prob=prob.moeda)
    face3 = sample(moeda, 1, replace=T, prob=prob.moeda)
    
    # Evento A
    if ((face1 == 'coroa' & face2 == 'cara') | (face1 == 'cara' & face2 == 'coroa')){
      evento.a[i] = 1
    }
    
    # Evento B
    if (face2 == 'coroa' & face3 == 'coroa'){
      evento.b[i] = 1
    }
  }
  
  # Item A
  PA = sum(evento.a) / nsamples
  PB = sum(evento.b) / nsamples
  
  print(paste("3.a) P(A) =", PA, "e P(B) =", PB))
  
  # Item B
  P.intersecao = prob.intersecao(evento.a, evento.b, nsamples)
  evento.eh.independente = eventos.independentes(PA, PB, P.intersecao)
}

# Questão 4
Q4.P2 <- function(){
  
  nsamples = 125
  
  letras.disp = c('A', 'B', 'C', 'D', 'E', 'F')
  
  evento = rep(0, 3)  # evento[1] = Evento A; evento[2] = Evento B; evento[3] = Evento C;
  
  for (i in 1:nsamples){
    # Gets a code randomly
    cod1 = sample(letras.disp, 3, replace=T)
    cod2 = sample(letras.disp, 3, replace=T)
    
    # Checks if cod2 == cod1
    while (cod2[1] == cod1[1] & cod2[2] == cod1[2] & cod2[3] == cod1[3]){
      cod2 = sample(letras.disp, 3, replace=T)
    }
    
    # Evento A
    # If there's at least a common character, it's not satisfied
    if (!(cod2[1] %in% cod1 | cod2[2] %in% cod1 | cod2[3] %in% cod1)){
      evento[1] = evento[1] + 1
    }
    
    # Evento B
    if ((cod2[1] %in% cod1 & cod2[1] != cod1[1]) & (cod2[2] %in% cod1 & cod2[2] != cod1[2]) & (cod2[3] %in% cod1 & cod2[3] != cod1[3])){
      evento[2] = evento[2] + 1
    }
    
    # Evento C
    if (cod1[1] %in% c('A', 'E') & cod1[2] %in% c('A', 'E')){
      evento[3] = evento[3] + 1
    }
  }
  
  P = c(evento[1]/nsamples, evento[2]/nsamples, evento[3]/nsamples)
  for (i in 1:length(P)){
    print(paste("P[",i,"] =",P[i]))
  }
}

# Questão 5
Q5.P2 <- function(){
  lista.defeitos = c('C1', 'C2', 'C3', 'C4')
  sem.manu = c(0.04, 0.04, 0.06, 0.06)
  com.manu = c(0.04, 0.04, 0.03, 0.03)
  
  perc.equip.manu = 0.4
  
  nsamples = 10000
  
  equip.com.defeito.com.manu = 0
  equip.com.defeito.sem.manu = 0
  equip.sem.defeito.com.manu = 0
  equip.sem.defeito.sem.manu = 0
  
  total.com.manu = 0
  total.sem.manu = 0
  
  equip.com.C1.com.manu = 0
  equip.com.C1.sem.manu = 0
  
  equip.com.C3.com.manu = 0
  equip.com.C3.sem.manu = 0
  
  for (i in 1:nsamples){
    manutencao = sample(c(F, T), 1, prob=c(1-perc.equip.manu, perc.equip.manu))
    
    cont.defeitos = rep(0, 4)
    
    if (manutencao == T){
      total.com.manu = total.com.manu + 1
        
      for (j in 1:4){
        cont.defeitos[j] = cont.defeitos[j] + sample(c(1, 0), 1, prob = c(com.manu[j], 1 - com.manu[j]))
      }
      
      if (sum(cont.defeitos) > 0){
        equip.com.defeito.com.manu = equip.com.defeito.com.manu + 1
      } else {
        equip.sem.defeito.com.manu = equip.sem.defeito.com.manu + 1
      }
      
      if (sum(cont.defeitos[1]) > 0 & sum(cont.defeitos[2:4]) == 0){
        equip.com.C1.com.manu = equip.com.C1.com.manu + 1
      }
      
      if (sum(cont.defeitos[3]) > 0 & sum(cont.defeitos[c(1,2,4)]) == 0){
        equip.com.C3.com.manu = equip.com.C3.com.manu + 1
      }
      
    } else {
      total.sem.manu = total.sem.manu + 1

      for (k in 1:4){
        cont.defeitos[k] = cont.defeitos[k] + sample(c(1, 0), 1, prob = c(sem.manu[k], 1 - sem.manu[k]))
      }
      
      if (sum(cont.defeitos) > 0){
        equip.com.defeito.sem.manu = equip.com.defeito.sem.manu + 1
      } else {
        equip.sem.defeito.sem.manu = equip.sem.defeito.sem.manu + 1
      }
      
      if (sum(cont.defeitos[1]) > 0 & sum(cont.defeitos[2:4]) == 0){
        equip.com.C1.sem.manu = equip.com.C1.sem.manu + 1
      }
      
      if (sum(cont.defeitos[3]) > 0 & sum(cont.defeitos[c(1,2,4)]) == 0){
        equip.com.C3.sem.manu = equip.com.C3.sem.manu + 1
      }
    }
    
    total.com.defeito = equip.com.defeito.sem.manu + equip.com.defeito.com.manu
    total.sem.defeito = equip.sem.defeito.sem.manu + equip.sem.defeito.com.manu

    total.C1 = equip.com.C1.sem.manu + equip.com.C1.com.manu
    total.C3 = equip.com.C3.sem.manu + equip.com.C3.com.manu
  }
  
  # total.com.defeito = equip.com.defeito.sem.manu + equip.com.defeito.com.manu
  # total.sem.defeito = equip.sem.defeito.sem.manu + equip.sem.defeito.com.manu
  # 
  # total.C1 = equip.com.C1.sem.manu + equip.com.C1.com.manu
  # total.C3 = equip.com.C3.sem.manu + equip.com.C3.com.manu
  
  # A.i) apresente defeito
  PAi = total.com.defeito / nsamples
  print(paste('Probabilidade de apresentar defeito:', PAi))
  
  # A.ii) apresente algum defeito se não for feita manutenção preventiva
  PAii = sum(equip.com.defeito.sem.manu) / total.sem.manu
  print(paste('Probabilidade de apresentar defeito sem manutencao:', PAii))
  
  # A.iii) apresente algum defeito se for feita a manutenção preventiva.
  PAiii = sum(equip.com.defeito.com.manu) / total.com.manu
  print(paste('Probabilidade de apresentar defeito com manutetencao:', PAiii))
  
  # B.i)
  PBi = equip.com.C1.com.manu / total.C1
  print(paste('Probabilidade de apresentar defeito C1 dado que foi feita manutencao:', PBi))
  
  # B.ii)
  PBii = equip.com.C3.com.manu / total.C3
  print(paste('Probabilidade de apresentar defeito C3 dado que foi feita manutencao:', PBii))
}

Q5.P2()

# Questão 6

# Questão 7

# Questão 8

# Questão 9




