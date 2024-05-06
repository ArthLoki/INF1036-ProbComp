
solucao.analitica <- function(){
  ladoQuadrado = 1  #km
  diagonalQuadrado = ladoQuadrado * sqrt(2)
  
  # r1 = ladoQuadrado
  # r2 = ladoQuadrado
  
  areaTotalR1 = (pi * ladoQuadrado^2) / 4
  areaTotalR2 = (pi * ladoQuadrado^2) / 4

  area.trianguloDiagonal = (diagonalQuadrado * (diagonalQuadrado / 2)) / 2  # (base * altura) / 2
  
  areaR1 = areaTotalR1 - area.trianguloDiagonal
  areaR2 = areaTotalR2 - area.trianguloDiagonal
  
  area.verde = areaR1 + areaR2
  
  adubo.por.km = 100  #kg
  meses = 12
  print(paste('O total de adubo que deverá ser comprado é de', adubo.por.km * meses * area.verde, 'Kg'))
}

solucao.simulacao <- function(){
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

solucao.analitica()
solucao.simulacao()