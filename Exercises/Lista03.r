constCont<-function(Ns=1000){
  library(triangle)
  
  #Passo 1-Gerar as amostras das vari?veis de risco
  Pacote1<-rtriangle(Ns,15,19,17)  
  Pacote1_cont<-rtriangle(Ns,20,25,22)
  Maior_cam<-rbinom(Ns,1,0.5)
  Pacote2<-rtriangle(Ns,41,47,42)
  Pacote2_cont<-rtriangle(Ns,45,50,47)
  N_infiltra<-rbinom(Ns,1,0.75)
  Pacote3<-rtriangle(Ns,100,110,105)
  Pacote4<-rtriangle(Ns,40,52,45)
  Pacote5<-rtriangle(Ns,35,40,36)
  Pacote6<-rtriangle(Ns,25,27,26)
  Pacote7<-rtriangle(Ns,15,19,17)
  
  #Passo 2-Montar a matriz de cen?rios
  Cenarios<-cbind(Pacote3,Pacote4,Pacote5,Pacote6,Pacote7,Pacote1,Pacote2,
                  Pacote1_cont,Pacote2_cont,Maior_cam,N_infiltra)
  
  #Passo 3-Avaliar cenarios
  Custo<-apply(Cenarios[,1:5],M=1,sum)+
    Maior_cam*Pacote1+(1-Maior_cam)*Pacote1_cont+
    N_infiltra*Pacote2+(1-N_infiltra)*Pacote2_cont
  
  #Passo 4-Mostrar resultados
  hist(Custo)
  plot(ecdf(Custo))
  
  #Passo 5-Calcular valor da obra considerando risco de 15 porcento
  custo_ordenado <- sort(Custo)
  
  posicao_risco <- (1-0.15)*Ns
  valor_obra <- custo_ordenado[posicao_risco]
  
  print(valor_obra)
  
  #Passo 6-Calcular valor do orcamento considerando media dos custos
  orcamento <- mean(Custo)
  print(orcamento)
  
  #Passo 7-Retorna resultado
  valor_obra - orcamento
}

constCont()