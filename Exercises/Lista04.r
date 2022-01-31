q1<-function(Ns=1000){
  library(igraph)
  library(triangle)
  library(hash)
  
  #1-fun??es auxiliares
  geraCen<-function(indiceInicial,matrix){
    #gera amostras para as dura??es das atividades
    n<-nrow(matrix)
    dur<-vector(mode="numeric",length=n)
    for(i in indiceInicial:n){
      dur[i]<-rtriangle(1,matrix[i,1],matrix[i,3],matrix[i,2])
    }
    dur
  }
  
  encontrarTempo<-function(dur,est,eft){
    print(dur)
    for(i in 1:length(g)){
      eft[i] <- est[i] + dur[i]
      c<-which(g[i]==1)
      if (length(c) > 0){
        for(j in 1:length(c)){
          if(est[c[j]] < eft[i]){
            est[c[j]]<-eft[i]
          }
        }
      }
    }
    
    list(est,eft)
  }
  
  durCC<-function(dur){
    #calcula a dura??o do cc do cenario 
    dc<-vector(mode="numeric",length=l)
    for(i in 1:l){
      dc[i]<-sum(d[sp[[i]]])
    }
    #acha o indice do m?ximo
    icc<-which.max(dc)
    
    #retorna o indice do caminho critico
    icc
  }
  
  #2-Estruturas de dados globais
  
  #parametros das triangulares dos custos
  custos<-matrix(data=c(300,450,600,
                        480,600,720,
                        3750,4500,5250,
                        8400,9600,10800,
                        300000,312000,322500,
                        37650,39600,41400,
                        10500,11550,12600,
                        36000,38400,40800,
                        48750,52500,56250,
                        360,450,540)
                  ,ncol=3,byrow=T)
  
  #par?metros das triangulares dos prazos
  d<-matrix(data=c(0,0,0,
                    2,4,18,
                    5,9,19,
                    4,10,28,
                    8,13,36,
                    44,60,100,
                    30,40,74,
                    9,20,43,
                    24,30,48,
                    28,29,96,
                    10,10,12)
            ,ncol=3,byrow=T)
  
  #lista de arestas
  elos<-c(1,2,1,3,2,4,4,5,3,6,5,6,6,7,6,8,7,9,8,9,7,10,9,11,10,11)
  
  #lista de aparencias em caminhos criticos
  cont_atividade<-vector(mode = 'numeric',length = 11)
  
  #3-Main 
  #cria grafo
  g<-make_graph(elos)
  tkplot(g,vertex.color='white')
  #cria caminhos
  sp<-all_simple_paths(g,from=1,to=11)
  
  l<-length(sp)
  
  #cria amostras de custo
  Custo<-vector(length=Ns)
  
  #cria amostras das dura??ess dos caminhos
  dcen <- hash()
  icen<-vector(length=Ns) #indice dos caminhos cr?ticos
  agendamentos<-hash()
  
  #gera Ns cenarios
  for (i in 1:Ns){
    print(i)
    index <- paste(i)
    
    # Calcula o custo
    Cenarios<-geraCen(1,custos)
    Custo[i]<-sum(Cenarios)
    
    #Calcula o prazo
    #inicializa vetor de tempo inicial e final de cada atividade
    est<-rep(0, times = 11)
    eft<-rep(0, times = 11)
    dur<-geraCen(2,d)
    timeN<-encontrarTempo(dur,est,eft)
    est <- timeN[1]
    eft <- timeN[2]
    agendamentos[[index]] <- est
    
    #encontra o caminho critico e a duracao dele
    durC<-durCC(dur)
    icen[i]<-indice_CC(dur) #vetor que pega o indice do caminho cr?tico de cada cen?rio
    dcen[[index]]<-sum(dur[sp[[icen[i]]]]) #somat?rio da dura??o dos caminhos cr?ticos
    
    for (j in 1:11){
      if(j %in% sp[[icen[i]]]){ #verificando se a atividade pertence ao caminho critico
        cont_atividade[j]<-cont_atividade[j]+1
      }   
    }
  }
  
  #histograma do custo
  hist(Custo)
  #ecdf do custo
  plot(ecdf(Custo))
  
  #Calcula as probabilidades das atividades fazerem parte do caminho critico
  probabilities<-lapply(cont_atividade, function(x) x/Ns)
  print("Probabilidade")
  print(probabilities)
  
  #Descobre o agendamento com 85 porcento de chance
  Prazo <- values(dcen, USE.NAMES=FALSE)
  dcen <- sort(values(dcen))
  dcen <- as.list(dcen)
  
  posicao <- (0.85)*Ns
  posicaoChance <- names(dcen)[posicao]
  
  print("Agendamento")
  print(values(agendamentos[posicaoChance])[[1]])
  
  #analisa preliminar do prazo  
  print(summary(Prazo))
  #histograma do prazo
  plot(hist(Prazo))
  #ecdfd do prazo
  plot(ecdf(Prazo))
  #testa normalidade do prazo
  qqnorm(Prazo);qqline(Prazo,col='red')
  
  #gr?fico de dispers?o da rela??o entre custo e prazo
  plot(Custo,Prazo)
  abline(h=mean(Prazo),v=mean(Custo),col='red')
}

q1()
