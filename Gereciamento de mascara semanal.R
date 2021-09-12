# Importando Livraria
library(ggplot2)
print("Gereciamento de mascaras semanais em R")
Contador = as.integer(1) 

#vetor de quantidade de mascaras para a criação do grafico
VMascara1<-c(0,0,0,0,0,0,0,0)
VMascara2<-c(0,0,0,0,0,0,0,0)
VMascara3<-c(0,0,0,0,0,0,0,0)

#while para passar os dias 
while (Contador <=7) {
  cat("DIA",Contador)
  
  #Criando estoque inicial de mascaras 
  if (Contador==1){
    Mascara1 <- readline(prompt="Digite a quantidade da mascaras 1: ")
    Mascara2 <- readline(prompt="Digite a quantidade da mascaras 2: ")
    Mascara3 <- readline(prompt="Digite a quantidade da mascaras 3: ")
  }
  
  #Transformando os valores em numerico para evitar erro
  Mascara1 <- as.numeric(Mascara1)
  Mascara2 <- as.numeric(Mascara2)
  Mascara3 <- as.numeric(Mascara3)
  
  Escolha = 0
  #Possiveis ações antes do fim do dia 
  while (Escolha !=3) {
    
    Escolha <- readline(prompt="Digite 1 para vendas , 2 para adicionar ao estoque,3 para finalizar o dia : ")
    
    #escolheu Vendas
    if (Escolha==1){
      #Qual foi vendida?
      TipoMascara <- readline(prompt="Qual mascaras foi vendida ?: ")
      
      #mascara 1 foi vendida
      if (TipoMascara==1){
        Vendas <- readline(prompt="Quantas máscaras do tipo 1 foram vendidas hoje?: ")
        Vendas<- as.numeric(Vendas)
        #Impossibilitando venda de Mascaras , se nao houver estoque suficiente
        if (Vendas>Mascara1){
          print("Não foi possivel adicionar a venda , você ja verificou o estoque ?")
        }
        
        else {
          Mascara1 = Mascara1 - Vendas
          VMascara1[Contador]= Vendas
        }
      }
      
      #mascara 2 vendida
      if (TipoMascara==2){
        Vendas <- readline(prompt="Quantas mascaras do tipo 2 foram vendidas hoje?: ")
        Vendas<- as.numeric(Vendas)
        #Impossibilitando venda de Mascaras , se nao houver estoque suficiente
        if (Vendas>Mascara2){
          print("Não foi possivel adicionar a venda , você ja verificou o estoque ?")
        }
        else {
          Mascara2 = Mascara2- Vendas
          VMascara2[Contador]=  VMascara2[Contador]+Vendas
        }
      }
      
      #mascara 3 foi vendida
      if (TipoMascara==3){
        Vendas <- readline(prompt="Quantas máscaras do tipo 3 foram vendidas hoje?: ")
        Vendas<- as.numeric(Vendas)
        #Impossibilitando venda de mascaras , se nao houver estoque suficiente
        if (Vendas>Mascara3){
          print("Não foi possivel adicionar a venda , você ja verificou o estoque ?")
        }
        else {
          Mascara3 = Mascara3- Vendas
          VMascara3[Contador]=  VMascara3[Contador]+Vendas
        }
      }
      
      else {
        print("Tipo de mascaras inválida")
      }
    }
    
    #Adicionando estoque
    if (Escolha==2){
      TipoMascara <- readline(prompt="Qual mascaras foi adicionada ?: ")
      
      #mascara 1 adicionada
      if (TipoMascara==1){
        Estoque <- readline(prompt="Quantas máscaras do tipo 1 foram adicionadas hoje ")
        Estoque<- as.numeric(Estoque)
        Mascara1 = Mascara1 + Estoque
      }
      
      #mascara 2 foi adicionada
      if (TipoMascara==2){
        Estoque <- readline(prompt="Quantas máscaras do tipo 2 foram adicionadas hoje?: ")
        Estoque<- as.numeric(Estoque)
        Mascara2 = Mascara2 + Estoque
      }
      
      #mascara 3 foi adicionada
      if (TipoMascara==3){
        Estoque <- readline(prompt="Quantas máscaras do tipo 3 foram adicionadas hoje?: ")
        Estoque<- as.numeric(Estoque)
        Mascara3 = Mascara3+Estoque
      }
      
      
      else {
        print("Tipo de mascaras inválida")
      } 
    }
    
    #Atualizando numero de mascaras
    Mascaras<-c(Mascara1,Mascara2,Mascara3)
  }
  
  
  Contador = Contador + 1
}