# teste do Ifood
# Estatística Descritiva
dados<-read.table("ml_project1_data.csv",h=TRUE, sep = ",")
View(dados)
head(dados)
summary(dados)
## pacotes utlizados
library(nycflights13)
library(tidyverse)
######################################################3
# Vamos analisar Consumo Segundo o numero de filhos
# vamos filtrando os dados de interesse

# filtrar por numero de crianças e adoslescentes (tem no máximo duas crianças e 2 adolescente)
  ## 0 crianças e 0 adoslescente
  kids0Teen0<-filter(dados, Kidhome==0&Teenhome==0)
  length(kids0Teen0$ID)#638
  
  ### vamos ver quem participou das campanhas
  kids0Teen0_Cmp1<- filter(dados, Kidhome==0&Teenhome==0&AcceptedCmp1==1)
  length(kids0Teen0_Cmp1$ID)#110

  kids0Teen0_Cmp2<-filter(dados, Kidhome==0&Teenhome==0&AcceptedCmp2==1)
  length(kids0Teen0_Cmp2$ID)#18

  kids0Teen0_Cmp3<-filter(dados, Kidhome==0&Teenhome==0&AcceptedCmp3==1)
  length(kids0Teen0_Cmp3$ID)#48

  kids0Teen0_Cmp4<-filter(dados, Kidhome==0&Teenhome==0&AcceptedCmp4==1)
  length(kids0Teen0_Cmp4$ID)#69

  kids0Teen0_Cmp5<-filter(dados, Kidhome==0&Teenhome==0&AcceptedCmp5==1)
  length(kids0Teen0_Cmp5$ID)#138
 
  kids0Teen0_Resp<-filter(dados, Kidhome==0&Teenhome==0&Response==1)
  length(kids0Teen0_Resp$ID)#169
 
 # banco de dados por campanha
    by_kids_Cmp<- data.frame(KidsTeen = c("k0T0_C1", "k0T0_C2", "k0T0_C3","k0T0_C4","k0T0_C5","k0T0_L"),
                  Quantidade = c(length(kids0Teen0_Cmp1$ID), length(kids0Teen0_Cmp2$ID), length(kids0Teen0_Cmp3$ID),length(kids0Teen0_Cmp4$ID),length(kids0Teen0_Cmp5$ID),length(kids0Teen0_Resp$ID)))
    arrange(by_kids_Cmp,desc(Quantidade))
    # Barras
    ggplot(by_kids_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
 ## Consumos
 sum(kids0Teen0$MntWines)#625.451
 sum(kids0Teen0$MntFruits)#56.104
 sum(kids0Teen0$MntMeatProducts)#353.697
 sum(kids0Teen0$MntFishProducts)#80.127
 sum(kids0Teen0$MntSweetProducts)# 57.457
 sum(kids0Teen0$MntGoldProds)#89.108
  # banco de dados para o consumo
  by_kids_Prod00<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                          Quantidade = c(sum(kids0Teen0$MntWines), sum(kids0Teen0$MntFruits), sum(kids0Teen0$MntMeatProducts),sum(kids0Teen0$MntFishProducts),sum(kids0Teen0$MntSweetProducts),sum(kids0Teen0$MntGoldProds)))
  arrange(by_kids_Prod00,desc(Quantidade))
   
  # Barras
  ggplot(by_kids_Prod00, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  #consumo total de todos os produtos
  Tot00<-sum(c(sum(kids0Teen0$MntWines), sum(kids0Teen0$MntFruits), sum(kids0Teen0$MntMeatProducts),sum(kids0Teen0$MntFishProducts),sum(kids0Teen0$MntSweetProducts),sum(kids0Teen0$MntGoldProds)))
  Tot00
 
 
  ####  0 crianças e 1 adoslescente ####
  kids0Teen1<-filter(dados, Kidhome==0&Teenhome==1)
  length(kids0Teen1$ID)# 625
  kids0Teen1_Cmp1<- filter(dados, Kidhome==0&Teenhome==1&AcceptedCmp1==1)
  length(kids0Teen1_Cmp1$ID)#22
  
  kids0Teen1_Cmp2<-filter(dados, Kidhome==0&Teenhome==1&AcceptedCmp2==1)
  length(kids0Teen1_Cmp2$ID)# 9

  kids0Teen1_Cmp3<-filter(dados, Kidhome==0&Teenhome==1&AcceptedCmp3==1)
  length(kids0Teen1_Cmp3$ID)#36

  kids0Teen1_Cmp4<-filter(dados, Kidhome==0&Teenhome==1&AcceptedCmp4==1)
  length(kids0Teen1_Cmp4$ID)#72

  kids0Teen1_Cmp5<-filter(dados, Kidhome==0&Teenhome==1&AcceptedCmp5==1)
  length(kids0Teen1_Cmp5$ID)#15
  
  kids0Teen1_Resp<-filter(dados, Kidhome==0&Teenhome==1&Response==1)
  length(kids0Teen1_Resp$ID)# 50
  
  # banco de dados
  by_kids01_Cmp<- data.frame(KidsTeen = c("k0T1_C1", "k0T1_C2", "k0T1_C3","k0T1_C4","k0T1_C5","k0T1_L"),
                           Quantidade = c(length(kids0Teen1_Cmp1$ID), length(kids0Teen1_Cmp2$ID), length(kids0Teen1_Cmp3$ID),length(kids0Teen1_Cmp4$ID),length(kids0Teen1_Cmp5$ID),length(kids0Teen1_Resp$ID)))
  arrange(by_kids01_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids01_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)

  ## Consumos por produtos
  sum(kids0Teen1$MntWines)# 260179
  sum(kids0Teen1$MntFruits) # 16910
  sum(kids0Teen1$MntMeatProducts)# 86688
  sum(kids0Teen1$MntFishProducts) # 22770
  sum(kids0Teen1$MntSweetProducts) # 17938
  sum(kids0Teen1$MntGoldProds) #  34990
  # banco de dados
  by_kids_Prod01<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade = c(sum(kids0Teen1$MntWines), sum(kids0Teen1$MntFruits), sum(kids0Teen1$MntMeatProducts),sum(kids0Teen1$MntFishProducts),sum(kids0Teen1$MntSweetProducts),sum(kids0Teen1$MntGoldProds)))
  arrange(by_kids_Prod01,desc(Quantidade))
  
  # Barras
  ggplot(by_kids_Prod01, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot01<-sum(c(sum(kids0Teen1$MntWines), sum(kids0Teen1$MntFruits), sum(kids0Teen1$MntMeatProducts),sum(kids0Teen1$MntFishProducts),sum(kids0Teen1$MntSweetProducts),sum(kids0Teen1$MntGoldProds)))
  Tot01
  
  #### 1 criança e 0 adoslescente ####
  kids1Teen0<-filter(dados, Kidhome==1&Teenhome==0)
  length(kids1Teen0$ID)# 503
  kids1Teen0_Cmp1<- filter(dados, Kidhome==1&Teenhome==0&AcceptedCmp1==1)
  length(kids1Teen0_Cmp1$ID)#5

  kids1Teen0_Cmp2<- filter(dados, Kidhome==1&Teenhome==0&AcceptedCmp2==1)
  length(kids1Teen0_Cmp2$ID)#0

  kids1Teen0_Cmp3<- filter(dados, Kidhome==1&Teenhome==0&AcceptedCmp3==1)
  length(kids1Teen0_Cmp3$ID)#49

  kids1Teen0_Cmp4<- filter(dados, Kidhome==1&Teenhome==0&AcceptedCmp4==1)
  length(kids1Teen0_Cmp4$ID)#7

  kids1Teen0_Cmp5<- filter(dados, Kidhome==1&Teenhome==0&AcceptedCmp5==1)
  length(kids1Teen0_Cmp5$ID)#6
  
  kids1Teen0_Resp<-filter(dados, Kidhome==1&Teenhome==0&Response==1)
  length(kids1Teen0_Resp$ID)# 66
  # banco de dados
  by_kids10_Cmp<- data.frame(KidsTeen = c("k1T0_C1", "k1T0_C2", "k1T0_C3","k1T0_C4","k1T0_C5","k1T0_L"),
                             Quantidade = c(length(kids1Teen0_Cmp1$ID), length(kids1Teen0_Cmp2$ID), length(kids1Teen0_Cmp3$ID),length(kids1Teen0_Cmp4$ID),length(kids1Teen0_Cmp5$ID),length(kids1Teen0_Resp$ID)))
  arrange(by_kids10_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids10_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  ## Consumos
  sum(kids1Teen0$MntWines)#41.043
  sum(kids1Teen0$MntFruits)#4.920
  sum(kids1Teen0$MntMeatProducts)#24.565
  sum(kids1Teen0$MntFishProducts)#7.281
  sum(kids1Teen0$MntSweetProducts)#4.955
  sum(kids1Teen0$MntGoldProds)# 11.004
  by_kids_Prod10<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids1Teen0$MntWines),
                                              sum(kids1Teen0$MntFruits),
                                              sum(kids1Teen0$MntMeatProducts),
                                              sum(kids1Teen0$MntFishProducts),
                                              sum(kids1Teen0$MntSweetProducts),
                                              sum(kids1Teen0$MntGoldProds)))
  arrange(by_kids_Prod10,desc(Quantidade))

  # Barras
  ggplot(by_kids_Prod10, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot10<- sum(c(sum(kids1Teen0$MntWines),
                sum(kids1Teen0$MntFruits),
                sum(kids1Teen0$MntMeatProducts),
                sum(kids1Teen0$MntFishProducts),
                sum(kids1Teen0$MntSweetProducts),
                sum(kids1Teen0$MntGoldProds)))
  Tot10
  
# 1 criança e 1 adoslescente
  kids1Teen1<-filter(dados, Kidhome==1&Teenhome==1)
  length(kids1Teen1$ID)# 374
  kids1Teen1_Cmp1<- filter(dados, Kidhome==1&Teenhome==1&AcceptedCmp1==1)
  length(kids1Teen1_Cmp1$ID)#3

  kids1Teen1_Cmp2<- filter(dados, Kidhome==1&Teenhome==1&AcceptedCmp2==1)
  length(kids1Teen1_Cmp2$ID)#2

  kids1Teen1_Cmp3<- filter(dados, Kidhome==1&Teenhome==1&AcceptedCmp3==1)
  length(kids1Teen1_Cmp3$ID)#25

  kids1Teen1_Cmp4<- filter(dados, Kidhome==1&Teenhome==1&AcceptedCmp4==1)
  length(kids1Teen1_Cmp4$ID)#13

  kids1Teen1_Cmp5<- filter(dados, Kidhome==1&Teenhome==1&AcceptedCmp5==1)
  length(kids1Teen1_Cmp5$ID)#1
  
  kids1Teen1_Resp<-filter(dados, Kidhome==1&Teenhome==1&Response==1)
  length(kids1Teen1_Resp$ID)# 42
  # banco de dados
  by_kids11_Cmp<- data.frame(KidsTeen = c("k1T1_C1", "k1T1_C2", "k1T1_C3","k1T1_C4","k1T1_C5","k1T1_L"),
                             Quantidade = c(length(kids1Teen1_Cmp1$ID), length(kids1Teen1_Cmp2$ID), length(kids1Teen1_Cmp3$ID),length(kids1Teen1_Cmp4$ID),length(kids1Teen1_Cmp5$ID),length(kids1Teen1_Resp$ID)))
  arrange(by_kids11_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids11_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  ## Consumos
  sum(kids1Teen1$MntWines)#46.282
  sum(kids1Teen1$MntFruits)#2.447
  sum(kids1Teen1$MntMeatProducts)# 3.555
  sum(kids1Teen1$MntFishProducts)# 2.813
  sum(kids1Teen1$MntSweetProducts)#3.555
  sum(kids1Teen1$MntGoldProds)# 2.813

  
  by_kids_Prod11<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids1Teen1$MntWines),
                                              sum(kids1Teen1$MntFruits),
                                              sum(kids1Teen1$MntMeatProducts),
                                              sum(kids1Teen1$MntFishProducts),
                                              sum(kids1Teen1$MntSweetProducts),
                                              sum(kids1Teen1$MntGoldProds)))
  
  arrange(by_kids_Prod11,desc(Quantidade))
  
  # Barras
  ggplot(by_kids_Prod11, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot11<-sum(c(sum(kids1Teen1$MntWines),
               sum(kids1Teen1$MntFruits),
               sum(kids1Teen1$MntMeatProducts),
               sum(kids1Teen1$MntFishProducts),
               sum(kids1Teen1$MntSweetProducts),
               sum(kids1Teen1$MntGoldProds)))
  Tot11
  
  ###### 0 crianças e 2 adoslescentes #######
  kids0Teen2<-filter(dados, Kidhome==0&Teenhome==2)
  length(kids0Teen2$ID)# 30
  kids0Teen2_Cmp1<- filter(dados, Kidhome==0&Teenhome==2&AcceptedCmp1==1)
  length(kids0Teen2_Cmp1$ID)#1

  kids0Teen2_Cmp2<-filter(dados, Kidhome==0&Teenhome==2&AcceptedCmp2==1)
  length(kids0Teen2_Cmp2$ID)# 1

  kids0Teen2_Cmp3<-filter(dados, Kidhome==0&Teenhome==2&AcceptedCmp3==1)
  length(kids0Teen2_Cmp3$ID)#3

  kids0Teen2_Cmp4<-filter(dados, Kidhome==0&Teenhome==2&AcceptedCmp4==1)
  length(kids0Teen2_Cmp4$ID)#3

  kids0Teen2_Cmp5<-filter(dados, Kidhome==0&Teenhome==2&AcceptedCmp5==1)
  length(kids0Teen2_Cmp5$ID)#2
  
  kids0Teen2_Resp<-filter(dados, Kidhome==0&Teenhome==2&Response==1)
  length(kids0Teen2_Resp$ID)# 3
  
  # banco de dados
  by_kids02_Cmp<- data.frame(KidsTeen = c("k0T2_C1", "k0T2_C2", "k0T2_C3","k0T2_C4","k0T2_C5","k0T2_L"),
                             Quantidade = c(length(kids0Teen2_Cmp1$ID), length(kids0Teen2_Cmp2$ID), length(kids0Teen2_Cmp3$ID),length(kids0Teen2_Cmp4$ID),length(kids0Teen2_Cmp5$ID),length(kids0Teen2_Resp$ID)))
  arrange(by_kids02_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids02_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  
  ## Consumos
  sum(kids0Teen2$MntWines)# 12287
  sum(kids0Teen2$MntFruits)#621
  sum(kids0Teen2$MntMeatProducts)#4.004
  sum(kids0Teen2$MntFishProducts)#1.011
  sum(kids0Teen2$MntSweetProducts)#574
  sum(kids0Teen2$MntGoldProds)#1.711
  by_kids_Prod02<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids0Teen2$MntWines),
                                              sum(kids0Teen2$MntFruits),
                                              sum(kids0Teen2$MntMeatProducts),
                                              sum(kids0Teen2$MntFishProducts),
                                              sum(kids0Teen2$MntSweetProducts),
                                              sum(kids0Teen2$MntGoldProds)))
  
  arrange(by_kids_Prod02,desc(Quantidade))
  # Barras
  ggplot(by_kids_Prod02, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot02<-sum(c(sum(kids0Teen2$MntWines),
  sum(kids0Teen2$MntFruits),
  sum(kids0Teen2$MntMeatProducts),
  sum(kids0Teen2$MntFishProducts),
  sum(kids0Teen2$MntSweetProducts),
  sum(kids0Teen2$MntGoldProds)))
  Tot02

#### 1 criança e 2 adoslescentes ####
kids1Teen2<-filter(dados, Kidhome==1&Teenhome==2)
  length(kids1Teen2$ID)# 22
  kids1Teen2_Cmp1<- filter(dados, Kidhome==1&Teenhome==2&AcceptedCmp1==1)
  length(kids1Teen2_Cmp1$ID)#1

  kids1Teen2_Cmp2<-filter(dados, Kidhome==1&Teenhome==2&AcceptedCmp2==1)
  length(kids1Teen2_Cmp2$ID)# 0

  kids1Teen2_Cmp3<-filter(dados, Kidhome==1&Teenhome==2&AcceptedCmp3==1)
  length(kids1Teen2_Cmp3$ID)#1

  kids1Teen2_Cmp4<-filter(dados, Kidhome==1&Teenhome==2&AcceptedCmp4==1)
  length(kids1Teen2_Cmp4$ID)#3

  kids1Teen2_Cmp5<-filter(dados, Kidhome==1&Teenhome==2&AcceptedCmp5==1)
  length(kids1Teen2_Cmp5$ID)#1
  
  kids1Teen2_Resp<-filter(dados, Kidhome==1&Teenhome==2&Response==1)
  length(kids1Teen2_Resp$ID)# 2
  
  # banco de dados
  by_kids12_Cmp<- data.frame(KidsTeen = c("k1T2_C1", "k1T2_C2", "k1T2_C3","k1T2_C4","k1T2_C5","k1T2_L"),
                             Quantidade = c(length(kids1Teen2_Cmp1$ID), length(kids1Teen2_Cmp2$ID), length(kids1Teen2_Cmp3$ID),length(kids1Teen2_Cmp4$ID),length(kids1Teen2_Cmp5$ID),length(kids1Teen2_Resp$ID)))
  arrange(by_kids12_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids12_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)

  ## Consumos
  sum(kids1Teen2$MntWines)#6.534
  sum(kids1Teen2$MntFruits)#290
  sum(kids1Teen2$MntMeatProducts)#2.484
  sum(kids1Teen2$MntFishProducts)#232
  sum(kids1Teen2$MntSweetProducts)#262
  sum(kids1Teen2$MntGoldProds)#626
  by_kids_Prod12<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids1Teen2$MntWines),
                                              sum(kids1Teen2$MntFruits),
                                              sum(kids1Teen2$MntMeatProducts),
                                              sum(kids1Teen2$MntFishProducts),
                                              sum(kids1Teen2$MntSweetProducts),
                                              sum(kids1Teen2$MntGoldProds)))
  arrange(by_kids_Prod12,desc(Quantidade))
  # Barras
  ggplot(by_kids_Prod12, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot12<-sum(c(sum(kids1Teen2$MntWines),
    sum(kids1Teen2$MntFruits),
    sum(kids1Teen2$MntMeatProducts),
    sum(kids1Teen2$MntFishProducts),
    sum(kids0Teen0$MntSweetProducts),
    sum(kids1Teen2$MntGoldProds)))
  
# 2 crianças e 0 adoslescentes
  kids2Teen0<-filter(dados, Kidhome==2&Teenhome==0)
  length(kids2Teen0$ID)# 17
  kids2Teen0_Cmp1<- filter(dados, Kidhome==2&Teenhome==0&AcceptedCmp1==1)
  length(kids2Teen0_Cmp1$ID)#0

  kids2Teen0_Cmp2<-filter(dados, Kidhome==2&Teenhome==0&AcceptedCmp2==1)
  length(kids2Teen0_Cmp2$ID)# 0

  kids2Teen0_Cmp3<-filter(dados, Kidhome==2&Teenhome==0&AcceptedCmp3==1)
  length(kids2Teen0_Cmp3$ID)#1

  kids2Teen0_Cmp4<-filter(dados, Kidhome==2&Teenhome==0&AcceptedCmp4==1)
  length(kids2Teen0_Cmp4$ID)#0

  kids2Teen0_Cmp5<-filter(dados, Kidhome==2&Teenhome==0&AcceptedCmp5==1)
  length(kids2Teen0_Cmp5$ID)#0
  
  kids2Teen0_Resp<-filter(dados, Kidhome==2&Teenhome==0&Response==1)
  length(kids2Teen0_Resp$ID)# 2
  
  # banco de dados
  by_kids20_Cmp<- data.frame(KidsTeen = c("k2T0_C1", "k2T0_C2", "k2T0_C3","k2T0_C4","k2T0_C5","k2T0_L"),
                             Quantidade = c(length(kids2Teen0_Cmp1$ID), length(kids2Teen0_Cmp2$ID), length(kids2Teen0_Cmp3$ID),length(kids2Teen0_Cmp4$ID),length(kids2Teen0_Cmp5$ID),length(kids2Teen0_Resp$ID)))
  arrange(by_kids20_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids20_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  ## Consumos
  sum(kids2Teen0$MntWines)#1.041
  sum(kids2Teen0$MntFruits)# 249
  sum(kids2Teen0$MntMeatProducts)# 715
  sum(kids2Teen0$MntFishProducts)# 228
  sum(kids2Teen0$MntSweetProducts)# 137
  sum(kids2Teen0$MntGoldProds)# 476
  by_kids_Prod20<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids2Teen0$MntWines),
                                              sum(kids2Teen0$MntFruits),
                                              sum(kids2Teen0$MntMeatProducts),
                                              sum(kids2Teen0$MntFishProducts),
                                              sum(kids2Teen0$MntSweetProducts),
                                              sum(kids2Teen0$MntGoldProds)))
  arrange(by_kids_Prod20,desc(Quantidade))
  
  # Barras
  ggplot(by_kids_Prod20, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot20<-sum(c(sum(kids2Teen0$MntWines),
  sum(kids2Teen0$MntFruits),
  sum(kids2Teen0$MntMeatProducts),
  sum(kids2Teen0$MntFishProducts),
  sum(kids2Teen0$MntSweetProducts),
  sum(kids2Teen0$MntGoldProds)))
  
  
# 2 crianças e 1 adoslescente
  kids2Teen1<-filter(dados, Kidhome==2&Teenhome==1)
  length( kids2Teen1$ID)# 31
  kids2Teen1_Cmp1<- filter(dados, Kidhome==2&Teenhome==1&AcceptedCmp1==1)
  length(kids2Teen1_Cmp1$ID)#2

  kids2Teen1_Cmp2<-filter(dados, Kidhome==2&Teenhome==1&AcceptedCmp2==1)
  length(kids2Teen1_Cmp2$ID)# 0

  kids2Teen1_Cmp3<-filter(dados, Kidhome==2&Teenhome==1&AcceptedCmp3==1)
  length(kids2Teen1_Cmp3$ID)#0

  kids2Teen1_Cmp4<-filter(dados, Kidhome==2&Teenhome==1&AcceptedCmp4==1)
  length(kids2Teen1_Cmp4$ID)#0

  kids2Teen1_Cmp5<-filter(dados, Kidhome==2&Teenhome==1&AcceptedCmp5==1)
  length(kids2Teen1_Cmp5$ID)#0
  
  kids2Teen1_Resp<-filter(dados, Kidhome==2&Teenhome==1&Response==1)
  length(kids2Teen1_Resp$ID)# 0
  
  # banco de dados
  by_kids21_Cmp<- data.frame(KidsTeen = c("k2T1_C1", "k2T1_C2", "k2T1_C3","k2T1_C4","k2T1_C5","k2T1_L"),
                             Quantidade = c(length(kids2Teen1_Cmp1$ID), length(kids2Teen1_Cmp2$ID), length(kids2Teen1_Cmp3$ID),length(kids2Teen1_Cmp4$ID),length(kids2Teen1_Cmp5$ID),length(kids2Teen1_Resp$ID)))
  arrange(by_kids21_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids21_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  ## Consumos
  sum(kids2Teen1$MntWines)# 2.549
  sum(kids2Teen1$MntFruits)# 76
  sum(kids2Teen1$MntMeatProducts)# 909
  sum(kids2Teen1$MntFishProducts)# 143
  sum(kids2Teen1$MntSweetProducts)# 89
  sum(kids2Teen1$MntGoldProds)# 360
  by_kids_Prod21<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids2Teen1$MntWines),
                                              sum(kids2Teen1$MntFruits),
                                              sum(kids2Teen1$MntMeatProducts),
                                              sum(kids2Teen1$MntFishProducts),
                                              sum(kids2Teen1$MntSweetProducts),
                                              sum(kids2Teen1$MntGoldProds)))
  arrange(by_kids_Prod21,desc(Quantidade))
  # Barras
  ggplot(by_kids_Prod21, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  # consumo total
  Tot21<-sum(c(sum(kids2Teen1$MntWines),
               sum(kids2Teen1$MntFruits),
               sum(kids2Teen1$MntMeatProducts),
               sum(kids2Teen1$MntFishProducts),
               sum(kids2Teen1$MntSweetProducts),
               sum(kids2Teen1$MntGoldProds)))
  
#### 2 crianças e 2 adoslescentes ####
kids2Teen2<-filter(dados, Kidhome==2&Teenhome==2)
  length(kids2Teen2$ID)# 
  kids2Teen2_Cmp1<- filter(dados, Kidhome==2&Teenhome==2&AcceptedCmp1==1)
  length(kids2Teen2_Cmp1$ID)#

  kids2Teen2_Cmp2<-filter(dados, Kidhome==2&Teenhome==2&AcceptedCmp2==1)
  length(kids2Teen2_Cmp2$ID)# 

  kids2Teen2_Cmp3<-filter(dados, Kidhome==2&Teenhome==2&AcceptedCmp3==1)
  length(kids2Teen2_Cmp3$ID)#

  kids2Teen2_Cmp4<-filter(dados, Kidhome==2&Teenhome==2&AcceptedCmp4==1)
  length(kids2Teen2_Cmp4$ID)#

  kids2Teen2_Cmp5<-filter(dados, Kidhome==2&Teenhome==2&AcceptedCmp5==1)
  length(kids2Teen2_Cmp5$ID)#
  
  kids2Teen2_Resp<-filter(dados, Kidhome==2&Teenhome==2&Response==1)
  length(kids2Teen2_Resp$ID)# 427
  #### NÃO HA RESIDENCIAS COM 2 CRIANÇAS E DOIS ADOLESCENTES.
  
  # banco de dados
  by_kids22_Cmp<- data.frame(KidsTeen = c("k2T2_C1", "k2T2_C2", "k2T2_C3","k2T2_C4","k2T2_C5","k2T2_L"),
                             Quantidade = c(length(kids2Teen2_Cmp1$ID), length(kids2Teen2_Cmp2$ID), length(kids2Teen2_Cmp3$ID),length(kids2Teen2_Cmp4$ID),length(kids2Teen2_Cmp5$ID),length(kids2Teen2_Resp$ID)))
  arrange(by_kids22_Cmp,desc(Quantidade))
  # Barras
  ggplot(by_kids22_Cmp, aes(y = Quantidade, x = KidsTeen,fill = KidsTeen)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
  
  ## Consumo
  sum(kids2Teen2$MntWines)# 22411
  sum(kids2Teen2$MntFruits)# 1236
  sum(kids2Teen2$MntMeatProducts)# 8112
  sum(kids2Teen2$MntFishProducts)#1614
  sum(kids2Teen2$MntSweetProducts)#1062
  sum(kids2Teen2$MntGoldProds)# 3173
  by_kids_Prod22<- data.frame(Produtos = c("Wines", "Fruits", "Meat","Fish","Sweet","Gold"),
                              Quantidade =  c(sum(kids2Teen2$MntWines),
                                              sum(kids2Teen2$MntFruits),
                                              sum(kids2Teen2$MntMeatProducts),
                                              sum(kids2Teen2$MntFishProducts),
                                              sum(kids2Teen2$MntSweetProducts),
                                              sum(kids2Teen2$MntGoldProds)))
  
  arrange(by_kids_Prod22,desc(Quantidade))
  # Barras
  ggplot(by_kids_Prod22, aes(y = Quantidade, x = Produtos,fill = Produtos)) +
    geom_bar(stat = "identity")
  ####  consumo 
  Tot22<-sum(c(sum(kids2Teen2$MntWines),
        sum(kids2Teen2$MntFruits),
        sum(kids2Teen2$MntMeatProducts),
        sum(kids2Teen2$MntFishProducts),
        sum(kids2Teen2$MntSweetProducts),
        sum(kids2Teen2$MntGoldProds)))

  # de um modo geral o consumo vai diminuindo de acordo com o número de crianças
  ### Consumo total #####
  by_kids<- data.frame(KidsTenn = c("k0T0", "k0T1","k0T2","k1T0",
                                    "k1T1","k1T2", "k2T0", "k2T1"),
                              Gastos =  c(Tot00,Tot01,Tot02,Tot10,Tot11,Tot12,Tot20,Tot21))
  arrange(by_kids,desc(Gastos))
  # Barras
  ggplot(by_kids, aes(y = Gastos, x = KidsTenn,fill = KidsTenn)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Gastos,label=Gastos), vjust = -0.2)
 
###############################################################
## Vamos analisar agora consumos corforme nível de escolaridade  

    ### Consumo de vinho segundo a educação ##
    ggplot(data = dados, mapping = aes(x = Education, y = MntWines)) +
    geom_boxplot()
    # Consumo de frutas segundo a educação
    ggplot(data = dados, mapping = aes(x = Education, y = MntFruits)) +
    geom_boxplot()
    # Consumo de carne segundo a educação
    ggplot(data = dados, mapping = aes(x = Education, y = MntMeatProducts)) +
    geom_boxplot()
    # Consumo de peixe segundo a educação
    ggplot(data = dados, mapping = aes(x = Education, y = MntFishProducts)) +
    geom_boxplot()
    # Consumo de Sweet segundo a educação
    ggplot(data = dados, mapping = aes(x = Education, y = MntSweetProducts)) +
    geom_boxplot()
    # Consumo de Gold segundo a educação
    ggplot(data = dados, mapping = aes(x = Education, y = MntGoldProds)) +
    geom_boxplot()

    
    ## Total de clientes por nível de escolaridade
    # Graduation
    dG <- subset(dados, dados[3]=="Graduation")
    length(dG$ID)#1127
    
    #PhD
    dPhd <- subset(dados, dados[3]=="PhD")
    length(dPhd$ID)#486
    # Master
    dM <- subset(dados, dados[3]=="Master")
    length(dM$ID)#370
    # 2n Cycle
    d2n <- subset(dados, dados[3]=="2n Cycle")
    length(d2n$ID)#
    #Basic
    dB <- subset(dados, dados[3]=="Basic")
    length(dB$ID)#
    
    by_Education<- data.frame(Educacao = c("Graduation", "PhD", "Master","2n Cycle","Basic"),
                              Quantidade = c(length(dG$ID),length(dPhd$ID), length(dM$ID), length(d2n$ID),length(dB$ID)))
    arrange(by_Education,desc(Quantidade))
    # Barras
    ggplot(by_Education, aes(y = Quantidade, x = Educacao,fill = Educacao)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    #####  Gastos com produtos por nível de escolaridade ######
    
    ## MntWines ##
    as.factor(dados$Education)

    gastos_classificacaoW <- dados %>%
      group_by(Education) %>%
      summarise(gastos_wine = sum(MntWines, na.rm = TRUE)) 

      gastos_classificacaoW%>%
      select(Education,gastos_wine)%>%
      arrange(desc(gastos_wine)) %>% 
      knitr::kable(col.names = c("Escolaridade", "Gastos com Wines"))
      
      # barra
      ggplot(gastos_classificacaoW, aes(y = gastos_wine, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_wine,label=gastos_wine), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Wine")

      ## MntFruits ##
      gastos_classificacaoF <- dados %>%
      group_by(Education) %>%
      summarise(gastos_Fruit = sum(MntFruits, na.rm = TRUE)) 

      gastos_classificacaoF%>%
      select(Education,gastos_Fruit)%>%
      arrange(desc(gastos_Fruit)) %>% 
      knitr::kable(col.names = c("Escolaridade", "Gastos com Fruit"))
      
      # barra
      ggplot(gastos_classificacaoF, aes(y = gastos_Fruit, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_Fruit,label=gastos_Fruit), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Fruit")
    
      ## MntMeatProducts
      gastos_classificacaoM <- dados %>%
      group_by(Education) %>%
      summarise(gastos_Meat = sum(MntMeatProducts, na.rm = TRUE)) 

      gastos_classificacaoM%>%
      select(Education,gastos_Meat)%>%
      arrange(desc(gastos_Meat)) %>% 
       knitr::kable(col.names = c("Escolaridade", "Gastos com Meat"))
      
      # barra
      ggplot(gastos_classificacaoM, aes(y = gastos_Meat, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_Meat,label=gastos_Meat), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Meat")
      
      
      ## MntFishProducts
      gastos_classificacaoFis <- dados %>%
      group_by(Education) %>%
      summarise(gastos_Fish = sum(MntFishProducts, na.rm = TRUE)) 

      gastos_classificacaoFis%>%
      select(Education,gastos_Fish)%>%
      arrange(desc(gastos_Fish)) %>% 
      knitr::kable(col.names = c("Escolaridade", "Gastos com Fish"))
      # barra
      ggplot(gastos_classificacaoFis, aes(y = gastos_Fish, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_Fish,label=gastos_Fish), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Fish")
      
      ## MntSweetProducts
      gastos_classificacaoS <- dados %>%
      group_by(Education) %>%
      summarise(gastos_Sweet = sum(MntSweetProducts, na.rm = TRUE)) 

      gastos_classificacaoS%>%
      select(Education,gastos_Sweet)%>%
      arrange(desc(gastos_Sweet)) %>% 
      knitr::kable(col.names = c("Escolaridade", "Gastos com Sweet"))
      
      # barra
      ggplot(gastos_classificacaoS, aes(y = gastos_Sweet, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_Sweet,label=gastos_Sweet), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Sweet")

      ## MntGoldProds
      gastos_classificacaoG <- dados %>%
      group_by(Education) %>%
      summarise(gastos_Gold = sum(MntGoldProds, na.rm = TRUE)) 

      gastos_classificacaoG%>%
      select(Education,gastos_Gold)%>%
      arrange(desc(gastos_Gold)) %>% 
      knitr::kable(col.names = c("Escolaridade", "Gastos com Gold"))

      # barra
      ggplot(gastos_classificacaoG, aes(y = gastos_Gold, x = Education,fill = Education)) +
        geom_bar(position = position_dodge(1),stat = "identity")+
        geom_text(aes(y=gastos_Gold,label=gastos_Gold), vjust = -0.2)+
        xlab("Nível de Escolaridade")  +
        ylab("Gastos com  Gold")
      ## Tentar alcançar clientes com menores escolaridades Basic e 2n ciclo
      ## Gasto geral por nível de escolaridade
      by_EdUC_cTOTAL<- data.frame(Educacao = c("Graduation", "PhD", "Master","2n Cycle","Basic"),
                                Quantidade = c(sum(c(dG$MntWines,dG$MntMeatProducts,dG$MntFruits,dG$MntFishProducts,dG$MntSweetProducts,dG$MntGoldProds)),
                                               sum(c(dPhd$MntWines,dPhd$MntMeatProducts,dPhd$MntFruits,dPhd$MntFishProducts,dPhd$MntSweetProducts,dPhd$MntGoldProds)),
                                               sum(c(dM$MntWines,dM$MntMeatProducts,dM$MntFruits,dM$MntFishProducts,dM$MntSweetProducts,dM$MntGoldProds)),
                                               sum(c(d2n$MntWines,d2n$MntMeatProducts,d2n$MntFruits,d2n$MntFishProducts,d2n$MntSweetProducts,d2n$MntGoldProds)),
                                               sum(c(dB$MntWines,dB$MntMeatProducts,dB$MntFruits,dB$MntFishProducts,dB$MntSweetProducts,dB$MntGoldProds))))
      arrange(by_EdUC_cTOTAL,desc(Quantidade))
      
      
      ##################################################################
      ## Por faixa de Escolaridade que teve mais aceitação nas campanhas
      
      # Graduation
      dG <- subset(dados, dados$Education=="Graduation")
      length(dG$ID)#1127 quantidade de pessoas com Graduação
      ### campanha 1
      dG_C1<-subset(dG, dG$AcceptedCmp1==1)
      length(dG_C1$ID)# 82 Aceitação 
      dG_C1N<-subset(dG, dG$AcceptedCmp1==0)
      length(dG_C1N$ID)# 1045 Não aceitação
      
      ### campanha 2
      dG_C2<-subset(dG, dG[25]==1)
      length(dG_C2$ID)# 16 Aceitação 
      dG_C2N<-subset(dG, dG[25]==0)
      length(dG_C2N$ID)# 368 Não aceitação
      ### campanha 3
      dG_C3<-subset(dG, dG[21]==1)
      length(dG_C3$ID)# 78 Aceitação 
      dG_C3N<-subset(dG, dG[21]==0)
      length(dG_C3N$ID)# 346 Não aceitação
      ### campanha 4
      dG_C4<-subset(dG, dG[22]==1)
      length(dG_C4$ID)# 31 Aceitação 
      dG_C4N<-subset(dG, dG[22]==0)
      length(dG_C4N$ID)# 339 Não aceitação
      ### campanha 5
      dG_C5<-subset(dG, dG[23]==1)
      length(dG_C5$ID)# 28 Aceitação 
      dG_C5N<-subset(dG, dG[23]==0)
      length(dG_C5N$ID)# 342 Não aceitação
      ### campanha last 
      dG_CL<-subset(dG, dG[29]==1)
      length(dG_CL$ID)# 57 Aceitação 
      dG_CLN<-subset(dG, dG[29]==0)
      length(dG_CLN$ID)# 313 Não aceitação
      
    
      ### PhD ######
      dPhd <- subset(dados, dados[3]=="PhD")
      length(dPhd$ID)#486
      ### campanha 1
      Phd_C1<-subset(dPhd, dPhd[24]==1)
      length(Phd_C1$ID)# 30 Aceitação 
      Phd_C1N<-subset(dPhd, dPhd[24]==0)
      length(Phd_C1N$ID)# 456 Não aceitação
      ### campanha 2
      Phd_C2<-subset(dPhd, dPhd[25]==1)
      length(Phd_C2$ID)# 10 Aceitação 
      Phd_C2N<-subset(dPhd, dPhd[25]==0)
      length(Phd_C2N$ID)# 476 Não aceitação
      ### campanha 3
      Phd_C3<-subset(dPhd, dPhd[21]==1)
      length(Phd_C3$ID)# 40 Aceitação 
      Phd_C3N<-subset(dPhd, dPhd[21]==0)
      length(Phd_C3N$ID)# 446 Não aceitação
      ### campanha 4
      Phd_C4<-subset(dPhd, dPhd[22]==1)
      length(Phd_C4$ID)# 45 Aceitação 
      Phd_C4N<-subset(dPhd, dPhd[22]==0)
      length(Phd_C4N$ID)# 441 Não aceitação
      ### campanha 5
      Phd_C5<-subset(dPhd, dPhd[23]==1)
      length(Phd_C5$ID)# 39 Aceitação 
      Phd_C5N<-subset(dPhd, dPhd[23]==0)
      length(Phd_C5N$ID)# 447 Não aceitação
      ### campanha last 
      Phd_CL<-subset(dPhd, dPhd[29]==1)
      length(Phd_CL$ID)# 101 Aceitação 
      Phd_CLN<-subset(dPhd, dPhd[29]==0)
      length(Phd_CLN$ID)# 385 Não aceitação
      
      
      ### Master #### 
      dM <- subset(dados, dados[3]=="Master")
      length(dM$ID)#370 quantidade de pessoas com mestrado
      ### campanha 1
      dM_C1<-subset(dM, dM[24]==1)
      length(dM_C1$ID)# 18 Aceitação 
      dM_C1N<-subset(dM, dM[24]==0)
      length(dM_C1N$ID)# 352 Não aceitação
      ### campanha 2
      dM_C2<-subset(dM, dM[25]==1)
      length(dM_C2$ID)# 2 Aceitação 
      dM_C2N<-subset(dM, dM[25]==0)
      length(dM_C2N$ID)# 368 Não aceitação
      ### campanha 3
      dM_C3<-subset(dM, dM[21]==1)
      length(dM_C3$ID)# 24 Aceitação 
      dM_C3N<-subset(dM, dM[21]==0)
      length(dM_C3N$ID)# 346 Não aceitação
      ### campanha 4
      dM_C4<-subset(dM, dM[22]==1)
      length(dM_C4$ID)# 31 Aceitação 
      dM_C4N<-subset(dM, dM[22]==0)
      length(dM_C4N$ID)# 339 Não aceitação
      ### campanha 5
      dM_C5<-subset(dM, dM[23]==1)
      length(dM_C5$ID)# 28 Aceitação 
      dM_C5N<-subset(dM, dM[23]==0)
      length(dM_C5N$ID)# 342 Não aceitação
      ### campanha last 
      dM_CL<-subset(dM, dM[29]==1)
      length(dM_CL$ID)# 57 Aceitação 
      dM_CLN<-subset(dM, dM[29]==0)
      length(dM_CLN$ID)# 313 Não aceitação
      
      
      ### 2n Cycle ###
      d2n <- subset(dados, dados[3]=="2n Cycle")
      length(d2n$ID)# 203
      ### campanha 1
      d2n_C1<-subset(d2n, d2n[24]==1)
      length(d2n_C1$ID)# 14 Aceitação 
      d2n_C1N<-subset(d2n, d2n[24]==0)
      length(d2n_C1N$ID)# 189 Não aceitação
      ### campanha 2
      d2n_C2<-subset(d2n, d2n[25]==1)
      length(d2n_C2$ID)# 2 Aceitação 
      d2n_C2N<-subset(d2n, d2n[25]==0)
      length(d2n_C2N$ID)# 201 Não aceitação
      ### campanha 3
      d2n_C3<-subset(d2n, d2n[21]==1)
      length(d2n_C3$ID)# 15 Aceitação 
      d2n_C3N<-subset(d2n, d2n[21]==0)
      length(d2n_C3N$ID)# 188 Não aceitação
      ### campanha 4
      d2n_C4<-subset(d2n, d2n[22]==1)
      length(d2n_C4$ID)# 10 Aceitação 
      d2n_C4N<-subset(d2n, d2n[22]==0)
      length(d2n_C4N$ID)# 193 Não aceitação
      ### campanha 5
      d2n_C5<-subset(d2n, d2n[23]==1)
      length(d2n_C5$ID)# 10 Aceitação 
      d2n_C5N<-subset(d2n, d2n[23]==0)
      length(d2n_C5N$ID)# 193Não aceitação
      ### campanha last 
      d2n_CL<-subset(d2n, d2n[29]==1)
      length(d2n_CL$ID)# 22 Aceitação 
      d2n_CLN<-subset(d2n, d2n[29]==0)
      length(d2n_CLN$ID)# 181 Não aceitação
      
      
      #### Basic #### 
      dB <- subset(dados, dados[3]=="Basic")
      length(dB$ID)# 54 total de pessoas de nível básico
      ### campanha 1
      dB_C1<-subset(dB, dB[24]==1)
      length(dB_C1$ID)# 0 Aceitação 
      dB_C1N<-subset(dB, dB[24]==0)
      length(dB_C1N$ID)# 54 Não aceitação
      ### campanha 2
      dB_C2<-subset(dB, dB[25]==1)
      length(dB_C2$ID)# 0 Aceitação 
      dB_C2N<-subset(dB, dB[25]==0)
      length(dB_C2N$ID)# 54 Não aceitação
      ### campanha 3
      dB_C3<-subset(dB, dB[21]==1)
      length(dB_C3$ID)# 6 Aceitação 
      dB_C3N<-subset(dB, dB[21]==0)
      length(dB_C3N$ID)# 48 Não aceitação
      ### campanha 4
      dB_C4<-subset(dB, dB[22]==1)
      length(dB_C4$ID)# 0 Aceitação 
      dB_C4N<-subset(dB, dB[22]==0)
      length(dB_C4N$ID)# 54 Não aceitação
      ### campanha 5
      dB_C5<-subset(dB, dB[23]==1)
      length(dB_C5$ID)# 0 Aceitação 
      dB_C5N<-subset(dB, dB[23]==0)
      length(dB_C5N$ID)# 54 Não aceitação
      ### campanha last 
      dB_CL<-subset(dB, dB[29]==1)
      length(dB_CL$ID)# 57 Aceitação 
      dB_CLN<-subset(dB, dB[29]==0)
      length(dB_CLN$ID)# 313 Não aceitação
      
  ############################
  ## Consumo por idade #######
  ############################
      

    ggplot(dados, aes(Year_Birth)) +
    geom_histogram(binwidth = 5, color = "black")+
    scale_x_continuous(breaks = seq(1893, 1996, 10)) +
    stat_bin(aes(y = ..count.., label = ..count..),
           geom = "text", binwidth = 5, vjust = -.5)
    summary(dados$Year_Birth) # Pode haver um problema com a idade. Investigar se foi erro de ditação

    #vamos criar a variável idade para ficar mais claro
    idade<- 2022-dados$Year_Birth
    dados[30]<-idade
    colnames(dados)[30] <- "Year"
    names(dados) # verificando se foi acrescentado a coluna como nome Year
    summary(dados$Year) # as idades vao de 29 a 129 anos. Há um erro. Investigar!!

    ggplot(dados, aes(x=Year)) +
    geom_histogram(binwidth = 10, color = "blue",fill="lightblue")+
    scale_x_continuous(breaks = seq(25, 130, 5)) +
    stat_bin(aes(y = ..count.., label = ..count..),
           geom = "text", binwidth = 10, vjust = -.5)+
      xlab("Idade")  +
      ylab("Quantidade")
    
    dados<- dados[-c(240,193,340),]# removendo as linhas com as idades 122,123,129
    length(dados$ID) # verificando se retirei as três linhas
    idade<- 2022-dados$Year_Birth
    dados[30]<-idade
    colnames(dados)[30] <- "Year"
    names(dados) # verificando se foi acrescentado a coluna como nome Year
    summary(dados$Year) # as idades vao de 29 a 82
    ## podemos ver que a maioria dos clientes estão entre as idades 35 a 65
    ## campanha para atingir os mais jovens de 25 a 35

    
    dados$Year[dados$Year>25&dados$Year<=35]# aqui só para pegar as idades e contar
    #### avaliar o consumo por faixa de idade
    Y25_35<-dados[dados$Year>25&dados$Year<=35,]#147
    Y35_45<-dados[dados$Year>35&dados$Year<=45,]#459
    Y45_55<-dados[dados$Year>45&dados$Year<=55,]#724
    Y55_65<-dados[dados$Year>55&dados$Year<=65,]#487
    Y65_75<-dados[dados$Year>65&dados$Year<=75,]#380
    Y75_129<-dados[dados$Year>75&dados$Year<=130,]#40

    # Y25_35 Consumo
    sum(Y25_35$MntWines)
    sum(Y25_35$MntFruits)
    sum(Y25_35$MntMeatProducts)
    sum(Y25_35$MntFishProducts)
    sum(Y25_35$MntSweetProducts)
    sum(Y25_35$MntGoldProds)


    by_Y25_35<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                     Gastos =  c(sum(Y25_35$MntWines),
                     sum(Y25_35$MntFruits),
                     sum(Y25_35$MntMeatProducts),
                     sum(Y25_35$MntFishProducts),
                     sum(Y25_35$MntSweetProducts),
                     sum(Y25_35$MntGoldProds)))
    arrange(by_Y25_35,desc(Gastos))
    sum(by_Y25_35$Gastos)

    # Y35_45 Consumo
    sum(Y35_45$MntWines)
    sum(Y35_45$MntFruits)
    sum(Y35_45$MntMeatProducts)
    sum(Y35_45$MntFishProducts)
    sum(Y35_45$MntSweetProducts)
    sum(Y35_45$MntGoldProds)


    by_Y35_Y45<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                       Gastos =  c(sum(Y35_45$MntWines),
                                   sum(Y35_45$MntFruits),
                                   sum(Y35_45$MntMeatProducts),
                                   sum(Y35_45$MntFishProducts),
                                   sum(Y35_45$MntSweetProducts),
                                   sum(Y35_45$MntGoldProds)))
    arrange(by_Y35_Y45,desc(Gastos))
    sum(by_Y35_Y45$Gastos)

    # Y45_55 Consumo
    sum(Y45_55$MntWines)
    sum(Y45_55$MntFruits)
    sum(Y45_55$MntMeatProducts)
    sum(Y45_55$MntFishProducts)
    sum(Y45_55$MntSweetProducts)
    sum(Y45_55$MntGoldProds)


    by_Y45_Y55<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                        Gastos =  c(sum(Y45_55$MntWines),
                                    sum(Y45_55$MntFruits),
                                    sum(Y45_55$MntMeatProducts),
                                    sum(Y45_55$MntFishProducts),
                                    sum(Y45_55$MntSweetProducts),
                                    sum(Y45_55$MntGoldProds)))
    arrange(by_Y45_Y55,desc(Gastos))
    sum(by_Y45_Y55$Gastos)

    # Y55_65 Consumo   Wine
    sum(Y55_65$MntWines)
    sum(Y55_65$MntFruits)
    sum(Y55_65$MntMeatProducts)
    sum(Y55_65$MntFishProducts)
    sum(Y55_65$MntSweetProducts)
    sum(Y55_65$MntGoldProds)


    by_Y55_Y65<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                        Gastos =  c(sum(Y55_65$MntWines),
                                    sum(Y55_65$MntFruits),
                                    sum(Y55_65$MntMeatProducts),
                                    sum(Y55_65$MntFishProducts),
                                    sum(Y55_65$MntSweetProducts),
                                    sum(Y55_65$MntGoldProds)))
    arrange(by_Y55_Y65,desc(Gastos))
    sum(by_Y55_Y65$Gastos)

    # Y65_75 Consumo   Wine
    sum(Y65_75$MntWines)
    sum(Y65_75$MntFruits)
    sum(Y65_75$MntMeatProducts)
    sum(Y65_75$MntFishProducts)
    sum(Y65_75$MntSweetProducts)
    sum(Y65_75$MntGoldProds)


    by_Y65_Y75<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                        Gastos =  c(sum(Y65_75$MntWines),
                                    sum(Y65_75$MntFruits),
                                    sum(Y65_75$MntMeatProducts),
                                    sum(Y65_75$MntFishProducts),
                                    sum(Y65_75$MntSweetProducts),
                                    sum(Y65_75$MntGoldProds)))
    arrange(by_Y65_Y75,desc(Gastos))
    sum(by_Y65_Y75$Gastos)

    # Barras
    ggplot(by_Y65_Y75, aes(y = Gastos, x = Produtos,fill = Produtos)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Gastos,label=Gastos), vjust = -0.2)

    # Y75_129 Consumo   Wine
    sum(Y75_129$MntWines)
    sum(Y75_129$MntFruits)
    sum(Y75_129$MntMeatProducts)
    sum(Y75_129$MntFishProducts)
    sum(Y75_129$MntSweetProducts)
    sum(Y75_129$MntGoldProds)


    by_Y75_129<- data.frame(Produtos = c("Wines", "Fruits","Meat","Fish","Sweet","Gold"),
                        Gastos =  c(sum(Y75_129$MntWines),
                                    sum(Y75_129$MntFruits),
                                    sum(Y75_129$MntMeatProducts),
                                    sum(Y75_129$MntFishProducts),
                                    sum(Y75_129$MntSweetProducts),
                                    sum(Y75_129$MntGoldProds)))
    arrange(by_Y75_129,desc(Gastos))
    sum(by_Y75_129$Gastos)

    # barras consumo total por grupo
    by_grupoCons<-data.frame(Grupos= c("25-35","35-45","45-55","55-65","65-75","75+"),
                         Consumo= c(sum(by_Y25_35$Gastos),
                                    sum(by_Y35_Y45$Gastos),
                                    sum(by_Y45_Y55$Gastos),
                                    sum(by_Y55_Y65$Gastos),
                                    sum(by_Y65_Y75$Gastos),
                                    sum(Y75_129$MntGoldProds)))

    arrange(by_grupoCons,desc(Consumo))
    
    # Barras
    ggplot(by_grupoCons, aes(y = Consumo, x = Grupos,fill = Grupos)) +
    geom_bar(position = position_dodge(1),stat = "identity")+
    geom_text(aes(y=Consumo,label=Consumo), vjust = -0.2)

    ### FAIXA ETÁRIA E PURCHASE (COMPRAS) - MEIOS DE COMPRAS
    ## 25-35
    by_Purc_25_35<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
      Quantidade= c(sum(Y25_35$NumDealsPurchases),
                    sum(Y25_35$NumCatalogPurchases),
                    sum(Y25_35$NumStorePurchases),
                    sum(Y25_35$NumWebPurchases)))

    arrange(by_Purc_25_35,desc(Quantidade))
    # Barras
    ggplot(by_Purc_25_35, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    
    ## 35-45
    by_Purc_35_45<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
                                Quantidade= c(sum(Y35_45$NumDealsPurchases),
                                              sum(Y35_45$NumCatalogPurchases),
                                              sum(Y35_45$NumStorePurchases),
                                              sum(Y35_45$NumWebPurchases)))
    
    arrange(by_Purc_35_45,desc(Quantidade))
    # Barras
    ggplot(by_Purc_35_45, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ## 45-55
    by_Purc_45_55<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
                                Quantidade= c(sum(Y45_55$NumDealsPurchases),
                                              sum(Y45_55$NumCatalogPurchases),
                                              sum(Y45_55$NumStorePurchases),
                                              sum(Y45_55$NumWebPurchases)))
    
    arrange(by_Purc_45_55,desc(Quantidade))
    # Barras
    ggplot(by_Purc_45_55, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ## 55-65
    by_Purc_55_65<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
                                Quantidade= c(sum(Y55_65$NumDealsPurchases),
                                              sum(Y55_65$NumCatalogPurchases),
                                              sum(Y55_65$NumStorePurchases),
                                              sum(Y55_65$NumWebPurchases)))
    
    arrange(by_Purc_55_65,desc(Quantidade))
    # Barras
    ggplot(by_Purc_55_65, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ## 65-75
    by_Purc_65_75<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
                                Quantidade= c(sum(Y65_75$NumDealsPurchases),
                                              sum(Y65_75$NumCatalogPurchases),
                                              sum(Y65_75$NumStorePurchases),
                                              sum(Y65_75$NumWebPurchases)))
    
    arrange(by_Purc_65_75,desc(Quantidade))
    # Barras
    ggplot(by_Purc_65_75, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ## 75+
    by_Purc_75_129<- data.frame( Compras=c("Deals","Catalog","Store","WebPurc"),
                                Quantidade= c(sum(Y75_129$NumDealsPurchases),
                                              sum(Y75_129$NumCatalogPurchases),
                                              sum(Y75_129$NumStorePurchases),
                                              sum(Y75_129$NumWebPurchases)))
    
    arrange(by_Purc_75_129,desc(Quantidade))
    # Barras
    ggplot(by_Purc_65_75, aes(y = Quantidade, x = Compras,fill = Compras)) +
      geom_bar(position = position_dodge(1),stat = "identity")+
      geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ### FAIXA ETÁRIA E VISIT WEB MENSALMENTE
    by_VistasWeb<-data.frame(Idade= c("25-35","35-45","45-55","55-65","65-75","75+"),
                            Visitas=c(sum(Y25_35$NumWebVisitsMonth),
                            sum(Y35_45$NumWebVisitsMonth),
                            sum(Y45_55$NumWebVisitsMonth),
                            sum(Y55_65$NumWebVisitsMonth),
                            sum(Y65_75$NumWebVisitsMonth),
                            sum(Y75_129$NumWebVisitsMonth)))
    arrange(by_VistasWeb,desc(Visitas))
    # Barras
    ggplot(by_VistasWeb, aes(y = Visitas, x = Idade,fill = Idade)) +
     geom_bar(position = position_dodge(1),stat = "identity")+
     geom_text(aes(y=Visitas,label=Visitas), vjust = -0.2)

    ##########################################
    ### Aceitação da campanha por faixa etária
    
    ## 25 a 35 ##
    # campanha 1
    Y25_35_C1<-Y25_35[Y25_35$AcceptedCmp1==1,]
    length(Y25_35_C1$ID)#16
    Y25_35_C1N<-Y25_35[Y25_35$AcceptedCmp1==0,]
    length(Y25_35_C1N$ID)#131
    # campanha 2
    Y25_35_C2<-Y25_35[Y25_35$AcceptedCmp2==1,]
    length(Y25_35_C2$ID)#2
    Y25_35_C2N<-Y25_35[Y25_35$AcceptedCmp2==0,]
    length(Y25_35_C2N$ID)#145
    # campanha 3
    Y25_35_C3<-Y25_35[Y25_35$AcceptedCmp3==1,]
    length(Y25_35_C3$ID)#13
    Y25_35_C3N<-Y25_35[Y25_35$AcceptedCmp3==0,]
    length(Y25_35_C3N$ID)#134
    # campanha 4
    Y25_35_C4<-Y25_35[Y25_35$AcceptedCmp4==1,]
    length(Y25_35_C4$ID)#8
    Y25_35_C4N<-Y25_35[Y25_35$AcceptedCmp4==0,]
    length(Y25_35_C4N$ID)#139
    # campanha 5
    Y25_35_C5<-Y25_35[Y25_35$AcceptedCmp5==1,]
    length(Y25_35_C5$ID)#19
    Y25_35_C5N<-Y25_35[Y25_35$AcceptedCmp5==0,]
    length(Y25_35_C5N$ID)#128
    # campanha Last
    Y25_35_CL<-Y25_35[Y25_35$Response==1,]
    length(Y25_35_CL$ID)#23
    Y25_35_CLN<-Y25_35[Y25_35$Response==0,]
    length(Y25_35_CLN$ID)#124
    
    
    ## 35 a 45 ##
    # campanha 1
    Y35_45_C1<-Y35_45[Y35_45$AcceptedCmp1==1,]
    length(Y35_45_C1$ID)# 31
    Y35_45_C1N<-Y35_45[Y35_45$AcceptedCmp1==0,]
    length(Y35_45_C1N$ID)# 428
    # campanha 2
    Y35_45_C2<-Y35_45[Y35_45$AcceptedCmp2==1,]
    length(Y35_45_C2$ID)#4
    Y35_45_C2N<-Y35_45[Y35_45$AcceptedCmp2==0,]
    length(Y35_45_C2N$ID)#455
    # campanha 3
    Y35_45_C3<-Y35_45[Y35_45$AcceptedCmp3==1,]
    length(Y35_45_C3$ID)#46
    Y35_45_C3N<-Y35_45[Y35_45$AcceptedCmp3==0,]
    length(Y35_45_C3N$ID)#433
    # campanha 4
    Y35_45_C4<-Y35_45[Y35_45$AcceptedCmp4==1,]
    length(Y35_45_C4$ID)#26
    Y35_45_C4N<-Y35_45[Y35_45$AcceptedCmp4==0,]
    length(Y35_45_C4N$ID)#433
    # campanha 5
    Y35_45_C5<-Y35_45[Y35_45$AcceptedCmp5==1,]
    length(Y35_45_C5$ID)#37
    Y35_45_C5N<-Y35_45[Y35_45$AcceptedCmp5==0,]
    length(Y35_45_C5N$ID)#422
    # campanha Last
    Y35_45_CL<-Y35_45[Y35_45$Response==1,]
    length(Y35_45_CL$ID)#78
    Y35_45_CLN<-Y35_45[Y35_45$Response==0,]
    length(Y35_45_CLN$ID)#381
    
    
    ## 45 a 55 ##
    # campanha 1
    Y45_55_C1<-Y45_55[Y45_55$AcceptedCmp1==1,]
    length(Y45_55_C1$ID)# 32
    Y45_55_C1N<-Y45_55[Y45_55$AcceptedCmp1==0,]
    length(Y45_55_C1N$ID)# 692
    # campanha 2
    Y45_55_C2<-Y45_55[Y45_55$AcceptedCmp2==1,]
    length(Y45_55_C2$ID)#11
    Y45_55_C2N<-Y45_55[Y45_55$AcceptedCmp2==0,]
    length(Y45_55_C2N$ID)#713
    # campanha 3
    Y45_55_C3<-Y45_55[Y45_55$AcceptedCmp3==1,]
    length(Y45_55_C3$ID)#55
    Y45_55_C3N<-Y45_55[Y45_55$AcceptedCmp3==0,]
    length(Y45_55_C3N$ID)#669
    # campanha 4
    Y45_55_C4<-Y45_55[Y45_55$AcceptedCmp4==1,]
    length(Y45_55_C4$ID)#50
    Y45_55_C4N<-Y45_55[Y45_55$AcceptedCmp4==0,]
    length(Y45_55_C4N$ID)#674
    # campanha 5
    Y45_55_C5<-Y45_55[Y45_55$AcceptedCmp5==1,]
    length(Y45_55_C5$ID)#40
    Y45_55_C5N<-Y45_55[Y45_55$AcceptedCmp5==0,]
    length(Y45_55_C5N$ID)#684
    # campanha Last
    Y45_55_CL<-Y45_55[Y45_55$Response==1,]
    length(Y45_55_CL$ID)#107
    Y45_55_CLN<-Y45_55[Y45_55$Response==0,]
    length(Y45_55_CLN$ID)#617
    
    
    ## 55 a 65 ##
    # campanha 1
    Y55_65_C1<-Y55_65[Y55_65$AcceptedCmp1==1,]
    length(Y55_65_C1$ID)# 31
    Y55_65_C1N<-Y55_65[Y55_65$AcceptedCmp1==0,]
    length(Y55_65_C1N$ID)# 456
    # campanha 2
    Y55_65_C2<-Y55_65[Y55_65$AcceptedCmp2==1,]
    length(Y55_65_C2$ID)#8
    Y55_65_C2N<-Y55_65[Y55_65$AcceptedCmp2==0,]
    length(Y55_65_C2N$ID)#479
    # campanha 3
    Y55_65_C3<-Y55_65[Y55_65$AcceptedCmp3==1,]
    length(Y55_65_C3$ID)#23
    Y55_65_C3N<-Y55_65[Y55_65$AcceptedCmp3==0,]
    length(Y55_65_C3N$ID)#464
    # campanha 4
    Y55_65_C4<-Y55_65[Y55_65$AcceptedCmp4==1,]
    length(Y55_65_C4$ID)#45
    Y55_65_C4N<-Y55_65[Y55_65$AcceptedCmp4==0,]
    length(Y55_65_C4N$ID)#442
    # campanha 5
    Y55_65_C5<-Y55_65[Y55_65$AcceptedCmp5==1,]
    length(Y55_65_C5$ID)#27
    Y55_65_C5N<-Y55_65[Y55_65$AcceptedCmp5==0,]
    length(Y55_65_C5N$ID)#460
    # campanha Last
    Y55_65_CL<-Y55_65[Y55_65$Response==1,]
    length(Y55_65_CL$ID)#60
    Y55_65_CLN<-Y55_65[Y55_65$Response==0,]
    length(Y55_65_CLN$ID)#427
    
    
    ## 65 a 75 ##
    # campanha 1
    Y65_75_C1<-Y65_75[Y65_75$AcceptedCmp1==1,]
    length(Y65_75_C1$ID)# 27
    Y65_75_C1N<-Y65_75[Y65_75$AcceptedCmp1==0,]
    length(Y65_75_C1N$ID)# 353
    # campanha 2
    Y65_75_C2<-Y65_75[Y65_75$AcceptedCmp2==1,]
    length(Y65_75_C2$ID)#5
    Y65_75_C2N<-Y65_75[Y65_75$AcceptedCmp2==0,]
    length(Y65_75_C2N$ID)#375
    # campanha 3
    Y65_75_C3<-Y65_75[Y65_75$AcceptedCmp3==1,]
    length(Y65_75_C3$ID)#23
    Y65_75_C3N<-Y65_75[Y65_75$AcceptedCmp3==0,]
    length(Y65_75_C3N$ID)#357
    # campanha 4
    Y65_75_C4<-Y65_75[Y65_75$AcceptedCmp4==1,]
    length(Y65_75_C4$ID)#35
    Y65_75_C4N<-Y65_75[Y65_75$AcceptedCmp4==0,]
    length(Y65_75_C4N$ID)#345
    # campanha 5
    Y65_75_C5<-Y65_75[Y65_75$AcceptedCmp5==1,]
    length(Y65_75_C5$ID)#32
    Y65_75_C5N<-Y65_75[Y65_75$AcceptedCmp5==0,]
    length(Y65_75_C5N$ID)#348
    # campanha Last
    Y65_75_CL<-Y65_75[Y65_75$Response==1,]
    length(Y65_75_CL$ID)#57
    Y65_75_CLN<-Y65_75[Y65_75$Response==0,]
    length(Y65_75_CLN$ID)#323
    
    ## 75 a 82 ##
    # campanha 1
    Y75_129_C1<-Y75_129[Y75_129$AcceptedCmp1==1,]
    length(Y75_129_C1$ID)# 7
    Y75_129_C1N<-Y75_129[Y75_129$AcceptedCmp1==0,]
    length(Y75_129_C1N$ID)# 33
    # campanha 2
    Y75_129_C2<-Y75_129[Y75_129$AcceptedCmp2==1,]
    length(Y75_129_C2$ID)#0
    Y75_129_C2N<-Y75_129[Y75_129$AcceptedCmp2==0,]
    length(Y75_129_C2N$ID)#40
    # campanha 3
    Y75_129_C3<-Y75_129[Y75_129$AcceptedCmp3==1,]
    length(Y75_129_C3$ID)#3
    Y75_129_C3N<-Y75_129[Y75_129$AcceptedCmp3==0,]
    length(Y75_129_C3N$ID)#37
    # campanha 4
    Y75_129_C4<-Y75_129[Y75_129$AcceptedCmp4==1,]
    length(Y75_129_C4$ID)#3
    Y75_129_C4N<-Y75_129[Y75_129$AcceptedCmp4==0,]
    length(Y75_129_C4N$ID)#37
    # campanha 5
    Y75_129_C5<-Y75_129[Y75_129$AcceptedCmp5==1,]
    length(Y75_129_C5$ID)#7
    Y75_129_C5N<-Y75_129[Y75_129$AcceptedCmp5==0,]
    length(Y75_129_C5N$ID)#33
    # campanha Last
    Y75_129_CL<-Y75_129[Y75_129$Response==1,]
    length(Y75_129_CL$ID)#9
    Y75_129_CLN<-Y75_129[Y75_129$Response==0,]
    length(Y75_129_CLN$ID)#31

    
    
    #################################################
    ### Aceitação da campanha pelo método de compra 
    
    ## Clientes que não compraram nada nos meios de venda com desconto
    Deals_zero<-dados[dados$NumDealsPurchases==0,]# não compraram
    length(Deals_zero$ID)#46
    Deals_C<-dados[dados$NumDealsPurchases>0,] # fizeram pelo menos uma compra
    length(Deals_C$ID)#2191
      # verificar quais aceitaram a campanha entre os que compraram
      ### Deals ###
        Deals_C1<- Deals_C[Deals_C$AcceptedCmp1==1,]#127
        length(Deals_C1$ID)
        Deals_C2<- Deals_C[Deals_C$AcceptedCmp2==1,]#27
        length(Deals_C2$ID)
        Deals_C3<- Deals_C[Deals_C$AcceptedCmp3==1,]#157
        length(Deals_C3$ID)
        Deals_C4<- Deals_C[Deals_C$AcceptedCmp4==1,]#157
        length(Deals_C4$ID)
        Deals_C5<- Deals_C[Deals_C$AcceptedCmp5==1,]#141
        length(Deals_C5$ID)
        Deals_CL<- Deals_C[Deals_C$Response==1,]#318
        length(Deals_CL$ID)
        (Total_deals<- sum(length(Deals_C1$ID),length(Deals_C2$ID),
                          length(Deals_C3$ID),length(Deals_C4$ID),
                          length(Deals_C5$ID),length(Deals_CL$ID)))#927
        # banco de dados
        by_Deals<- data.frame(Deals = c("C1", "C2", "C3","C4","C5","Last"),
                                   Quantidade = c(length(Deals_C1$ID), length(Deals_C2$ID), length(Deals_C3$ID),
                                                  length(Deals_C4$ID),length(Deals_C5$ID),length(Deals_CL$ID)))
        arrange(by_Deals,desc(Quantidade))
        # Barras
        ggplot(by_Deals, aes(y = Quantidade, x = Deals,fill = Deals)) +
          geom_bar(position = position_dodge(1),stat = "identity")+
          geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
        
    ### Catalog ### 
    Catalog_zero<-dados[dados$NumCatalogPurchases==0,]# não compraram
    length(Catalog_zero$ID)#585
    Catalog_C<-dados[dados$NumCatalogPurchases>0,]# fizeram pelo menos uma compra
    length(Catalog_C$ID) #1652
        # verificar quais aceitaram a campanha entre os que compraram
        Catalog_C1<- Catalog_C[Catalog_C$AcceptedCmp1==1,]#144
        length(Catalog_C1$ID)
        Catalog_C2<- Catalog_C[Catalog_C$AcceptedCmp2==1,]#28
        length(Catalog_C2$ID)
        Catalog_C3<- Catalog_C[Catalog_C$AcceptedCmp3==1,]#145
        length(Catalog_C3$ID)
        Catalog_C4<- Catalog_C[Catalog_C$AcceptedCmp4==1,]#163
        length(Catalog_C4$ID)
        Catalog_C5<- Catalog_C[Catalog_C$AcceptedCmp5==1,]#162
        length(Catalog_C5$ID)
        Catalog_CL<- Catalog_C[Catalog_C$Response==1,]#312
        length(Catalog_CL$ID)
        (Total_Catalog<-sum(length(Catalog_C1$ID),length(Catalog_C2$ID),
        length(Catalog_C3$ID),length(Catalog_C4$ID),
        length(Catalog_C5$ID),length(Catalog_CL$ID)))# 954
        # banco de dados
        by_Catalog<- data.frame(Catalog = c("C1", "C2", "C3","C4","C5","Last"),
                              Quantidade = c(length(Catalog_C1$ID), length(Catalog_C2$ID), length(Catalog_C3$ID),
                                             length(Catalog_C4$ID),length(Catalog_C5$ID),length(Catalog_CL$ID)))
        arrange(by_Catalog,desc(Quantidade))
        # Barras
        ggplot(by_Catalog, aes(y = Quantidade, x = Catalog,fill = Catalog)) +
          geom_bar(position = position_dodge(1),stat = "identity")+
          geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
        
    ### Web ###
    Web_zero<-dados[dados$NumWebPurchases==0,]# não compraram
    length(Web_zero$ID)# 49
    Web_C<-dados[dados$NumWebPurchases>0,]# fizeram pelo menos uma compra
    length(Web_C$ID)# 2188
        # verificar quais aceitaram a campanha entre os que compraram
        Web_C1<- Web_C[Web_C$AcceptedCmp1==1,]#144
        length(Web_C1$ID)
        Web_C2<- Web_C[Web_C$AcceptedCmp2==1,]#30
        length(Web_C2$ID)
        Web_C3<- Web_C[Web_C$AcceptedCmp3==1,]#162
        length(Web_C3$ID)
        Web_C4<- Web_C[Web_C$AcceptedCmp4==1,]#166
        length(Web_C4$ID)
        Web_C5<- Web_C[Web_C$AcceptedCmp5==1,]#162
        length(Web_C5$ID)
        Web_CL<- Web_C[Web_C$Response==1,]#333
        length(Web_CL$ID)
        
        (Total_Web<- sum(length(Web_C1$ID),length(Web_C2$ID),
        length(Web_C3$ID),length(Web_C4$ID),
        length(Web_C5$ID),length(Web_CL$ID)))#997
        
        # banco de dados
        by_Web<- data.frame(Web = c("C1", "C2", "C3","C4","C5","Last"),
                                Quantidade = c(length(Web_C1$ID), length(Web_C2$ID), length(Web_C3$ID),
                                               length(Web_C4$ID),length(Web_C5$ID),length(Web_CL$ID)))
        arrange(by_Web,desc(Quantidade))
        # Barras
        ggplot(by_Web, aes(y = Quantidade, x = Web,fill = Web)) +
          geom_bar(position = position_dodge(1),stat = "identity")+
          geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    ### Store ### 
    Store_zero<-dados[dados$NumStorePurchases==0,]# não compraram
    length(Store_zero$ID)#15   
    Store_C<-dados[dados$NumStorePurchases>0,]# fizeram pelo menos uma compra
    length(Store_C$ID)#2222
        # verificar quais aceitaram a campanha entre os que compraram
        Store_C1<- Store_C[Store_C$AcceptedCmp1==1,]#144
        length(Store_C1$ID)
        Store_C2<- Store_C[Store_C$AcceptedCmp2==1,]#30
        length(Store_C2$ID)
        Store_C3<- Store_C[Store_C$AcceptedCmp3==1,]#162
        length(Store_C3$ID)
        Store_C4<- Store_C[Store_C$AcceptedCmp4==1,]#167
        length(Store_C4$ID)
        Store_C5<- Store_C[Store_C$AcceptedCmp5==1,]#162
        length(Store_C5$ID)
        Store_CL<- Store_C[Store_C$Response==1,]#334
        length(Store_CL$ID)
        

       (Total_Store <-sum(length(Store_C1$ID), length(Store_C2$ID), length(Store_C3$ID),
        length(Store_C4$ID), length(Store_C5$ID),length(Store_CL$ID)))#999
        
        
        # banco de dados
        by_Store<- data.frame(Store = c("C1", "C2", "C3","C4","C5","Last"),
                                Quantidade = c(length(Store_C1$ID), length(Store_C2$ID), length(Store_C3$ID),
                                               length(Store_C4$ID),length(Store_C5$ID),length(Store_CL$ID)))
        arrange(by_Store,desc(Quantidade))
        # Barras
        ggplot(by_Store, aes(y = Quantidade, x = Store,fill = Store)) +
          geom_bar(position = position_dodge(1),stat = "identity")+
          geom_text(aes(y=Quantidade,label=Quantidade), vjust = -0.2)
    
    # vendas por catálogo vão mal
    # campanha dois foi a de menor desempenho
    # a última campanha foi a de melhor desempenho.
    
        
        ## Clientes que reclamaram
        reclamaram<-dados[dados$Complain==1,]
        length(reclamaram$ID) # 20 reclamaram
        reclamaram_N<-dados[dados$Complain==0,]
        length(reclamaram_N$ID) # 2217 reclamaram
        # não tem muita relação com número de reclamações e aceitação da campanha
        # uma vez que apenas 20 clientes  reclamaram nos últimos dois anos
          
    ##### Parte Dois #### 
    ##### Criar um modelo de regressão para prever o consumo ###
        
        ## vamos criar um variável para somar todos os consumos
        Spent_tot<- dados$MntWines+dados$MntFruits+
                    dados$MntMeatProducts+dados$MntFishProducts+
                    dados$MntSweetProducts+dados$MntGoldProds
        # Spent_tot é a nossa variável resposta
library(lubridate)# pacote que trabalha com datas

## Dt_Customer é a data que o cliente entrou para a companhia. Transformar em anos
Dt_Customer<- year(dados$Dt_Customer) # extrair somente os anos
Dt_customer_Anos<- 2022 - Dt_Customer # para saber quantos anos são clientes

# "Z_CostContact"       "Z_Revenue" não entrou no modelo pois não  as identifiqui

#transformando variáveis em fator
Education<- as.factor(dados$Education)
Marital.Status<-as.factor(dados$Marital_Status)
Kidhome<-as.factor(dados$Kidhome)
Teenhome<-as.factor(dados$Teenhome)
Complain<- as.factor(dados$Complain)
AcceptedCmp1<-as.factor(dados$AcceptedCmp1)
AcceptedCmp2<- as.factor(dados$AcceptedCmp2)
AcceptedCmp3<- as.factor(dados$AcceptedCmp3)
AcceptedCmp4<- as.factor(dados$AcceptedCmp4)
AcceptedCmp5<- as.factor(dados$AcceptedCmp5)
Response<- as.factor(dados$Response)
# variáveis numéricas
Dt_customer_Anos<-as.numeric(Dt_customer_Anos)
Income<-as.numeric(dados$Income)
Year<-as.numeric(dados$Year)
Recency<-as.numeric(dados$Recency)
NumDealsPurchases<-as.numeric(dados$NumDealsPurchases)
NumWebPurchases<-as.numeric(dados$NumWebPurchases)
NumCatalogPurchases<-as.numeric(dados$NumCatalogPurchases)
NumStorePurchases<-as.numeric(dados$NumStorePurchases) 
NumWebVisitsMonth<-as.numeric(dados$NumWebVisitsMonth)

    #### criar um data frame com as variáveis arrumadas
    dados_N<- data.frame(Spent_tot,Education,Marital.Status, Kidhome,Teenhome,
    Complain,AcceptedCmp1,AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response,
    Dt_customer_Anos, Income,Year,Recency,NumDealsPurchases, NumWebPurchases,
    NumCatalogPurchases,NumStorePurchases, NumWebVisitsMonth) 
    length(names(dados_N))# 21 variáveis

    # verificar se tem NA
    summary(dados_N)
    # 24 nA's em Income
    dados<-na.omit(dados_N)# removendo onde tem Na's
    length(dados$Spent_tot)#2237 observações restantes
    names(dados)
    length(names(dados))# 21 
    
    ##########################
    #### Modelos Testados#####
    Mod01<- lm(Spent_tot~., data=dados) # Modelo Saturado - com todas as variáveis
    summary(Mod01)# percebemos que algumas variáveis não foram significativas, então temos
                  # que retirar e ajustar novamente.
    
    # marital.status não foi significativo para o modelo. vamos retirar e modelar novamente
    Mod02<- lm(Spent_tot~ Education+ Kidhome+Teenhome+
               Complain+AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+AcceptedCmp4+
              AcceptedCmp5+Response+Dt_customer_Anos+Income+Year+Recency+NumDealsPurchases+
              NumWebPurchases+NumCatalogPurchases+NumStorePurchases+
              NumWebVisitsMonth, data = dados)
    summary(Mod02)#  Complain não foi significativo 
    
    # retirar camplain e modelar novamente
    Mod03<- lm(Spent_tot~ Education+ Kidhome+Teenhome+
                AcceptedCmp1+AcceptedCmp4+Year+
                 AcceptedCmp5+Response+Dt_customer_Anos+Income+Recency+
                 NumDealsPurchases+ NumWebPurchases+
                 NumCatalogPurchases+NumStorePurchases+ NumWebVisitsMonth, data = dados)
    summary(Mod03) # Year não foi significatico
    
    # retirar Year e modelar novamnete
    Mod04<- lm(Spent_tot~ Education+ Kidhome+Teenhome+
                 AcceptedCmp1+AcceptedCmp4+
                 AcceptedCmp5+Response+Dt_customer_Anos+Income+Recency+
                 NumDealsPurchases+ NumWebPurchases+
                 NumCatalogPurchases+NumStorePurchases+ NumWebVisitsMonth, data = dados)
    summary(Mod04) # NumDealsPurchases não foi significatico
    
    # retirar NumDealsPurchases e modelar novamnete
    Mod05<- lm(Spent_tot~ Education+ Kidhome+Teenhome+
                 AcceptedCmp1+AcceptedCmp4+
                 AcceptedCmp5+Response+Dt_customer_Anos+Income+Recency+
                  NumWebPurchases+
                 NumCatalogPurchases+NumStorePurchases+ NumWebVisitsMonth, data = dados)
    summary(Mod05) # Recency não foi significatico
    
    # retirar Recency, NumWebVisitsMonth  e modelar novamnete
    Mod06<- lm(Spent_tot~ Education+ Kidhome+Teenhome+
                 AcceptedCmp1+AcceptedCmp4+
                 AcceptedCmp5+Response+Dt_customer_Anos+Income+
                 NumWebPurchases+
                 NumCatalogPurchases+NumStorePurchases, data = dados)
    summary(Mod06) # Modelo aceitável
    plot(Mod06$residuals)# estão em torno de zero
    plot(Mod06)
    plot(dados$Spent_tot,Mod06$fitted.values)# tem que estar os mais próximos possível
    cor(dados$Spent_tot,Mod06$fitted.values)#0.89
    round(coefficients(Mod06),4)
    
    
    ## fazendo transformação na variável resposta o modelo fica
    Mod06A<- lm(log(Spent_tot)~ Education+ Kidhome+Teenhome
              +AcceptedCmp4+
                 AcceptedCmp5+Response+Dt_customer_Anos+Income+
                 NumWebPurchases+NumDealsPurchases+
                 NumCatalogPurchases+NumStorePurchases+ NumWebVisitsMonth, data = dados)
    summary(Mod06A) # Modelo aceitável
    plot(Mod06A$residuals)# estão em torno de zero
    plot(Mod06A)
    plot(dados$Spent_tot,Mod06A$fitted.values)# tem que estar os mais próximos possível
    cor(dados$Spent_tot,Mod06A$fitted.values)#0.86
    round(coefficients(Mod06A),4)
    ### MODELO 6A PODE SER O ESCOLHIDO FINAL
    ### lembrando que não é o modelo mais certo, porémn aceitável.
    
    
    
    

        