#Instala e carrega pacotes

pacotes <-
  c("readxl", "foreign", "rgdal", "maptools", "sp", "spdep")

lapply(pacotes, install.packages)

lapply(pacotes, library, character.only = TRUE)

#Carrega dados

dados <-
  read_excel("D:\\Dados - Regressão Espacial.xlsx")

#Análise descritiva

attach(dados)

hist(`prop da população em domicílios com água encanada`)
hist(`Índice de Gini`)
hist(`prop da população em domicílios com banheiro e água encanada`)
hist(`prop da população em domicílios com coleta de lixo`)
hist(`prop da população em domicílios com energia elétrica`)
hist(
  `prop de pessoas em domicílios com abastecimento de água e esgotamento sanitário inadequados`
)

summary(dados$ÍndicedeGini)
summary(dados$propdapopulaçãoemdomicílioscomcoletadelixo)
summary(dados$propdapopulaçãoemdomicílioscomenergiaelétrica)
hist(`prop de pessoas em domicílios com paredes inadequadas`)

detach(dados)

#Apenas Gini apresenta uma distribuição próxima da Normal

dbfMG <-
  read.dbf(file = "C:\\Users\\b16778363\\Downloads\\MG_Mun97_region\\MG_Mun97_region.dbf")

#Testa se os arquivos estão compatíveis

confdados <- data.frame(dados$Município, dados$CODMUN6)
names(confdados) <- c("Município", "CODMUN6")

confdbf <- data.frame(dbfMG$MUNIC_PIO, dbfMG$CODMUN6)
names(confdbf) <- c("Município", "CODMUN6")

str(confdados)
str(confdbf)

confdados$Município <- as.character(confdados$Município)
confdbf$Município <- as.character(confdbf$Município)

confdados$CODMUN6 <- as.factor(confdados$CODMUN6)
confdados$Município <- tolower(confdados$Município)

confdbf$Município <- tolower(confdbf$Município)
join <- anti_join(x = confdados, y = confdbf, by = "CODMUN6")

#Todos os códigos são iguais tanto para os dados quanto para o DBF. Pode-se prosseguir com o trabalho.

#Lendo o shapefile

MG <-
  readOGR(dsn = "C:\\Users\\201830001-61\\Downloads\\MG_Mun97_region", layer =
            "MG_Mun97_region")


#Adicionando os dados ao objeto que contém o shapefile

MG@data = cbind(MG@data, dados)

#Plotando variável

spplot(MG, "ÍndicedeGini", col.regions = terrain.colors(256))
spplot(MG,
       "propdapopulaçãoemdomicílioscomenergiaelétrica",
       col.regions = terrain.colors(256))
spplot(MG,
       "propdapopulaçãoemdomicílioscomcoletadelixo",
       col.regions = terrain.colors(256))

## encontrando quem é vizinho de quem
nb <- poly2nb(MG, queen = TRUE)

## ponderacao (linhas somando 1)

nbw <- nb2listw(nb, style = "W")
names(nbw)
nbw$nei[[1]]
nbw$wei[[1]]

imoran = moran.mc(MG@data$ÍndicedeGini, nbw, nsim = 999)
imoran

imoran1 = moran.mc(MG@data$propdapopulaçãoemdomicílioscomenergiaelétrica,
                   nbw,
                   nsim = 999)
imoran1

imoran2 = moran.mc(MG@data$propdapopulaçãoemdomicílioscomcoletadelixo,
                   nbw,
                   nsim = 999)
imoran2


#Indice de Moran Local

Ilocal = localmoran(MG@data$ÍndicedeGini, nbw)

MG@data$Ilocal = Ilocal[, 1]

spplot(MG, "Ilocal", col.regions = topo.colors(256))

#Plotando apenas os significativos

MG@data$Ilocal.z = Ilocal[, 4]
spplot(
  MG,
  "Ilocal.z",
  at = c(min(Ilocal[, 4]),-1.96, 1.96, max(Ilocal[, 4])),
  col.regions = c("blue", "white", "red")
)

moran.plot(
  MG@data$ÍndicedeGini,
  nbw,
  label = T,
  xlab = "Y",
  ylab = "WY"
)


#Indice de Moran Local

Ilocal1 = localmoran(MG@data$propdapopulaçãoemdomicílioscomenergiaelétrica, nbw)
MG@data$Ilocal1 = Ilocal1[, 1]
spplot(MG, "Ilocal1", col.regions = topo.colors(256))

#Plotando apenas os significativos

MG@data$Ilocal1.z = Ilocal1[, 4]
spplot(
  MG,
  "Ilocal1.z",
  at = c(min(Ilocal1[, 4]),-1.96, 1.96, max(Ilocal1[, 4])),
  col.regions = c("blue", "white", "red")
)


moran.plot(
  MG@data$propdapopulaçãoemdomicílioscomenergiaelétrica,
  nbw,
  label = T,
  xlab = "X",
  ylab = "WX"
)



#Indice de Moran Local

Ilocal2 = localmoran(MG@data$propdapopulaçãoemdomicílioscomcoletadelixo, nbw)

MG@data$Ilocal2 = Ilocal2[, 1]

spplot(MG, "Ilocal2", col.regions = topo.colors(256))

#Plotando apenas os significativos

MG@data$Ilocal2.z = Ilocal2[, 4]

spplot(
  MG,
  "Ilocal2.z",
  at = c(min(Ilocal2[, 4]),-1.96, 1.96, max(Ilocal2[, 4])),
  col.regions = c("blue", "white", "red")
)


moran.plot(
  MG@data$propdapopulaçãoemdomicílioscomcoletadelixo,
  nbw,
  label = T,
  xlab = "X1",
  ylab = "WX1"
)


#Variáveis da regressão

y = MG@data$ÍndicedeGini
x = MG@data$propdapopulaçãoemdomicílioscomenergiaelétrica
x1 = MG@data$propdapopulaçãoemdomicílioscomcoletadelixo




#Regressão Linear Comum


modlm <- lm(y ~ x + x1)
summary(modlm)
imoran.lm = moran.mc(modlm$residuals, nbw, nsim = 999, alternative = "greater")
imoran.lm


#Regressão Espacial

SEM <-
  errorsarlm(y ~ x + x1, data = data.frame(cbind(x, y)), nbw, method = "eigen")
SAR <-
  lagsarlm(y ~ x + x1, data = data.frame(cbind(x, y)), nbw, method = "eigen")
summary(SEM)
summary(SAR)


MG@data$residuos_SEM = SEM$residuals
MG@data$residuos_SAR = SAR$residuals

spplot(MG, "residuos_SEM", col.regions = terrain.colors(256))
spplot(MG, "residuos_SAR", col.regions = terrain.colors(256))

#cuidado com hipótese alternativa

imoran.SEM = moran.mc(SEM$residuals, nbw, nsim = 999)
imoran.SAR = moran.mc(SAR$residuals, nbw, nsim = 999)
imoran.SEM
imoran.SAR

shapiro.test(SAR$residuals)
shapiro.test(SEM$residuals)

hist(SAR$residuals)
hist(SEM$residuals)
