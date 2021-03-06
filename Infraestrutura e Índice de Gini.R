#Instala e carrega pacotes

pacotes <-
  c("readxl", "foreign", "rgdal", "maptools", "sp", "spdep")

lapply(pacotes, install.packages)

lapply(pacotes, library, character.only = TRUE)

#Carrega dados

dados <-
  read_excel("D:\\Dados - Regress�o Espacial.xlsx")

#An�lise descritiva

attach(dados)

hist(`prop da popula��o em domic�lios com �gua encanada`)
hist(`�ndice de Gini`)
hist(`prop da popula��o em domic�lios com banheiro e �gua encanada`)
hist(`prop da popula��o em domic�lios com coleta de lixo`)
hist(`prop da popula��o em domic�lios com energia el�trica`)
hist(
  `prop de pessoas em domic�lios com abastecimento de �gua e esgotamento sanit�rio inadequados`
)

summary(dados$�ndicedeGini)
summary(dados$propdapopula��oemdomic�lioscomcoletadelixo)
summary(dados$propdapopula��oemdomic�lioscomenergiael�trica)
hist(`prop de pessoas em domic�lios com paredes inadequadas`)

detach(dados)

#Apenas Gini apresenta uma distribui��o pr�xima da Normal

dbfMG <-
  read.dbf(file = "C:\\Users\\b16778363\\Downloads\\MG_Mun97_region\\MG_Mun97_region.dbf")

#Testa se os arquivos est�o compat�veis

confdados <- data.frame(dados$Munic�pio, dados$CODMUN6)
names(confdados) <- c("Munic�pio", "CODMUN6")

confdbf <- data.frame(dbfMG$MUNIC_PIO, dbfMG$CODMUN6)
names(confdbf) <- c("Munic�pio", "CODMUN6")

str(confdados)
str(confdbf)

confdados$Munic�pio <- as.character(confdados$Munic�pio)
confdbf$Munic�pio <- as.character(confdbf$Munic�pio)

confdados$CODMUN6 <- as.factor(confdados$CODMUN6)
confdados$Munic�pio <- tolower(confdados$Munic�pio)

confdbf$Munic�pio <- tolower(confdbf$Munic�pio)
join <- anti_join(x = confdados, y = confdbf, by = "CODMUN6")

#Todos os c�digos s�o iguais tanto para os dados quanto para o DBF. Pode-se prosseguir com o trabalho.

#Lendo o shapefile

MG <-
  readOGR(dsn = "C:\\Users\\201830001-61\\Downloads\\MG_Mun97_region", layer =
            "MG_Mun97_region")


#Adicionando os dados ao objeto que cont�m o shapefile

MG@data = cbind(MG@data, dados)

#Plotando vari�vel

spplot(MG, "�ndicedeGini", col.regions = terrain.colors(256))
spplot(MG,
       "propdapopula��oemdomic�lioscomenergiael�trica",
       col.regions = terrain.colors(256))
spplot(MG,
       "propdapopula��oemdomic�lioscomcoletadelixo",
       col.regions = terrain.colors(256))

## encontrando quem � vizinho de quem
nb <- poly2nb(MG, queen = TRUE)

## ponderacao (linhas somando 1)

nbw <- nb2listw(nb, style = "W")
names(nbw)
nbw$nei[[1]]
nbw$wei[[1]]

imoran = moran.mc(MG@data$�ndicedeGini, nbw, nsim = 999)
imoran

imoran1 = moran.mc(MG@data$propdapopula��oemdomic�lioscomenergiael�trica,
                   nbw,
                   nsim = 999)
imoran1

imoran2 = moran.mc(MG@data$propdapopula��oemdomic�lioscomcoletadelixo,
                   nbw,
                   nsim = 999)
imoran2


#Indice de Moran Local

Ilocal = localmoran(MG@data$�ndicedeGini, nbw)

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
  MG@data$�ndicedeGini,
  nbw,
  label = T,
  xlab = "Y",
  ylab = "WY"
)


#Indice de Moran Local

Ilocal1 = localmoran(MG@data$propdapopula��oemdomic�lioscomenergiael�trica, nbw)
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
  MG@data$propdapopula��oemdomic�lioscomenergiael�trica,
  nbw,
  label = T,
  xlab = "X",
  ylab = "WX"
)



#Indice de Moran Local

Ilocal2 = localmoran(MG@data$propdapopula��oemdomic�lioscomcoletadelixo, nbw)

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
  MG@data$propdapopula��oemdomic�lioscomcoletadelixo,
  nbw,
  label = T,
  xlab = "X1",
  ylab = "WX1"
)


#Vari�veis da regress�o

y = MG@data$�ndicedeGini
x = MG@data$propdapopula��oemdomic�lioscomenergiael�trica
x1 = MG@data$propdapopula��oemdomic�lioscomcoletadelixo




#Regress�o Linear Comum


modlm <- lm(y ~ x + x1)
summary(modlm)
imoran.lm = moran.mc(modlm$residuals, nbw, nsim = 999, alternative = "greater")
imoran.lm


#Regress�o Espacial

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

#cuidado com hip�tese alternativa

imoran.SEM = moran.mc(SEM$residuals, nbw, nsim = 999)
imoran.SAR = moran.mc(SAR$residuals, nbw, nsim = 999)
imoran.SEM
imoran.SAR

shapiro.test(SAR$residuals)
shapiro.test(SEM$residuals)

hist(SAR$residuals)
hist(SEM$residuals)
