######################################
# Concurso Agetic - Preprocesamiento
# Autor: Ayar Paco
######################################

library(readxl)
library(ggplot2)
library(Amelia)
library(VIM)
library(mice)
library(grid)
library(gridExtra)

# Lectura BD
bd <- read_excel("C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/base-5536-bdfinalcorregido2.xlsx")

#####################
# Valores faltantes
#####################

mice_plot <- aggr(bd,
                  col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(bd), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"),
                  plot = F)

missing_data <- mice_plot$missings
rownames(missing_data) <- NULL
missing_data$Percent <- missing_data$Count/length(bd$P152)
missing_data[order(missing_data$Percent,decreasing = T),]

##################################################
# Fitrar columnas con 5% de Faltantes Max.
##################################################

var_consideradas <- as.numeric(rownames(missing_data[missing_data$Percent<=0.05,]))

bd_f <- bd[,var_consideradas]
missmap(bd_f,y.labels=NULL,y.at=NULL)

#############################################################
# Ajustar formato de variables y definir NA's adecuadamente
#############################################################

summary(bd_f)

bd_f$Tipo <- as.factor(bd_f$Tipo)
bd_f$P2   <- as.factor(bd_f$P2)
bd_f$P4   <- as.factor(bd_f$P4)
bd_f$P6   <- as.factor(bd_f$P6)
bd_f$P10  <- as.factor(bd_f$P10)
bd_f$P12  <- as.factor(bd_f$P12)
bd_f$P15A <- as.factor(bd_f$P15A)
bd_f$P16  <- as.factor(bd_f$P16)
bd_f$P17  <- as.factor(bd_f$P17)
bd_f$P20  <- as.factor(bd_f$P20)
bd_f$P23  <- as.factor(bd_f$P23)
bd_f$P28  <- as.factor(bd_f$P28)
bd_f$P31A <- as.factor(bd_f$P31A) # OJO decartar o considerar el resto B-H
bd_f$P32  <- as.factor(bd_f$P32)
bd_f$P33  <- as.factor(bd_f$P33)
bd_f$P35  <- as.factor(bd_f$P35)

table(bd_f$P36A)
bd_f$P36A[bd_f$P36A==9] <- NA
bd_f$P36A  <- as.factor(bd_f$P36A)
table(bd_f$P129A)
bd_f$P129A[bd_f$P129A==9] <- NA
bd_f$P129A <- as.factor(bd_f$P129A)
table(bd_f$P130A)
bd_f$P130A[bd_f$P130A==9] <- NA
bd_f$P130A <- as.factor(bd_f$P130A)
table(bd_f$P131A)
bd_f$P131A[bd_f$P131A==9] <- NA
bd_f$P131A <- as.factor(bd_f$P131A)
table(bd_f$P132A)
bd_f$P132A[bd_f$P132A==9] <- NA
bd_f$P132A <- as.factor(bd_f$P132A)
table(bd_f$P133A)
bd_f$P133A[bd_f$P133A==9] <- NA
bd_f$P133A <- as.factor(bd_f$P133A)
table(bd_f$P134)
bd_f$P134[bd_f$P134==9] <- NA
bd_f$P134  <- as.factor(bd_f$P134)
table(bd_f$P135)
bd_f$P135[bd_f$P135==9] <- NA
bd_f$P135  <- as.factor(bd_f$P135)
table(bd_f$P136)
bd_f$P136[bd_f$P136==9] <- NA
bd_f$P136  <- as.factor(bd_f$P136)
table(bd_f$P137)
bd_f$P137[bd_f$P137==9] <- NA
bd_f$P137  <- as.factor(bd_f$P137)
table(bd_f$P138)
bd_f$P138[bd_f$P138==9] <- NA
bd_f$P138  <- as.factor(bd_f$P138)
table(bd_f$P139)
bd_f$P139[bd_f$P139==9] <- NA
bd_f$P139  <- as.factor(bd_f$P139)
table(bd_f$P140)
bd_f$P140[bd_f$P140==9] <- NA
bd_f$P140  <- as.factor(bd_f$P140)

bd_f$P148  <- as.factor(bd_f$P148)
bd_f$P149  <- as.factor(bd_f$P149)
bd_f$P150  <- as.factor(bd_f$P150)
bd_f$P151  <- as.factor(bd_f$P151)
bd_f$P152  <- factor(bd_f$P152,labels = c("AS","CP","NP","PI","EM","ES","AC","RJ","SB","SN"))
bd_f$P153  <- as.factor(bd_f$P153)
bd_f$P154A <- as.factor(bd_f$P154A)
bd_f$P155  <- as.factor(bd_f$P155)
bd_f$P156A <- as.factor(bd_f$P156A)
bd_f$P156B <- as.factor(bd_f$P156B)
bd_f$P156C <- as.factor(bd_f$P156C)
bd_f$P156D <- as.factor(bd_f$P156D)
bd_f$P156E <- as.factor(bd_f$P156E)
bd_f$P156F <- as.factor(bd_f$P156F)
bd_f$P156G <- as.factor(bd_f$P156G)
bd_f$P156H <- as.factor(bd_f$P156H)
bd_f$P156I <- as.factor(bd_f$P156I)
bd_f$P156J <- as.factor(bd_f$P156J)
bd_f$P156K <- as.factor(bd_f$P156K)
bd_f$P157  <- as.factor(bd_f$P157)
bd_f$P158  <- as.factor(bd_f$P158)
bd_f$P159  <- as.factor(bd_f$P159)
bd_f$P160  <- as.factor(bd_f$P160) 
bd_f$CodP160 <- as.factor(bd_f$CodP160)
bd_f$Nse <- as.factor(bd_f$Nse)

# Definir NA's adecuadamente
summary(bd_f$P14)
length(bd_f$P14[bd_f$P14>9 & !is.na(bd_f$P14)])
bd_f$P14[bd_f$P14==9] <- NA
summary(bd_f$P14)
length(bd_f$P14[!is.na(bd_f$P14)])

summary(bd_f$P30)
length(bd_f$P30[bd_f$P30!=99 & !is.na(bd_f$P30)])
bd_f$P30[bd_f$P30==99] <- NA
summary(bd_f$P30)
length(bd_f$P30[!is.na(bd_f$P30)])

summary(bd_f$P34)
length(bd_f$P34[bd_f$P34!=99 & !is.na(bd_f$P34)])
bd_f$P34[bd_f$P34==99] <- NA
summary(bd_f$P34)
length(bd_f$P34[!is.na(bd_f$P34)])

summary(bd_f)

##################################################
# Alternativa 1: 
# - Considerar solo casos completos
##################################################

bd_fa1 <- bd_f[complete.cases(bd_f),]
summary(bd_fa1$Cat.Ocup)
table(bd_fa1$P2)

write.csv(bd_fa1,"C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/bd_obs_completas.csv")
saveRDS(bd_fa1,"C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/bd_obs_completas.rds")


##################################################
# Alternativa 2: 
# - Imputar faltantes
##################################################

bd_fa2 <- bd_f
missmap(bd_fa2,y.labels=NULL,y.at=NULL)

# Ojo, esto tarda una eternidad
# tempData <- mice(bd_fa2,m=5,seed=500, nnet.MaxNWts = 3000)

saveRDS(tempData,"C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/tempDataImput.rds")

colnames(bd_fa2c)[39] <- "P152" # si vuelven a correr tempData, esto no es necesario

summary(tempData)

plots <- list()
plots[[1]]  <- stripplot(tempData,P14  , pch = 20, cex = 1.2)
plots[[2]]  <- stripplot(tempData,P15A , pch = 20, cex = 1.2)
plots[[3]]  <- stripplot(tempData,P30  , pch = 20, cex = 1.2)
plots[[4]]  <- stripplot(tempData,P31A , pch = 20, cex = 1.2)
plots[[5]]  <- stripplot(tempData,P32  , pch = 20, cex = 1.2)
plots[[6]]  <- stripplot(tempData,P33  , pch = 20, cex = 1.2)
plots[[7]]  <- stripplot(tempData,P34  , pch = 20, cex = 1.2)
plots[[8]]  <- stripplot(tempData,P35  , pch = 20, cex = 1.2)
plots[[9]]  <- stripplot(tempData,P36A , pch = 20, cex = 1.2)
plots[[10]] <- stripplot(tempData,P129A, pch = 20, cex = 1.2)
plots[[11]] <- stripplot(tempData,P130A, pch = 20, cex = 1.2)
plots[[12]] <- stripplot(tempData,P131A, pch = 20, cex = 1.2)
plots[[13]] <- stripplot(tempData,P132A, pch = 20, cex = 1.2)
plots[[14]] <- stripplot(tempData,P133A, pch = 20, cex = 1.2)
plots[[15]] <- stripplot(tempData,P134 , pch = 20, cex = 1.2)
plots[[16]] <- stripplot(tempData,P135 , pch = 20, cex = 1.2)
plots[[17]] <- stripplot(tempData,P136 , pch = 20, cex = 1.2)
plots[[18]] <- stripplot(tempData,P137 , pch = 20, cex = 1.2)
plots[[19]] <- stripplot(tempData,P138 , pch = 20, cex = 1.2)
plots[[20]] <- stripplot(tempData,P139 , pch = 20, cex = 1.2)
plots[[21]] <- stripplot(tempData,P140 , pch = 20, cex = 1.2)

grid.arrange(grobs=plots[1:9], ncol=3)
grid.arrange(grobs=plots[10:17], ncol=3)
grid.arrange(grobs=plots[18:21], ncol=3)

bd_fa2c <- complete(tempData,1)
summary(bd_fa2c)
missmap(bd_fa2c,y.labels=NULL,y.at=NULL)

write.csv(bd_fa2c,"C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/bd_imputada.csv")
saveRDS(bd_fa2c,"C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/bd_imputada.rds")

# Be-she-za!!!
