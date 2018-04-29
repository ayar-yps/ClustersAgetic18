######################################
# Concurso Agetic - Modelación
# Autor: Ayar Paco
######################################

library(clustMixType)
library(randomForest)
library(party)

data <- readRDS("C:/Users/AYAR/Google Drive/Club de Ciencia de Datos/Bases de Datos/Encuesta Agetic/bd_imputada.rds")

summary(data)

# Quitar columnas que dificultan o no aportan valor a la modelación
colnames(data)[c(1,17,57,59,60)]
input <- data[,-c(1,17,57,58,59,60)]

###############
# K-Prototypes 
###############

# Por fines de interpretación se proponen 3 conglomerados
clust.mod <- kproto(input,3)

summary(clust.mod)

#############################################
# Preparar datos para seleccionar variables 
# tomando un enfoque supervisado
#############################################

new_data <- input
new_data$cluster <- as.factor(clust.mod$cluster)
summary(new_data)

train = sample(1: nrow(new_data), nrow(new_data)/2)

############################
# Random Forest
############################

rf.mod <- randomForest(cluster ~ ., data = new_data[,-55], subset=train,
                       importance=TRUE, ntree=1000)

vi.rf.mod <- rf.mod$importance[,4]
dotchart(vi.rf.mod[order(vi.rf.mod)],main = "RF - Important Variables",xlab = "MeanDecreaseAccuracy")

clust.rf.hat <- predict(rf.mod,newdata = new_data[-train,-55])

t(t(vi.rf.mod[vi.rf.mod>0.01][order(vi.rf.mod[vi.rf.mod>0.01])]))

table(clust.rf.hat,new_data[-train,-55]$cluster)

#############################
# Conditional Random Forest
#############################

crf.mod <- cforest(cluster ~ ., data = new_data, subset = train,
                   controls=cforest_unbiased(ntree=2000, mtry=7))

vi.crf.mod  <- varimp(crf.mod)

dotchart(vi.crf.mod[order(vi.crf.mod)],main = "CRF - Important Variables",xlab = "MeanDecreaseAccuracy")

t(t(vi.crf.mod[vi.crf.mod>0.01]))

clust.crf.hat <- predict(crf.mod,newdata = new_data[-train,])

table(clust.crf.hat,new_data[-train,]$cluster)

######################################################
# Nuevos conglomerados tomando variables importantes
######################################################

ivar <- names(vi.rf.mod[vi.rf.mod>0.01])
c_input_f <- new_data[,ivar]
clust.mod_f <- kproto(c_input_f,3)
summary(clust.mod_f)

clprofiles(clust.mod_f,c_input_f)

####################################################
# Verificar comportamiento aplicando RF nuevamente
####################################################

new_data_f <- c_input_f
new_data_f$cluster <- as.factor(clust.mod_f$cluster)

rf.mod_f <- randomForest(cluster ~ ., data = new_data_f, subset=train,
                       importance=TRUE, ntree=1000)

clust.rf.hat_f <- predict(rf.mod_f,newdata = new_data_f[-train,])

table(clust.rf.hat_f,new_data_f[-train,]$cluster)
