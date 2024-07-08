library(corrr) #matriz correlación
library(corrplot) #gráfica mátriz correlación
library(factoextra) #Métodos de obtención del número de clusters
library(fclust) #C-MEANS C-MEDOIDS
library(cluster) # K-MEDOIDS
library(GGally) #GGPAIRS
library(NbClust) #fviz_nbclust
library(plotly) # gráficas 3d
library(Kmedians) #k-medians
library(klaR) #k-modes
library(cSEM) #c-sem
library(clustMixType) #kproto
library(FactoMineR) #HCPC y FAMD y PCA
library(clValid) #Dunn index
library(knitr) #kable
library(geocmeans) #fukuyama-sugeno
library(ggplot2)
library(readxl)
df <- na.omit(read_excel("C:/Users/lagon/Desktop/TFT/BD.xlsx"))
set.seed(12)  #reproducibilidad
df$V07[df$V07 == 2] <- 1

# -----------------------------------------------------------------------------
# Análisis de Componentes Principales (PCA)
# -----------------------------------------------------------------------------

# 1. PCA - MIEDO RUINA
# --------------------
# Creación del dataframe con las variables seleccionadas
MR <- data.frame(
  MR1 = df$V61,
  MR2 = df$V62,
  MR3 = df$V63
)
pca <- PCA(X = MR, scale.unit=T, graph=T)
summary(pca)
MR$Factor1 <- pca$ind$coord[,1]
MR$Factor1 <- round((MR$Factor1 - min(MR$Factor1))/(max(MR$Factor1) - min(MR$Factor1)),4)
corrplot(cor(MR), method = "shade", tl.col = "black",addCoef.col = "black",
         col = colorRampPalette(c("red", "#a0f0a0", "#046307"))(50))


# 2. PCA - MIEDO GESTIÓN
# -----------------------

# Creación del dataframe con las variables seleccionadas
MG <- data.frame(
  MG1 = df$V58,
  MG2 = df$V59,
  MG3 = df$V60
)

pca <- PCA(X = MG, scale.unit=T, graph=T)
summary(pca)
MG$Factor1 <- pca$ind$coord[,1]
MG$Factor1 <- round((MG$Factor1 - min(MG$Factor1))/(max(MG$Factor1) - min(MG$Factor1)),4)
corrplot(cor(MG), method = "shade", tl.col = "black",addCoef.col = "black",
         col = colorRampPalette(c("red", "#a0f0a0", "#046307"))(50))


# 3. PCA - MIEDO FINANCIERO
# -------------------------

# Creación del dataframe con las variables seleccionadas
MF <- data.frame(
  MF1 = df$V46,
  MF2 = df$V47,
  MF3 = df$V48
)

pca <- PCA(X = MF, scale.unit=T, graph=T)
summary(pca)
MF$Factor1 <- pca$ind$coord[,1]
MF$Factor1 <- round((MF$Factor1 - min(MF$Factor1))/(max(MF$Factor1) - min(MF$Factor1)),4)
corrplot(cor(MF), method = "shade", tl.col = "black",addCoef.col = "black",
         col = colorRampPalette(c("red", "#a0f0a0", "#046307"))(50))



# 4. PCA - MIEDO PRODUCTO
# -----------------------

# Creación del dataframe con las variables seleccionadas
MP <- data.frame(
  MP1 = df$V49,
  MP2 = df$V50,
  MP3 = df$V51
)
pca <- PCA(X = MP, scale.unit=T, graph=T)
summary(pca)
MP$Factor1 <- pca$ind$coord[,1]
MP$Factor1 <- round((MP$Factor1 - min(MP$Factor1))/(max(MP$Factor1) - min(MP$Factor1)),4)
corrplot(cor(MP), method = "shade", tl.col = "black",addCoef.col = "black",
         col = colorRampPalette(c("red", "#a0f0a0", "#046307"))(50))


# ------------------------------------------------------------------------------
# AC1 - Miedo al fracaso (K-means, K-medoids, C-means, C-medoids)
# ------------------------------------------------------------------------------

dfClusterMiedos <- data.frame(
  MR = MR$Factor1,
  MG = MG$Factor1,
  MF = MF$Factor1,
  MP = MP$Factor1
)

# Clúster jerárquico
HCPC(dfClusterMiedos, min=2, max=5)


# 1. K-MEANS
# ----------
# a. Método del codo
elbow <- fviz_nbclust(dfClusterMiedos, kmeans, method = "wss") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Means - Miedo al fracaso",
       subtitle = "Método del Codo", 
       x = "Número de clusters", 
       y = "Suma de cuadrados inter-clúster (WSS)") + theme_light()
print(elbow)

# b. Método de la Silueta
silhouette <- fviz_nbclust(dfClusterMiedos, kmeans, method = "silhouette", linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Means - Miedo al fracaso",
       subtitle = "Método de la Silueta", 
       x = "Número de clusters", 
       y = "Ancho promedio de Silueta") + theme_light()
print(silhouette)

# c. Estadística Gap
gap <- fviz_nbclust(dfClusterMiedos, kmeans, method = "gap_stat", nboot = 100, linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Means - Miedo al fracaso",
       subtitle = "Método Estadística Gap", 
       x = "Número de clusters", 
       y = "Estadística Gap") + theme_light()
print(gap)

# d. K-means
set.seed(12)  #Ejecutar junto con el algoritmo k-means para reproducibilidad
kmMiedos <- kmeans(dfClusterMiedos, 2); kmMiedos$center

pieKmMiedos <- ggplot(data =data.frame(Cluster = factor(kmMiedos$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC1: K-Means", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white") +
  scale_fill_manual(values = c("#89CFF0", "#50C878"))
print(pieKmMiedos)

dispKmMiedos <- ggpairs(dfClusterMiedos, aes(color = as.factor(kmMiedos$cluster),alpha = 0.5),
        title = "Clúster K-Means Miedo al Fracaso") + scale_color_manual(values = c("#377eb8", "#50C878")) +
  scale_fill_manual(values = c("#377eb8", "#50C878")); print(dispKmMiedos)

dfClusterMiedosKmeans <- data.frame(cbind(dfClusterMiedos, cluster = kmMiedos$cluster))
par(mfrow=c(2, 2))
for (i in 1:4) {
  boxplot(dfClusterMiedosKmeans[,i] ~ dfClusterMiedosKmeans$cluster,
          main = paste("Distribución de", names(dfClusterMiedosKmeans)[i], "por Clúster"),
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# 2. K-MEDOIDS
# ------------

# a. Método del codo
elbow <- fviz_nbclust(dfClusterMiedos, pam, method = "wss") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Medoids - Miedo al fracaso",
       subtitle = "Método del Codo", 
       x = "Número de clusters", 
       y = "Suma de cuadrados inter-clúster (WSS)") + theme_light()
print(elbow)

# b. Método de la Silueta
silhouette <- fviz_nbclust(dfClusterMiedos, pam, method = "silhouette", linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Medoids - Miedo al fracaso",
       subtitle = "Método de la Silueta", 
       x = "Número de clusters", 
       y = "Ancho promedio de Silueta") + theme_light()
print(silhouette)

# c. Estadística Gap
gap <- fviz_nbclust(dfClusterMiedos, pam, method = "gap_stat", nboot = 100, linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Medoids - Miedo al fracaso",
       subtitle = "Método Estadística Gap", 
       x = "Número de clusters", 
       y = "Estadística Gap") + theme_light()
print(gap)

# d. K-medoids
set.seed(12)  #Ejecutar junto con el algoritmo k-medoids para reproducibilidad
kmedMiedos <- pam(dfClusterMiedos, k = 2); kmedMiedos$medoids

pieKmedMiedos <- ggplot(data =data.frame(Cluster = factor(kmedMiedos$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC1: K-Medoids", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white") +
  scale_fill_manual(values = c("#89CFF0", "#50C878"))
print(pieKmedMiedos)

dispKmedMiedos <- ggpairs(dfClusterMiedos, aes(color = as.factor(kmedMiedos$cluster), alpha = 0.5),
        title = "Clúster K-Medoids Miedo al Fracaso") + scale_color_manual(values = c("#377eb8", "#50C878")) +
  scale_fill_manual(values = c("#377eb8", "#50C878")); print(dispKmedMiedos)

dfClusterMiedosKmedoids <- data.frame(cbind(dfClusterMiedos, cluster = kmedMiedos$cluster))
par(mfrow=c(2, 2))
for (i in 1:4) {
  boxplot(dfClusterMiedosKmedoids[,i] ~ dfClusterMiedosKmedoids$cluster,
          main = paste("Distribución de", names(dfClusterMiedosKmedoids)[i], "por Clúster"),
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# 3. Función obtención Índicadores de fuzzificación
# -------------------------------------------------
fuzzyIndexTable <- function(data, method, fuzzyIndex, clusterNumber) {
  fuzzyTable <- data.frame(XB = numeric(), FS = numeric(), PC = numeric(), PE = numeric())
  set.seed(12)  #Reproducibilidad
  for (clusters in clusterNumber) {
    for (index in fuzzyIndex) {
      if (method == "c-means") {
        model <- FKM(data, clusters, index, 1)
      } else if (method == "c-medoids") {
        model <- FKM.med(data, clusters, index, 1)
      } else {
        stop("Esta función solo puede ser utilizada para 'c-means' o 'c-medoids'")
      }
      
      # Índices para el modelo c-means o c-medoids
      PC <- Fclust.index(model, "PC")
      PE <- Fclust.index(model, "PE")
      XB <- Fclust.index(model, "XB")
      FS <- calcFukuyamaSugeno(as.matrix(data), model$U, model$H, index)
      
      # Crear nombre de fila y añadir los resultados a fuzzyTable
      rowname <- paste(clusters, index, sep = "-")
      fuzzyTable <- rbind(fuzzyTable, setNames(data.frame(PC, PE, XB, FS), c("PC", "PE", "XB", "FS")))
      rownames(fuzzyTable)[nrow(fuzzyTable)] <- rowname
    }
  }
  return(fuzzyTable)
}

fuzzyIndex <- c(1.1,1.2,1.4,1.6,1.8,2) # Índices de fuzzificación
fuzzyClusterNumber <- c(2,3,4,5) # Número de clústeres

# 4. C-MEANS
# ----------
fuzzyIndexCmeans <- fuzzyIndexTable(dfClusterMiedos, "c-means", fuzzyIndex, fuzzyClusterNumber)
kable(fuzzyIndexCmeans)


#Gráfica Coeficiente de Partición
valIndice <- matrix(fuzzyIndexCmeans$PC, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(
    title = 'Coeficiente de Partición (PC) - AC1: C-Means',
    scene = list(
      xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
      yaxis = list(title = 'Borrosidad'),
      zaxis = list(title = 'Coeficiente de Partición')))

#Gráfica Entropía de Partición
valIndice <- matrix(fuzzyIndexCmeans$PE, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(
    title = 'Entropía de Partición (PE) - AC1: C-Means',
    scene = list(
      xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
      yaxis = list(title = 'Borrosidad'),
      zaxis = list(title = 'Entropía de Partición')))

#Gráfica Xie-Beni
valIndice <- matrix(fuzzyIndexCmeans$XB, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(title = 'Índice Xie-Beni (XB) - AC1: C-Means',
    scene = list(
      xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
      yaxis = list(title = 'Borrosidad'),
      zaxis = list(title = 'Índice Xie-Beni')))

#Gráfica Fukuyama-Sugeno
valIndice <- matrix(fuzzyIndexCmeans$FS, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(title = 'Índice Fukuyama y Sugeno (FS) - AC1: C-Means',
         scene = list(
           xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
           yaxis = list(title = 'Borrosidad'),
           zaxis = list(title = 'Índice Fukuyama-Sugeno')))


#Clúster C-Means
set.seed(12)  #Ejecutar junto con el algoritmo c-means para reproducibilidad
cmeansMiedos <- FKM(dfClusterMiedos, 2, 1.1, 1)

pieCmMiedos <- ggplot(data =data.frame(Cluster = factor(cmeansMiedos$clus[,1])), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC1: C-Means", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white") +
  scale_fill_manual(values = c("#89CFF0", "#50C878"))
print(pieCmMiedos)

dispCmMiedos <- ggpairs(dfClusterMiedos, aes(color = as.factor(cmeansMiedos$clus[,1]), alpha = 0.5),
                          title = "Clúster C-Means Miedo al Fracaso") +
  scale_color_manual(values = c("#377eb8", "#50C878")) +
  scale_fill_manual(values = c("#377eb8", "#50C878")); print(dispCmMiedos)

dfClusterMiedosCm <- data.frame(cbind(dfClusterMiedos, cluster = cmeansMiedos$clus[,1]))
par(mfrow=c(2, 2))
for (i in 1:4) {
  boxplot(dfClusterMiedosCm[,i] ~ dfClusterMiedosCm$cluster,
          main = paste("Distribución de", names(dfClusterMiedosCm)[i], "por Clúster"),
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# 5. C-MEDOIDS
# ------------
fuzzyIndexCmedoids <- fuzzyIndexTable(dfClusterMiedos, "c-medoids", fuzzyIndex, fuzzyClusterNumber)
kable(fuzzyIndexCmedoids)

#Gráfica Coeficiente de Partición
valIndice <- matrix(fuzzyIndexCmedoids$PC, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(
    title = 'Coeficiente de Partición (PC) - AC1: C-Medoids',
    scene = list(
      xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
      yaxis = list(title = 'Borrosidad'),
      zaxis = list(title = 'Coeficiente de Partición')))

#Gráfica Entropía de Partición
valIndice <- matrix(fuzzyIndexCmedoids$PE, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(
    title = 'Entropía de Partición (PE) - AC1: C-Medoids',
    scene = list(
      xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
      yaxis = list(title = 'Borrosidad'),
      zaxis = list(title = 'Entropía de Partición')))

#Gráfica Xie-Beni
valIndice <- matrix(fuzzyIndexCmedoids$XB, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(title = 'Índice Xie-Beni (XB) - AC1: C-Medoids',
         scene = list(
           xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
           yaxis = list(title = 'Borrosidad'),
           zaxis = list(title = 'Índice Xie-Beni')))

#Gráfica Fukuyama-Sugeno
valIndice <- matrix(fuzzyIndexCmedoids$FS, nrow = length(fuzzyIndex))
plot_ly(x=~fuzzyClusterNumber, y=~fuzzyIndex ,z =~valIndice, type = "surface") %>% 
  layout(title = 'Índice Fukuyama-Sugeno (FS) - AC1: C-Medoids',
         scene = list(
           xaxis = list(title = 'Número de Clusters', tickvals = 2:length(fuzzyIndex)),
           yaxis = list(title = 'Borrosidad'),
           zaxis = list(title = 'Índice Fukuyama-Sugeno')))


#Clúster C-Medoids
set.seed(12) #Ejecutar junto con el algoritmo c-medoids para reproducibilidad
cmedoidsMiedos <- FKM.med(dfClusterMiedos, k=2, 1.1, 1)

pieCmedMiedos <- ggplot(data =data.frame(Cluster = factor(cmedoidsMiedos$clus[,1])), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC1: C-Medoids", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white") +
  scale_fill_manual(values = c("#89CFF0", "#50C878"))
print(pieCmedMiedos)

dispCmedMiedos <- ggpairs(dfClusterMiedos, aes(color = as.factor(cmedoidsMiedos$clus[,1]), alpha = 0.5),
                        title = "Clúster C-Medoids Miedo al Fracaso") +
  scale_color_manual(values = c("#377eb8", "#50C878")) +
  scale_fill_manual(values = c("#377eb8", "#50C878")); print(dispCmedMiedos)

dfClusterMiedosCmed <- data.frame(cbind(dfClusterMiedos, cluster = cmedoidsMiedos$clus[,1]))
par(mfrow=c(2, 2))
for (i in 1:4) {
  boxplot(dfClusterMiedosCmed[,i] ~ dfClusterMiedosCmed$cluster,
          main = paste("Distribución de", names(dfClusterMiedosCmed)[i], "por Clúster"),
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# ------------------------------------------------------------------------------
# AC2 - Género y P.Ingenieros - Var. categoricas (K-modes)
# ------------------------------------------------------------------------------
dfGenPIng <- data.frame(
  genero = df$V02,
  padresIngenieros = df$V07
)

# a. Clúster jerárquico
HCPC(dfGenPIng, min=2, max=5)

# b. Método de la Silueta
silhouette <- fviz_nbclust(dfGenPIng, kmodes, method = "silhouette",k.max=4, linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Modes - AC2: Género y P.Ingenieros",
       subtitle = "Método de la Silueta", 
       x = "Número de clusters", 
       y = "Ancho promedio de Silueta") + theme_light()
print(silhouette)

calcularDunn <- function(data, distance_matrix, range) {
  set.seed(12) # Reproducibilidad
  dunn_indices <- numeric(length(range))
  for (i in seq_along(range)) {
    kmodes_result <- kmodes(data, modes = range[i])
    dunn_indices[i] <- dunn(distance_matrix, kmodes_result$cluster)
  }
  indexResults <- data.frame(
    k = range,
    Dunn = dunn_indices
  )
  pDunn <- ggplot(data = indexResults, aes(x = k, y = Dunn)) +
    geom_line(color = "#50C878") + geom_point(shape = 19, color = "#046307") + 
    theme_light() +
    labs(x = "Número de clusters", y = "Valor Índice Dunn", title = "Índice de Dunn")
  print(pDunn)
  return(dunn_indices)
}

gower_dist <- daisy(dfGenPIng, metric = "gower")
dunn <- calcularDunn(dfGenPIng, gower_dist, 2:4)


# 1. K-MODES (4 clústeres)
set.seed(12) #Ejecutar junto con el algoritmo k-modes para reproducibilidad
kmodesGenPIng <- kmodes(dfGenPIng, 4)

pieKmodGenPIng1 <- ggplot(data =data.frame(Cluster = factor(kmodesGenPIng$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC2: K-Modes (4 clústeres)", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmodGenPIng1)

# 2. K-MODES (3 clústeres)
set.seed(12) #Ejecutar junto con el algoritmo k-modes para reproducibilidad
kmodesGenPIng <- kmodes(dfGenPIng, 3)

pieKmodGenPIng2 <- ggplot(data =data.frame(Cluster = factor(kmodesGenPIng$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC2: K-Modes (3 clústeres)", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmodGenPIng2)

dfClusterKmodesGenPIng <- data.frame(cbind(dfGenPIng, cluster = kmodesGenPIng$cluster))
par(mfrow=c(1, 2))
for (i in 1:2) {
  boxplot(dfClusterKmodesGenPIng[,i] ~ dfClusterKmodesGenPIng$cluster,
          main = names(dfClusterKmodesGenPIng)[i],
          xlab = "Clúster",
          ylab = "Valor",
          border = "darkblue")
}
par(mfrow=c(1, 1))

dispKmodGenPIng <- ggpairs(dfGenPIng, aes(color = as.factor(kmodesGenPIng$cluster), alpha = 0.5),
                           title = "Clúster K-Modes Género y P.Ingenieros")
print(dispKmodGenPIng)
# ------------------------------------------------------------------------------
# AC3 - P.Ingenieros y F.Empresario - Var. categoricas (K-modes)
# ------------------------------------------------------------------------------
dfPIFE <- data.frame(
  padresIngenieros = df$V07,
  famEmpresario = df$V08
)

# a. Clúster jerárquico
HCPC(dfPIFE, kk=Inf, min=2, max=5)

# b. Método de la Silueta
silhouette <- fviz_nbclust(dfPIFE, kmodes, method = "silhouette",k.max=4, linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Modes - AC3: P.Ingenieros y F.Empresario",
       subtitle = "Método de la Silueta", 
       x = "Número de clusters", 
       y = "Ancho promedio de Silueta") + theme_light()
print(silhouette)

# c. Índice de Dunn
gower_dist <- daisy(dfPIFE, metric = "gower")
dunn <- calcularDunn(dfPIFE, gower_dist, 2:4)

# 1. K-MODES (4 clústeres)
set.seed(12) #Ejecutar junto con el algoritmo k-modes para reproducibilidad
kmodesPIFE <- kmodes(dfPIFE, 4)
pieKmod3 <- ggplot(data =data.frame(Cluster = factor(kmodesPIFE$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC3: K-Modes (4 clústeres)", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmod3)

# 1. K-MODES (3 clústeres)
set.seed(12) #Ejecutar junto con el algoritmo k-modes para reproducibilidad
kmodesPIFE <- kmodes(dfPIFE, 3)

pieKmod4 <- ggplot(data =data.frame(Cluster = factor(kmodesPIFE$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC3: K-Modes (3 clústeres)", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmod4)

dispKmodPIFE <- ggpairs(dfPIFE, aes(color = as.factor(kmodesPIFE$cluster), alpha = 0.5),
                           title = "Clúster K-Modes P.Ingenieros y F.Empresario")
print(dispKmodPIFE)

dfClusterkmodesPIFE <- data.frame(cbind(dfPIFE, cluster = kmodesPIFE$cluster))
par(mfrow=c(1, 2))
for (i in 1:2) {
  boxplot(dfClusterkmodesPIFE[,i] ~ dfClusterkmodesPIFE$cluster,
          main = names(dfClusterkmodesPIFE)[i],
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# ------------------------------------------------------------------------------
# AC4 - Género y F.Empresario - Var. categoricas (K-modes)
# ------------------------------------------------------------------------------
dfGenFE <- data.frame(
  genero = df$V02,
  famEmpresario = df$V08
)

# a. Clúster jerárquico
HCPC(dfGenFE, kk=Inf, min=2, max=5)

# b. Método de la Silueta
silhouette <- fviz_nbclust(dfGenFE, kmodes, method = "silhouette",k.max=4, linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(title = "Número óptimo de clústeres K-Modes - AC4: Género y F.Empresario",
       subtitle = "Método de la Silueta", 
       x = "Número de clusters", 
       y = "Ancho promedio de Silueta") + theme_light()
print(silhouette)

# c. Índice de Dunn
gower_dist <- daisy(dfGenFE, metric = "gower")
dunn <- calcularDunn(dfGenFE, gower_dist, 2:4)

# 1. K-MODES
set.seed(12) #Ejecutar junto con el algoritmo k-modes para reproducibilidad
kmodesGenFE <- kmodes(dfGenFE, 4)

pieKmod5 <- ggplot(data =data.frame(Cluster = factor(kmodesGenFE$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC4: K-Modes", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmod5)

dispKmodGenFE <- ggpairs(dfGenFE, aes(color = as.factor(kmodesGenFE$cluster), alpha = 0.5),
                           title = "Clúster K-Modes Género y F.Empresario")
print(dispKmodGenFE)

dfClusterkmodesGenFE <- data.frame(cbind(dfGenFE, cluster = kmodesGenFE$cluster))
par(mfrow=c(1, 2))
for (i in 1:2) {
  boxplot(dfClusterkmodesGenFE[,i] ~ dfClusterkmodesGenFE$cluster,
          main = names(dfClusterkmodesGenFE)[i],
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))

# ------------------------------------------------------------------------------
# AC5 - Género, P.Ingenieros, F.Empresario y Miedo al fracaso - Var. mixtas (K-prototypes)
# ------------------------------------------------------------------------------
dfKproto1 <- data.frame(
  genero = as.factor(df$V02),
  padresIngenieros = as.factor(df$V07),
  famEmpresario = as.factor(df$V08),
  MR = MR$Factor1,
  MF = MF$Factor1,
  MP = MP$Factor1,
  MG = MG$Factor1
)

calcKprotoIndex <- function(df, range) {
  set.seed(12) #reproducibilidad
  IndSilh <- numeric(length(range))
  IndMcClain <- numeric(length(range))
  IndCIndex <- numeric(length(range))
  IndDunn <- numeric(length(range))
  
  for (i in seq_along(range)) {
    kpres <- kproto(df, k = range[i])
    IndSilh[i] <- validation_kproto("silhouette", kpres)
    IndMcClain[i] <- validation_kproto("mcclain", kpres)
    IndCIndex[i] <- validation_kproto("cindex", kpres)
    IndDunn[i] <- validation_kproto("dunn", kpres)
  }
  indexResults <- data.frame(
    k = range,
    Silhouette = IndSilh,
    McClain = IndMcClain,
    Cindex = IndCIndex,
    Dunn = IndDunn
  )
  
  return(indexResults)
}

indexResults <- calcKprotoIndex(dfKproto1, 2:5); indexResults

# Silhouette
pIndSilh <- ggplot(data = data.frame(indexResults$k, indexResults$Silhouette),
                   aes(x = indexResults$k, y = indexResults$Silhouette), linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(x = "Número de clusters", y = "AC5: Índice Silhouette",
       title = "AC5: Silhouette - K-Prototypes") + theme_light()
print(pIndSilh)

# McClain
pIndMcClain <- ggplot(data = data.frame(indexResults$k, indexResults$McClain),
                      aes(x = indexResults$k, y = indexResults$McClain), linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(x = "Número de clusters", y = "Índice McClain",
       title = "AC5: McClain - K-Prototypes") + theme_light()
print(pIndMcClain)

# c-Index
pIndCIndex <- ggplot(data = data.frame(indexResults$k, indexResults$Cindex),
                     aes(x = indexResults$k, y = indexResults$Cindex), linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(x = "Número de clusters", y = "Índice C-index",
       title = "AC5: C-index - K-Prototypes") + theme_light()
print(pIndCIndex)

# Dunn
pIndDunn <- ggplot(data = data.frame(indexResults$k, indexResults$Dunn),
                   aes(x = indexResults$k, y = indexResults$Dunn), linecolor = "#003366") +
  geom_line(aes(group = 1), color = "#50C878") + geom_point(color = "#046307") +
  labs(x = "Número de clusters", y = "Índice Dunn",
       title = "AC5: Dunn - K-Prototypes") + theme_light()
print(pIndDunn)

# K-PROTOTYPES
set.seed(12) #Ejecutar junto con el algoritmo k-prototypes para reproducibilidad
kproto1 <- kproto(dfKproto1, k = 6, cat.vars = c("genero", "padresIngenieros", "famEmpresario"))

pieKmod6 <- ggplot(data =data.frame(Cluster = factor(kproto1$cluster)), aes(x = "", fill = Cluster)) +
  geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + theme_void() + 
  labs(title = "Distribución de datos por Clúster AC5: K-Prototypes", fill = "Clúster", x = NULL, y = NULL) +
  geom_text(aes(label = scales::percent(after_stat(count/sum(count)))),
            position = position_stack(vjust = 0.5), stat = "count", color = "white")
print(pieKmod6)

dfClusterkproto <- data.frame(cbind(dfKproto1, cluster = kproto1$cluster))
par(mfrow=c(3, 3))
for (i in 1:7) {
  boxplot(dfClusterkproto[,i] ~ dfClusterkproto$cluster,
          main = names(dfClusterkproto)[i],
          xlab = "Clúster",
          ylab = "Valor",
          col = c("lightblue", "lightgreen"),
          border = "darkblue")
}
par(mfrow=c(1, 1))


# ------------------------------------------------------------------------------
# PLS-SEM - MODELO ESTRUCTURAL ESTÁNDAR
# ------------------------------------------------------------------------------
modelcsem <- "
# Structural model
Actitud ~ Autosuf
Intencion ~ Autosuf + Actitud

# Measurement models
Autosuf =~ V64 + V65 + V66 + V67 + V68 + V69
Actitud =~ V75 + V76 + V77
Intencion =~ V78 + V79 + V80
"

rawCsem <- csem(.data=df, .model=modelcsem, .disattenuate = F,
                .resample_method = 'bootstrap',.R = 5000, .seed=12)

verify(rawCsem)
summarize(rawCsem) #loadings, path estimates
assess(rawCsem)


# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC1: K-MEANS
# ------------------------------------------------------------------------------
df$AC1kmeans <- kmMiedos$cluster
kmMiedosCsem <- csem(.data=df, .model=modelcsem,
                 .disattenuate = F,
                 .tolerance = 1e-5, .seed=12,
                 .resample_method = 'bootstrap',.R = 5000,
                 .id='AC1kmeans')

# MICOM
outMICOM <- testMICOM(kmMiedosCsem, .seed=12); outMICOM #verificado

# Análisis Multigrupo
outoverall=testMGD(.object = kmMiedosCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC1: K-MEDOIDS
# ------------------------------------------------------------------------------
df$AC1kmedoids <- kmedMiedos$cluster
kmedMiedosCsem <- csem(.data=df, .model=modelcsem,
                     .disattenuate = F,
                     .tolerance = 1e-5, .seed=12,
                     .resample_method = 'bootstrap',.R = 5000,
                     .id='AC1kmedoids')

# MICOM
outMICOM <- testMICOM(kmedMiedosCsem, .seed=12); outMICOM #verificado

# Análisis Multigrupo
outoverall=testMGD(.object = kmedMiedosCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC1: C-MEANS
# ------------------------------------------------------------------------------
df$AC1cmeans <- cmeansMiedos$clus[,1]
cmMiedosCsem <- csem(.data=df, .model=modelcsem,
                       .disattenuate = F,
                       .tolerance = 1e-5, .seed=12,
                       .resample_method = 'bootstrap',.R = 5000,
                       .id='AC1cmeans')

# MICOM
outMICOM <- testMICOM(cmMiedosCsem, .seed=12); outMICOM #verificado

# Análisis Multigrupo
outoverall=testMGD(.object = cmMiedosCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC1: C-MEDOIDS
# ------------------------------------------------------------------------------
df$AC1cmedoids <- cmedoidsMiedos$clus[,1]
cmedMiedosCsem <- csem(.data=df, .model=modelcsem,
                     .disattenuate = F,
                     .tolerance = 1e-5, .seed=12,
                     .resample_method = 'bootstrap',.R = 5000,
                     .id='AC1cmedoids')

# MICOM
outMICOM <- testMICOM(cmedMiedosCsem, .seed=12); outMICOM #verificado

# Análisis Multigrupo
outoverall=testMGD(.object = cmedMiedosCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC2: K-MODES
# ------------------------------------------------------------------------------
df$AC2kmodes <- kmodesGenPIng$cluster
AC2kmodesCsem <- csem(.data=df, .model=modelcsem,
                   .disattenuate = F,
                   .tolerance = 1e-5, .seed=12,
                   .resample_method = 'bootstrap',.R = 5000,
                   .id='AC2kmodes')
# MICOM
outMICOM <- testMICOM(AC2kmodesCsem, .seed=12); outMICOM #verificado (menos grupo 1_2)

# Análisis Multigrupo
outoverall=testMGD(.object = AC2kmodesCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC3: K-MODES
# ------------------------------------------------------------------------------
df$AC3kmodes <- kmodesPIFE$cluster
AC3kmodesCsem <- csem(.data=df, .model=modelcsem,
                 .disattenuate = F,
                 .tolerance = 1e-5, .seed=12,
                 .resample_method = 'bootstrap',.R = 5000,
                 .id='AC3kmodes')
# MICOM
outMICOM <- testMICOM(AC3kmodesCsem, .seed=12); outMICOM #verificado (menos grupo 1_2)

# Análisis Multigrupo
outoverall=testMGD(.object = AC3kmodesCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC4: K-MODES
# ------------------------------------------------------------------------------
df$AC4kmodes <- kmodesGenFE$cluster
AC4kmodesCsem <- csem(.data=df, .model=modelcsem,
                  .disattenuate = F,
                  .tolerance = 1e-5, .seed=12,
                  .resample_method = 'bootstrap',.R = 5000,
                  .id='AC4kmodes')
# MICOM
outMICOM <- testMICOM(AC4kmodesCsem, .seed=12); outMICOM #verificado

# Análisis Multigrupo
outoverall=testMGD(.object = AC4kmodesCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall

# Se rechaza el MGA -> No hay diferencias significativas

# ------------------------------------------------------------------------------
# ANÁLISIS MULTIGRUPO - AC5: K-PROTOTYPES
# ------------------------------------------------------------------------------
df$AC5kproto <- kproto1$cluster
AC5kprotoCsem <- csem(.data=df, .model=modelcsem,
                   .disattenuate = F,
                   .tolerance = 1e-5, .seed=12,
                   .resample_method = 'bootstrap',.R = 5000,
                   .id='AC5kproto')
# MICOM
outMICOM <- testMICOM(AC5kprotoCsem, .seed=12); outMICOM #verificado (menos grupo 2_5)

# Análisis Multigrupo
outoverall=testMGD(.object = AC5kprotoCsem, .type_vcv='construct', .seed=12,
                   .approach_mgd = 'Chin'); outoverall
# Se acepta el MGA -> Hay diferencias significativas entre los grupos 2_3 y 2_4

summarize(AC5kprotoCsem) #path estimates
