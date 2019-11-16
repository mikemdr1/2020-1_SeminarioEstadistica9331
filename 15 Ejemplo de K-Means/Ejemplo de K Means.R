####################################################################################################
# K MEANS
####################################################################################################

####################################################################################################
# DATOS :: SOCIAL NETWORK ADS
####################################################################################################

DATOS <- read.csv("Mall_Customers.csv")

KMEDIAS <- kmeans(x = as.matrix(DATOS[,c(4,5)]), centers = 3 )
plot(x = DATOS[,4], y = DATOS[,5], pch=19, col=KMEDIAS$cluster ); grid()

### PARA VISUALIZAR LOS METODOS POR SEPARADO
set.seed(8198123)
METODOS     <- c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
K_SECUENCIA <- seq(from = 1, to = 20)
WCSS        <- vector(mode = "numeric",length = length(K_SECUENCIA))
par(mfrow=c(2,2))
for( M in METODOS ){
  for( K in K_SECUENCIA ) WCSS[K] <- kmeans(x = as.matrix(DATOS[,c(4,5)]), centers = K, iter.max = 10000, algorithm = M )$tot.withinss
  plot(x = K_SECUENCIA, y = WCSS, pch = 19, col="#1CACDB", main = M ); grid()
}
par(mfrow=c(1,1))


### PARA VISUALIZAR LOS METODOS EN LA MISMA GRAFICA
set.seed(8198123) # 8198123 | 123
METODOS     <- c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
K_SECUENCIA <- seq(from = 1, to = 20)
WCSS        <- vector(mode = "numeric",length = length(K_SECUENCIA))
MATRIZ_WCSS <- NULL
for( M in METODOS ){
  for( K in K_SECUENCIA ) WCSS[K] <- kmeans(x = as.matrix(DATOS[,c(4,5)]), centers = K, iter.max = 10000, algorithm = M )$tot.withinss
  MATRIZ_WCSS <- rbind(MATRIZ_WCSS, WCSS)
}
rownames(MATRIZ_WCSS) <- METODOS
plot(x = K_SECUENCIA, y = rep(0, length(K_SECUENCIA)), type = "n", main = "K-Means", ylim=c(0, max(MATRIZ_WCSS))); grid()
points(x = K_SECUENCIA, y = MATRIZ_WCSS[1,], pch=19, col=1)
points(x = K_SECUENCIA, y = MATRIZ_WCSS[2,], pch=19, col=2)
points(x = K_SECUENCIA, y = MATRIZ_WCSS[3,], pch=19, col=3)
points(x = K_SECUENCIA, y = MATRIZ_WCSS[4,], pch=19, col=4)
legend("topright",legend = METODOS, fill=c(1,2,3,4))

# MODELO SELECCIONADO
set.seed(123)
KMEDIAS <- kmeans(x = as.matrix(DATOS[,c(4,5)]), centers = 5, nstart = 100 )
plot(x = DATOS[,4], y = DATOS[,5], pch=19, col=KMEDIAS$cluster, main="K-Means", xlab="Annual Income", ylab="Spending Score" ); grid()
points(x = KMEDIAS$centers[,1], y = KMEDIAS$centers[,2], pch="*", col="purple", cex=3 )

# predict(object = KMEDIAS, newdata = as.matrix(DATOS[,c(4,5)]) ) NO ES POSIBLE!!!!

####################################################################################################
# DATOS :: SOCIAL NETWORK ADS
####################################################################################################

DATOS <- read.csv("Social_Network_Ads.csv")
DATOS$Color <- ifelse( DATOS$Purchased == 1, "red", "blue" )
plot( EstimatedSalary ~ Age, col = Color, pch = 19, cex = sqrt(2), data = DATOS ); grid()

KMEDIAS <- kmeans(x = as.matrix(DATOS[,c(3,4)]), centers = 2, iter.max = 10000, algorithm = "Forgy" )

par(mfrow=c(2,1))
plot( EstimatedSalary ~ Age, col = Color, pch = 19, cex = sqrt(2), data = DATOS ); grid()
plot( EstimatedSalary ~ Age, col = ifelse( KMEDIAS$cluster == 1, "red", "blue" ) , pch = 19, cex = sqrt(2), data = DATOS ); grid()
par(mfrow=c(1,1))

####################################################################################################
# DATOS :: IRIS
####################################################################################################



####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################