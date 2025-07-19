# 1. Crear los datos


X <- data.frame(
  Empleado = paste0("E", 1:10),
  Productividad = c(42, 38, 45, 39, 47, 41, 43, 80, 36, 44),
  Puntualidad   = c(2, 3, 1, 4, 1, 2, 2, 1, 5, 2),
  Calidad       = c(88, 85, 90, 83, 92, 87, 89, 60, 82, 88)
)

X <- X[,c(2:4)]

# 2. Calcular vector de medias
mu <- colMeans(X)

X_centered <- t(apply(X, 1, function(x) x - mu))
X_centered <- as.matrix(X_centered)


# # # 2. Crear matriz de medias repetidas usando multiplicación por una columna de unos
# unos <- matrix(1, nrow = nrow(X), ncol = 1)
# mu_matrix <- unos %*% t(mu)  # Resultado: matriz 5x3
# 
# # 3. Restar matriz a matriz
# X_centered <- X - mu_matrix



# 3. Calcular matriz de covarianza e inversa
S <- cov(X)
S_inv <- solve(S)

# 4. Calcular distancia de Mahalanobis manualmente (paso a paso)
dist_manual <- (X_centered) %*% S_inv %*% t(X_centered)
dist_manual<-(diag(dist_manual))

# 5. Calcular con la función base de R
dist_r <- mahalanobis(X, center = mu, cov = S)

# 6. Comparar resultados
resultado <- data.frame(
  Observacion = rownames(X),
  Dist_Mahal_Manual = dist_manual,
  Dist_Mahal_R       = dist_r,
  Diferencia = abs(dist_manual - dist_r)
)

print(resultado)

# Umbral usando el percentil 97.5 de chi-cuadrado con 3 grados de libertad
umbral <- qchisq(0.975, df = 3)

# Agregar al dataframe
resultado$dist_maha <- dist_r
resultado$Atipico <- ifelse(resultado$dist_maha > umbral, "Sí", "No")

# Mostrar resultado
print(resultado)
