### inicializa la grafica
v_dim <- 0:8
v_prop <- seq(0.4, 1, len = 10)
v_prop[5] <- 0.62
a <- 1
b <- a * v_prop
inicializar_grafica <- function() {
  par(pty = "s", mar = c(2,2,3,2), cex = 1.2)
  plot(
    v_dim,
    v_dim,
    asp = 1,
    type = "n",
    axes = FALSE,
    ylab = NA,
    xlab = NA,
    main = "Escoge tu rectángulo favorito"
  )
}

agregar_rectangulos <- function() {
  list_puntos <- expand.grid(v_dim, v_dim)
  list_puntos <- list_puntos * 2
  vl_filtro <- list_puntos$Var1 < 8 & list_puntos$Var2 < 8
  list_x_izq <- list_puntos[vl_filtro,]
  list_otros <- list_puntos[!vl_filtro,]
  # points(list_x_izq, col = "red")
  # points(list_otros, col = "blue")
  v_indices <- sample(1:nrow(list_x_izq), 10)
  list_elegidos <- list_x_izq[v_indices,]
  
  rect(
    list_elegidos$Var1,
    list_elegidos$Var2,
    list_elegidos$Var1 + a,
    list_elegidos$Var2 + b
  )
  list_elegidos
}
####

agregar_etiquetas <- function(list_elegidos) {
  v_etiquetas <- sample(1:(nrow(list_elegidos)), 10)
  df_res <- data.frame(prop = v_prop, etiquetas = v_etiquetas)
  
  text(list_elegidos$Var1 + a / 2,
       list_elegidos$Var2 + b / 2,
       labels = v_etiquetas,
       col = "gray")
  df_res
}
recuperar_respuesta <- function(df_res, p_ele) {
  # recuperar respuesta
  # dataframe para recuperar proporcion de rectangulo elegido
  n_ele <- p_ele
  renglon_ele <- which(df_res$etiquetas == n_ele)
  prop_ele <- df_res[renglon_ele, 1]
  prop_ele
}
