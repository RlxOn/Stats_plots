############ Visualizacion estadistica de variables y pruebas ###################
pacman::p_load(ggplot2, gridExtra, taylor, modeest, e1071, ggcorrplot, vcd, dplyr)

#Escribimos una funcion que tome como argumentos un dataframe y una variable a analizar
var_extract <- function(dataframe, variable){
  return(dataframe[[variable]])
}

#Escribimos una funcion que nos de las estadisticas de la variable
statcats <- function(x){
  #Creamos un df que regrese min, max, media, mediana, moda, sd
  minimo <- round(min(x, na.rm = TRUE), 4)
  maximo <- round(max(x, na.rm = TRUE), 4)
  media <- round(mean(x, na.rm = TRUE), 4)
  mediana <- round(median(x, na.rm = TRUE), 4)
  ds <- round(sd(x, na.rm = TRUE),4)
  moda <- round(mlv(x, na.rm = TRUE, method = "meanshift")[1],4)
  sesgo <- round(skewness(x), 4)
  curtosis <- round(kurtosis(x),4)
  
  Estadisticas <- c("Minimo", "Maximo", "Media", "Mediana", "Moda", "Desv. est.",
                    "Sesgo", "Curtosis")
  Valores <- c(minimo, maximo, media, mediana, moda, ds, sesgo, curtosis)
  stats <- data.frame(Estadisticas, Valores) %>%
    as_tibble()
  
  return(stats)
}


#Construimos la funcion que toma el df y una variable
analyze <- function(data, variable){
  # Verificamos si la variable está presente en el dataframe
  if (!variable %in% names(data)) {
    stop("La variable especificada no se encuentra en el dataframe.")
  }
  
  vr <- variable
  var <- var_extract(data, variable)
  
  #Caso 1. Variable numerica continua 
  if(class(var) == "numeric"){
    #Creamos el grafico de densidad
    density_plot <- ggplot(data = NULL, aes(x = var)) + geom_density(fill = "#2F4858") +
      theme_bw() + ylab(NULL) + xlab(NULL) + ggtitle(vr) 
    
    #Creamos el grafico de boxplot
    boxplot <- ggplot(data = NULL, aes(x = 0, y = var)) + geom_boxplot(col = "#800000") +
      theme_minimal() + xlab(NULL) + ylab(NULL) + scale_x_continuous(breaks = 0)
    
    #Combinamos los graficos en una unica figura
    combined_plot <- grid.arrange(density_plot, boxplot, ncol = 2)
    
    
    #Utilizamos la funcion statcats
    return(statcats(var))
    combined_plot
  }
  
  #Caso 2. Variable categorica
  if(class(var) == "factor"){
    # Crear el grafico de barras
    ts_pal <- color_palette(album_palettes$midnights, n = length(levels(var)))
    
    bar_plot <- ggplot(data = NULL, aes(x = var)) + geom_bar(fill = ts_pal) +
      theme_minimal() + ylab(NULL) + xlab(NULL) + ggtitle(vr)
    
    # Crear el grafico de pastel
    new_tab <- as.data.frame(table(var)) %>%
      setNames(c("Category", "Freq"))
    
    
    # Creamos el grafico de pastel
    pie_plot <- ggplot(new_tab, aes(x = "", y = Freq, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) + theme_void()
    
    #Hacemos el arreglo de ambos
    grid_plot <- grid.arrange(bar_plot, pie_plot, ncol = 2)
    
    #Creamos la tabla de valores para mostrar
    tabla <- table(var)
    
    return(tabla)
    grid_plot
  }
  
  #Caso 3. Variable discreta
  if(class(var) == "integer"){
    #Creamos el grafico de densidad
    bar_plot <- ggplot(data = NULL, aes(x = var)) + geom_bar(fill = "#2F4858") +
      theme_bw() + ylab(NULL) + xlab(NULL) + ggtitle(vr)
    
    #Creamos el grafico de boxplot
    boxplot <- ggplot(data = NULL, aes(x = 0, y = var)) + geom_boxplot(col = "#800000") +
      theme_minimal() + xlab(NULL) + ylab(NULL) + scale_x_continuous(breaks = 0)
    
    #Combinamos los graficos en una unica figura
    combined_plot <- grid.arrange(bar_plot, boxplot, ncol = 2)
    
    #Utilizamos la funcion statcats
    return(statcats(var))
    combined_plot
    
  }
}

#Construimos una funcion que muestre la correlacion entre variables
correlation <- function(data, variable1, variable2){
  # Verificamos si la variable está presente en el dataframe
  if (!variable1 %in% names(data) | !variable2 %in% names(data)) {
    stop("Alguna variable especificada no se encuentra en el dataframe.")
  }
  
  vr1 <- variable1
  vr2 <- variable2
  
  var1 <- var_extract(data, variable1)
  var2 <- var_extract(data, variable2)
  
  #Caso 1. Ambas variables son numericas
  if(class(var1) %in% c("numeric", "integer") &
     class(var2) %in% c("numeric", "integer")){
    
    #Creamos un df auxiliar
    df <- data.frame(var1, var2)
    names(df) <- c(vr1, vr2)
    
    #Creamos un df de los coeficientes y p-values
    methods <- c("pearson", "kendall", "spearman")
    
    coefs <- c()
    p_values <- c()
    
    for (i in methods) {
      aux <- cor.test(var1,var2, alternative = "two.sided", method = i)
      
      coefs <- append(coefs, aux$estimate)
      p_values <- append(p_values, aux$p.value)
    }
    
    cor_tests <- data.frame(coefs, p_values)
    rownames(cor_tests) <- c("Pearson", "Kendall", "Spearman")
    
    points_plot <- ggplot(df, aes(x = var1, y = var2)) + 
      geom_point(size = 3, col = "#042760") + xlab(vr1) + ylab(vr2) + theme_minimal()
    
    cor_plot <- ggcorrplot(cor(df), hc.order = T, type = "full", method = "square",
               colors = c("#042760","white","#98073e"))
    
    combine_plot <- grid.arrange(points_plot, cor_plot, ncol =2)
    
    combine_plot
    
    return(cor_tests)
    
  }
  
  
  #Caso 2. Ambas variables son categoricas
  if(class(var1) == "factor" & class(var2) == "factor"){
    
    #Creamos la tabla de contingencia
    tabla <- table(var1, var2)
    
    print(
    ggplot(as.data.frame(tabla), aes(x = var1, y = Freq, fill = var2)) + 
      geom_bar(stat = "identity") + xlab(vr1) + ylab("") + scale_y_continuous(breaks = 0) +
      theme_minimal() + labs(fill = vr2))
    
    cat("Prueba Ji cuadrada", "\n")
    
    #Prueba ji-cuadrada
    ji_test <- chisq.test(tabla)
    
    if(ji_test$p.value < 0.05){
      cat("p-value = ", ji_test$p.value, "\n")
      cat("Existe asociacion entre las variables", "\n")
    }else{
      cat("p-value = ", ji_test$p.value, "\n")
      cat("No existe asociacion entre las variables", "\n")
    }
    
    #Prueba exacta de Fisher
    cat("\n")
    cat("Prueba exacta de Fisher", "\n")
    f_test <- fisher.test(tabla)
    
    if(f_test$p.value < 0.05){
      cat("p-value = ", f_test$p.value, "\n")
      cat("Existe asociacion entre las variables", "\n")
    }else{
      cat("p-value = ", f_test$p.value, "\n")
      cat("No existe asociacion entre las variables", "\n")
    }
    
    #Coeficientes de asociacion
    coef_as <- assocstats(tabla)
    
    cat("\n")
    cat("Coeficientes de asociación", "\n")
    
    coef_as <- data.frame(
      Coeficiente = c("Phi", "Contigencia", "V de Cramer"),
      Valores = c(coef_as$phi, coef_as$contingency, coef_as$cramer))
    
    print(coef_as)
    
    list_factor <- list(ji_test, f_test, coef_as)
    names(list_factor) <- c("Ji_cuadrada", "Fisher", "Coeficientes")
    
    return(list_factor)
  }

  
  #Caso 3. Variable numerica y variable categorica
  if((class(var1) %in% c("integer", "numeric") & class(var2) == "factor") |
     (class(var2) %in% c("integer", "numeric") & class(var1) == "factor")){
    
    if(class(var1) %in% c("integer", "numeric")){
      
      kw_test <- kruskal.test(x = var1, g = var2)
      anova_t <- aov(var1 ~ var2)
      anova_test <- broom::tidy(anova_t)
      
      tukey_result <- TukeyHSD(anova_t)
      
      tukey_df <- as.data.frame(tukey_result$var2)
      
      #Kruskal-Wallis
      cat("Kruskal-Wallis", "\n")
      
      if(kw_test$p.value < 0.05){
        cat("p-value = ", kw_test$p.value, "\n")
        print("Existen diferencias significativas entre grupos")
      }else{
        cat("p-value = ", kw_test$p.value, "\n")
        print("No existen diferencias significativas entre grupos")
      }
      
      dens_plot <- ggplot(data = NULL, aes(x = var1, fill = var2)) + geom_density(alpha = 0.6) + 
          theme_minimal() + xlab(vr1) + labs(fill = "Grupos") + ylab(NULL)
      
      #ANOVA
      cat("ANOVA", "\n")
      
      if(anova_test$p.value[1] < 0.05){
        cat("p-value = ", anova_test$p.value[1], "\n")
        print("Existen diferencias significativas entre grupos")
      }else{
        cat("p-value = ", anova_test$p.value[1], "\n")
        print("No existen diferencias significativas entre grupos")
      }
      
      int_plot <- ggplot(tukey_df, aes(y = reorder(rownames(tukey_df), diff), x = diff, xmin = lwr, xmax = upr)) + 
          geom_errorbar(linewidth = 2, width = 0.2) + geom_pointrange(size = 1, col = "#800000") + 
          labs(x = "Diferencia media", y = NULL) + theme_minimal()
      
      print(
        grid.arrange(dens_plot, int_plot, ncol = 2)
      )
      
    }
    
    if(class(var2) %in% c("integer", "numeric")){
      
      kw_test <- kruskal.test(x = var2, g = var1)
      anova_t <- aov(var2 ~ var1)
      anova_test <- broom::tidy(anova_t)
      
      tukey_result <- TukeyHSD(anova_t)
      
      tukey_df <- as.data.frame(tukey_result$var1)
      
      #Kruskal-Wallis
      cat("Kruskal-Wallis", "\n")
      
      if(kw_test$p.value < 0.05){
        cat("p-value = ", kw_test$p.value, "\n")
        print("Existen diferencias significativas entre grupos")
      }else{
        cat("p-value = ", kw_test$p.value, "\n")
        print("No existen diferencias significativas entre grupos")
      }
      
      dens_plot <- ggplot(data = NULL, aes(x = var2, fill = var1)) + geom_density(alpha = 0.6) + 
          theme_minimal() + xlab(vr2) + labs(fill = "Grupos") + ylab(NULL)
      
      #ANOVA
      cat("ANOVA", "\n")
      
      if(anova_test$p.value[1] < 0.05){
        cat("p-value = ", anova_test$p.value[1], "\n")
        print("Existen diferencias significativas entre grupos")
      }else{
        cat("p-value = ", anova_test$p.value[1], "\n")
        print("No existen diferencias significativas entre grupos")
      }
      
      int_plot <- ggplot(tukey_df, aes(y = reorder(rownames(tukey_df), diff), x = diff, xmin = lwr, xmax = upr)) + 
          geom_errorbar(linewidth = 2, width = 0.2) + geom_pointrange(size = 1, col = "#800000") + 
          labs(x = "Diferencia media", y = NULL) + theme_minimal()
      
      print(
        grid.arrange(dens_plot, int_plot, ncol = 2)
      )
    }
    
    ak_list <- list(kw_test, anova_t)
    names(ak_list) <- c("KW_Test", "ANOVA_Test")
    return(ak_list)
  }
}
