#_______________________________________________________________________________

#SECCION DE FUNCIONES PARA EL CALCULO DEL INDICE

#funcion para obtener las variables de mes actual, mes anterior y meses del año
inputdata <- function(month_number) {
  month_names <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
  
  if (month_number < 1 || month_number > 12) {
    stop("Por favor ingresar numero entre 1 y 12.")
  }
  
  mes <- month_names[month_number]
  
  if (month_number == 1) {
    mes_ant <- month_names[12]
  }
  else {
    mes_ant <- month_names[month_number - 1]
  }
  
  colstorename <- 2:(month_number + 1)
  
  months <- month_names[1:month_number]
  
  col_indices <- c(1, seq(2, 2 * month_number, by = 2))
  
  rows <- 1:month_number
  
  return(list(mes = mes, mes_ant = mes_ant, colstorename = colstorename, months = months, col_indices = col_indices, rows = rows))
}


#funcion para calcular los indices de los grupos superiores
calc_indx <- function(col_name){
  col_sym <- ensym(col_name)
  ind_prod %>% left_join(ponderaciones %>% select(PRODUCTO, !!col_sym, ponderacion), by = join_by(PRODUCTO)) %>% 
    group_by(!!col_sym) %>% 
    summarize(sum1 = sum(indice*ponderacion),
              sum2 = sum(ponderacion)) %>% 
    mutate(indice = sum1/sum2)
}

#_______________________________________________________________________________

#SECCION DE FUNCIONES PARA IMPUTACION

#esta seria una lista de productos que si se podrian imputar porque tienen 3 o mas variedades
can_imput <- c('C101041', 'C103011', 'C103012', 'C103013', 'C103014', 'C104011', 'C105012', 'C106111', 'C106131', 'C106211', 'C107112', 'C107311', 'C107511', 'C107911', 'C108011', 'C110111', 'C110211', 'C110411', 'C131211', 'C139211', 'C141011', 'C151211', 'C152011', 'C161011', 'C162111', 'C170911', 'C170211', 'C201121', 'C201211', 'C202111', 'C202211', 'C202311', 'C202911', 'C210011', 'C222011', 'C231011', 'C239211', 'C239212', 'C239511', 'C239611', 'C239911', 'C241011', 'C259911', 'C281911', 'C310011', 'C322111', 'C323011', 'C329011')


#funcion para realizar la imputacion con los precios anteriores
imputacion <- function(){
  # Se genera un dataframe con las variedades cuyos precios se pueden imputar
  sipuede_imp <- precios %>% 
    select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    filter(!!sym(mes) == 0) %>% 
    filter(PRODUCTO %in% can_imput)
  
  # Se genera un dataframe con las variedades cuyos precios no se pueden imputar
  nopuede_imp <- precios %>% 
    select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    filter(!!sym(mes) == 0) %>% 
    filter(!(PRODUCTO %in% can_imput))
  
  # Se calcula el relativo los productos del mes actual
  precios_imp <- precios %>% 
    select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    filter(!!sym(mes) != 0 & !!sym(mesant) != 0) %>% 
    filter(PRODUCTO %in% can_imput) %>% 
    mutate(rel_var = !!sym(mes) / !!sym(mesant)) %>% 
    group_by(PRODUCTO) %>% 
    summarize(rel_prod = geometric.mean(rel_var))
  
  # Se genera un dataframe con los precios del mes anterior, ya que en algunos casos
  # el mes anterior no presenta precios se toman los promedios de las variedades que
  # pertenecen a un mismo producto
  precioant_imp <- precios %>% select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    #filter(!!sym(mes) != 0 & !!sym(mesant) != 0) %>% 
    filter(PRODUCTO %in% can_imput) %>% 
    filter(!!sym(mesant) != 0) %>% 
    group_by(PRODUCTO) %>% 
    summarize(media = mean(!!sym(mesant)))
  
  # Se genera un dataframe con los precios imputados
  precios_imputados <- precios_imp %>% left_join(precioant_imp, by='PRODUCTO') %>% 
    mutate(p_imp = rel_prod*media)
  
  imputados <- sipuede_imp %>% left_join(precios_imputados, by = 'PRODUCTO') %>% 
    mutate(!!sym(mes) := p_imp) %>% 
    select(PRODUCTO, VARIEDAD, !!sym(mes), !!sym(mesant))
  
  # Se genera un dataframe con todos los precios para la realización del cálculo
  boleta <- bol %>% 
    rbind(imputados) %>% 
    rbind(nopuede_imp)
  
  return(boleta)
}


#_______________________________________________________________________________

#SECCION DE FUNCIONES PARA EXPORTAR DATOS A EXCEL

#funcion para obtener los datos que no se tienen datos en el mes actual
missing_prices <- function(){
  cann_imput <- precios %>% 
    select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    filter(!!sym(mes) == 0) %>% 
    filter(PRODUCTO %in% can_imput) %>% 
    distinct(VARIEDAD)
  
  cant_imput <- precios %>% 
    select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
    filter(!!sym(mes) == 0) %>% 
    filter(!(PRODUCTO %in% can_imput)) %>% 
    distinct(VARIEDAD)
  
  return(list(cann_imput = cann_imput, cant_imput = cant_imput))
}



#funcion para exportar los datos de los indices a excel
export_index <- function(){
  wb <- createWorkbook()
  
  addWorksheet(wb, "Sheet1")
  writeData(wb, "Sheet1", ind_prod01)
  addWorksheet(wb, "Sheet2")
  writeData(wb, "Sheet2", ind_act)
  addWorksheet(wb, "Sheet3")
  writeData(wb, "Sheet3", ind_clase)
  addWorksheet(wb, "Sheet4")
  writeData(wb, "Sheet4", ind_grupo)
  addWorksheet(wb, "Sheet5")
  writeData(wb, "Sheet5", ind_div)
  
  addWorksheet(wb, mes)
  writeData(wb, mes, ind)
  
  addWorksheet(wb, paste0('imputados_', mes))
  writeData(wb, paste0('imputados_',mes), imputables)
  
  addWorksheet(wb, paste0('noimputados_', mes))
  writeData(wb, paste0('noimputados_',mes), no_imputables)
  
  file_name <- paste0("ind_", mes, ".xlsx")
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
}


#funcion para leer los excel resumen y generar el "informe con todos los meses"
readind <- function(months, sheet_names){
  dflist <- vector("list", length(months))
  
  for (i in seq_along(months)) {
    file_path <- paste0("ind_", months[i], ".xlsx")
    sheet_name <- sheet_names[i]
    dflist[[i]] <- read_excel(file_path, sheet = sheet_name)
  }
  return(dflist)
}


#funcion para leer los excel de los precios que se imputaron y no se pudieron imputar
#durante cada mes para compararlos
count_imput <- function(){
  
  sheet_names <- paste0('imputados_', months)
  missingprices <- readind(months, sheet_names)
  
  imputados <- bind_rows(missingprices, .id = "source") %>% 
    mutate(value = 1) %>% 
    mutate(source = case_when(
      source %in% rows ~ months[match(source, rows)],
      TRUE ~ as.character(source))) %>%
    pivot_wider(names_from = 'source', values_from = value, values_fill = 0) %>% 
    mutate(total = rowSums(across(all_of(months))))
  
  sheet_names_noimput <- paste0('noimputados_', months)
  missingprices_noimput <- readind(months, sheet_names_noimput)
  
  noimputados <- bind_rows(missingprices_noimput, .id = "source") %>% 
    mutate(value = 1) %>% 
    mutate(source = case_when(
      source %in% rows ~ months[match(source, rows)],
      TRUE ~ as.character(source))) %>%
    pivot_wider(names_from = 'source', values_from = value, values_fill = 0) %>% 
    mutate(total = rowSums(across(all_of(months))))
  
  return(list(imputados = imputados, noimputados = noimputados))
}



#_______________________________________________________________________________

#SECCION DE FUNCIONES PARA GRAFICAR DATOS

#funcion para realizar el grafico del indice
grafica <- function(data, x_col, y_col) {
  ggplot(data, aes_string(x = x_col, y = y_col, group = 1)) +
    geom_line(color = "purple", size = 1) +
    geom_point(size = 2.5) +
    labs(x = x_col, y = y_col) +
    theme_gray()
}






#funcion para realizar la imputacion (este procedimiento es provisional con los
# datos que se tienen hasta ahora)
# impute_prices <- function(mes, mesant) {
#   
#   # Estos serían los que sí se pueden imputar

#   sipuede_imp <- precios %>% 
#     select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
#     filter(!!sym(mes) == 0) %>% 
#     filter(PRODUCTO %in% can_imput)
#   
#   # Estos serían los que no se pueden imputar
#   nopuede_imp <- precios %>% 
#     select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
#     filter(!!sym(mes) == 0) %>% 
#     filter(!(PRODUCTO %in% can_imput))
#   
#   # Estos serían los que no son cero sobre los que se va a calcular el relativo para imputación
#   precios_imp <- precios %>% 
#     select(PRODUCTO, VARIEDAD, all_of(mes), all_of(mesant)) %>% 
#     filter(!!sym(mes) != 0 & !!sym(mesant) != 0) %>% 
#     filter(PRODUCTO %in% can_imput) %>% 
#     mutate(rel_var = !!sym(mes) / !!sym(mesant)) %>% 
#     group_by(PRODUCTO) %>% 
#     summarize(rel_prod = geometric.mean(rel_var),
#               #media = median(!!sym(mes)) #esto es usando la mediana
#               media = mean(!!sym(mes)) #esto es usando la media
#               ) %>% 
#     mutate(p_imp = rel_prod * media)
#   
#   # Se obtiene el dataframe con los precios imputados
#   imputados <- sipuede_imp %>% 
#     left_join(precios_imp, by = "PRODUCTO") %>% 
#     mutate(!!sym(mes) := p_imp) %>% 
#     select(PRODUCTO, VARIEDAD, !!sym(mes), !!sym(mesant))
#   
#   # Se regresan los precios imputados y los que no se pudieron imputar a la boleta
#   boleta <- bol %>% 
#     rbind(imputados) %>% 
#     rbind(nopuede_imp)
#   
#   return(boleta)
# }