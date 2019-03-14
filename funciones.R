## En este script se encuentran distintas funciones de uso general

# Funcion para ejecutar queries -------------------------------------------

get_query <- function(query, db){
  if (tolower(db) == "teradata" | tolower(db) == "td"){
    
    # ejecuto query
    options( java.parameters = "-Xmx8g" )
    library(RJDBC)
    library(teradataR)
    
    # Basicos
    source("/home/Compartida_CD_GI/d_visa_scoring/scripts/basicos.R")
    
    #Definimos la conexion
    drv <- JDBC("com.teradata.jdbc.TeraDriver",paste0(dir_compartida,"terajdbc4.jar:",dir_compartida,"tdgssconfig.jar"))
    con <- tdConnect("192.168.51.2/CHARSET=UTF8,TMODE=TERA,SESSIONS= 1 ", uid = usuarioTeradata, pwd = contrasenaTeradata, dType ="jdbc")
    
    ## Importamos archivo
    result <- dbGetQuery(con, query)
    
  } else{
    
    if (tolower(db) == "hadoop" | tolower(db) == "hive" | tolower(db) == "hdp"){
      # ejecuto query
    }
    
    else{
      print("db no reconocida")
    } 
  }
  
  return(result)
  
}



# Funcion que genera la matriz para xgboost -------------------------------

get_xgboost_matrix <- function(df,entrenamiento=FALSE){
  
  # partition es == 1 si se splitea en train y test y partition == 2 si se splitea en train, test y calibration
  
  library(data.table)
  library(stringr)
  dt <- as.data.table(df)
  
  ## Me quedo con los is.na(clase) == FALSE
  
  dt <- dt[is.na(clase) == FALSE]
  
  colnames(dt) <- str_replace(colnames(dt),"\\$","__")
  
  dt <- dt[,lapply(.SD,as.numeric)] %>% as.matrix
  
  dt <- as.data.frame(dt)
  if(entrenamiento){
    dt$clase <- as.factor(dt$clase)
  }
  
  return(dt)
  
}


# Funcion que arma split entre train, test y calibration ------------------


ks <- function(tpr, fpr){
  # se podria agregar el plot
  
  #this code builds on ROCR library by taking the max delt
  #between cumulative bad and good rates being plotted by
  #ROCR see https://cran.r-project.org/doc/contrib/Sharma-CreditScoring.pdf
  ks = max(tpr-fpr)
  return(ks)
  
}


