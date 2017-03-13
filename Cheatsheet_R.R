#----------------------------------------------------------------------------
# FUNCIÓN APPLY 
#----------------------------------------------------------------------------

# --------------------- TAPPLY: Group by sobre vectores ---------------------
tapply(iris$Petal.Length, iris$Species, mean)

# --------------------- SAPPLY -----------------------------------------------
sapply(1:10, cuadrado) # te devuelve un vector
sapply(iris, length)

# --------------------- LAPPLY -----------------------------------------------
lapply(1:10, cuadrado) # te devuelve una lista
lapply(iris, class)


#----------------------------------------------------------------------------
# TABLAS 
#----------------------------------------------------------------------------

pob.aragon.2014 <- read.table("data/pob_aragon_2014.csv", header = T, sep = "\t")

#-------------- CREAR  SUMATORIOS ------------------------------------
df$Total <- rowSums(df[1:4])
df["Total",] <- c(sum(df$Examination), sum(df$Education), sum(df$Infant.Mortality, na.rm = TRUE))

#-------------- CAMBIAR ORDENES DE LAS COLUMNAS  ----------------------------
# c) Change the order of the columns so total is the first variable.
df <- df[, c(5, 1:4)]

#-------------- CAMBIAR NOMBRE FILAS Y COL------------------------------------
colnames(df) <- paste("variable_", 1:ncol(df))
rownames(df) <- paste("id_", 1:nrow(df))

#Rename the column names so only the first 3 letters after the full 
#stop appear (e.g. States.abb will be abb).
names(df) <- substr(names(df), 7, 9)

#-------------- SUBSTITUIR TEXTO ------------------------------------
paro$Periodo <- gsub("IV",  "4", paro$Periodo)

#-------------- FILTRAR------------------------------------
df[df$Income == max(df$Income) ,]

#-------------- ORDENAR------------------------------------
df4[order(df$Age), ] 

#-------------- CONTAR FILAS ------------------------------------
nrow(df[df$Income < 4300,])
nrow(subset(df, df$Income < 4300))


#-------------- IFELSE------------------------------------
dfa$illi <- ifelse(dfa$Illiteracy < 1,
                   "Low Illiteracy",
                   ifelse(dfa$Illiteracy >= 1 & dfa$Illiteracy < 2,
                          "Some Illiteracy",
                          "High Illiteracy"))




#----------------------------------------------------------------------------
# CAMBIAR FORMATO TABLAS 
#----------------------------------------------------------------------------

#-------------- FORMATO LARGO ------------------------------------
pob.aragon.2014.largo <- melt(pob.aragon, id.vars = c("Provincia", "Periodo"))           # mismos datos en otro formato... ¡formato largo!


#-------------- FORMATO ANCHO ------------------------------------
# a partir del formato largo se puede pasar a distintos tipos de formatos anchos:
dcast(pob.aragon.2014.largo, Provincia ~ variable)
dcast(pob.aragon.2014.largo, variable ~ Provincia)

iris.long <- melt(iris)
dcast(iris.long, Species ~ variable, fun.aggregate = mean) 
dcast(iris.long, Species ~ variable, value.var = "value", fun.aggregate = mean)  




#----------------------------------------------------------------------------
# PLYR: procesamiento de tablas por trozos
#----------------------------------------------------------------------------

# --------------------- DDPLY -----------------------------------------------
res <- ddply(paro, .(Gender, Periodo, Situation), summarize, total = sum(value))

dat.preds <- ddply(dat, .(school), transform, 
                   pred = predict(lm(extro ~ open + agree + social + class)))

# --------------------- DLPLY -----------------------------------------------
res <- dlply(airquality, .(Month), function(x) lm(Temp ~ Solar.R, data = x)) 
lapply(res, coefficients)
ldply(res, coefficients)






