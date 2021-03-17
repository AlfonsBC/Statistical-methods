library(data.table)

setwd("D:/Desktop/Data Science IIMAS/6th Semester/6.- ME/")

path="/5. Datos/iter_00_cpv2020_csv/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv"

data <- read.csv(path, stringsAsFactors = FALSE)
str(data)


#Sub-totales para cada municipio

#Getting the rows and columns of data base
data_selected <- subset(data, (data$LOC == 0) & (data$MUN!=0))

str(data_selected)

#Getting the size of the data selected
object.size(data_selected)

#Example of why not aggregating rows with NA
sum(c(NA,5,10,6,3,8,NA))

# Para cada 32 Entidades Federativas, y 16 alcaldías
data_selected_mex <- subset(data, (data$LOC == 0) & (data$MUN==0))
data_selected_cdmx <- subset(data, (data$LOC == 0) & (data$MUN!=0) & (data$ï..ENTIDAD == 9))


