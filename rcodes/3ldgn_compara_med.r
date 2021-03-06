# Definimos un directorio de trabajo
#setwd("/Volumes/GoogleDrive/Mi unidad/MET_ESTAD/7DATOS")
# librerias necesarias
library("data.table")
library(ggplot2)
# Los datos se descargan de 
# https://datos.cdmx.gob.mx/dataset/base-covid-sinave
# aquí también se descarga el diccionario de variables

####################
# Leemos los datos #
####################
tt <- system.time(BD <- as.data.frame(fread("C:/Users/pc/Desktop/Data Science IIMAS/6th Semester/6.- ME/5. Datos/Pandemia/sisver_public.csv", 
                                            showProgress = TRUE, 
                                            sep = ",", header = TRUE,
                                            na.strings= "")))[3]
tt/60
# Tamaño
print(object.size(BD),units="Gb")

# Filtro
BD1 <- BD[BD$entresi == "CIUDAD DE MEXICO" & BD$resdefin == "SARS-CoV-2", ]

# FALLECIMIENTOS Y HOSPITALIZACIONES
BD1$DEF <- 1*!is.na(BD1$fecdef) 
BD1$HOSP <- 1*(BD1$tipacien == "HOSPITALIZADO")

tb1 <- table(BD1$DEF)
tb2 <- table(BD1$HOSP)

round(100*tb1/sum(tb1), 1)
round(100*tb2/sum(tb2), 1)


################
# LDGN - media #
################
alfa <- 0.05
k <- seq(0.05, 3, length.out = 20)
n <- round(1/(alfa*(k^2)), 0)

ss <- data.frame(k = k, n = n)

plot(ss, type = "l")

j <- 3
mu <- mean(BD1$edad, na.rm = TRUE)
nn <- ss[j,2]
m <- 10000
x_bar <- rep(NA, m)
for (t in 1:m) {
  x_bar[t] <- mean(sample(BD1$edad, nn, replace = TRUE), na.rm = TRUE)
}

ss

sigma <- sd(BD1$edad, na.rm = TRUE)
kk <- ss[j,1]

# Se cumple lo que dice la LDGN ?
sum(abs(x_bar - mu) < kk*sigma)/m >= 0.95

#
# ¡LA LDGN nos da una cota muy conservadora!
#

#####################
# LDGN - proporción #
#####################
alfa <- 0.05
eps <- seq(0.05, 0.5, length.out = 20)
n <- round(0.25/(alfa*(eps^2)), 0)

ss <- data.frame(eps = eps, n = n)

plot(ss, type = "l", lwd = 2)

j <- 2
p <- mean(BD1$DEF)
nn <- ss[j,2]
m <- 10000
p_hat <- rep(NA, m)
for (t in 1:m) {
  p_hat[t] <- mean(sample(BD1$DEF, nn, replace = TRUE), na.rm = TRUE)
}

ss
sigma <- sd(BD1$DEF, na.rm = TRUE)
kk <- ss[j,1]

# Se cumple lo que dice la LDGN ?
sum(abs(p_hat - p) < kk*sigma)/m >= 0.95

#
# ¡LA LDGN nos da una cota muy conservadora!
#


#########################
# COMPARACION DE MEDIAS #
#########################

# EDAD DE LOS FALLECIMIENTOS
bar_w <- tapply(BD1$edad, BD1$DEF, mean, na.rm = TRUE)
s_w <- tapply(BD1$edad, BD1$DEF, sd, na.rm = TRUE)
n_w <- tapply(BD1$edad, BD1$DEF, length)

alfa <- 0.05
kk <- 1/sqrt(n_w*alfa) 
lim_inf <- bar_w - kk*s_w
lim_sup <- bar_w + kk*s_w

data.frame(mu = bar_w, lim_inf = lim_inf, lim_sup = lim_sup, 
           row.names = c("NO FALLECEN", "FALLECEN"))

# PORCENTAJE DE HOSPITALIZADOS X ALCALDIA
p_hat <- 100*tapply(BD1$HOSP, BD1$mpioresi, mean, na.rm = TRUE)
s_p <- 100*tapply(BD1$HOSP, BD1$mpioresi, sd, na.rm = TRUE)
n_p <- tapply(BD1$HOSP, BD1$mpioresi, length)


kk <- 1/sqrt(n_p*alfa) 
res <- data.frame(alcaldia = substr(names(p_hat), 1, 5), 
                  p_hat = as.numeric(p_hat),
                  lim_inf = as.numeric(p_hat - kk*s_p), 
                  lim_sup = as.numeric(p_hat + kk*s_p), 
                  stringsAsFactors = FALSE)

ggplot(res, aes(x = alcaldia, y = p_hat, group = alcaldia)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lim_inf, ymax = lim_sup)) + 
  theme_bw()



