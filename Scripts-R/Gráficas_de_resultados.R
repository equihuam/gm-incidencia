library(tidyverse)
library(xlsx)
library(ggplot2)
library(tm)
library(wordcloud2)
library(webshot)
library(ggwordcloud)


dirs <- list.dirs("..", full.names = TRUE)
Encoding(dirs) <-  "UTF-8"
dir_0 <- dirs[grepl("Resultados.*Vevox", dirs)]
Encoding(dir_0) <-  "latin1"

arch_0 <- list.files(dir_0, pattern = "xlsx", full.names = TRUE)
Encoding(arch_0) <- "UTF-8"
  
datos <- read.xlsx(arch_0, sheetName = "Polling Results", header = FALSE, rowIndex = 10:95)
preguntas <- read.xlsx(arch_0, sheetName = "Polling Results", header = FALSE, rowIndex = 6, 
                       as.data.frame = TRUE)
preguntas <-  t(preguntas)
row.names(preguntas) <-  NULL

colnames(preguntas) <-  "preguntas"
preguntas <- data.frame(pregntas=preguntas[preguntas != ""][-1])

preg_grupo <- c("grup_met_af", "grup_met_di", "grup_tem_af", "grup_tem_di", "grup_ter_af", "grup_ter_di")
preguntas$id <- c("inter1", paste0(rep("conocer", 1), 1:3), "inter2",
                  paste0(rep(preg_grupo, 9), rep(1:9, each=6)),
                  "nube1", paste0(rep(preg_grupo, 9), rep(10:18, each=6)),
                  "nube2")

names(datos) <- c("nombre", "apellido", "resp_corr", preguntas$id)
#datos$nombre <- trimws(datos$nombre, which = "both", whitespace = "[ \\t\\r\\n]")
#datos$apellido <- trimws(datos$apellido, which = "both", whitespace = "[ \\t\\r\\n]")

# Limmpieza de datos
# Elimina Anonymous
datos <- datos[!grepl("Anonymous", datos$nombre),]

#Encoding(datos$nombre) <- "UTF-8"
#Encoding(datos$apellido) <- "UTF-8"

## Elimina a los miembros de comité académico
equipo <- c("MiguelEquihua", "MiguelEq1", "MiguelEq", "AnaPohlenz", "AnaPohlenzde Tavira", "IndraMorandin", "GianDelgado", 
            "KayNava", "AnaPohlenzdeTavira", "KarimeLeón", "KayNava", "AnaPohlenz", 
            "IndraIndraMorandin", "AramisOlivos", "Indraporcomputadora", "AnaDeLuca", 
            "KarimeLeón", "JulianaMerçon", "MiguelMartínezRamis", "IreriSuazoOrtuño", 
            "MiguelMartinez", "CristóbalPérez", "IndraMorandin", 
            "IleanaEspejel", "AramisOlivos", "ShirleyReyes")

participantes <- sub("\\s", "", paste0(datos$nombre, datos$apellido))
participantes[!participantes %in% equipo]

datos_part <- datos[!participantes %in% equipo,]
colnames(datos_part) <- colnames(datos)
row.names(datos_part) <- NULL

# Elimina a quienes no respondieron
datos_part <- datos_part[!is.na(datos_part$conocer3),]

# asigna NA a vacíos y convierte a numéricas las respuestas cuantitativas
datos_part[datos_part==""] <- NA
datos_part[,c(5,7,9:62,64:117)] <- data.frame(sapply(datos_part[, c(5,7,9:62,64:117)], as.numeric))

# nubes de palabras
nube_1 <- unlist(sapply(unlist(datos$nube1, use.names = F, recursive = T), 
                        strsplit, "\\n", simplify = T), use.names = F)
nube_1 <-  sort(nube_1[!is.na(nube_1)])
unique(nube_1)
nube_1 <- gsub(",", "", nube_1)
nube_1 <- gsub(" ", "-", nube_1)
nube_1 <- gsub("\\.", "", nube_1)
nube_1 <- gsub("agroecologia", "agroecología", nube_1)
nube_1 <- gsub("agroecologia", "agroecología", nube_1)
nube_1 <- gsub("innovaciones", "innovación", nube_1)
nube_1 <- gsub("cocreación", "co-creación", nube_1)
nube_1 <- gsub("cocrear", "co-creación", nube_1)
nube_1 <- gsub("zona-árida", "zonas-áridas", nube_1)
nube_1 <- gsub("soberanía-alimentari", "soberanía-alimentaria", nube_1)
nube_1 <- gsub("cocrear", "co-creación", nube_1)
nube_1 <- gsub("cogeneración", "co-generación", nube_1)
nube_1 <- gsub("complementaridad", "complementariedad", nube_1)
nube_1 <- gsub("agroecologico", "agroecología", nube_1)
nube_1 <- gsub("ganaderíasustentable", "ganadería-sustentable", nube_1)
nube_1 <- gsub("interés", "interesante", nube_1)
nube_1 <- gsub("transdisciplinario", "transdisciplina", nube_1)
nube_1 <- gsub("sinergías", "sinergía", nube_1)
nube_1 <- gsub("participativo", "participativa", nube_1)
nube_1 <- gsub("resiliencia-comunida", "resiliencia-comunidad", nube_1)
nube_1 <- gsub("sustentable", "sustentabilidad", nube_1)
nube_1 <- gsub("sosteninilidad", "sustentabilidad", nube_1)
nube_1 <- gsub("territorios", "territorio", nube_1)
nube_1 <- gsub("sinergias", "sinergía", nube_1)
nube_1 <- gsub("tansdisciplina", "transdisciplina", nube_1)
nube_1 <- gsub("cambio-climatico", "cambio-climático", nube_1)
nube_1 <- gsub("comunidad", "comunidades", nube_1)
nube_1 <- gsub("comunidadeses", "comunidades", nube_1)
nube_1 <- gsub("ganadería-sustentabilidad", "ganadería-sustentable", nube_1)


# Prepara corpus de la nube 1
corpus <- VCorpus(VectorSource(paste(nube_1, collapse = " ")))
tdm <-  TermDocumentMatrix(corpus)
m  <-  as.matrix(tdm)
frec_palabras  <-  sort(rowSums(m), decreasing=TRUE) 
palabras_1 <-  data.frame(word=names(frec_palabras), freq=frec_palabras)

wordcloud2(palabras_1, color = "random-light", backgroundColor = "white", 
           size = 0.5)
wordcloud2(palabras_1, color = "random-light", backgroundColor = "silver", 
           size = 0.6, figPath = "Mano-derecha.png")
letterCloud(data = palabras_1, word = "P", color='random-light', backgroundColor="black")

wc1 <- ggplot(data = palabras_1, 
       aes(label = word, size = freq, col = as.character(freq))) + 
       geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9) +
       scale_size_area(max_size = 7) +
       scale_color_brewer(palette = "Paired", direction = -1)+
       theme_void()

print(wc1)
ggsave("wordCloud_1.png", wc1, height = 20, width = 28, dpi = 300)
ggsave("wordCloud_1.svg", wc1, height = 15, width = 20)

saveWidget(wc,"tmp.html", selfcontained = F)
webshot("tmp.html","test-cloud.pdf", delay = 5, vwidth = 480, vheight = 480)

# Prepara corpus de la nube 2
nube_2 <- unlist(sapply(unlist(datos$nube2, use.names = F, recursive = T), 
                        strsplit, "\\n", simplify = T), use.names = F)
nube_2 <-  sort(nube_2[!is.na(nube_2)])
unique(nube_2)
nube_2 <- gsub(",", "", nube_2)
nube_2 <- gsub(" ", "-", nube_2)
nube_2 <- gsub("\\.", "", nube_2)
nube_2 <- gsub("acompañar", "acompañarnos", nube_2)
nube_2 <- gsub("acompañarnosnos", "acompañarnos", nube_2)
nube_2 <- gsub("acompañarnosnosnos", "acompañarnos", nube_2)
nube_2 <- gsub("agroecologia", "agroecología", nube_2)
nube_2 <- gsub("aprendizajes", "aprendizaje", nube_2)
nube_2 <- gsub("cocreación", "co-creación", nube_2)
nube_2 <- gsub("cocreacion", "co-creación", nube_2)
nube_2 <- gsub("codiseño", "co-diseño", nube_2)
nube_2 <- gsub("comunidad", "comunidades", nube_2)
nube_2 <- gsub("coproducción", "co-producción", nube_2)
nube_2 <- gsub("impactos", "impacto", nube_2)
nube_2 <- gsub("soberanía-alimentar", "soberanía-alimentaría", nube_2)
nube_2 <- gsub("soluciones-integrale", "soluciones-integrales", nube_2)
nube_2 <- gsub("transdiciplina", "transdisciplina", nube_2)
nube_2 <- gsub("transdisciplinaria", "transdisciplina", nube_2)
nube_2 <- gsub("soberanía-alimentaríaíaía", "soberanía-alimentaria", nube_2)

corpus_2 <- VCorpus(VectorSource(paste(nube_2, collapse = " ")))
tdm <-  TermDocumentMatrix(corpus_2)
m  <-  as.matrix(tdm)
frec_palabras  <-  sort(rowSums(m), decreasing=TRUE) 
palabras_2 <-  data.frame(word=names(frec_palabras), freq=frec_palabras)
wordcloud2(palabras_2, color = "random-light", backgroundColor = "grey", shape = 'circle', size = 0.6)

wc2 <- ggplot(data = palabras_2, 
              aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9) +
  scale_size_area(max_size = 7) +
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()

ggsave("wordCloud_2.png", wc2, height = 20, width = 28, dpi = 300)
ggsave("wordCloud_2.svg", wc2, height = 15, width = 20)

# Combinación de términos de encuestas 1 y 2
nube_3 <-  c(nube_1, nube_2)
nube_3 <-  sort(nube_3[!is.na(nube_3)])
unique(nube_3)
nube_3 <- gsub("ganadería-sustentabilidad", "ganadería-sustentable", nube_3)
nube_3 <- gsub("participativo", "participativa", nube_3)
nube_3 <- gsub("soberanía-alimentaríaíaía", "soberanía-alimentaria", nube_3)

corpus_3 <- VCorpus(VectorSource(paste(nube_3, collapse = " ")))
tdm <-  TermDocumentMatrix(corpus_3)
m  <-  as.matrix(tdm)
frec_palabras  <-  sort(rowSums(m), decreasing=TRUE) 
palabras_3 <-  data.frame(word=names(frec_palabras), freq=frec_palabras)
wordcloud2(palabras_3, color = "random-light", backgroundColor = "grey", shape = 'circle', size = 0.6)

wc3 <- ggplot(data = palabras_3, 
              aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9) +
  scale_size_area(max_size = 7) +
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()

ggsave("wordCloud_3.png", wc2, height = 20, width = 28, dpi = 300)
ggsave("wordCloud_3.svg", wc2, height = 15, width = 20)


# distancia a afinidad máxima

dist <- function(x, y)
{
  d <-  sqrt((10 - x)^2 + (10 - y)^2)
  return(d)
}

# datos de apoyo
for (i in 1:18)
{
  nombre <-  c(paste0("met_af_", i), paste0("met_di_", i), paste0("met_dist_", i),
                paste0("tem_af_", i), paste0("tem_di_", i), paste0("tem_dist_", i),
                paste0("ter_af_", i), paste0("ter_di_", i), paste0("ter_dist_", i))
  
  var_a <- c(paste0("grup_met_af", i), paste0("grup_met_di", i), 
             paste0("grup_tem_af", i), paste0("grup_tem_di", i),
             paste0("grup_ter_af", i), paste0("grup_ter_di", i))
  

  temp <-  data.frame(mean(datos_part[, var_a[1]], na.rm = TRUE), 
                      mean(datos_part[, var_a[2]], na.rm = TRUE), i,
                      mean(datos_part[, var_a[3]], na.rm = TRUE), 
                      mean(datos_part[, var_a[4]], na.rm = TRUE), i,
                      mean(datos_part[, var_a[5]], na.rm = TRUE), 
                      mean(datos_part[, var_a[6]], na.rm = TRUE), i)
  names(temp) <- nombre 
  if (i == 1) {medias <- temp} else {medias <- cbind(medias, temp)}
}

# Grado de afinidad
for (i in 1:18)
{
  var_a <- c(paste0("grup_met_af", i), paste0("grup_met_di", i), 
             paste0("grup_tem_af", i), paste0("grup_tem_di", i),
             paste0("grup_ter_af", i), paste0("grup_ter_di", i))

  d_tmp <- data.frame(((15 - dist(datos_part[, var_a[1]], datos_part[, var_a[2]])) / 15),
                      ((15 - dist(datos_part[, var_a[3]], datos_part[, var_a[4]])) / 15),
                      ((15 - dist(datos_part[, var_a[5]], datos_part[, var_a[6]])) / 15))
  names(d_tmp) <- c(paste0("dist_met_", i), paste0("dist_tem_", i), paste0("dist_ter_", i))

  if (i == 1) {temp <- d_tmp} else {temp <- cbind(temp, d_tmp)}
}

datos_part <- cbind(datos_part, temp)
names(datos_part)

# Conteo de participantes en los cuadrantes
for (i in 1:18)
{
  nombre <-  c(paste0("met_cuad_Da_", i), paste0("met_cuad_DA__", i), 
               paste0("met_cuad_da__", i), paste0("met_cuad_dA__", i),
               paste0("tem_cuad_Da_", i), paste0("tem_cuad_DA__", i), 
               paste0("tem_cuad_da__", i), paste0("tem_cuad_dA__", i),
               paste0("ter_cuad_Da_", i), paste0("ter_cuad_DA__", i), 
               paste0("ter_cuad_da__", i), paste0("ter_cuad_dA__", i))
  
  var_a <- c(paste0("grup_met_af", i), paste0("grup_met_di", i), 
             paste0("grup_tem_af", i), paste0("grup_tem_di", i),
             paste0("grup_ter_af", i), paste0("grup_ter_di", i))
  
  # Contando cuadrantes en sentido contrario a las manecillas del reloj. El 1 es sup, der.
  cond <- data.frame(met_cd_1 = datos_part[, var_a[1]]  > 5 & !is.na(datos_part[, var_a[1]]) &
                                datos_part[, var_a[2]]  > 5 & !is.na(datos_part[, var_a[2]]),
                     met_cd_2 = datos_part[, var_a[1]] <= 5 & !is.na(datos_part[, var_a[1]]) &
                                datos_part[, var_a[2]]  > 5 & !is.na(datos_part[, var_a[2]]),
                     met_cd_3 = datos_part[, var_a[1]] <= 5 & !is.na(datos_part[, var_a[1]]) &
                                datos_part[, var_a[2]] <= 5 & !is.na(datos_part[, var_a[2]]),
                     met_cd_4 = datos_part[, var_a[1]] <= 5 & !is.na(datos_part[, var_a[1]]) &
                                datos_part[, var_a[2]]  > 5 & !is.na(datos_part[, var_a[2]]),
                     
                     tem_cd_1 = datos_part[, var_a[3]]  > 5 & !is.na(datos_part[, var_a[3]]) &
                                datos_part[, var_a[4]]  > 5 & !is.na(datos_part[, var_a[4]]),
                     tem_cd_2 = datos_part[, var_a[3]] <= 5 & !is.na(datos_part[, var_a[3]]) &
                                datos_part[, var_a[4]]  > 5 & !is.na(datos_part[, var_a[4]]),
                     tem_cd_3 = datos_part[, var_a[3]] <= 5 & !is.na(datos_part[, var_a[3]]) &
                                datos_part[, var_a[4]] <= 5 & !is.na(datos_part[, var_a[4]]),
                     tem_cd_4 = datos_part[, var_a[3]] <= 5 & !is.na(datos_part[, var_a[3]]) &
                                datos_part[, var_a[4]]  > 5 & !is.na(datos_part[, var_a[4]]),
                     
                     ter_cd_1 = datos_part[, var_a[5]]  > 5 & !is.na(datos_part[, var_a[5]]) &
                                datos_part[, var_a[6]]  > 5 & !is.na(datos_part[, var_a[6]]),
                     ter_cd_2 = datos_part[, var_a[5]] <= 5 & !is.na(datos_part[, var_a[5]]) &
                                datos_part[, var_a[6]]  > 5 & !is.na(datos_part[, var_a[6]]),
                     ter_cd_3 = datos_part[, var_a[5]] <= 5 & !is.na(datos_part[, var_a[5]]) &
                                datos_part[, var_a[6]] <= 5 & !is.na(datos_part[, var_a[6]]),
                     ter_cd_4 = datos_part[, var_a[5]] <= 5 & !is.na(datos_part[, var_a[5]]) &
                                datos_part[, var_a[6]]  > 5 & !is.na(datos_part[, var_a[6]]))
    
  temp <-  data.frame(sum(cond$met_cd_1), sum(cond$met_cd_2), sum(cond$met_cd_3), sum(cond$met_cd_4),
                      sum(cond$tem_cd_1), sum(cond$tem_cd_2), sum(cond$tem_cd_3), sum(cond$tem_cd_4),
                      sum(cond$ter_cd_1), sum(cond$ter_cd_2), sum(cond$ter_cd_3), sum(cond$ter_cd_4))
  names(temp) <- nombre 
  if (i == 1) {conteos <- temp} else {conteos <- cbind(conteos, temp)}
}


# Gráficos
for (i in 1:17)
{
  var_a <- c(paste0("grup_met_af", i), paste0("grup_met_di", i), 
             paste0("grup_tem_af", i), paste0("grup_tem_di", i),
             paste0("grup_ter_af", i), paste0("grup_ter_di", i),
             paste0("dist_met_", i), paste0("dist_tem_", i), paste0("dist_ter_", i),
             paste0("met_af_", i), paste0("met_di_", i), 
             paste0("tem_af_", i), paste0("tem_di_", i),
             paste0("ter_af_", i), paste0("ter_di_", i),
             "metodo", "tema", "sitio")

  dat <- datos_part %>%
    filter(!is.na(datos_part[, var_a[1]]), !is.na(datos_part[, var_a[2]]))
    
    p <- ggplot(dat, aes(x = get(var_a[1]), y = get(var_a[2]), 
                         color = "metodo", alpha = get(var_a[7])), na.rm = T) + 
    geom_point(show.legend = FALSE, na.rm = TRUE) + 
    geom_point(data = medias, size = 4, aes(x = get(var_a[10]), y = get(var_a[11]), 
               color = "metodo", alpha = 1), show.legend = FALSE, na.rm = TRUE) + 
    
    geom_point(aes(x = get(var_a[3]), y = get(var_a[4]), 
               color = "tema", alpha = get(var_a[8])), show.legend = FALSE, na.rm = TRUE) +
    geom_point(data = medias, size = 4, aes(x = get(var_a[12]), y = get(var_a[13]), 
               color = "tema", alpha = 1), show.legend = FALSE, na.rm = TRUE) +
    
    geom_point(aes(x = get(var_a[5]), y = get(var_a[6]), 
               color = "sitio", alpha = get(var_a[9])), show.legend = FALSE, na.rm = TRUE) +
    geom_point(data = medias, size = 4, aes(x = get(var_a[14]), y = get(var_a[15]), 
               color = "sitio", alpha = 1), show.legend = TRUE, na.rm = TRUE) +
    
    geom_hline(yintercept = 5, color = "gray") + geom_vline(xintercept = 5, color = "gray") +
    labs(title = paste0("Equipo ", i), x = "Afinidad", y = "Disposición") +
    scale_color_manual(name='Aspecto del proyecto',
                     breaks=c('metodo', 'tema', 'sitio'),
                     values=c('metodo'='#2E1890', 'tema'='#C51162', 'sitio'='#006064')) +
    guides(alpha = "none", color = "legend") +
    coord_fixed(ratio = 1)
    
  # Despliega o guarda las gráficas  
  #print(p)  
  ggsave(paste0("./graf/Grupo_", i, ".png"), p, width = 5, height = 5)
}

# gráficos de conteos

for (i in 1:17)
{
  equipo <- paste0("Equipo ", i)
  d_blq <- seq(from = (i - 1) * 12 + 1, to = 12 + (i - 1) * 12) 
  d_tmp <- data.frame(cuad = factor(c("AFIN-DISP", "afin-DISP", "afin-disp", "AFIN-disp")), 
                      cuenta = as.integer(conteos[, d_blq]), 
                      asp = rep(c("Metodología", "Temática", "Territorio"), each = 4))

  p <- ggplot(data = d_tmp, aes(x= cuad, y=cuenta, fill=cuad)) + geom_col(show.legend = FALSE) +
       labs(title = equipo, y  = "conteo", x = "cuadrante") +
       scale_fill_manual(values=c("#eff59d", "#e7c3f7", "#b6d9f2", "#93faaf")) +
       facet_wrap(~ asp)
#  print(p)
  ggsave(paste0("./graf/Grupo_cuenta_", i, ".png"), p, width = 10, height = 5)
}
