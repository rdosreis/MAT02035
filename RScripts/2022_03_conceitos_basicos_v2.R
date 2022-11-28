## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'-----------
# ----------------------------------------------------
set.seed(40)

knitr::kable(chumbo.df[sample(x = chumbo.df$id, size = 2, replace = F),],
             col.names = c("ID", "Grupo", "Linha de base", "Semana 1", "Semana 4", "Semana 6"), caption = "Níveis de chumbo no sangue de dez crianças do estudo TLC")


## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'-----------
# ----------------------------------------------------
# Carregando o arquivo de dados

epilep.df <- read_dta(file = here::here("data", "epilepsy.dta"))

epilep.df$trt <- factor(epilep.df$trt, labels = c("Placebo", "Progabide"))

# ----------------------------------------------------
# Reformando o objeto: de "largo" para "longo"

epilep.df.longo <- gather(data = epilep.df, key = "tempo", value = "convusoes", -id, -trt, -age)

# ----------------------------------------------------
# Formata variáveis

epilep.df.longo$tempo <- factor(epilep.df.longo$tempo, labels = c("0", "1", "2", "3", "4"))
epilep.df.longo$tempo.num <- as.numeric(as.character(epilep.df.longo$tempo))

# ----------------------------------------------------
set.seed(10)

knitr::kable(epilep.df[sample(x = epilep.df$id, size = 2, replace = F), - 3],
             col.names = c("ID", "Grupo", "Linha de base", "Sem 1", "Sem 2", "Sem 3", "Sem 4"), caption = "Número de convulsões por semana")

