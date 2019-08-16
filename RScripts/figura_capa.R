# ----------------------------------------------------
# Porto Alegre, 12 de agosto de 2019
# Capa do github de MAT02035

# ----------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(hexSticker)
library(showtext)

# ----------------------------------------------------
# Carregando o arquivo de dados

chol.df <- read_dta(file = here("data", "cholesterol.dta"))

# ----------------------------------------------------
# Reformando o objeto: de "largo" para "longo"

chol.df.longo <- gather(data = chol.df, key = "tempo", value = "colesterol", -id, -group)

# ----------------------------------------------------
# Formata variáveis

chol.df.longo$tempo <- as.factor(as.numeric(as.factor(chol.df.longo$tempo)))
chol.df.longo$group <- as.factor(chol.df.longo$group)

# ----------------------------------------------------
# Gráfico de perfis

p <- ggplot(data = chol.df.longo,
            mapping = aes(x = tempo, y = colesterol, group = id, colour = group)) +
  geom_line(alpha = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(mapping = aes(x = tempo, y = colesterol, group = group),
              method = "loess", se = TRUE) +
  labs(x = "Tempo", y = "Colesterol", colour = "Grupo") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap( ~ group)

p 

ggsave(plot = p,
       filename = here("output_figs", "capa.png"),
       width = 10.84, height = 5.42, units = "cm", dpi = 300)


font_add_google("Gochi Hand", "gochi")

## Automatically use showtext to render text for future devices
showtext_auto()

## use the ggplot2 example

p <- ggplot(data = chol.df.longo,
            mapping = aes(x = tempo, y = colesterol, group = id)) +
  geom_line(size = 0.2) +
  labs(x = "Tempo", y = "") + 
  theme(legend.position = "none")
p <- p + theme_transparent()

sticker(p, package = "MAT02035", p_size = 16, s_x = .8, s_y = .75, s_width = 1.8, s_height = 1.5,
        p_family = "gochi", filename = here("output_figs", "mat02035hex.png"))
