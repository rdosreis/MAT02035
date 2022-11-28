library(here)
library(rmarkdown)

pasta <- "Rmds"
arquivo <- "2022_03_conceitos_basicos_v2"

arquivo_rmd <- paste0(arquivo, ".Rmd")
arquivo_pdf <- paste0(arquivo, ".pdf")
input <- here::here(pasta, arquivo_rmd)
arquivo_r <- paste0(arquivo, ".R")

rmarkdown::render(input = input, 
                  output_format = beamer_presentation(theme = "Antibes",
                                                      highlight = "default",
                                                      colortheme = "dolphin",
                                                      fonttheme = "structurebold",
                                                      includes = list(in_header = here::here("styles", "mystyle.tex")),
                                                      slide_level = 2,
                                                      keep_tex = FALSE,
                                                      number_sections = FALSE,
                                                      fig_caption = FALSE),
                  output_file = arquivo_pdf,
                  output_dir = here::here("output_pdf_v2"),
                  encoding = "UTF-8")

# knitr::purl(input = here::here("avaliacoes", "2022_01_ava_pontual_resolucao.Rmd"),
#             output = here::here("RScripts", "2022_01_ava_pontual_resolucao.R"),
#             documentation = 1,
#             encoding = "UTF-8")


# rmarkdown::render(input = input, 
#                   output_format = "powerpoint_presentation")

knitr::purl(input = input,
            output = here::here("RScripts", arquivo_r),
            documentation = 1,
            encoding = "UTF-8")