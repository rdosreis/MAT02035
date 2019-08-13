library(here)
library(rmarkdown)

pasta <- "Rmds"
arquivo <- "00_apresenta_curso"

arquivo_rmd <- paste0(arquivo, ".Rmd")
arquivo_pdf <- paste0(arquivo, ".pdf")
input <- here(pasta, arquivo_rmd)

rmarkdown::render(input = input, 
                  output_format = beamer_presentation(theme = "metropolis", #"AnnArbor",
                                                      highlight = "zenburn",
                                                      # colortheme = "dolphin",
                                                      # fonttheme = "structurebold",
                                                      # includes = list(in_header = here("styles", "mystyle.tex")),
                                                      slide_level = 2,
                                                      keep_tex = FALSE,
                                                      fig_caption = FALSE),
                  output_file = arquivo_pdf,
                  output_dir = here("output_pdf"),
                  encoding = "UTF-8")
