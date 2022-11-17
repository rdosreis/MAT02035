library(haven)
library(tidyr)
library(dplyr)
library(geepack)
library(doBy)

df <- read_dta(file = here::here("data", "respir.dta"))
df

df.longo <- df %>% 
  gather(key = "tempo", value = "y", -id, -trt, -clinic)

df.longo$clinic <- factor(df.longo$clinic)

df.longo$trt <- factor(df.longo$trt)
df.longo$trt <- relevel(df.longo$trt, ref = "P")

df.longo$tempo.fac <- factor(df.longo$tempo,
                             levels = paste0("y", 0:4),
                             labels = 0:4)

# df.longo$trt.tempo <- df.longo$trt == "A" * df.longo$tempo.fac

df.longo$tempo.num <- as.numeric(df.longo$tempo.fac)
df.longo$scale <- 1
df.longo <- df.longo %>% 
  arrange(clinic, trt, id, tempo)

fit1 <- geeglm(y ~ tempo.fac*trt,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               # scale.fix = T,
               # scale.value = scale,
               corstr = "unstructured",
               std.err = "san.se")
summary(fit1)

fit1ex <- geeglm(y ~ tempo.fac*trt,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               corstr = "exchangeable",
               std.err = "san.se")
summary(fit1ex)



fit2 <- geeglm(y ~ tempo.fac + trt,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               corstr = "unstructured",
               std.err = "san.se")
summary(fit2)

L <- rbind(c(rep(0, 6), 1, 0, 0, 0),
           c(rep(0, 6), 0, 1, 0, 0),
           c(rep(0, 6), 0, 0, 1, 0),
           c(rep(0, 6), 0, 0, 0, 1))
esticon(fit1, L, joint.test = TRUE)

anova(fit1, fit2)


pred.df <- data.frame(
  expand.grid(tempo.fac = unique(df.longo$tempo.fac),
              trt = unique(df.longo$trt)))

pred.df$Odds <- exp(stats::predict(fit1, newdata = pred.df))
pred.df$OR <- c(NA, pred.df$Odds[2:5]/pred.df$Odds[1], NA, pred.df$Odds[7:10]/pred.df$Odds[6])
pred.df$OR2 <- c(rep(NA, 5), pred.df$Odds[6:10]/pred.df$Odds[1:5])
pred.df

fit3 <- geeglm(y ~ tempo.fac*trt + clinic*tempo.fac + clinic*trt,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               corstr = "unstructured",
               std.err = "san.se")
summary(fit3)

fit4 <- geeglm(y ~ tempo.fac*trt + clinic*trt,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               corstr = "unstructured",
               std.err = "san.se")

fit5 <- geeglm(y ~ tempo.fac*trt + clinic,
               id = id, 
               family = binomial(link = "logit"),
               data = df.longo,
               corstr = "unstructured",
               std.err = "san.se")

anova(fit3, fit1)
anova(fit4, fit3)
anova(fit4, fit1)
anova(fit4, fit5)

pred.df <- data.frame(
  expand.grid(tempo.fac = unique(df.longo$tempo.fac),
              clinic = unique(df.longo$clinic),
              trt = unique(df.longo$trt)))

pred.df$probs <- stats::predict(fit4, newdata = pred.df, type = "response")

pred.df <- pred.df[c("clinic", "trt", "tempo.fac", "probs")]
pred.df <- pred.df %>% arrange(clinic, trt, tempo.fac)


library(ggplot2)

p <- ggplot(data = pred.df, mapping = aes(x = tempo.fac, y = probs,
                                          color = interaction(clinic, trt))) +
  geom_point() +
  labs(x = "Tempo", y = "Probabilidade (Estado respiratório bom)",
       color = "Clínica/Tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p
