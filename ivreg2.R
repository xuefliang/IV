library(dplyr)
library(AER)
zonghe <- read.csv("/home/xuefliang/Downloads/综合2.csv", header = T)
zonghe <- group_by(zonghe, Adress) %>% summarize(met = mean(FLTscore2)) %>% 
  left_join(zonghe) %>% as.data.frame()

# tobit
fm.tobit <- tobit(ZB2 ~ QD1e1 + QD1e1^2 + QD1f + QD1g + QD1h1 + 
  QD1a + QD1c + QD1d + log(QD3a) + log(QD3a)^2 + log(QD1e1) + 
  log(QD1e1)^2 + House + J102 + J104 + J106 + Creditconstraint1, 
  data = zonghe)
summary(fm.tobit)

# probit
probit.fit2 <- glm(Creditconstraint1 ~ QD1e1 + QD1e1^2 + QD1f + 
  QD1g + QD1h1 + QD1a + QD1c + QD1d + log(QD3a) + +log(QD3a)^2 + 
  log(QD1e1) + log(QD1e1)^2 + House + J102 + J104 + J106, family = quasibinomial(link = "probit"), 
  data = zonghe)
summary(probit.fit2)

# DHW
iv2 = ivreg(ZB2 ~ Kscore + MinorityAera + PerCapitaIncome + 
              QD1a + QD1k + House +QD1e1 + QD1g + QD1a + QD1c + QD1d + QD1k + 
              Creditconstraint1 | MinorityAera + PerCapitaIncome + QD1a + 
              QD1k + House + QD1e1 + QD1g + QD1a + QD1c + QD1d + QD1k + 
              Creditconstraint1 + met, data = zonghe)
summary(iv2, vcov = sandwich, diagnostics = TRUE)