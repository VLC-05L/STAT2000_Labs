
#--------------------------------------------------------------------------------------------
#
# Random effects ANOVA
#
# Glen Livingston Jr
# Date: 25/03/2020
#
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
# Creating Data for Random Effects Model 
#--------------------------------------------------------------------------------------------

rm(list=ls())

# Bean Counters 

#set.seed(200)

n_partners = 4
n_clients = 10

med = lar = round(n_clients/7)
sma = n_clients - med - lar 
med2 = sma2 = round(n_clients/3)
lar2 = n_clients - med2 - sma2
med3 = sma3 = round(n_clients/7)
lar3 = n_clients - med3 - sma3


partner = factor(rep(1:n_partners,each = n_clients))
size = c(
  c(rep("small",sma),  rep("medium", med),  rep("large", lar )),
  rep(c(rep("small",sma2), rep("medium", med2), rep("large", lar2)), n_partners-2),
  c(rep("small",sma3), rep("medium", med3), rep("large", lar3))
)

dat = data.frame(partner = partner, size = size)


sigma2 = 18000^2
alpha_size = c(4000, 20000, 40000)

# Invoice sizes - 20K , 150K, 600K 
write_off = NULL
per_error = NULL


for (i in 1:(n_partners*n_clients)){
  write_off[i] = alpha_size[dat$size[i] == c("small","medium","large")] + rnorm(1,0,sqrt(sigma2))
  
}

dat$write_off = round(write_off)

plot(write_off~partner,data=dat)
plot(write_off~size,data=dat)


fit_1 = lmer(write_off~(1|partner) + size,data=dat, REML = TRUE)
summary(fit_1)
anova(fit_1) 
rand(fit_1)

fit_1 = lmer((write_off)~(1|partner),data=dat, REML = TRUE)
summary(fit_1)
anova(fit_1) 
rand(fit_1)


#write.csv(x = dat,file = "Lab 06 - Accounting.csv", row.names = F)

