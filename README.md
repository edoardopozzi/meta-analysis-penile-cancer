# meta-analysis-penile-cancer

library(readxl)
data=read_excel("dslnb.xlsx")
data=read_excel("RLND_meta analysis test.xlsx")

library(mada)
library(bamdit)
library(rjags)


library(rjags)

as.numeric = data
madad(data)

summary(data[,2])
hist((data[,2]))

madad(data, level = 0.95)

forest(madad(data), type = "sens")


rs
forest(madad(data), type = "spec")
forest(madauni(data))
madauni(data, level = 0.975)
rs <- rowSums(data)


madauni(data,method="MH",comb.fixed=F,comb.random=T)



#Zafer part ---- 



library(rmeta)
library(meta)
library(metafor)
metaprop(data$TP,data$TP+data$FN,comb.fixed=F, comb.random=T, sm="PLOGIT",
         methodci="CP")

poor_qualty_studies=c(8,9,10,11)

data$quality="good"
colnames(data)
data[poor_qualty_studies,9]="poor"
####

library(bamdit)


data_bayes=cbind(data[,3],(data[,6]),data[,4],(data[,5]),data[,9])

data_bayes=cbind(data[,3],(data[,5]+data[,3]),data[,4],(data[,6]+data[,4]),data[,9])

colnames(data_bayes)=c("tp","n1","fp","n2")
data_bayes=as.data.frame(data_bayes)
data_bayes[,1:4]=apply(data_bayes[,1:4],2,as.numeric)
data_bayes$quality=factor(data_bayes$quality)
class(data_bayes$quality)

###



edoardo <- metadiag(data_bayes, re = "sm", link = "logit", df.estimate = TRUE,
                 split.w = TRUE, nr.burnin = 100, nr.iterations = 1000,
                  nr.chains = 4, r2jags = TRUE)

summary(edoardo)


gr <- with(data_bayes, factor(quality))
plotw(m = data_tes, group = gr)
plotw(ct.m)
data("ct")
ct

####Re do Bayes for all
glas
class(data_bayes)

show(data_bayes)
plotdata(data_bayes)

data_bayes=data_bayes[-21,]
glas.m1 <- metadiag(data_bayes, re = "normal", re.model = "DS",
                     link = "logit", sd.Fisher.rho = 1.7, nr.burnin = 1000,
                     nr.iterations = 1000, nr.chains = 4, r2jags = TRUE)
summary(glas.m1, digits = 3)
library("R2jags")
attach.jags(glas.m1)
cor(se.pool, sp.pool)

glas
plot(glas.m1, level = c(0.5, 0.75, 0.95), parametric.smooth = TRUE)
plotsesp(glas.m1)
bsroc(glas.m1)
bsroc(glas.m1, level = c(0.025, 0.5, 0.975), plot.post.bauc = TRUE,
       fpr.x = seq(0.01, 0.75, 0.01), lower.auc = 0.01, upper.auc = 0.75,
       partial.AUC = FALSE)




#RADICAL BAYESIAN -----------------------------------------------------------------

library(readxl)
data=read_excel("rlnd.xlsx")

show(rlnd)

library(bamdit)

data=as.data.frame(data)

colnames(data)=c("tp","n1","fp","n2")
class(data)
head(data)
show(data)

plotdata(data)

analysis <- metadiag(data = data, re = "normal", re.model = "DS", nr.iterations = 10000)


summary(analysis)





datasetrlnd <- metadiag(data, re = "normal", re.model = "DS",
                    link = "logit", sd.Fisher.rho = 1.7, nr.burnin = 1000,
                    nr.iterations = 1000, nr.chains = 4, r2jags = TRUE)

summary(data)



library("R2jags")
attach.jags(analysis)
cor(se.pool, sp.pool)


plot(analysis, level = c(0.5, 0.75, 0.95), parametric.smooth = TRUE)
plotsesp(analysis)
bsroc(analysis)
bsroc(analysis, level = c(0.025, 0.5, 0.975), plot.post.bauc = TRUE,
      fpr.x = seq(0.01, 0.75, 0.01), lower.auc = 0.01, upper.auc = 0.75,
      partial.AUC = FALSE)
