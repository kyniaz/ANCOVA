library('ggplot2')
library('ggpubr')
library('ggrepel')

#seed = 40

taus = c(-5,-2.5,1,6.5)
mu = 45 #Intercepto (aula)
var_sim = 4

#medias_covar = c(53,54,55,56)
#sd_covr = 5
n = 15

beta = 2
k = 1

covar_vals = rgamma(4*n, 3, 1/3)
dados_init = c(0,0,0)
for(i in 1:length(taus)){
  for(j in 1:n){
    #var_c = rnorm(1,medias_covar[i], sd_covr)
    vals_linha = c(rnorm(1, mu + taus[i] + beta*covar_vals[k],var_sim), covar_vals[k] , i)
    dados_init = rbind(dados_init, vals_linha)
    k = k+1
  }
  #print(k)
}
# 
# colnames(dados_init) = c('Saldo', 'Renda', 'Cidade')

###################################

dados_init = dados_simulados_ancova

dados_init$Cidade = ifelse(dados_init$Cidade == 1, 'C1', 
                           ifelse(dados_init$Cidade == 2, 'C2', 
                                  ifelse(dados_init$Cidade == 3, 'C3',
                                         'C4')))

dados_init$Cidade = as.factor(dados_init$Cidade)

for (i in 1:length(taus)){
  print(c(mean(dados_init$Renda[dados_init$Cidade == paste0("C",i)]),
          mean(dados_init$Saldo[dados_init$Cidade == paste0("C",i)])))
}

#Estimando o beta geral
estima_beta = function(m_plan, y){
  m_plan = as.matrix(m_plan)
  return(solve(t(m_plan)%*%m_plan)%*%t(m_plan)%*%y)
}

matriz_reg = data.frame(matriz)

estima_beta(matriz_reg, dados_init$Saldo)

#Estimando beta para cada
for (i in 1:length(taus)){
  y = dados_init$Saldo[dados_init$Cidade == paste0('C',i)]
  dados_tau = dados_init[dados_init$Cidade == paste0('C',i),c('Renda')]
  l_tau = sapply(dados_tau, as.numeric)
  m_tau = matrix(c(rep(1,n), l_tau), ncol = 2)
  print(solve(t(m_tau)%*%m_tau)%*%t(m_tau)%*%y)
}

#grafico de Y contra X.

#Função automática

library('HH')

ancova_hh = ancova(Saldo ~ Renda + Cidade, data = dados_init, contrasts = list(Cidade = 'contr.sum'))

#ANCOVA R
#boxplot(dados_init$Saldo ~ dados_init$Cidade)

ggplot(dados_init, aes(x = Cidade, y = Renda, fill = Cidade)) + 
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = 'Set1')

ggplot(dados_init, aes(x = Cidade, y = Saldo, fill = Cidade)) + 
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = 'Set1')

ggplot(dados_init, aes(x = Renda, y = Saldo, col = Cidade)) + geom_point() +
  theme_minimal() + 
  labs(x = 'Renda - média(Renda)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_colour_brewer(palette = 'Set1')

#covariavel e tratamente difernetes
model =  aov(Renda ~ Cidade, data = dados_init)
summary(model)

#SIM

#variâncias homogêneas para clareza;
library('car')

leveneTest(Saldo ~ Cidade, data = dados_init)

#SIM
ggscatter(dados_init, x = "Renda", y = "Saldo",
          color = "Cidade", add = "reg.line" ) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Cidade)
  ) +
  theme_minimal() + 
  labs(x = "Renda - média(Renda)") +
  theme(plot.title = element_text(hjust = 0.5))  + scale_colour_brewer(palette = 'Set1')
##ANCOVA

ancova_model = aov(Saldo ~ Cidade + Renda, data = dados_init, contrasts = list(Cidade = contr.sum))
Anova(ancova_model, type="III")

ancova_model$coefficients

summary(ancova_model)

res = residuals(ancova_model)
pred = fitted(ancova_model)

ggplot(data.frame(res,pred), aes(x = pred, y = res)) +
  labs(x = "Preditos", title = "Resíduos vs Preditos",y ="Resíduos") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(color = "springgreen4") 

rst = rstudent(ancova_model)

rst_matrix = data.frame(index = 1:60, rst)

ggplot(rst_matrix) + geom_point(aes(x = index, y = rst), color = 'red') + theme_minimal() + 
  geom_text_repel(aes(x = index, y = rst, label = index), data = rst_matrix[abs(rst_matrix$rst)> 3,], color = 'red')+
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = '', y = 'Resíduo Externamente Studentizado') +
  geom_abline(intercept = 3, slope = 0, col = 'steelblue') +
  geom_abline(intercept = -3, slope = 0, col = 'steelblue') +
  ylim(-3.5, 3.5)

ggplot(data = data.frame(res), aes(sample = res))  +
  labs(x = "Teóricos", title = "Qqplot resíduos",y ="Observados") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_qq(color = "springgreen4") + stat_qq_line()

library('nortest')
shapiro.test(res)
ad.test(res)
cvm.test(res)


#Pela matriz de planejamento(tabelas anova para os testes de hipótese)

#Completo
dados_init$Renda = dados_init$Renda - mean(dados_init$Renda)

matriz = model.matrix(Saldo ~ Renda + Cidade, dados_init, contrasts = list(Cidade = 'contr.sum'))

HAT = matriz%*%solve(t(matriz)%*%matriz)%*%t(matriz)

estim = HAT%*%matrix(dados_init$Saldo,ncol=1)
estim
pred = HAT%*%estim
SQE_c = sum((dados_init$Saldo-pred)^2)

#Reduzido(sem os taus)
matriz_r = model.matrix(Saldo ~ Renda, dados_init)

HAT_r = matriz_r%*%solve(t(matriz_r)%*%matriz_r)%*%t(matriz_r)

estim = HAT_r%*%matrix(dados_init$Saldo,ncol=1)
estim
pred = HAT_r%*%estim
SQE_r = sum((dados_init$Saldo-pred)^2)

FF = ((SQE_r - SQE_c)/(3))/(SQE_c/(60-5))

FF

#Reduzido 2 (sem o coeficiente beta)
matriz_r2 = model.matrix(Saldo ~ Cidade, dados_init,  contrasts = list(Cidade = 'contr.sum'))

HAT_r2 = matriz_r2%*%solve(t(matriz_r2)%*%matriz_r2)%*%t(matriz_r2)

estim2 = HAT_r2%*%matrix(dados_init$Saldo,ncol=1)
estim2
pred2 = HAT_r2%*%estim2
SQE_r2 = sum((dados_init$Saldo-pred2)^2)

FF2 = ((SQE_r2 - SQE_c)/(1))/(SQE_c/(60-5))

FF2


#Intervalos de confiança
estim = SQE_c/55

var_estima = estim*solve(t(matriz)%*%matriz)

coef_taus = ancova_model$coefficients[2:4]

for(i in 3:4){
  for(j in (i+1):5){
    var_diff = var_estima[i,i] + var_estima[j,j] - var_estima[i,j] - var_estima[j,i]
    quantil_f = qf(0.95,length(taus) - 1, 55)
    margem_erro = sqrt((length(taus) - 1)*quantil_f*var_diff)
    #scheffe
    print(paste0("Tau",(i-2),"-Tau",(j-2)," = ",
                 c(coef_taus[i-2] - coef_taus[j-2] - margem_erro,
                   coef_taus[i-2] - coef_taus[j-2] + margem_erro),"var: ",var_diff))
  }
}

#para tau4
var_tau4 = sum(var_estima[3:5,3:5])
cov_tau1_tau4 = var_tau4 + var_estima[3,3] - 2*( -var_estima[3,3] - var_estima[3,4] - var_estima[3,5])
cov_tau2_tau4 = var_tau4 + var_estima[4,4] - 2*( -var_estima[4,4] - var_estima[4,3] - var_estima[4,5])
cov_tau3_tau4 = var_tau4 + var_estima[5,5] - 2*( -var_estima[5,5] - var_estima[5,3] - var_estima[5,4])

c((-sum(coef_taus) - coef_taus[1])- sqrt(3*quantil_f*cov_tau1_tau4),
  (-sum(coef_taus) - coef_taus[1])+ sqrt(3*quantil_f*cov_tau1_tau4))

c((-sum(coef_taus) - coef_taus[2])- sqrt(3*quantil_f*cov_tau2_tau4),
  (-sum(coef_taus) - coef_taus[2])+ sqrt(3*quantil_f*cov_tau2_tau4))

c((-sum(coef_taus) - coef_taus[3])- sqrt(3*quantil_f*cov_tau3_tau4),
  (-sum(coef_taus) - coef_taus[3])+ sqrt(3*quantil_f*cov_tau3_tau4))

########Bonferroni

for(i in 3:4){
  for(j in (i+1):5){
    var_diff = var_estima[i,i] + var_estima[j,j] - var_estima[i,j] - var_estima[j,i]
    quantil_t = qt(1-0.05/12, 55)
    margem_erro = quantil_t*sqrt(var_diff)
    #scheffe
    print(paste0("Tau",(i-2),"-Tau",(j-2)," = ",
                 c(coef_taus[i-2] - coef_taus[j-2] - margem_erro,
                   coef_taus[i-2] - coef_taus[j-2] + margem_erro),"var: ",var_diff))
  }
}

#para tau4
var_tau4 = sum(var_estima[3:5,3:5])
cov_tau1_tau4 = var_tau4 + var_estima[3,3] - 2*( -var_estima[3,3] - var_estima[3,4] - var_estima[3,5])
cov_tau2_tau4 = var_tau4 + var_estima[4,4] - 2*( -var_estima[4,4] - var_estima[4,3] - var_estima[4,5])
cov_tau3_tau4 = var_tau4 + var_estima[5,5] - 2*( -var_estima[5,5] - var_estima[5,3] - var_estima[5,4])

c((-sum(coef_taus) - coef_taus[1])- quantil_t* sqrt(cov_tau1_tau4),
  (-sum(coef_taus) - coef_taus[1])+ quantil_t* sqrt(cov_tau1_tau4))

c((-sum(coef_taus) - coef_taus[2])- quantil_t* sqrt(cov_tau2_tau4),
  (-sum(coef_taus) - coef_taus[2])+ quantil_t* sqrt(cov_tau2_tau4))

c((-sum(coef_taus) - coef_taus[3])- quantil_t* sqrt(cov_tau3_tau4),
  (-sum(coef_taus) - coef_taus[3])+ quantil_t* sqrt(cov_tau3_tau4))

library('emmeans')

marginal = emmeans(ancova_model, ~Cidade)

confint(pairs(marginal, adjust="scheffe"))
#write.csv(dados_init, 'dados_simulados_ancova_final.csv')

confint(pairs(marginal, adjust="bonf"))

# library(multcomp)
# postHocs <- glht(ancova_model, linfct = mcp(Cidade = "bonf"))
# summary(postHocs)
