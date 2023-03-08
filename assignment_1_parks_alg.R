library(MASS)
library(tibble)
library(ggplot2)
library(readr)
library(dagitty)
library(ggdag)
library(glue)
library(igraph)

df_dat = data.frame(read_csv("a1_data.csv"))

variables = names(df_dat)
results <- list()
W = matrix(0,11,11)

for(i in 1:10){

  for (j in (i+1):11){
    
    # option 1
    corr= cor.test(df_dat[,i], df_dat[,j], method=c("pearson"))

    if(corr$estimate==0) {
      W[i,j]<-0
      relation<-glue("{i} {n}")
    }
    
    #OPTION 2
    x <- df_dat[,i]
    y <- df_dat[,j]
    df <- data.frame(x, y)
    
    fit <- lm(y ~ x, df)
    beta_hat <- summary(fit)$coef[2, "Estimate"]
    se_hat <- summary(fit)$coef[2, "Std. Error"]
    t <- beta_hat/se_hat
    ( pvalue <- 2 * pnorm(abs(t), lower.tail = FALSE) )
    
    res.ftest <- var.test(df_dat[,i],df_dat[,j] , data = df_dat, alternative="less")

    
    if(res.ftest$estimate < 1) {
      W[i,j]<-1
      relation<-glue("{i}->{n}")
    }else {
      W[j,i]<-1
      relation<-glue("{i}<-{n}")
    }
    
    results <- append(results,relation)
  }
}

colnames(W) <- variables
rownames(W) <- variables
network <- graph_from_adjacency_matrix(W , mode='directed')
plot.igraph(network, layout=layout.circle, main="circle", vertex.color="SkyBlue", size=25)


