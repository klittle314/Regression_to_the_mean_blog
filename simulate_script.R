#SPC regression to the mean 3 April 2019
#Kevin Little, Ph.D.
library(zoo)
library(ggplot2)

set.seed(1234)

#simulate values from a base line of 20 observations using normal(mu,sd)ul; 
#simulate a further n values from the same normal distribution
#compute the predictions according to the simple betting strategy:
#if value(i) is less than the mean, predict value(i+1) will be greater than value(i)
#and symmetrically if value(i) is greater than the mean.
simul_function <- function(n0=20,n,mu=15,sd=3,center_line){
    x1 <- rnorm(n0,mu,sd)
    testMean <- mean(x1)
    testMedian <- median(x1)
    x2 <- rnorm(n,mu,sd)
    bet_base <- c(x1[n0],x2[1:n-1])
    predict_out <- rep(NA,n)
    actual_out <- rep(NA,n)
    bet_frac <- rep(NA,n)
    ifelse(center_line=="mean",center_line1 <- testMean,center_line1 <-testMedian)
    for(i in 1:n){
      predict_out[i] <- ifelse(bet_base[i] < center_line1, "higher", "lower")
      actual_out[i] <- ifelse(bet_base[i] < x2[i],"higher","lower")
      bet_frac[i] <- ifelse(predict_out[i]==actual_out[i],1,0)
    }
    df_final <- cbind.data.frame(bet_base,x2,predict_out,actual_out,bet_frac)
    fraction <- mean(df_final$bet_frac)
    return(fraction)
}

#simulate 1000 repetitions of n size 50 and examine the performance of the betting rul

bet_success <- rep(NA,1000)
for(j in 1:1000){
  bet_success[j] <- simul_function(n=50,center_line="median")
}

hist(bet_success)
mean(bet_success)
summary(bet_success)

#the mean is about 74%

#make the individuals control chart used in the blog post

set.seed(12345)
x1_values <- rnorm(n=20,15,3)
x2_values <- rnorm(n=20,15,3)
mean_line <- mean(x1_values)
median_line <- median(x1_values)


#choose center_line as mean_line for control chart plot or median_line for run chart plot

center_line <- mean_line

R_bar <- mean(abs(diff(x1_values)))

UCL <- mean_line + 2.66*R_bar
LCL <- mean_line - 2.66*R_bar
df_plot <- cbind.data.frame(1:20,x1_values,rep("base",20))

names(df_plot) <- c("order","values","epoch")
df_plot_new <- cbind.data.frame(21:40,x2_values,rep("new sample",20))
names(df_plot_new) <- c("order","values","epoch")
df_all <- rbind.data.frame(df_plot,df_plot_new)

#choose center_line as mean_line if you want to display the control chart
#choose center_line as median_line if you want to display the run chart

bet_base <- c(x1_values[20],x2_values[1:19])
predict_out <- rep(NA,20)
actual_out <- rep(NA,20)
bet_frac <- rep(NA,20)
for(i in 1:20){
  predict_out[i] <- ifelse(bet_base[i] < center_line, "higher", "lower")
  actual_out[i] <- ifelse(bet_base[i] < x2_values[i],"higher","lower")
  bet_frac[i] <- ifelse(predict_out[i]==actual_out[i],1,0)
}
df_bet <- cbind.data.frame(bet_base,x2_values,predict_out,actual_out,bet_frac)
names(df_bet)[5] <- "bet_wins"

df_all1 <- cbind.data.frame(df_all,c(rep(0,20),df_bet$bet_wins))
names(df_all1)[4] <- "bet_wins"
df_all1$bet_wins <- as.character(df_all1$bet_wins)
df_all1$shape1 <- as.factor(paste0(df_all1$epoch,"_",as.character(df_all1$bet_wins)))

df_all1$shape1 <- gsub("base_0","base",df_all1$shape1)
df_all1$shape1 <- gsub("new sample_0","bet loses",df_all1$shape1)
df_all1$shape1 <- gsub("new sample_1","bet wins",df_all1$shape1)
names(df_all1)[5] <- "point_history"

#make sure you use center_line <- mean_line

p1 <- ggplot(data=df_all1,aes(x=order,y=values,shape=point_history))+
      theme_bw()+
      geom_point(size=rel(2.5))+
      geom_hline(yintercept=center_line)+
      geom_hline(yintercept=UCL,linetype="dashed")+
      geom_hline(yintercept=LCL,linetype="dashed")+
      geom_vline(xintercept=20.5,linetype="dotted")+
      labs(title="Individuals Control Chart",
           subtitle="Center line: mean of base values; dashed lines: control limits from base values")+
      ylab("value")+
      xlab("order")

p1 + labs(shape="Point history")

#make sure you use center_line as median_line

p2 <- ggplot(data=df_all1,aes(x=order,y=values,shape=point_history))+
  theme_bw()+
  geom_point(size=rel(2.5))+
  geom_hline(yintercept=center_line)+
  geom_vline(xintercept=20.5,linetype="dotted")+
  labs(title="Run Chart",
       subtitle="Center line: median of base values")+
  ylab("value")+
  xlab("order")

p2 + labs(shape="Point history")