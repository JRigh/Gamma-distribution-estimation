#------------------------
# Method of Moments Gamma
#------------------------

# function that implements the estimators above
MoMgamma <- function(x) {
  
  n <- length(x)
  sample_moment_1 <- sum(x) / n
  sample_moment_2 <- sum(x^2) / n
  
  alpha_mom <- sample_moment_1^2 / (sample_moment_2 - sample_moment_1^2)
  beta_mom <- sample_moment_1 / (sample_moment_2 - sample_moment_1^2)
  
  output <- NULL
  output$alpha_mom <- alpha_mom
  output$beta_mom <- beta_mom
  
  return(output)
}

# generate artificial data
set.seed(2023)
x <- round(rgamma(n = 100, shape = 2, rate = 0.08), 2)
x[1:20]
# [1] 17.49 14.93 16.01 17.85  7.37  5.96 12.27 16.44 28.34 30.28 10.66 25.77 17.21 34.99 28.53 12.98  9.32
# [18] 22.67 12.55 16.81

# save a copy of entire dataset .csv
write.csv(x, 
          "C:/Users/julia/OneDrive/Desktop/github/37. momgamma/dataset.csv",
          row.names = FALSE)

# perform MoM estimation
MoMgamma(x = x)$alpha_mom
# $alpha_mom
# [1] 1.900579
# $beta_mom
# [1] 0.07399452

# ploting the histogram and the theoretical generative distribution

library(tidyverse)
df = data.frame(x)

ggplot(df, aes(x=x)) + 
  geom_histogram(color="white", fill="black", aes(y = ..density..)) +
  stat_function(fun = dgamma,
                args = list(shape= MoMgamma(x = x)$alpha_mom,
                            rate=MoMgamma(x = x)$beta_mom),
                col = "darkred",
                size = 1.5) +
  labs(title = 'Histogram and estimated theoretical density',
       subtitle = 'Gamma dataset',
       y="density", x="x") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Hazard function after 24 weeks

library("flexsurv")

Hgamma(x = c(12, 24, 36), shape = MoMgamma(x = x)$alpha_mom, 
            rate = MoMgamma(x = x)$beta_mom, log=FALSE)
# [1] 0.2872906 0.8226064 1.4585461

# ploting
dfhazard <- data.frame(week = 0:40,
                       CH = Hgamma(x = 0:40, shape = MoMgamma(x = x)$alpha_mom,
                                   rate = MoMgamma(x = x)$beta_mom, log=FALSE))

ggplot(dfhazard, aes(x = week, y = CH))+
  geom_point() + 
  labs(title = 'Cumulative Hazard Rates',
       subtitle = 'For 12, 24 and 36 weeks',
       y="Cumulative Hazard Rate", x="Week") +
  geom_segment(aes(x = 12, y = 0, xend = 12, yend = dfhazard[13,2]), color = 'darkred') +
  annotate('text', label = '0.287', x = 12, y = dfhazard[12,2] +0.2, colour = "darkred", size=5) +
  geom_segment(aes(x = 24, y = 0, xend = 24, yend = dfhazard[25,2]), color = 'darkred') +
  annotate('text', label = '0.823', x = 24, y = dfhazard[25,2] +0.2, colour = "darkred", size=5) +
  geom_segment(aes(x = 36, y = 0, xend = 36, yend = dfhazard[37,2]), color = 'darkred') +
  annotate('text', label = '1.459', x = 36, y =  dfhazard[37,2] +0.2, colour = "darkred", size=5) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----
# end
#----

