library(ggplot2)
library(viridis)


x <- 1:1000

h_1 <- 0.1*(x-300)^2

h_2 <- 0.5*h_1

ggplot(data.frame(Time = c(x,x),Hazard = c(h_1,h_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Hazard, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_color_viridis_d()


f_1 <- function(x){ 0.00000001*(x-300)^2}
f_2 <- function(x){ 0.00000001*0.5*(x-300)^2}

H_1 <- sapply(x, function(y) integrate(f_1,0,y)$value)
H_2 <- sapply(x, function(y) integrate(f_2,0,y)$value)

S_1 <- exp(-H_1)
S_2 <- exp(-H_2)

ggplot(data.frame(Time = c(x,x),Survival = c(S_1,S_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Survival, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  scale_color_viridis_d()


# ------ Prop Example 2

x <- 1:1000

h_1 <- log(x)

h_2 <- 3*h_1

ggplot(data.frame(Time = c(x,x),Hazard = c(h_1,h_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Hazard, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_color_viridis_d()


f_1 <- function(x){ 0.0005*log(x)}
f_2 <- function(x){ 0.0005*3*log(x)}

H_1 <- sapply(x, function(y) integrate(f_1,0,y)$value)
H_2 <- sapply(x, function(y) integrate(f_2,0,y)$value)

S_1 <- exp(-H_1)
S_2 <- exp(-H_2)

ggplot(data.frame(Time = c(x,x),Survival = c(S_1,S_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Survival, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  scale_color_viridis_d()

# non prop example 1 -------

x <- 1:1000

h_1 <- rep(4,1000)

h_2 <- 3 + 1*(x > 500)*0.001*(x-500)

ggplot(data.frame(Time = c(x,x),Hazard = c(h_1,h_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Hazard, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_color_viridis_d()


f_1 <- function(x){0.0005*(x> 0)*4}
f_2 <- function(x){0.0005*( 3 + 1*(x > 500)*0.001*(x-500))}

H_1 <- sapply(x, function(y) integrate(f_1,0,y)$value)
H_2 <- sapply(x, function(y) integrate(f_2,0,y)$value)

S_1 <- exp(-H_1)
S_2 <- exp(-H_2)

ggplot(data.frame(Time = c(x,x),Survival = c(S_1,S_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Survival, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  scale_color_viridis_d()

# non prop example 2 -------

x <- 1:1000

h_1 <- x^2

h_2 <- (x-500)^2

ggplot(data.frame(Time = c(x,x),Hazard = c(h_1,h_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Hazard, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_color_viridis_d()


f_1 <- function(x){ 0.00000003*x^2}
f_2 <- function(x){ 0.00000003*(x-500)^2}

H_1 <- sapply(x, function(y) integrate(f_1,0,y)$value)
H_2 <- sapply(x, function(y) integrate(f_2,0,y)$value)

S_1 <- exp(-H_1)
S_2 <- exp(-H_2)

ggplot(data.frame(Time = c(x,x),Survival = c(S_1,S_2),
                  Group = c(rep("1",1000),rep("2",1000))),
       aes(x = Time, y = Survival, col = Group)) + 
  geom_line(size = 1.2) + theme_classic() +
  scale_color_viridis_d()

