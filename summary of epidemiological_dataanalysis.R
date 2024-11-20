
## load the required libraries
library(SpatialEpi)
library(INLA)
library(dplyr)
library(ggplot2)
library(spdep)

##load data
data("pennLC")

#---------------------------------------------------------
##Exercise 1 
##data exploration
class(pennLC)
names(pennLC)


##exploring data structure
head(pennLC$data)
head(pennLC$smoking)
plot(pennLC$spatial.polygon)

#---------------------------------------------------------

###data preparation
##Group the data for each county
d <- group_by(pennLC$data, county) %>% summarize(Y = sum(cases))
head(d)

#Expected cases computation:
# Ei = ∑[r(s)j × n(i)j]
# • Ei : Expected count in county i
# • r(s)j : Rate (number of cases ÷ population) in stratum j of the Pennsylvania population
# • n(i)j : Population in stratum j of county i
# • j : Index for strata (with m total strata implied)

###expected() function of the package SpatialEpi


##arranging all data into a dataframe
pennLC$data <- pennLC$data[order(pennLC$data$county, pennLC$data$race, pennLC$data$gender, pennLC$data$age), ]

## computing expected values
E <- expected(population = pennLC$data$population, cases = pennLC$data$cases, n.strata = 16)

d$E <- E


##smokers percentage
d <- merge(d, pennLC$smoking, by = "county")



###Exercise 2 ----------------------------------------------------

d$SMR = d$Y/d$E


map <- st_as_sf(pennLC$spatial.polygon)
rownames(d) <- d$county
map$county <- d$county
##join sf geometry with d
map |> left_join(d) -> map
##plot SMR

plot(map)




##-----------------------------------------------------------------


####Modelling
##Exercise 3-------------------------------------------------------


##generating a neighborhood structure
nb <- poly2nb(map)
head(nb)

nb2INLA("map.adj", nb)

g <- inla.read.graph(filename = "map.adj")

plot(map$geometry)
plot(nb, coords = st_centroid(map$geometry), add = TRUE, col = "red")


##create index for the random effects
map$re_u <- 1:nrow(map)
map$re_v <- 1:nrow(map)


##define model formula
formula <- Y ~ smoking + 
  f(re_u, model = "besag", graph = g, scale.model = TRUE) + 
  f(re_v, model = "iid")

##run the model
result<-inla(formula = formula, family = "poisson",E=E,data=map)

summary(result)

# We see the intercept β^0=
#   -0.323 with a 95% credible interval equal to (-0.621, -0.028), and the coefficient of smoking is β^1=
#   1.156 with a 95% credible interval equal to (-0.081, 2.384). We can plot the posterior distribution of the smoking coefficient. We do this by calculating a smoothing of the marginal distribution of the coefficient with inla.smarginal() and then plot it with ggplot() of the ggplot2 package.


marginal <- inla.smarginal(result$marginals.fixed$smoking)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()





##
head(result$summary.fitted.values)

##adding results to the map dataframe
map$RR <- result$summary.fitted.values[, "mean"]
map$LL <- result$summary.fitted.values[, "0.025quant"]
map$UL <- result$summary.fitted.values[, "0.975quant"]


##plot spatial random effect
plot(map$RR)
ggplot(map) +
  geom_sf(aes(fill =  RR),linewidth=0.0001, alpha=0.9)+
  scale_fill_viridis_c()+
  labs(title="Related Risk")
  theme_minimal()+
  theme(legend.position,inside=c(0.5,0),
        axis.text = element_blank(),
        axis.title = element_blank())
##spatial random effect
map$re_u_coeff <- result$summary.random$re_u$mean

ggplot(map) +
   geom_sf(aes(fill =  re_u_coeff),linewidth=0.0001, alpha=0.9) +
   scale_fill_viridis_c()+
   labs(title="Spatial Random Effect")
   theme_minimal()+
   theme(legend.position,inside=c(0.5,0),
         axis.text = element_blank(),
         axis.title = element_blank())
   