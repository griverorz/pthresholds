## When does a voter consider himself as nationalist. Variation by party
## @griverorz
## Tue Oct 29 17:38:09 PDT 2013

setwd("~/Documents/datablog/pthresholds")

set.seed(20130947)

library(ggplot2)
library(foreign)
library(rjags)
library(mnormt)

# mycolors
theme_set(theme_bw())
myblue <- rgb(100, 170, 200, max = 255)
myred <- rgb(240, 140, 100, max = 255)
mygreen <- rgb(70, 120, 35, max = 255)


#################### read data ####################
parties <- lapply(list.files(path = "./dta", pattern = ".sav", full.names = TRUE), 
                  function(x) read.spss(x, to.data.frame = TRUE))
names(parties) <- c("Catalonia", "Euskadi", "Galicia")

locs <- list(NA, 3)
locs$Catalonia <- data.frame("nac" = parties$Catalonia$P29,
                             "onac" = parties$Catalonia$P24,
                             "voto" = parties$Catalonia$P20A)
locs$Euskadi <- data.frame("nac" = parties$Euskadi$P20,
                           "onac" = parties$Euskadi$P22,
                           "voto" = parties$Euskadi$P40A)
locs$Galicia <- data.frame("nac" = parties$Galicia$P20,
                           "onac" = parties$Galicia$P22,
                           "voto" = parties$Galicia$P40A)

#################### CLEAN VARIABLES ####################
# cheat catalonia: ciu, psoe, pp, erc, icv
levels(locs$Catalonia$voto) <- c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA, NA, NA)
locs$Catalonia$nacparty <- ifelse(locs$Catalonia$voto %in% c(1,4,5), 1, 2)

# cheat euskadi: pp, pse, pnv, amaiur
levels(locs$Euskadi$voto) <- c(1, 2, NA, NA, 3, 4, NA, NA, NA, NA, NA)
locs$Euskadi$nacparty <- ifelse(locs$Euskadi$voto %in% c(3,4), 1, 2)

# cheat galicia: pp, psoe, bng
levels(locs$Galicia$voto) <- c(1, 2, NA, NA, 3, NA, NA, NA, NA, NA)
locs$Galicia$nacparty <- ifelse(locs$Galicia$voto == 3, 1, 2)

#################### remove na's ####################
levels(locs$Catalonia$onac) <- 1:8
locs$Catalonia$onac <- as.numeric(locs$Catalonia$onac)
locs$Catalonia[, "onac"] <- ifelse(locs$Catalonia[, "onac"] >= 6, 
                                   NA, 
                                   locs$Catalonia[, "onac"])

levels(locs$Euskadi$onac) <- 1:8
locs$Euskadi$onac <- as.numeric(locs$Euskadi$onac)
locs$Euskadi[, "onac"] <- ifelse(locs$Euskadi[, "onac"] >= 6, 
                                 NA, 
                                 locs$Euskadi[, "onac"])

levels(locs$Galicia$onac) <- 1:8
locs$Galicia$onac <- as.numeric(locs$Galicia$onac)
locs$Galicia[, "onac"] <- ifelse(locs$Galicia[, "onac"] >= 6, 
                                 NA, 
                                 locs$Galicia[, "onac"])

#################### join data ####################
locs <- rbind(locs$Catalonia, locs$Euskadi, locs$Galicia)

locs$election <- c(rep("Catalonia", nrow(parties$Catalonia)),
                   rep("Euskadi", nrow(parties$Euskadi)),
                   rep("Galicia", nrow(parties$Galicia)))

locs$voto <- as.numeric(as.character(locs$voto))

locs[, "nac"] <- ifelse(locs[, "nac"] > 90, NA, locs[, "nac"])

locs <- na.omit(locs)

#################### jags model ####################

region <- c("Catalonia", "Euskadi", "Galicia")
jfit <- list(NA, 3)
for (i in 1:3) {
    jdata <- list("N" = nrow(locs[locs$election == region[i],]), 
                  "T" = 4, # nthresholds
                  "P" = c(5,4,3)[i], # nparties
                  "onac" = locs$onac[locs$election == region[i]],
                  "nac" = locs$nac[locs$election == region[i]], 
                  "voto" = locs$voto[locs$election == region[i]], 
                  "pred" = rep(NA, nrow(locs[locs$election == region[i],]))) 
    
    jinits <- function() {
        return(list(
            "thr" = matrix(rep(sort(runif(jdata$T, 1, 10)), jdata$P), 
                nrow = jdata$P, 
                byrow = TRUE)))
        
    }
    
    jmodel <- jags.model("src/jags/pthresholds.jags", 
                         data = jdata,
                         inits = jinits,
                         n.chains = 1,
                         n.adapt = 1E4)
  
    jfit[[i]] <- coda.samples(model = jmodel,
                              variable.names = "thr", 
                              n.iter = 3E3, 
                              thin = 1)
}

## One threshold per party
dfit <- llply(jfit, as.matrix)
getQuantiles <- function(x) quantile(x, c(.025, .5, .975))
dfit <- lapply(dfit, function(x) apply(x, 2, getQuantiles))
dfit <- t(do.call(cbind, dfit))
dfit <- as.data.frame(dfit, row.names = 1:nrow(dfit))
names(dfit) <- c("ymin", "ymean", "ymax")

## organize data for plot
elections <- mapply(rep, region, 
                    sapply(jfit, function(x) ncol(as.matrix(x))))
elections <- do.call(c, elections)

party <- c(rep(c("CIU", "PSOE", "PP", "ERC", "ICV"), times = 4),
           rep(c("PP", "PSE", "PNV", "Amaiur"), times = 4),
           rep(c("PP", "PSOE", "BNG"), times = 4))

dfit <- data.frame(as.vector(dfit),
                   "Threshold" =  factor(c(rep(1:4, each = 5),
                                           rep(1:4, each = 4),
                                           rep(1:4, each = 3))),
                   "party" = party, 
                   "election" = elections)

#################### plot ####################
p <- ggplot(dfit, aes(x = party, 
                      y = ymean, ymin = ymin, ymax = ymax, 
                      group = party, colour = Threshold))
pq <- p + geom_pointrange(size = 0.75) +
    facet_grid( ~ election, scales = "free_x") +
    ggtitle("Posterior distribution of the thresholds") +
    scale_y_continuous("Nationalism", limits = c(1, 10)) +
    scale_x_discrete("Party") +
    geom_hline(aes(yintercept = 5.5), linetype = 3)
ggsave("img/pthreshold.png", pq, width = 2*par("din")[1])
