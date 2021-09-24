###############
### Plotting diversification rates
###############

library(ggplot2)
source("scripts/multiplot.R")
data <- read.table("output/hisse/HiSSE.log",header=TRUE)
data <- read.table("output/hisse/HiSSE.log",header=TRUE)

start <- round(0.5*length(data$extinction.1))
end <- length(data$extinction.1)
HiSSE_types <- rep(c("1A", "2A", "1B", "2B"), each = length(data$extinction.1[start:
end]))
HiSSE_types2 <- rep(c("12", "21"), each = length(data$extinction.1[start:
end]))

#extinction
dat_ext <- data.frame(dens = c(data$extinction.1[start:end], data$extinction.2[start:
end], data$extinction.3[start:end], data$extinction.4[start:end]), Type =
HiSSE_types)

#speciation
dat_spec <- data.frame(dens = c(data$speciation.1[start:end], data$speciation.2[start:
end], data$speciation.3[start:end], data$speciation.4[start:end]), Type =
HiSSE_types)

#dispersal
dat_disp<- data.frame(dens = c(data$rate_12[start:end], data$rate_21[start:
end]), Type =HiSSE_types2)

#diversification
dat_div <- data.frame(dens = c(data$speciation.1[start:end]-data$extinction.1[start:
end], data$speciation.2[start:end]-data$extinction.2[start:end], data$speciation.3[
start:end]-data$extinction.3[start:end], data$speciation.4[start:end]-
data$extinction.4[start:end]), Type = HiSSE_types)

#turnover
dat_rel <- data.frame(dens = c(data$extinction.1[start:end]/data$speciation.1[start:
end], data$extinction.2[start:end]/data$speciation.2[start:end], data$extinction.3[
start:end]/data$speciation.3[start:end], data$extinction.4[start:end]/
data$speciation.4[start:end]), Type = HiSSE_types)


pdf("RevBayes_HiSSE_Results.pdf")
p1 <- ggplot(dat_spec, aes(x = dens, fill = Type)) + labs(title = "Speciation", x="
Rate", y="Posterior Density") + geom_density(alpha = 0.5) + guides(fill=
guide_legend(ncol=2,byrow=TRUE))
p2 <- ggplot(dat_ext, aes(x = dens, fill = Type)) + labs(title = "Extinction", x="Rate
", y="Posterior Density") + geom_density(alpha = 0.5) + guides(fill=guide_legend(
ncol=2,byrow=TRUE)) + xlim(0, 0.02)
p3 <- ggplot(dat_div, aes(x = dens, fill = Type)) + labs(title = "Net-Diversification
", x="Rate", y="Posterior Density") + geom_density(alpha = 0.5) + guides(fill=
guide_legend(ncol=2,byrow=TRUE))
p4 <- ggplot(dat_rel, aes(x = dens, fill = Type)) + labs(title = "Relative Extinction
", x="Rate", y="Posterior Density") + geom_density(alpha = 0.5) + guides(fill=
guide_legend(ncol=2,byrow=TRUE))+ xlim(-0.01, 0.8)
p5 <- ggplot(dat_disp, aes(x = dens, fill = Type)) + labs(title = "Transition", x="
Rate", y="Posterior Density") + geom_density(alpha = 0.5) + guides(fill=
guide_legend(ncol=2,byrow=TRUE))
multiplot(p1, p2, p3, p4, p5) 
dev.off()

