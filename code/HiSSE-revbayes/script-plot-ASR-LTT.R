###################################
##### scritp to create the ARS-LTT plot. This is a LTT plot based on the reconstruction of ancestral states in HiSSE
###################################

library(TreePar)
library(treeio)
library(tidytree)

## The file "extract-results-anc_states_HiSSE_results.csv" was created from the resulting tree of the ancestral character reconstruction in HiSSE, here named: anc_states_HiSSE_results.tree 
read.csv("output/hisse/extract-results-anc_states_HiSSE_results.csv", header=T, sep=";", stringsAsFactors = T)->tabla
tabla[,10:11]->tab
which(tab[,2]=="Terrestrial")->k
tab[k,-2]->Terrestrial_times
which(tab[,2]=="Aquatic")->k
tab[k,-2]->Aquatic_times
class(Aquatic_times)
as.vector(Aquatic_times)->Aquatic_times
as.vector(Terrestrial_times)->Terrestrial_times

for_plot_Aquatic<-cbind(sort(as.numeric(Aquatic_times),decreasing=TRUE),seq(1:length(Aquatic_times)))
for_plot_Terrestrial<-cbind(sort(as.numeric(Terrestrial_times),decreasing=TRUE),seq(1:length(Terrestrial_times)))

pdf("LTT-ASR-HiSSE.pdf")
plot(-for_plot_Aquatic[,1],log(for_plot_Aquatic[,2]),pch=1,col="deepskyblue2",xlim=c(-140,0),xlab="Time",ylab="Log Number of lineages", ylim=c(0,8),las=1, bty="n")
points(-for_plot_Terrestrial[,1],log(for_plot_Terrestrial[,2]),pch=1, col="darkgoldenrod3")
dev.off()
