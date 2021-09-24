
################
## Plotting ancestral states HISSE
################

pdf("RevBayes_Anc_States_HiSSE.pdf", paper="a4")

library(RevGadgets)
tree_file = "outpu/hisse/anc_states_HiSSE_results.tree"

plot_ancestral_states(tree_file, summary_statistic="MAP",
tip_label_size=0,
xlim_visible=NULL,
node_label_size=0,
show_posterior_legend=TRUE,
node_size_range=c(2, 6),
alpha=0.75)
output_file = "RevBayes_Anc_States_HiSSE.pdf"
#ggsave(output_file, width = 11, height = 9)

dev.off()