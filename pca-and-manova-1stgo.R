gc.analysis.df
?prcomp
gc.analysis.df.lite = gc.analysis.df[,5:length(gc.analysis.df)]
# row.names(gc.analysis.df.lite) = gc.analysis.df.lite$sample

gc.pca = prcomp(~mu + lambda + A + AUC, data = gc.analysis.df.lite, scale = TRUE, center = TRUE)
gc.pca

# capture.output(gc.pca)
# summ = summary(gc.pca)$importance
# summ = as.data.frame(summ)

prop.var = as.data.frame(summary(gc.pca)$importance)[2,]
varExplained = data.frame(PC = 1:4, prop.variance = t(prop.var)[,1])
varExplained

gc.pc = data.frame(gc.analysis.df.lite[,1:5], as.data.frame(gc.pca$x))
gc.pc

ggplot(gc.pc, aes(x = PC1, y = PC2, fill = sample)) +
  geom_point(size = 3.5, colour = "black", pch = 21) +
  scale_fill_manual(values = brewer.pal(8, "Paired")) +
  ggtitle("PCA; all variables centered & scaled by prcomp()") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.9, size = 14),
        axis.text.x = element_text(size=12),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
# ggsave("pca-biplot.png", dpi = 300)

ggplot(varExplained, aes(x = factor(PC), y = prop.variance)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Principal componant") +
  scale_y_continuous(name = "Proportion of variance explained") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.9, size = 14),
        axis.text.x = element_text(size=12),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
# ggsave("pca-varPlot.png", dpi = 300)

# PC2 looks kind of meaningful, even is the proportion of variance explained is very low
# anova of PC1
pc1.aov = aov(PC1 ~ media * gcn20.genotype * arb1.genotype, data = gc.pc)
pc1.aov
summary(pc1.aov)
capture.output(summary(pc1.aov), file = "summary-pc1-anova.txt")


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(gc.pc, aes(x = sample, y = PC1, colour = media, shape = strain)) +
  geom_point(position = position_jitter(w = 0.15), size = 2.5) +
  scale_colour_manual(values=cbPalette[2:3]) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.4, size = 14),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y  = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

foo = TukeyHSD(pc1.aov, ordered = TRUE)
foo
# 
# I think I'm overfitting.... too many DF = 1 and p adj = 0

pc1.aov2 = aov(PC1 ~ media * strain, data = gc.pc)
pc1.aov2
summary(pc1.aov2)
capture.output(summary(pc1.aov2), file = "summary-pc1-anova2.txt")
bar = TukeyHSD(pc1.aov2)
capture.output(TukeyHSD(pc1.aov2), file = "summary-HSD-pc1-anova2.txt")
bar

# This seems fairer somehow...

# 
?manova
## Example on producing plastic film from Krzanowski (1998, p. 381)
tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)
Y
rate <- factor(gl(2,10), labels = c("Low", "High"))
additive <- factor(gl(2, 5, length = 20), labels = c("Low", "High"))
rate
additive
fit <- manova(Y ~ rate * additive)
summary.aov(fit)             # univariate ANOVA tables
summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda
summary(fit)                # same F statistics as single-df terms


gc.analysis.df.lite
fit = manova(Y ~ media * gcn20.genotype * arb1.genotype ,data = gc.analysis.df.lite)
Y = as.matrix(gc.analysis.df.lite[,6:9])
Y

summary.aov(fit)
summary(fit, test = "Wilks")


fit2 = manova(Y ~ gcn20.genotype * media * arb1.genotype ,data = gc.analysis.df.lite)
summary(fit2, test = "Wilks")
