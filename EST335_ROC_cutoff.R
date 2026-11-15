# EST335 — ROC: cutoff decrescente, ponto da esquerda para a direita
# install.packages(c('ggplot2', 'gganimate', 'gifski', 'pROC'))
library(ggplot2)
library(gganimate)
library(gifski)
library(pROC)

set.seed(335)
n0 <- 350; n1 <- 150
classe <- c(rep(0L, n0), rep(1L, n1))
escore <- c(rbeta(n0, 2.2, 5), rbeta(n1, 4.6, 2.8))

# Curva empírica completa, mantida fixa na animação.
limiares <- c(Inf, sort(unique(escore), decreasing = TRUE), -Inf)
curva <- data.frame(
  FPR = vapply(limiares, function(c) mean(escore[classe == 0] >= c), numeric(1)),
  TPR = vapply(limiares, function(c) mean(escore[classe == 1] >= c), numeric(1))
)
auc_val <- as.numeric(pROC::auc(pROC::roc(classe, escore,
                  levels = c(0, 1), direction = '<', quiet = TRUE)))

# IMPORTANTE: os quadros são numerados em ordem explícita.
cortes <- seq(1, 0, length.out = 81)
pontos <- data.frame(
  quadro = seq_along(cortes),
  cutoff = cortes,
  FPR = vapply(cortes, function(c) mean(escore[classe == 0] >= c), numeric(1)),
  TPR = vapply(cortes, function(c) mean(escore[classe == 1] >= c), numeric(1))
)
pontos$rotulo <- sprintf('Cutoff: %.2f   |   TPR: %.1f%%   |   FPR: %.1f%%   |   AUC: %.3f',   
                         pontos$cutoff, 100*pontos$TPR, 100*pontos$FPR, auc_val)

p <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey70') +
  geom_step(data = curva, aes(FPR, TPR), color = '#2465A4', linewidth = 1.2) +
  geom_segment(data = pontos, aes(x = FPR, xend = FPR, y = 0, yend = TPR),
               linetype = 'dotted', color = '#D45A3B') +
  geom_segment(data = pontos, aes(x = 0, xend = FPR, y = TPR, yend = TPR),
               linetype = 'dotted', color = '#D45A3B') +
  geom_point(data = pontos, aes(FPR, TPR), color = '#D45A3B', size = 3.8) +
  geom_label(data = pontos, aes(x = 0.50, y = 0.09, label = rotulo),
             size = 4.1, fill = 'white', label.size = 0.2) +
  labs(title = NULL,
       subtitle = NULL,
       x = 'Taxa de falsos positivos (FPR)',
       y = 'Taxa de verdadeiros positivos (TPR)',
       caption = NULL) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = 'off') +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = 'bold'),
        plot.margin = margin(22, 35, 25, 25)) +
  transition_manual(quadro)  # Preserva a ordem 1, 2, ..., 81.

animacao <- animate(p, nframes = nrow(pontos), fps = 10,
                    width = 1400, height = 680,
                    renderer = gifski_renderer(loop = TRUE))
anim_save('images/regressao/ROC_cutoff.gif', animation = animacao)

