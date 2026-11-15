# EST335 — GIF da curva Precision–Recall com cutoff decrescente
# Instale uma vez: install.packages(c('ggplot2','gganimate','gifski'))
library(ggplot2)
library(gganimate)
library(gifski)

# O CSV acompanha este script e permite reproduzir a mesma simulação do GIF.
dados <- read.csv('EST335_PR_dados.csv')
y <- dados$classe
s <- dados$escore
prev <- mean(y == 1)

# Curva PR completa, construída ordenando os escores (maior -> menor)
ord <- order(s, decreasing = TRUE)
y_ord <- y[ord]
vp_ac <- cumsum(y_ord == 1)
fp_ac <- cumsum(y_ord == 0)
curva <- data.frame(
  recall = c(0, vp_ac / sum(y == 1)),
  precision = c(1, vp_ac / (vp_ac + fp_ac))
)

# Cutoffs em ordem NUMÉRICA DECRESCENTE: 1 -> 0
cutoffs <- c(1, seq(0.99, 0.01, length.out = 55), 0)
pontos <- do.call(rbind, lapply(seq_along(cutoffs), function(i) {
  c <- cutoffs[i]
  vp <- sum(s >= c & y == 1)
  fp <- sum(s >= c & y == 0)
  data.frame(quadro = i, cutoff = c, VP = vp, FP = fp,
             recall = vp / sum(y == 1),
             precision = if ((vp + fp) == 0) 1 else vp / (vp + fp))
}))

pontos$rotulo <- sprintf(
  'Cutoff: %.2f  |  Recall: %.1f%%  |  Precision: %.1f%%  |  VP: %d  |  FP: %d',
  pontos$cutoff, 100 * pontos$recall, 100 * pontos$precision,
  pontos$VP, pontos$FP
)

# A curva permanece fixa; apenas o ponto e o texto são animados.
p <- ggplot() +
  geom_hline(yintercept = prev, linetype = 'dashed', color = 'grey55') +
  geom_step(data = curva, aes(recall, precision),
            direction = 'hv', color = '#2868AA', linewidth = 1.1) +
  geom_segment(data = pontos,
               aes(x = recall, xend = recall, y = 0, yend = precision),
               linetype = 'dotted', color = '#D54F36', alpha = .5) +
  geom_point(data = pontos, aes(recall, precision),
             color = '#D54F36', size = 3.8) +
  geom_label(data = pontos,
             aes(x = .52, y = .16, label = rotulo),
             size = 4, label.size = .15, fill = 'white') +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  labs(title = NULL,
       subtitle = sprintf('Linha tracejada: prevalência positiva = %.1f%%', 100 * prev),
       x = 'Recall (sensibilidade)', y = 'Precision (precisão)') +
  theme_minimal(base_size = 17) +
  theme(plot.title = element_text(face = 'bold', color = '#293C67'),
        plot.margin = margin(18, 24, 18, 18)) +
  transition_manual(quadro)  # garante a ordem correta dos quadros

anim <- animate(p, nframes = nrow(pontos), fps = 7,
                width = 1400, height = 720, res = 120,
                renderer = gifski_renderer(loop = TRUE))
anim_save('images/regressao/PR_cutoff.gif', animation = anim)
