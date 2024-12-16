
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(ggrepel)

# Load custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='Pt Mono'), 
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 8), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 19, angle = 90), 
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=30, hjust = 0.5, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=15, hjust = 0, margin = margin(b = 10), family = 'Proxima Nova'), 
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    ) 
}




LM_CSI <- read.csv('/Users/davidetissino/Desktop/LM_CSI.csv')

LT_CSI <- read.csv('/Users/davidetissino/Desktop/LT_CSI.csv')





lm_csi <- LM_CSI[, c(136:138)]


lm_summary <- lm_csi %>%
  group_by(
    Were.you.aware.of.the.existence.of.the.PoliPsi.psychological.support.service.before.taking.this.survey.
    ) %>%  # Group by cluster and position
  summarise(Count = n()) %>%  # Count number of players in each group
  ungroup() # Ungroup for the next summarization
 
colnames(lm_summary)[1] <- 'Answer'

tot <- sum(lm_summary$Count)

lm_summary$perc <- round(lm_summary$Count / tot, 4)




ggplot(lm_summary, aes(x = "", y = Count, fill = Answer)) +
  ggtitle('Were You Aware of the Existence of the PoliPsi Psychological Support Service Before Taking This Survey?') +
  geom_col(color = "black") + 
  # geom_text(
  #   aes(
  #     label = paste0(round(perc * 100, 1), '%')
  #   ),
  #   position = position_stack(vjust = 0.5),
  #   color = 'white',
  #   fontface = 'bold',
  #   size = 6,
  #   #nudge_x = 0.25
  #   ) +
  coord_polar(
    theta = "y", 
    start = 0
    ) +
  theme_davide() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(),
    legend.position = 'right', 
    legend.direction = 'vertical', 
    legend.margin = margin(3, 3, 3, 3), 
    plot.margin = margin(t = 15, b = 25), 
    legend.text = element_text(size = 13), 
    plot.title = element_text(hjust = 0, size = 24)
  ) + 
  scale_fill_brewer(
    palette = 'Set1'
  )




ggsave('/Users/davidetissino/Desktop/Were_You_Aware_1.png', dpi = 'retina', height = 9, width = 11)





lt_csi <- LT_CSI[, c(109, 110)]


lt_summary <- lt_csi %>%
  group_by(
    Conosci.il.servizio.di.supporto.psicologico.del.Politecnico...PoliPSI.
  ) %>%  # Group by cluster and position
  summarise(Count = n()) %>%  # Count number of players in each group
  ungroup() # Ungroup for the next summarization

colnames(lt_summary)[1] <- 'Answer'

tot <- sum(lt_summary$Count)

lt_summary$perc <- round(lt_summary$Count / tot, 4)



ggplot(lt_summary, aes(x = "", y = Count, fill = Answer)) +
  ggtitle('Conosci il Servizio di Supporto Psicologico del Politecnico...PoliPSI?') +
  geom_col(color = "black") + 
  # geom_text(
  #   aes(
  #     label = paste0(round(perc * 100, 1), '%')
  #   ),
  #   position = position_stack(vjust = 0.5),
  #   color = 'white',
  #   fontface = 'bold',
  #   size = 6,
  #   #nudge_x = 0.25
  #   ) +
  coord_polar(
    theta = "y", 
    start = 0
  ) +
  theme_davide() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(),
    legend.position = 'right', 
    legend.direction = 'vertical', 
    legend.margin = margin(3, 3, 3, 3), 
    plot.margin = margin(t = 15, b = 25), 
    legend.text = element_text(size = 13), 
    plot.title = element_text(hjust = 0, size = 24)
  ) + 
  scale_fill_brewer(
    palette = 'Set1'
  )


ggsave('/Users/davidetissino/Desktop/Conosci_POLIPSI_1.png', dpi = 'retina', height = 9, width = 11)


