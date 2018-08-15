
## incisor measurements from Jeremy

library(dplyr)
library(reshape2)
library(ggplot2)

incisors <- read.csv('spreadsheets/brown-Porcupine Measurements.csv', na.strings = 'N/A',
                     colClasses = c(rep('factor', 3), rep('numeric', 8)))

head(incisors)
colnames(incisors) <- c('specimenID', 'species', 'sex', 'RUI_l', 'RUI_w', 'LUI_l', 'LUI_w', 'RLI_l', 'RLI_w', 'LLI_l', 'LLI_w')

  erdo_incisors <- incisors[incisors$species == 'E. dorsatum',]  # porcupine
  myco_inisors <- incisors[incisors$species == 'M. coypus',]     # nutria

# grouped by species/sex
  melted <- melt(incisors, id.vars = c('specimenID', 'species', 'sex'))
  grouped <- group_by(melted, species, sex, variable)

  stats_spec_sex <-  summarise(grouped, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), n = sum(!is.na(value)),
            se = sd(value, na.rm = TRUE)/sqrt(sum(!is.na(value))))
  
  stats_spec_sex
    ## will have some NAs for SD and SE because some combos have only n = 1 (e.g., ERDO male)
  
# grouped by just species:
  grouped_simple <- group_by(melted, species, variable)

  stats_spec <-   summarise(grouped_simple, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), 
                            n = sum(!is.na(value)), se = sd(value, na.rm = TRUE)/sqrt(sum(!is.na(value))))
  stats_spec    


  
# plot

  ## get widths only (length doesn't really matter)
  melted_w <- melted[melted$variable == 'RUI_w' | melted$variable == 'LUI_w' | melted$variable == 'RLI_w' | melted$variable == 'LLI_w',]

spec_plot <- ggplot(melted_w, aes(x = factor(variable), y = value, fill = species)) +
                    geom_boxplot(alpha = 0.5) +
                    geom_point(aes(fill = species), size = 4, shape = 21, position = position_jitterdodge()) +
                    ylab('Incisor width (mm)') +  
                    scale_fill_grey() + theme_classic() +
              theme(axis.text=element_text(size=20, colour = 'black'),
                    axis.text.x=element_text(angle = 35, hjust = 1),
                    axis.title.x=element_blank(),
                    axis.title.y=element_text(size=22, color = 'black', margin = margin(0,15,0,0)),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    axis.line.x = element_line(size = 1, colour = 'black'),
                    axis.line.y = element_line(size = 1, colour = 'black'),
                    axis.ticks = element_line(size = 1, colour = 'black'),
                    panel.background = element_rect(fill = 'white'),
                    legend.position = 'none') ## optionally remove legend.position = 'none

spec_plot
    
## why 2 sets of points? 


## averages

head(melted_w)

melted_w_erdo <- melted_w[melted_w$species == 'E. dorsatum',]
melted_w_myco <- melted_w[melted_w$species == 'M. coypus',]

stats_width_erdo <- summarise(melted_w_erdo, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
                         n = sum(!is.na(value)), se = sd(value, na.rm = TRUE)/sqrt(sum(!is.na(value))))
stats_width_erdo

min(melted_w_erdo$value[!is.na(melted_w_erdo$value)])
max(melted_w_erdo$value[!is.na(melted_w_erdo$value)])

stats_width_myco <- summarise(melted_w_myco, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
                              n = sum(!is.na(value)), se = sd(value, na.rm = TRUE)/sqrt(sum(!is.na(value))))
stats_width_myco

min(melted_w_myco$value[!is.na(melted_w_myco$value)])
max(melted_w_myco$value[!is.na(melted_w_myco$value)])
