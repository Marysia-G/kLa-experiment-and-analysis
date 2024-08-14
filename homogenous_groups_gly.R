# library

library('viridis')
library('ggplot2')
library('multcompView')
library('dplyr')

# Create data
data <- data.frame(
  vessel = c("O_96_0.25",	"O_96_0.25",	"O_96_0.55",	"O_96_0.55",	"O_48_0.29",	"O_48_0.29",	"O_48_0.56",	"O_48_0.56",	"O_24_0.29",	"O_24_0.29",	"O_24_0.56",	"O_24_0.56",	"□_24_0.55",	"□_24_0.55",	"□_24_1.10",	"□_24_1.10",	"T_0.25",	"T_0.25",	"T_0.56",	"T_0.56",	"F_0.25",	"F_0.25",	"F_0.56",	"F_0.56",	"F_1.12",	"F_1.12"),
  gly = c(29.0250157675576,	27.5154599233521,	33.2827606929065,	31.0044074638739,	43.5839303026593,	37.5968375175949,	98.4775046147802,	98.5766631846862,	69.3035405536721,	74.9004366591758,	100,	100,	78.5464178931004,	77.9316825034497,	100,	100,	0.38833439063314,	6.54301223631286,	43.355814857921,	35.4472534213869,	16.9192130478563,	20.0936820442251,	28.8310430963686,	32.0804430122482,	43.9322106606102,	34.7980861109465))

# What is the effect of the treatment on the value ?
anova <- aov(gly ~ vessel, data = data)
summary(anova)

# Tukey test to study each pair of treatment :
tukey <- TukeyHSD(x = anova, 'vessel', conf.level = 0.95)
print(tukey)

# Tuckey test representation :
plot(tukey , las = 1, col = "brown")

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(data, vessel) %>%
  summarise(mean=mean(gly), quant = quantile(gly, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$vessel)
Tk$cld <- cld$Letters

print(Tk)

# Create a vector named "new_order" containing the desired order
new_order <- with(data, reorder(vessel, gly, median, na.rm = TRUE))

# boxplot

ggplot(data, aes(x = new_order, gly)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "", y = "consumed glycerol [%]") +
  ylim(0, 110) +
  ggtitle('Consumed glycerol') +
  theme_bw(base_size = 20) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Rotate x-axis labels
  geom_text(data = Tk, aes(x = vessel, y = quant, label = cld), size = 6, vjust = -1.5, hjust = 0.5) +
  scale_fill_viridis_d(option='mako')


# saving the final figure
ggsave("boxplot_gly.png", width = 7, height = 7, dpi = 1000)