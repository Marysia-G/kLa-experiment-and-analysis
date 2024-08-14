# library

library('viridis')
library('ggplot2')
library('multcompView')
library('dplyr')

# Create data
data <- data.frame(
  vessel = c("O_96_0.25",	"O_96_0.25",	"O_96_0.55",	"O_96_0.55",	"O_48_0.29",	"O_48_0.29",	"O_48_0.56",	"O_48_0.56",	"O_24_0.29",	"O_24_0.29",	"O_24_0.56",	"O_24_0.56",	"□_24_0.55",	"□_24_0.55",	"□_24_1.10",	"□_24_1.10",	"T_0.25",	"T_0.25",	"T_0.56",	"T_0.56",	"F_0.25",	"F_0.25",	"F_0.56",	"F_0.56",	"F_1.12",	"F_1.12"),
  ca = c(0,	0,	0.483701310469734,	0.423706103924822,	0.382216810392963,	0.478019480612761,	0.467558482716349,	0.385083110688221,	0.313957921474518,	0.341196710470746,	0.641677255576111,	0.815339537191298,	0,	0,	0,	0,	0.354158505852974,	0.292907081875059,	0.687941592751435,	0.653987280214229,	0.345904679430385,	0.327601771898057,	0,	0,	0.729234115804932,	0.709534451562692))

# What is the effect of the treatment on the value ?
anova <- aov(ca ~ vessel, data = data)
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
  summarise(mean=mean(ca), quant = quantile(ca, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$vessel)
Tk$cld <- cld$Letters

print(Tk)

# Create a vector named "new_order" containing the desired order
new_order <- with(data, reorder(vessel, ca, median, na.rm = TRUE))

# boxplot

ggplot(data, aes(x = new_order, ca)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "", y = "concentration [g/L") +
  ylim(0,1) +
  ggtitle('Citric acid') +
  theme_bw(base_size = 20) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Rotate x-axis labels
  geom_text(data = Tk, aes(x = vessel, y = quant, label = cld), size = 6, vjust = -2, hjust = 0.5) +
  scale_fill_viridis_d(option='rocket')


# saving the final figure
ggsave("boxplot_ca.png", width = 7, height = 7, dpi = 1000)