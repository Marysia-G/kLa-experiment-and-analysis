# library

library('viridis')
library('ggplot2')
library('multcompView')
library('dplyr')

# Create data
data <- data.frame(
  vessel = c("O_96_0.25",	"O_96_0.25",	"O_96_0.55",	"O_96_0.55",	"O_48_0.29",	"O_48_0.29",	"O_48_0.56",	"O_48_0.56",	"O_24_0.29",	"O_24_0.29",	"O_24_0.56",	"O_24_0.56",	"□_24_0.55",	"□_24_0.55",	"□_24_1.10",	"□_24_1.10",	"T_0.25",	"T_0.25",	"T_0.56",	"T_0.56",	"F_0.25",	"F_0.25",	"F_0.56",	"F_0.56",	"F_1.12",	"F_1.12"),
  man = c(0,	0,	0.728872840728205,	0.71936840346914,	0.999500632001313,	1.1108408145763,	4.92683346550735,	4.51883654458295,	4.21288230754655,	3.97425796241714,	5.60380921127946,	6.16317883978955,	4.0773173654659,	4.17461086077335,	0,	0,	0.358907854978449,	0.326203641144429,	1.40573902268395,	1.28779353993668,	1.36389294449668,	1.51979626394819,	2.94977774438496,	2.81641674974548,	1.31970381207437,	1.44685182901915))

# What is the effect of the treatment on the value ?
anova <- aov(man ~ vessel, data = data)
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
  summarise(mean=mean(man), quant = quantile(man, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$vessel)
Tk$cld <- cld$Letters

print(Tk)

# Create a vector named "new_order" containing the desired order
new_order <- with(data, reorder(vessel, man, median, na.rm = TRUE))

# boxplot

ggplot(data, aes(x = new_order, man)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "", y = "concentration [g/L") +
  ylim(0,7) +
  ggtitle('Mannitol') +
  theme_bw(base_size = 20) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Rotate x-axis labels
  geom_text(data = Tk, aes(x = vessel, y = quant, label = cld), size = 6, vjust = -1.5, hjust = 0.5) +
  scale_fill_viridis_d(option='inferno')


# saving the final figure
ggsave("boxplot_man.png", width = 7, height = 7, dpi = 1000)