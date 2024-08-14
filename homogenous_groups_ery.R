# library

library('viridis')
library('ggplot2')
library('multcompView')
library('dplyr')

# Create data
data <- data.frame(
  vessel = c("O_96_0.25",	"O_96_0.25",	"O_96_0.55",	"O_96_0.55",	"O_48_0.29",	"O_48_0.29",	"O_48_0.56",	"O_48_0.56",	"O_24_0.29",	"O_24_0.29",	"O_24_0.56",	"O_24_0.56",	"□_24_0.55",	"□_24_0.55",	"□_24_1.10",	"□_24_1.10",	"T_0.25",	"T_0.25",	"T_0.56",	"T_0.56",	"F_0.25",	"F_0.25",	"F_0.56",	"F_0.56",	"F_1.12",	"F_1.12"),
  ery = c(0,	0,	0.682485742367238,	0.539745374719671,	0.670565991267252,	0.797044098536813,	0.573829300662807,	0.935692053587379,	0.900208925455609,	0.897636605309924,	1.86393925943494,	2.01270167579619,	0.472125957098351,	0.537306077063745,	0,	0,	0.326714177450413,	0.316314734805785,	1.14007032690131,	0.998706697021912,	0.324656504937301,	0.478147177796531,	0.845964910179456,	0.90422062454191,	1.12110473992559,	1.05274817250716))

# What is the effect of the treatment on the value ?
anova <- aov(ery ~ vessel, data = data)
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
  summarise(mean=mean(ery), quant = quantile(ery, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$vessel)
Tk$cld <- cld$Letters

print(Tk)

# Create a vector named "new_order" containing the desired order
new_order <- with(data, reorder(vessel, ery, median, na.rm = TRUE))

# boxplot

ggplot(data, aes(x = new_order, ery)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(x = "", y = "concentration [g/L") +
  ylim(-0.05, 2.5) +
  ggtitle('Erythritol') +
  theme_bw(base_size = 20) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Rotate x-axis labels
  geom_text(data = Tk, aes(x = vessel, y = quant, label = cld), size = 5, vjust = -2, hjust = 0.5) +
  scale_fill_viridis_d(option='cividis')


# saving the final figure
ggsave("boxplot_ery.png", width = 7, height = 7, dpi = 1000)