# Loading the necessary libraries
library(tidyverse) # Data wrangling and plotting 
library(patchwork) # Formating plot output

# Load data
employee <- read_csv('data/employee.csv')

# Two quantitative data

p1 <- ggplot(data = employee, aes(salary, salbegin)) + 
  geom_point() +
  geom_smooth() +
  labs(title = "Begining salary by salary",
       y = 'Beginning salary in USD',
       x = 'Salary') +
  theme(text = element_text(size = 20))


# Two qualitative data (One plotted, other used as fill)

p2 <- ggplot(data = employee, aes(x = jobcat, fill = gender)) +
  geom_bar() +
  labs(title = "Individuals in job categories by gender",
       y = 'Number of individuals',
       x = 'Job category of the professional') +
  theme(text = element_text(size = 20))



# Quantitative and Qualitative

p3 <- ggplot(data = employee, aes(x = reorder(gender,log(salbegin)), 
                            y = log(salbegin))) +
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  labs(title = 'Beginning salary by gender',
       y = "Beginning salary in USD (log)",
       x = "Gender of the professional") 

# All the plots together
patched <- p1/(p2 + p3)
patched + plot_annotation(tag_levels = 'A')

ggsave(filename = 'data/three_plots.png', dpi = 300)
