df_sero <- read.csv("C:/Users/DELL/Desktop/df_sero.csv")

data <- read.csv("C:/Users/DELL/Desktop/Incidence.csv")

p1 <- ggplot(data, aes(x = as.character(month), y = Incidence, group = 1)) +
  geom_bar(stat = "identity", fill = "#1f78b4", alpha = 0.5) +
  geom_line(color = "black",size=0.3) +  # 添加�?
  geom_point(color = "grey50", size = 1) +
  theme_minimal() +
  labs(x = "Time",
       y = "Proportion of RSV positive cases\n among ILI cases (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.grid=element_blank()) + 
  theme(plot.background = element_blank() ,
        panel.grid.major = element_blank() ,
        panel.grid.minor = element_blank() ,
        panel.border = element_blank() ,
        panel.background = element_blank() ) +
  theme(axis.ticks.length = unit(0.1, "cm"),               # 刻度线长�?
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = 'black'), 
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.0, 12), breaks=c(0,3,6,9,12)) +
  scale_x_discrete(labels = data$Monthlabel) +
  theme_ft() +
  ggtitle("A") +
  theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))
  


age_labels <- c("<=5 years", "6-18 years", "19-59 years", "60-74 years", "75+ years")
visit_labels <- c("Baseline", "Visit 1", "Visit 2", "Visit 3", "Visit 4")


p2 <- ggplot(df_sero, aes(x = as.factor(r), y = values)) +
  geom_violin(alpha = 0.5, fill = "#e6550d", trim = TRUE, scale = "width", width = 0.8) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5, color = "grey50") +
  geom_boxplot(width = 0.15, fill = "white", alpha = 1, 
               outlier.shape = NA, color = "black", size = 0.2) +
  scale_x_discrete(labels = visit_labels) + 
  theme_bw() +  
  theme(panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9)) +
  xlab("Rounds") +  
  ylab("PreF antibody titre log(10)") +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 6), breaks = 1:6) +
  geom_hline(yintercept = 2.30102999, color = "grey50", linetype = "dotted", size = 0.5) +
  theme_ft() +
  ggtitle("B")+
  theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

p3 <- ggplot(df_sero_long, aes(x = Age, y = values, fill = as.factor(r))) +
  geom_boxplot(alpha = 0.6) +   
  scale_x_discrete(labels = age_labels) + 
  theme_bw() +  
  theme(panel.grid=element_blank()) + 
  theme(plot.background = element_blank() ,
        panel.grid.major = element_blank() ,
        panel.grid.minor = element_blank() ,
        panel.border = element_blank() ,
        panel.background = element_blank() ) +
  theme(axis.line = element_line(color = 'black'), axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9)) +
  xlab("Age Group") +  ylab("PreF antibody titre log(10)") +
  scale_y_continuous(expand=c(0,0), limits=c(1, 6), breaks=c(1,2,3,4,5,6)) +
  geom_hline(yintercept = 2.30102999, color = "grey50", linetype = "dotted", size = 0.5) +
  theme_ft() +
  ggtitle("C") + 
  theme(legend.position = "None") +
  theme(plot.title = element_text(hjust = 0, size = 18, color = "gray10", face = "bold", margin = margin(b = 10)))

p12 <- p1 + p2
p12/p3
