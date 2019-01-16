## Original code by Rockenschaub, Patrick with additions and modifications by Meg Cupp

library(tidyverse)
library(grid)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(tibble)
library(RColorBrewer)


# Clean work space
remove(list = ls())

# Load the ICU dataset created in step 1
load("icu.Rdata")

levels(icu$Study.Cat)
levels(icu$Non.Comm.Disease) <- c("No", "Yes", "Major NCD", "Sepsis")
levels(icu$Non.Comm.Disease)

# add a new variable to icu data set to tag which conditions are includes as WHO major NCDs
Major.NCD.list <- c("NEOPLASIA", "AORTIC DISSECTION", "STROKE", "DIABETES MELLITUS",
                    "CHRONIC OBSTRUCTIVE PULMONARY DISEASE", "ASTHMA", 
                    "CARDIAC ARRYTHMIAS AND ARREST", "CONGESTIVE HEART FAILURE",
                    "INTRACRANIAL HAEMORRHAGE", "ACUTE MYOCARDIAL INFARCTION")
Septic <- c("SEPSIS", "PNEUMONIA")

icu$Non.Comm.Disease[icu$Study.Cat %in% Major.NCD.list] <- "Major NCD"

icu$Non.Comm.Disease[icu$Study.Cat %in% Septic] <- "Sepsis"


theme_pub <- theme_foundation() +
  theme(text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(), 
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"))

#############################################################################################
#
#    Graph 1: bill size by time
#

icu.g1 <- icu %>% filter(!is.na(Income.Cat))
colours <- brewer.pal(12, name = "Paired")

# Alternative
icu.g1.alt <- icu.g1 %>%
  filter(Income.Cat %in% c("<2328", ">3782")) %>% 
  filter(Year.As.Num > 1, !is.na(CCI.Cat)) %>% 
  mutate(CCI.Bin = CCI != 0)

median_IQR <- function(x){
  tibble(
    ymin = quantile(x, 0.25, type = 2), 
    y = median(x), 
    ymax = quantile(x, 0.75, type = 2)
  )
}

# y.lim <- c(3500, 300000)
# y.breaks <- c(5000, 20000, 80000, 320000)
# y.labels <- paste(y.breaks / 1000)

y.lim <- c(0, 90000)
y.breaks <- c(20000, 40000, 60000, 80000)
y.labels <- paste(y.breaks / 1000)

label_b <- icu.g1.alt %>% 
  group_by(Income.Cat) %>% 
  summarise(
    m_los = median(LoS), 
    m_bill = median(Gross.Amount, na.rm = TRUE)
  ) %>% 
  mutate(x = c(53, 50),
         y = c(18000, 12000))


time <- icu.g1.alt %>% 
  ggplot(aes(Year, Gross.Amount, colour = Income.Cat, group = Income.Cat)) + 
  stat_summary(fun.data = median_IQR, position = position_dodge(width = 0.5)) + 
  #scale_y_log10(expression(paste(Hospital~bill~(log[10]))), breaks = y.breaks, labels = y.labels) + 
  scale_y_continuous(expression(paste(Hospital~bill~(log[10]))), breaks = y.breaks, labels = y.labels) +
  scale_color_manual(values = colours[c(8, 2)]) + 
  coord_cartesian(ylim = y.lim) + 
  guides(colour = FALSE) +
  theme_pub + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(hjust = -3),
        plot.margin=unit(c(0.5,0.25,0.25,0.5), "cm"))

los <- icu.g1.alt %>%
  ggplot(aes(LoS, Gross.Amount, fill = Income.Cat)) + 
  stat_density_2d(data = icu.g1.alt %>% filter(Income.Cat != "<2328"), 
                  aes(alpha = ..level..), geom = "polygon") + 
  stat_density_2d(data = icu.g1.alt %>% filter(Income.Cat == "<2328"), alpha = 0.5, colour = colours[8]) + 
  geom_text(data = label_b, aes(x, y, label = paste("Median stay:", m_los), 
                                colour = Income.Cat), size = 5, hjust = 0, inherit.aes = FALSE) + 
  geom_segment(data = label_b, aes(x, y, xend = m_los, yend = m_bill, colour = Income.Cat), inherit.aes = FALSE) + 
  geom_point(data = icu.g1.alt %>% 
               group_by(Income.Cat) %>% 
               summarise(x = median(LoS), y = median(Gross.Amount)),
             aes(x, y, fill = Income.Cat), colour = "white", size = 4, shape = 21) + 
  scale_x_log10(breaks = c(2, 7, 28, 112, 448), labels = c("2", "7", "28", "112", "448")) + 
  #scale_y_log10(breaks = y.breaks) + 
  scale_y_continuous(breaks = y.breaks) + 
  scale_colour_manual(values = colours[c(8, 2)]) + 
  scale_fill_manual(values = colours[c(8, 2)]) + 
  scale_alpha_continuous(range = c(0.1, 0.5)) + 
  coord_cartesian(ylim = y.lim, xlim = c(2, 400)) + 
  guides(colour = FALSE, alpha = FALSE, fill = FALSE) + 
  theme_pub + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin=unit(c(0.5, 0.5, 0.25, 0.25), "cm"))

# Hack to make the polygon go to the edges
gtab <- ggplotGrob(los)
tree <- gtab$grobs[[6]]
poly <- tree$children[[grep(names(tree$children), pattern = "^geom_polygon")]]
poly$x <- unit(c(poly$x, rep(0.0454545454545455, 3)), units = "native")
poly$y <- unit(c(poly$y, rep(0.081368028843323, 3)), units = "native")
poly$id <- c(poly$id, c(1L, 2L, 4L))
tree$children[[grep(names(tree$children), pattern = "^geom_polygon")]] <- poly
gtab$grobs[[6]] <- tree

com_perc <- icu.g1.alt %>% 
  group_by(Income.Cat, Year) %>% 
  summarise(perc = round(sum(CCI.Bin) / n() * 100, 1)) %>% 
  mutate(
    y = if_else(Income.Cat == "<3,201", 7000, 2500)
  )

cci <- icu.g1.alt %>% 
  ggplot(aes(Year, Gross.Amount, colour = CCI.Bin, fill = CCI.Bin)) + 
  stat_summary(fun.data = median_IQR, position = position_dodge(width = 0.5)) + 
  geom_text(data = tibble(header = c("CCI > 0"),Year = "2012", y = 12000), fontface = "bold", size = 3,
            aes(Year, y, label = header), hjust = 0, nudge_x = -0.55, inherit.aes = FALSE) +
  geom_text(data = tibble(header = c( "≤ $2328:", "> $3782:"),Year = "2012", y = c(7000, 2500)), 
            aes(Year, y, label = header), size = 3, hjust = 0, nudge_x = -0.55, inherit.aes = FALSE) + 
  geom_text(data = com_perc, aes(Year, y, label = paste0(perc, "%")), hjust = -0.1, size = 3, inherit.aes = FALSE) + 
  #scale_y_log10(expression(paste(Hospital~bill~(log[10]))), breaks = y.breaks, labels = y.labels) + 
  scale_y_continuous(expression(paste(Hospital~bill~(log[10]))), breaks = y.breaks, labels = y.labels) + 
  scale_colour_manual(values = c("grey55", "grey25"), labels = c("No", "Yes")) + 
  scale_fill_manual(values = c("grey55", "grey25"), labels = c("≤ $2328", "> $3782")) + 
  coord_cartesian(ylim = y.lim) + 
  guides(
    fill = guide_legend("Income", order = 1,  ncol = 1, title.theme = element_text(face = "bold", angle = 0), override.aes = list(colour = colours[c(8,2)])), 
    colour = guide_legend("Co-morbidity", ncol = 1, title.theme = element_text(face = "bold", angle = 0))
  ) + 
  theme_pub + 
  theme(panel.spacing = unit(0.1, "lines"), 
        legend.position = "bottom", 
        legend.key = element_rect(colour = FALSE), 
        legend.spacing.x = unit(0.5, "cm"),
        axis.title.y = element_text(hjust = 1.4),
        plot.margin = unit(c(0.25, 0.25, 0.5, 0.5), "cm"))


comb <- icu.g1.alt %>% 
  ggplot(aes(cut(LoS, breaks = c(1, 7, 28, 112, Inf)), Gross.Amount, colour = paste(Income.Cat, CCI.Bin))) +
  stat_summary(fun.data = median_IQR, position = position_dodge(width = 0.8)) + 
  scale_x_discrete("Length of hospital stay (days)", labels = c("< 7", "8-28", "29-112", "> 112")) + 
  #scale_y_log10(breaks = y.breaks, labels = as.character(y.breaks)) + 
  scale_y_continuous(breaks = y.breaks, labels = y.labels) + 
  scale_color_manual(values = colours[c(7:8, 1:2)],  labels = c("≤ $2328 w/o co-morbidity", "≤ $2328 with co-morbidity", "> $3782 w/o co-morbidity", "> $3782 with co-morbidity")) +
  coord_cartesian(ylim = y.lim) + 
  guides(colour = guide_legend("Interaction", ncol = 2, title.theme = element_text(face = "bold", angle = 0))) +
  theme_pub + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        panel.spacing = unit(0.1, "lines"), 
        legend.position = "bottom",
        legend.justification = "left",
        legend.key = element_rect(colour = FALSE), legend.title = element_text(),
        plot.margin=unit(c(0.25 ,0.5, 0.5, 0.25), "cm"))

grid <- arrangeGrob(time, gtab, cci, comb,
                    ncol = 2, nrow = 2, 
                    widths = c(2, 2), heights = c(8, 11), 
                    padding = unit(0, "line"))

#ggsave(file.path("inc_com_los.png"), grid)


#############################################################################################
#
#    Graph 2: Hospital costs by reason for admission/number
#

disease_of_interest <- tribble(
  ~ name, ~abbr, 
  # Non-communicable
  "myasthenia gravis neuromuscular diseases", "A",
  "peripheral vacular disease", "B",
  "aortic dissection", "C",
  "trauma (non-head)", "D",
  #insert repel label E
  #insert repel label F
  "nonrheumatic valvular disease", "G",
  "cirrhosis of liver", "H",
  "acute abdomen", "I",
  #insert repel label J
  #insert repel label K
  "renal failure", "L",
  "diabetes mellitus", "M",
  "stroke", "N",
  "traumatic head injury", "O",
  "neoplasia", "P", 
  "chronic obstructive pulmonary disease", "Q",
  "poisoning", "R",
  "asthma", "S",
  "cardiac arrythmias and arrest", "T",
  "congestive heart failure", "U",
  "sepsis", "V", 
  "intracranial haemorrhage", "W", 
  "pneumonia", "X",
  "acute myocardial infarction", "Y")

disease_of_interest_repel <- tribble(
  ~ name, ~abbr2, 
"status epilepticus", "J",
"acute pancreatitis", "F",
"complications", "E",
"gastrointestinal hemorrhage", "K")

colours <- brewer.pal(11, "Spectral")

icu %>% 
  filter(Year == "2015") %>% 
  group_by(Study.Cat) %>% 
  filter(Study.Cat!="NO FIT") %>%
  filter(Study.Cat!="OTHERS") %>%
  summarise(
    non.comm = unique(Non.Comm.Disease), 
    n = n(), 
    mean = mean(Gross.Amount, na.rm = TRUE), 
    median = median(Gross.Amount, na.rm = TRUE), 
    sum = sum(Gross.Amount, na.rm = TRUE)
  ) %>% 
  mutate(Study.Cat = tolower(Study.Cat)) %>% 
  left_join(disease_of_interest, by = c("Study.Cat" = "name")) %>% 
  left_join(disease_of_interest_repel, by = c("Study.Cat" = "name")) %>%
  ggplot(aes(n, mean, colour = non.comm, size = sum)) + 
  geom_text(aes(label= abbr), hjust=0.5, vjust=0.5, size=3, show.legend = FALSE) +
  geom_point(shape = 16, stroke = 1, alpha = 3/10) + 
  geom_text_repel(aes(label = abbr2), 
                   show.legend = FALSE, 
                   size = 3) + 
  scale_x_continuous() + 
  scale_color_manual(values = colours[c(4, 9, 2, 10)], labels = c("Non-Communicale", "Communicable", "Major NCD", "Sepsis"),
                     guide = guide_legend("Disease Type",
                                          title.theme = element_text(size=12, angle= FALSE, face="bold"))) +
  scale_size(range = c(2, 40), guide = FALSE) + 
  labs(x = "Number of Admissions", y = "Mean Hospital Costs") + 
  labs(title="Hospital Costs for Critically Ill Patients in Singapore (2015)") +
  coord_cartesian(ylim = c(0, 80000)) + 
  theme_pub +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(colour = FALSE), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=12)
  )

ggsave(file.path("cost_per_disease.png"))

### ADD COLORATION OR SHADING FOR (IE. DARKER RED FOR MAJOR NCDS & SEPSIS V NON)

#############################################################################################
#
#    Graph 3: Length of stay by income category
#

icu %>%
  filter(!is.na(Income.Cat)) %>% 
  gather(key = var, value = length_of_stay, LoS, ICU.LoS) %>% 
  ggplot(aes(x = length_of_stay, fill = Income.Cat)) + 
  geom_density(colour = NA, bw = 0.08) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000)) + 
  coord_cartesian(xlim = c(1, 800)) + 
  facet_grid(Income.Cat ~ var, scales = "free") + 
  theme_minimal() +
  guides(fill=guide_legend(title="Income Category")) +
  labs(y="Density", x="Length of Stay (days)", title="Length of Stay in ICU and Hospital") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path("length_of_stay_ICU.png"))


#############################################################################################
#
#    Graph 4: Affluence and bill size (U-curve)
#
  
OverallSpend <- icu %>%
  filter(!is.na(Income.Cat)) %>% 
  ggplot(aes(x=Income.Cat, y=Gross.Amount)) + stat_summary(fun.data=mean_cl_normal)  +
  labs(y="Bill Size", x="", title="Overall Expenditure") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(15000, 39000)) +
  theme_minimal()

ggsave(file.path("affluence_bill_size_ICU.png"))

ICUSpend <- icu %>%
  filter(!is.na(Income.Cat)) %>% 
  ggplot(aes(x=Income.Cat, y=ICU.Gross.Amount)) + stat_summary(fun.data=mean_cl_normal)  +
  labs(y="Bill Size", x="Income Category", title="Affluence and ICU Expenditure") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(ylim = c(5000, 39000)) +
  theme_minimal()

ggsave(file.path("affluence_ICU_bill_size_ICU.png"))

grid.arrange(OverallSpend, ICUSpend, nrow=1)

icu$nonicu <- icu$Gross.Amount - icu$ICU.Gross.Amount

nonICUSpend <- icu %>%
  filter(!is.na(Income.Cat)) %>% 
  ggplot(aes(x=Income.Cat, y=nonicu)) + stat_summary(fun.data=mean_cl_normal)  +
  labs(y="", x="", title="Expenditure excluding ICU") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(ylim = c(15000, 39000)) +
  theme_minimal()

ggsave(file.path("affluence_bill_size_nonICU.png"))

grid.exp <- grid.arrange(OverallSpend, nonICUSpend, nrow=1,
                         top = textGrob("Affluence and Hospital Expenditure",gp=gpar(fontsize=20)),
                         bottom = textGrob("Income Category",gp=gpar(fontsize=12)))

ggsave(file.path("affluence_grid.png"))       
