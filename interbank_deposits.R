#Uploading libraries 
if (!require("tidyverse")) install.packages("tidyverse") # Collection of data science packages
if (!require("igraph")) install.packages("igraph")# Collection of network analysis tools
if(!require("scales")) install.packages("scales")
if (!require("plotly")) install.packages("plotly") #3D visualizations
if (!require("mgcv")) install.packages("mgcv")
if(!require("lubridate")) install.packages("lubridate")
if (!require("lme4")) install.packages("lme4")  # For Linear Mixed Models (lmer)
if(!require("lmerTest")) install.packages("lmerTest") # To get p-values in the lmer summary

library(tidyverse)
library(igraph)
library(scales)
library(plotly)
library(mgcv)
library(lubridate)
library(lme4)     
library(lmerTest)  

# --- EXERCISE 1 ---

# Uploading the selected data (time = 1)
nodes = read.table("IB_nodes_77.txt", header = TRUE, stringsAsFactors = FALSE)
edges = read.table("IB_edges_77.txt", header = TRUE, stringsAsFactors = FALSE)

# Removing duplicated nodes 
nodes = nodes[!duplicated(nodes), ]

# Creating the graph object through graph_from_data_frame takes the edge list (d) and the node list (vertices)
# directed = TRUE because in our graph liquidity flows from the lender to the borrower 
g_raw = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
g_raw

g = simplify(g_raw, remove.multiple = TRUE, remove.loops = FALSE, 
             edge.attr.comb = list(weight_vol = "sum", weight_trans = "sum", "ignore")) # edge with sum of transactions and volume between each pair 
g                                                                                       # it ignores other attributes not specified

# Setting Node and Edge properties for the different plots 

# This function applies a log transformation (to handle large variances and zeros) and normalizes values to a specific range (min_out to max_out).
get_scaled_values = function(values, min_out, max_out) {
  
  vals_log = log1p(values)
  if (max(vals_log) == min(vals_log)) {
    return(rep((min_out + max_out) / 2, length(values)))
  } else {
    return((vals_log - min(vals_log)) / (max(vals_log) - min(vals_log)) * (max_out - min_out) + min_out)
  }
}

# Setting Node Color based on Group 
target_levels = c(0, 1, 2, 3, 4, 5)
target_labels = c("Large", "Very Large", "Medium", "Very Small", "NA (Foreign Bank)", "Small")

V(g)$category = factor(V(g)$group, levels = target_levels, labels = target_labels)
group_colors = rainbow(length(target_labels))
names(group_colors) = target_labels
V(g)$color = group_colors[as.character(V(g)$category)]


# --- Plot 1: Fruchterman-Reingold (FR) ---
# Node Size: lend_V (Lending Volume)
# Edge Width: weight_vol (Volume Weight)
V(g)$size = get_scaled_values(V(g)$lend_V, 1, 9)
E(g)$width = get_scaled_values(E(g)$weight_vol, 0.2, 2)

png(filename = "plot(1).png", width = 2000, height = 1400)

l1 = layout_with_fr(g)
plot(g, layout = l1,
     main = "Layout Fruchterman-Reingold (Size: Lending Volume, Width: Volume Weight)",
     vertex.label.font = 2, vertex.frame.color = "white",
     vertex.label.dist = 0, #Distance of the label from the center of the node
     arrow.size = 0.1, rescale = TRUE)
legend("bottomright", title= "Bank's size", legend= names(group_colors), pt.cex= 2, 
       col= group_colors, pt.bg= group_colors, pch= 21, cex=2)

dev.off()

# --- Plot 2: Kamada-Kawai (KK) ---
# Node Size: lend_T (Lending Transactions)
# Edge Width: weight_trans (Transaction Weight)
V(g)$size = get_scaled_values(V(g)$lend_T, 2, 12)
E(g)$width = get_scaled_values(E(g)$weight_trans, 0.2, 2)

png(filename = "plot(2).png", width = 2000, height = 1400)

l2 = layout_with_kk(g)
plot(g, layout = l2,
     main= "Layout Kamada-Kawai (Size: Lending Transactions, Width: Transaction Weight)", 
     vertex.label.font = 2, vertex.frame.color = "white",
     arrow.size = 0.1, vertex.label.dist = 0, #Distance of the label from the center of the node
     rescale = TRUE)
legend("bottomright", title= "Bank's size", legend= names(group_colors), pt.cex= 2, 
       col= group_colors, pt.bg= group_colors, pch= 21, cex=2)

dev.off()

# --- Plot 3: In Circle ---
# Node Size: borr_V (Borrowing Volume)
# Edge Width: weight_vol (Volume Weight)
V(g)$size = get_scaled_values(V(g)$borr_V, 1, 12)
E(g)$width = get_scaled_values(E(g)$weight_vol, 0.2, 2)

png(filename = "plot(3).png", width = 2000, height = 1400)

l3 = layout_in_circle(g, order = order(V(g)$borr_V))
plot(g, layout = l3,
     main= "Layout Circle (Size: Borrowing Volume, Width: Volume Weight)", 
     vertex.label.font = 2, vertex.frame.color = "white",
     arrow.size = 0.1, vertex.label.dist = 0, #Distance of the label from the center of the node
     rescale = TRUE)
legend("bottomright", title= "Bank's size", legend= names(group_colors), pt.cex= 2, 
       col= group_colors, pt.bg= group_colors, pch= 21, cex=2)

dev.off()

# --- Plot 4: Sugiyama ---
# Node Size: borr_T (Borrowing Transactions)
# Edge Width: weight_trans (Transaction Weight)
V(g)$size = get_scaled_values(V(g)$borr_T, 2, 12)
E(g)$width = get_scaled_values(E(g)$weight_trans, 0.2, 2)

png(filename = "plot(4).png", width = 2000, height = 1400)

l4 = layout_with_sugiyama(g)
plot(g, layout = l4,
     main= "Layout Sugiyama (Size: Borrowing Transactions, Width: Transaction Weight)",
     vertex.label.font = 2, vertex.frame.color = "white",
     arrow.size = 0.1, vertex.label.dist = 0, #Distance of the label from the center of the node
     rescale = TRUE)
legend("bottomright", title= "Bank's size", legend= names(group_colors), pt.cex= 2, 
       col= group_colors, pt.bg= group_colors, pch= 21, cex=2)

dev.off()



# --- EXERCISE 2 ---

# --- POINT 1 ----
# Arrays to store results
diameters = numeric(132)
time_steps = 1:132

# Loop through all 50 files
for(i in 1:132) {
  
  # Construct filename dynamically (IB_edges_1.txt/IB_nodes_1.txt, IB_edges_2.txt/IB_nodes_2.txt...)
  file_edges = paste0("IB_edges_", i, ".txt")
  file_nodes = paste0("IB_nodes_", i, ".txt")
  
  # Check if file exists to avoid errors
  if(file.exists(file_edges)) {
    
    # Read the files
    edges = read.table(file_edges, header = TRUE, stringsAsFactors = FALSE)
    nodes = read.table(file_nodes, header = TRUE, stringsAsFactors = FALSE)
    nodes = nodes[!duplicated(nodes), ]
    
    # Create Graph (Directed)
    net_raw = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    net = simplify(net_raw, remove.multiple = TRUE, remove.loops = FALSE)
    
    # Calculate Diameter (longest geodesic)
    # If the graph is disconnected, this returns the diameter of the largest component
    d = diameter(net)
    diameters[i] = d
    
  } else {
    # If a file is missing, put NA
    diameters[i] = NA
  }
}

# Plot the evolution of the network diameter over time
png(filename = "diameter evolution.png", width = 1000, height = 600)

plot(time_steps, diameters, type = "l",  # "l" for line
     col = "blue", lwd = 2, 
     xlab = "Time Period (File Number)", ylab = "Network Diameter",
     main = "Evolution of Interbank Network Diameter")

# Add points on top of the line to see actual data points
points(time_steps, diameters, pch = 20, lwd = 3, col = "darkblue")

dev.off()


# --- POINT 2 ----
edges = read.table("IB_edges_77.txt", header = TRUE)
nodes = read.table("IB_nodes_77.txt", header = TRUE)
nodes = nodes[!duplicated(nodes), ]

net = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Calculate Metrics
deg_in  = degree(net, mode = "in")  # Incoming links (borrowing)
deg_out = degree(net, mode = "out") # Outgoing links (lending)
strength_net = strength(net, weights = E(net)$weight_vol) # Strength (Weighted Degree)

# --- Plot 1: Incoming degree  ---
mean_d1 = mean(deg_in)

png(filename = "in_degree.png", width = 1000, height = 600)
hist(deg_in, breaks = 20, col = "lightgreen", border = "white",
     main = "Distribution of Banks' degree", xlab = "Incoming degree", ylab = "Frequency", las = 1)
abline(v = mean_d1, col = "red", lwd = 2, lty = 2)
text(mean_d1, 3, paste("Mean:", round(mean_d1, 2)), col = "red", pos = 3, srt=90)
dev.off()

# --- Plot 2: Outgoing degree ---
mean_d2 = mean(deg_out)

png(filename = "out_degree.png", width = 1000, height = 600)
hist(deg_out, breaks = 20, col = "skyblue", border = "white",
     main = "Distribution of Banks' degree", xlab = "Outgoing degree", ylab = "Frequency", las = 1)
abline(v = mean_d2, col = "red", lwd = 2, lty = 2) # Add Mean Line
text(mean_d1, 3, paste("Mean:", round(mean_d2, 2)), col = "red", pos = 3, srt=90)
dev.off()

# --- Plot 3: Strength (Volume) ---
mean_s = mean(strength_net)

png(filename = "strength.png", width = 1000, height = 600)
hist(strength_net, breaks = 20, col = "salmon", border = "white",
     main = "Distribution of Banks' strength", xlab = "Volume weighted degree", ylab = "Frequency", las = 1)
abline(v = mean_s, col = "blue", lwd = 2, lty = 2)
text(mean_s, 3, paste("Mean:", round(mean_s, 2)), col = "blue", pos = 3, srt=90)
dev.off()

# --- Plot 4: Total degree ---
deg = degree(net, mode = "all") #Total links (in+out)

# We exclude nodes with degree 0 (because log(0) = -infinity)
deg = deg[deg > 0] 

# Complementary Cumulative Distribution Function
# P(X >= k) = Probability that a node has degree greater than or equal to k
d_sort = sort(deg)
prob_cum = 1 - (0:(length(d_sort)-1)) / length(d_sort)


df_plot = data.frame(Degree = d_sort, Cumulative_Freq = prob_cum)

png(filename = "degree.png", width = 1000, height = 600)
ggplot(df_plot, aes(x = Degree, y = Cumulative_Freq)) +
  geom_point(shape = 16, size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.8) + # linear regression on logarithms to simulate the power law
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  labs(title = "Degree Distribution (Log-Log Scale)",
    subtitle = "Evidence of Power Law Tail",
    x = "Degree (k)", y = "Cumulative Frequency P(K >= k)") +
  theme(panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, face = "bold"))
dev.off()

# --- Plot 5: Degree vs Clustering Coefficient ---
group_labels = c("0" = "Large", "1" = "Very Large", "2" = "Medium", 
                 "3" = "Very Small", "4" = "Foreign", "5" = "Small")

nodes$Group_Name = group_labels[as.character(nodes$group)]

nodes$Degree_Total = degree(net, mode = "all")
nodes$Clustering = transitivity(net, type = "local", isolates = "zero")

df_plot1 = nodes %>%
  filter(Degree_Total > 0 & Clustering > 0) %>%
  mutate(Group_Name = factor(Group_Name, levels = c("Very Large", "Large", "Medium", 
                                        "Small", "Very Small", "Foreign")))

png(filename = "degree_clust.png", width = 1000, height = 600)
ggplot(df_plot1, aes(x = Degree_Total, y = Clustering, color = Group_Name)) +
  geom_point(size = 3, alpha = 0.7) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("Very Large" = "red", "Large" = "black", 
                                "Medium" = "gold", "Small" = "orange", 
                                "Very Small" = "green", "Foreign" = "blue")) +
  theme_bw() +
  labs(title = "Clustering Coefficient vs. Degree (Log-Log)",
       subtitle = "Analysis of Hierarchical Structure",
       x = "Degree (k)", y = "Clustering Coefficient C(k)",
       color = "Bank Size") +
  
  theme(text = element_text(size = 12), legend.position = "right",
    panel.grid.minor = element_blank())
dev.off()


# --- POINT 3 ----
clustering_values = numeric(132)
time_steps = 1:132

# Loop through all 50 files
for(i in 1:132) {
  
  # Construct filename dynamically (IB_edges_1.txt/IB_nodes_1.txt, IB_edges_2.txt/IB_nodes_2.txt...)
  file_edges = paste0("IB_edges_", i, ".txt")
  file_nodes = paste0("IB_nodes_", i, ".txt")
  
  # Read the files
  edges = read.table(file_edges, header = TRUE, stringsAsFactors = FALSE)
  nodes = read.table(file_nodes, header = TRUE, stringsAsFactors = FALSE)
  nodes = nodes[!duplicated(nodes), ]
  
  # Create Graph (Directed)
  net_raw = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  net = simplify(net_raw, remove.multiple = TRUE, remove.loops = FALSE)
  
  # Calculate Global Transitivity (Global Clustering Coefficient) with range: [0, 1]. 
  # 0 = No triangles (Star or Tree topology)
  # 1 = Complete clique (Everyone trades with everyone)
  clust_val = transitivity(net, type = "global")
  clustering_values[i] = clust_val
}

# Plot the evolution of the network clustering coefficient over time
png(filename = "clustering evolution.png", width = 1000, height = 600)

plot(time_steps, clustering_values, type = "l",  # "l" for line
     col = "green", lwd = 2, 
     xlab = "Time Period (File Number)", ylab = "Clustering Coefficient",
     main = "Evolution of Network Clustering Coefficient (Transitivity)")

# Add points on top of the line to see actual data points
points(time_steps, clustering_values, pch = 20, lwd = 3, col = "darkgreen")

dev.off()


# --- POINT 4 ----
edges = read.table("IB_edges_77.txt", header = TRUE)
nodes = read.table("IB_nodes_77.txt", header = TRUE)
nodes = nodes[!duplicated(nodes), ]

group_labels = c("0" = "Large", "1" = "Very Large", "2" = "Medium", 
                 "3" = "Very Small", "4" = "NA (Foreign Bank)", "5" = "Small")
nodes$Group_Name = group_labels[as.character(nodes$group)]
group_map = setNames(nodes$Group_Name, nodes$ID)

net = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)


# Node Degree (Total links)
# How much a bank is connected to others (Lending + Borrowing)?
metric_degree = degree(net)

# Closeness Centrality (Speed)
# How few steps to reach everyone else?
# weights = NA ensures we treat it as a topological distance (hops), not cost
metric_closeness = closeness(net, mode = "all", normalized = TRUE)

# Betweenness Centrality (Control)
# How often does this bank sit on the shortest path (geosedic) between two others?
metric_betweenness = betweenness(net, normalized = TRUE)

# Eigenvector Centrality (Influence)
# Connection to other highly connected nodes
metric_eigen = eigen_centrality(net, weights = E(net)$weight_vol)$vector

get_top_5 = function(metric_vector) {
  sorted_metric = sort(metric_vector, decreasing = TRUE) # Sort descending
  top_5 = head(sorted_metric, 5) # Take top 5
  ids = names(top_5)
  groups = group_map[ids]
  return(list(ID = ids, Group = groups, Value = round(top_5, 3)))
}

# Extract Top 5 for each metric
top_deg = get_top_5(metric_degree)
top_clo = get_top_5(metric_closeness)
top_bet = get_top_5(metric_betweenness)
top_eig = get_top_5(metric_eigen)

# Combine into a single data frame
comparison_table = data.frame(Rank = 1:5, Deg_ID = top_deg$ID, Deg_Group = top_deg$Group, Deg_Value = top_deg$Value,
  Close_ID = top_clo$ID, Close_Group = top_clo$Group, Close_Value = top_clo$Value,
  Betw_ID = top_bet$ID, Betw_Group = top_bet$Group, Betw_Val = top_bet$Value,
  Eigen_ID = top_eig$ID, Eigen_Group = top_eig$Group, Eigen_Val = top_eig$Value)

row.names(comparison_table) = NULL
print(comparison_table)



# --- EXERCISE 3 ---

# Linear Mixed Model (LMM) on the longitudinal dataset spanning the full 132-month period. 
# The dependent variable is defined as the borrowing spread or lending spread. 
# The fixed effects include the standardized centrality metrics (In-Degree, Eigenvector, Betweenness) 
# calculated over the entire sample, and a categorical variable 'Group' capturing bank size. 
# Crucially, the model incorporates a time trend (centered month) to control for 
# macroeconomic fluctuations and random intercepts at the bank level to account 
# for unobserved idiosyncratic heterogeneity and serial correlation.


# PANEL DATASET CONSTRUCTION (All Months)

panel_data = data.frame()
group_labels = c("0" = "Large", "1" = "Very Large", "2" = "Medium", 
                 "3" = "Very Small", "4" = "Foreign", "5" = "Small")

for(i in 1:132) {
  file_edges = paste0("IB_edges_", i, ".txt")
  file_nodes = paste0("IB_nodes_", i, ".txt")
  
  # Check if files exist to avoid errors
  if(file.exists(file_edges) && file.exists(file_nodes)) {
    
    edges = read.table(file_edges, header = TRUE)
    nodes = read.table(file_nodes, header = TRUE)
    nodes = nodes[!duplicated(nodes$ID), ] # Remove duplicates
    
    # Graph for the current month
    g2 = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    
    # Metrics for this specific month
    curr_ids = as.character(nodes$ID)
    
    in_deg  = degree(g2, mode="in")[curr_ids]
    out_deg = degree(g2, mode="out")[curr_ids]
    betw = betweenness(g2, directed=TRUE, normalized=TRUE, weights=NA)[curr_ids]
    
    if(!is.null(E(g)$weight_vol)) {
      eigen = eigen_centrality(g2, weights=E(g2)$weight_vol)$vector[curr_ids]
    } else {
      eigen = eigen_centrality(g2)$vector[curr_ids]
    }
    
    # Create the row for the final dataset
    monthly_df = nodes %>%
      mutate(Month = i, # Time reference
        Group_Name = group_labels[as.character(group)],
        In_Degree_Raw = in_deg,
        Out_Degree_Raw = out_deg,
        Eigen_Raw = eigen,
        Betw_Raw = betw)
    
    # Merge into the main panel dataset
    panel_data = bind_rows(panel_data, monthly_df)
  }
}


# DATA PREPARATION FOR REGRESSION

# Set Group Factor with "Large" as Baseline
panel_data$Group_Factor = relevel(factor(panel_data$Group_Name), ref = "Large")

# Standardization (Z-Scores) over the entire period
# This makes a bank with high degree in 2000 comparable to one in 2010
panel_data$In_Degree_Std = scale(panel_data$In_Degree_Raw)
panel_data$Eigen_Std = scale(panel_data$Eigen_Raw)
panel_data$Betw_Std = scale(panel_data$Betw_Raw)
panel_data$Month_Centered = scale(panel_data$Month) # Control for time trend



# REGRESSION ANALYSIS (LINEAR MIXED MODELS)
# We use lmer() instead of lm() because we have repeated data over time.
# (1 | ID) is the Random Intercept: controls for the "idiosyncrasies" of each bank.


# 1. BORROWING SIDE ANALYSIS (Who pays less?)

# Filter only those who borrowed
df_borr = panel_data %>% filter(!is.na(borrowing_spread))

# Base Model (Bank's size and Time Trend Only)
# Hypothesis: "The rate depends only on whether I am Big or Small"
m0_borr = lmer(borrowing_spread ~ Group_Factor + Month_Centered + (1 | ID), 
               data = df_borr, REML = FALSE)

# Advanced Model (Size + Time Trend + In-Degree + Eigenvector) 
# Hypothesis: "The rate also depends on how many banks lend me money and bank's reputation/connection to Big players"
m1_borr = lmer(borrowing_spread ~ Group_Factor + Month_Centered + In_Degree_Std + Eigen_Std + Betw_Std + (1 | ID), 
               data = df_borr, REML = FALSE)

# RESULTS AND COMPARISON
print("1. Base Model Summary (Size + Time)")
print(summary(m0_borr))

print("2. ANOVA Comparison: Does Network Structure Matter?")
# H0: Models are equal (centrality measures adds nothing)
# H1: Advanced Model is better (network structure explains borrowing costs)
anova_borr = anova(m0_borr, m1_borr)
print(anova_borr)

print(summary(m1_borr))


# 2. LENDING SIDE ANALYSIS (Who earns more?)

# Filter only those who borrowed
df_lend = panel_data %>% filter(!is.na(lending_spread))

# Base Model (Bank's size and Time Trend Only)
# Hypothesis: "The rate depends only on whether I am Big or Small"
m0_lend = lmer(lending_spread ~ Group_Factor + Month_Centered + (1 | ID), 
               data = df_lend, REML = FALSE)

# Advanced Model (Size + Time Trend + In-Degree + Eigenvector) 
# Hypothesis: "The rate also depends on how many banks lend me money and bank's reputation/connection to Big players"
m1_lend = lmer(lending_spread ~ Group_Factor + Month_Centered + In_Degree_Std + Eigen_Std + Betw_Std + (1 | ID), 
               data = df_lend, REML = FALSE)

# RESULTS AND COMPARISON
print("1. Base Model Summary (Size + Time)")
print(summary(m0_lend))

print("2. ANOVA Comparison: Does Network Structure Matter?")
anova_lend = anova(m0_lend, m1_lend)
print(anova_lend)

print(summary(m1_lend))



# Principal Component Analysis for the network in 2004 before Euribor manipulation in 2005

# Calculation of metrics for each individual Bank
edges = read.table("IB_edges_77.txt", header = TRUE)
nodes = read.table("IB_nodes_77.txt", header = TRUE)
nodes = nodes[!duplicated(nodes), ]

g3 = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# List of all banks in the network
bank_ids = V(g3)$name
bank_groups = nodes$group
bank_stats = data.frame(ID = bank_ids, Group = bank_groups, stringsAsFactors = FALSE)

# Centrality measures (Node Level) ---
# Using both In-Degree (borrowing liquidity) and Out-Degree (lending liquidity)

# In-Degree: How many banks lend you money? 
bank_stats$In_Degree = degree(g3, mode = "in")[bank_ids]

# Out-Degree: How many banks do you lend money to?
bank_stats$Out_Degree = degree(g3, mode = "out")[bank_ids]

# Eigenvector: Are you connected to the "big banks" based on trading volume?
bank_stats$Eigen = eigen_centrality(g3, weights=E(g)$weight_vol)$vector[bank_ids]

# Betweenness: Are you a necessary bridge?
bank_stats$Betweenness = betweenness(g3, normalized = TRUE)[bank_ids]

# Closeness: How fast do you catch up with others? How fast does a crisis propagate?
bank_stats$Closeness = closeness(g3, mode = "all", normalized = TRUE)[bank_ids]


# Average Rate calculation for Banks
# 1) Borrowing Cost: weighted average of rates by the volume when the Bank is Borrower
borrow_data = nodes %>%
  mutate(ID = as.character(ID))%>%
  group_by(ID) %>%
  summarise(Avg_Bank_Borrowing = weighted.mean(borrowing_spread, borr_V),
    Total_Borrowed = sum(borr_V))

# 2) Lending Yield: weighted average of rates by the volume when the Bank is Lender
lending_data = nodes %>%
  mutate(ID = as.character(ID))%>%
  group_by(ID) %>%
  summarise(Avg_Bank_Lending = weighted.mean(lending_spread, lend_V),
    Total_Lent = sum(lend_V))


# Final merge and cleanup NA
final_df = bank_stats %>%
  left_join(borrow_data, by = "ID") %>%
  left_join(lending_data, by = "ID") 

# Since some banks may only lend or only receive:
# - if Avg Borrowing Rate is NA -> replace with the global average (Assumption: financing would be at the average market rate)
# - if Avg Lending Rate is NA -> replace with the global average (Assumption: would lend at the average market rate)
global_avg_borrow = weighted.mean(nodes$borrowing_spread, nodes$borr_V, na.rm = TRUE)
global_avg_lend = weighted.mean(nodes$lending_spread, nodes$lend_V, na.rm = TRUE)

final_df = final_df %>%
  mutate(Avg_Bank_Borrowing = ifelse(is.na(Avg_Bank_Borrowing), global_avg_borrow, Avg_Bank_Borrowing),
         Avg_Bank_Lending   = ifelse(is.na(Avg_Bank_Lending), global_avg_lend, Avg_Bank_Lending),
         Spread = Avg_Bank_Lending - Avg_Bank_Borrowing)

# Select Top 5 Banks with the best spread (difference between the yield of liquidity and the cost of liquidity)
top_5_banks = final_df %>%
  arrange(desc(Spread)) %>%
  slice(1:5)

print("Top 5 Banks (Best Spread):")
print(top_5_banks[, c("ID", "Group", "Spread", "Avg_Bank_Borrowing", "Avg_Bank_Lending")])


# Select the structural metrics
pca_features = final_df %>% 
  select(In_Degree, Out_Degree, Eigen, Betweenness, Closeness)%>%
  mutate_all(~ifelse(is.na(.), 0, .)) # to handle possible NAs

# Execution of PCA
pca_res = prcomp(pca_features, center = TRUE, scale. = TRUE) # Transform all data into Z-Scores to ensure comparability across dimensions

# Adding coordinates PCA
final_df$PC1 = pca_res$x[, 1] # X-axis: component that explains more variance 
final_df$PC2 = pca_res$x[, 2] # Y-axis: component that explains remaining variance

print(summary(pca_res))


# 3D Visualization (Banks in space)
# Grid Base
axis_x = seq(min(final_df$PC1), max(final_df$PC1), length.out = 40)
axis_y = seq(min(final_df$PC2), max(final_df$PC2), length.out = 40)
grid_data = expand.grid(PC1 = axis_x, PC2 = axis_y)

# Borrowing surface (red, Generalized Additive Model explain how the borrowing rate changes as PC1 and PC2 vary)
gam_borrow = gam(Avg_Bank_Borrowing ~ s(PC1, PC2, k=10), data = final_df)
grid_data$Borrow_Pred = predict(gam_borrow, newdata = grid_data)
z_borrow = matrix(grid_data$Borrow_Pred, nrow = 40, ncol = 40)

# Lending surface (green, Generalized Additive Model explain how the lending rate changes as PC1 and PC2 vary)
gam_lend = gam(Avg_Bank_Lending ~ s(PC1, PC2, k=10), data = final_df)
grid_data$Lend_Pred = predict(gam_lend, newdata = grid_data)
z_lend = matrix(grid_data$Lend_Pred, nrow = 40, ncol = 40)


# Plotting
fig = plot_ly() %>%
  
  # Borrowing surface area (cost of money by bank type)
  add_surface(x = axis_x, y = axis_y, z = z_borrow,
    colorscale = 'Reds', opacity = 0.6, name = "Borrowing Cost Trend",
    showscale = TRUE, colorbar = list(title = "Borrowing Spread")) %>%
  
  # Lending surface area (return on money by Bank type)
  add_surface(x = axis_x, y = axis_y, z = z_lend,
    colorscale = 'Greens', opacity = 0.6, name = "Lending Yield Trend",
    showscale = TRUE,colorbar = list(title = "Lending Spread")) %>%
  
  # Other Banks (black points)
  add_markers(data = final_df %>% filter(!ID %in% top_5_banks$ID),
    x = ~PC1, y = ~PC2, z = ~Avg_Bank_Borrowing,
    marker = list(size = 3, color = 'black', line = list(width=2, color='grey')),
    text = ~paste("Bank:", ID), name = "Other Banks") %>%
  
  # TOP 5 Banks (gold points)
  add_markers(data = final_df %>% filter(ID %in% top_5_banks$ID),
    x = ~PC1, y = ~PC2, z = ~Avg_Bank_Borrowing, 
    marker = list(size = 7, color = '#FFD700', line = list(width=2, color='grey')),
    text = ~paste("<b>🏆 TOP PERFORMER</b>","<br>ID:", ID, "<br>Spread:", round(Spread, 3), "%",
                  "<br>Borrow:", round(Avg_Bank_Borrowing, 3), "%",
                  "<br>Lend:", round(Avg_Bank_Lending, 3), "%"),
    name = "Top 5 Banks") %>%
  
  layout(title = list(text="Microstructural Analysis: Bank Profile and Spreads", font=list(color="black")),
    scene = list(xaxis = list(title = "PC1 (Network Structure)", gridcolor = "black"),
      yaxis = list(title = "PC2 (Systemic Importance)", gridcolor = "black"),
      zaxis = list(title = "Avg Spreads", gridcolor = "black"),
      bgcolor = "white"), paper_bgcolor = "white",font = list(color = "black"), margin = list(r = 100))

fig




# Principal Component Analysis for the evolution of the network along time

# Focus on all the different networks to see the difference in Avg Spread between pre and post Euribor 
# manipulation started in Septemper 2025 up to 2008 
history_stats = data.frame()

for(i in 1:132) {
  
  # Construct filename dynamically (IB_edges_1.txt/IB_nodes_1.txt, IB_edges_2.txt/IB_nodes_2.txt...)
  file_edges = paste0("IB_edges_", i, ".txt")
  file_nodes = paste0("IB_nodes_", i, ".txt")
  
  # Read the files
  edges = read.table(file_edges, header = TRUE, stringsAsFactors = FALSE)
  nodes = read.table(file_nodes, header = TRUE, stringsAsFactors = FALSE)
  nodes = nodes[!duplicated(nodes), ]
  
  # Create graph as directed
  g4 = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Topological metrics for each network
  deg = degree(g4, mode="all")
  eigen = eigen_centrality(g4, weights=E(g4)$weight_vol)$vector
  conc_index = centr_eigen(g4, normalized = TRUE)$centralization
  betw = betweenness(g4, normalized=TRUE)
  close = closeness(g4, mode="all", normalized=TRUE)
  
  # Calculation of separate rates (Borrowing vs Lending) 
  # We use weighted.mean because rates on large volumes matter more
  avg_borrow = weighted.mean(nodes$borrowing_spread, nodes$borr_V, na.rm=TRUE)
  avg_lend = weighted.mean(nodes$lending_spread, nodes$lend_V, na.rm=TRUE)
  avg_spread = avg_borrow - avg_lend
  
  # Creation of a dataframe with the inputs and target outputs for PCA
  monthly_row = data.frame(Month_ID = i, Avg_Degree = mean(deg), Std_Degree = sd(deg),
    Max_Eigen = max(eigen), Concentration_Index = conc_index, Avg_Betweenness = mean(betw),
    Avg_Closeness = mean(close), Density = edge_density(g4),
    Avg_Borrowing = avg_borrow, Avg_Lending = avg_lend, Avg_Spread = avg_spread)
  
  history_stats = bind_rows(history_stats, monthly_row)
}
print(history_stats)


# We only use structural metrics to define the X and Y axes
features_pca = history_stats %>% 
  select(Avg_Degree, Concentration_Index, Avg_Betweenness, Avg_Closeness, Density)

pca_results = prcomp(features_pca, center = TRUE, scale. = TRUE) # Transform all data into Z-Scores

history_stats$PC1 = pca_results$x[, 1] # X-axis: component that explains more variance
history_stats$PC2 = pca_results$x[, 2] # Y-axis: component that explains remaining variance


# Create the basic grid
axis_x = seq(min(history_stats$PC1), max(history_stats$PC1), length.out = 40)
axis_y = seq(min(history_stats$PC2), max(history_stats$PC2), length.out = 40)
grid_data = expand.grid(PC1 = axis_x, PC2 = axis_y)

# We interpolate two different surfaces:
# - Borrowing surface (Generalized Additive Model explain how the borrowing rate changes as PC1 and PC2 vary)
gam_borrow = gam(Avg_Borrowing ~ s(PC1, PC2, k=10), data = history_stats) 
grid_data$Borrow_Pred = predict(gam_borrow, newdata = grid_data)
z_borrow = matrix(grid_data$Borrow_Pred, nrow = 40, ncol = 40)

# - Lending surface (Generalized Additive Model explain how the lending rate changes as PC1 and PC2 vary)
gam_lend = gam(Avg_Lending ~ s(PC1, PC2, k=10), data = history_stats)
grid_data$Lend_Pred = predict(gam_lend, newdata = grid_data)
z_lend = matrix(grid_data$Lend_Pred, nrow = 40, ncol = 40)


# Divide data in two groups (before and after Euribor manipulation 07/2005)
data_pre  = history_stats %>% filter(Month_ID <= 90)
data_post = history_stats %>% filter(Month_ID >= 91)

# Plotting
fig = plot_ly() %>%
  
  # Upper surface (Borrowing, Expensive, Warm Tones)
  add_surface(x = axis_x, y = axis_y, z = z_borrow,
    colorscale = 'Reds', opacity = 0.6, name = "Avg Borrowing Rate",
    colorbar = list( title = "Avg Borrowing Spread")) %>%
  
  # Lower Surface (Lending, Yield, Cool Tones)
  add_surface(x = axis_x, y = axis_y, z = z_lend, 
              colorscale = 'Greens', opacity = 0.6, name = "Avg Lending Rate", 
              showscale = TRUE, colorbar = list( title = "Avg Lending Spread"))%>%
  
  # Add the points for reference 
  add_markers(data = data_pre, x = ~PC1, y = ~PC2, z = ~Avg_Borrowing,
    marker = list(size = 3, color = 'black', line=list(width=2, color='grey')),
    text = ~paste("Mese:", Month_ID, "<br>Avg Spread:", round(Avg_Spread, 4)), name = "Monthly networks(1999-2005)") %>%
  
  add_markers(data = data_post, x = ~PC1, y = ~PC2, z = ~Avg_Borrowing,
              marker = list(size = 3, color = '#FFD700', line=list(width=2, color='grey')),
              text = ~paste("Mese:", Month_ID, "<br>Avg Spread:", round(Avg_Spread, 4)), name = "Monthly networks(2005-2009)") %>%
  
  layout(title = list(text="Evolution of the Interbank Network: Borrowing vs Lending Spread", font=list(color="black")), 
         scene = list(xaxis = list(title = "PC1 (Network Structure)", gridcolor = "black"),
                      yaxis = list(title = "PC2 (Centralization)", gridcolor = "black"),
                      zaxis = list(title = "Avg Spreads", gridcolor = "black"),
                      bgcolor = "white"), paper_bgcolor = "white", font = list(color = "black"))

fig









