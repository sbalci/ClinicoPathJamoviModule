# Create comprehensive test data for jjarcdiagram function
# This script generates various network datasets to test all features

library(dplyr)

# Set seed for reproducible data
set.seed(123)

# Create basic network test data (social network)
social_network_data <- data.frame(
  source = c("Alice", "Alice", "Bob", "Bob", "Bob", "Charlie", "Charlie",
             "Diana", "Diana", "Eva", "Eva", "Frank", "Frank", "Grace", "Grace",
             "Henry", "Henry", "Ivy", "Ivy", "Jack"),
  target = c("Bob", "Charlie", "Charlie", "Diana", "Eva", "Diana", "Frank",
             "Eva", "Grace", "Frank", "Henry", "Grace", "Ivy", "Henry", "Jack",
             "Ivy", "Jack", "Jack", "Alice", "Alice"),
  weight = c(8.5, 3.2, 4.1, 5.7, 2.9, 3.8, 6.4, 4.5, 7.2, 5.1,
             3.6, 8.9, 2.3, 6.7, 4.8, 5.4, 7.1, 3.9, 6.2, 4.6),
  group = c("Family", "Friend", "Family", "Friend", "Colleague", "Friend", "Colleague",
            "Friend", "Family", "Colleague", "Family", "Colleague", "Friend",
            "Family", "Friend", "Colleague", "Family", "Friend", "Colleague", "Family"),
  interaction_type = c("Daily", "Weekly", "Daily", "Monthly", "Weekly", "Monthly", "Daily",
                      "Weekly", "Daily", "Monthly", "Weekly", "Daily", "Monthly",
                      "Weekly", "Monthly", "Daily", "Weekly", "Monthly", "Daily", "Weekly")
)

# Create academic collaboration network
academic_network_data <- data.frame(
  author1 = c("Smith_J", "Smith_J", "Johnson_M", "Johnson_M", "Brown_K", "Brown_K",
              "Davis_L", "Davis_L", "Wilson_R", "Wilson_R", "Taylor_S", "Taylor_S",
              "Anderson_P", "Anderson_P", "Thomas_C", "Thomas_C", "Jackson_D", "Jackson_D"),
  author2 = c("Johnson_M", "Brown_K", "Brown_K", "Davis_L", "Davis_L", "Wilson_R",
              "Wilson_R", "Taylor_S", "Taylor_S", "Anderson_P", "Anderson_P", "Thomas_C",
              "Thomas_C", "Jackson_D", "Jackson_D", "Smith_J", "Smith_J", "Johnson_M"),
  publications = c(5, 3, 8, 2, 6, 4, 7, 3, 9, 5, 4, 6, 2, 8, 3, 7, 5, 4),
  department = c("Biology", "Biology", "Biology", "Chemistry", "Chemistry", "Physics",
                 "Physics", "Mathematics", "Mathematics", "Biology", "Biology", "Chemistry",
                 "Chemistry", "Physics", "Physics", "Mathematics", "Mathematics", "Biology"),
  collaboration_years = c(3, 2, 5, 1, 4, 2, 6, 2, 7, 3, 2, 4, 1, 5, 2, 6, 3, 4)
)

# Create organizational hierarchy data
org_hierarchy_data <- data.frame(
  employee = c("CEO", "CEO", "VP_Sales", "VP_Sales", "VP_Marketing", "VP_Marketing",
               "VP_Tech", "VP_Tech", "Sales_Mgr1", "Sales_Mgr1", "Sales_Mgr2", "Sales_Mgr2",
               "Marketing_Mgr", "Marketing_Mgr", "Tech_Lead1", "Tech_Lead1", "Tech_Lead2", "Tech_Lead2"),
  reports_to = c("VP_Sales", "VP_Marketing", "Sales_Mgr1", "Sales_Mgr2", "Marketing_Mgr", "Marketing_Analyst",
                 "Tech_Lead1", "Tech_Lead2", "Sales_Rep1", "Sales_Rep2", "Sales_Rep3", "Sales_Rep4",
                 "Marketing_Analyst", "Marketing_Coord", "Developer1", "Developer2", "Developer3", "Developer4"),
  relationship_strength = c(9.5, 8.7, 8.2, 7.9, 8.5, 7.3, 9.1, 8.4, 7.8, 8.1, 7.6, 8.3, 7.9, 8.2, 8.7, 8.9, 8.1, 7.8),
  department = c("Executive", "Executive", "Sales", "Sales", "Marketing", "Marketing",
                 "Technology", "Technology", "Sales", "Sales", "Sales", "Sales",
                 "Marketing", "Marketing", "Technology", "Technology", "Technology", "Technology"),
  level = c(1, 1, 2, 2, 2, 3, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
)

# Create gene regulatory network data (simplified)
gene_network_data <- data.frame(
  regulator = c("TP53", "TP53", "MYC", "MYC", "BRCA1", "BRCA1", "EGFR", "EGFR",
                "PIK3CA", "PIK3CA", "KRAS", "KRAS", "APC", "APC", "CDKN2A", "CDKN2A"),
  target = c("MDM2", "CDKN1A", "CCND1", "CDK4", "BARD1", "RAD51", "PIK3CA", "AKT1",
             "AKT1", "MTOR", "PIK3CA", "RAF1", "CTNNB1", "MYC", "CDK4", "RB1"),
  regulation_score = c(8.9, 9.2, 7.8, 8.1, 9.5, 8.7, 8.3, 7.9, 8.6, 7.4, 8.8, 9.1, 7.6, 8.4, 8.2, 8.7),
  pathway = c("DNA_Damage", "Cell_Cycle", "Cell_Cycle", "Cell_Cycle", "DNA_Repair", "DNA_Repair",
              "Growth_Factor", "Growth_Factor", "PI3K_AKT", "PI3K_AKT", "MAPK", "MAPK",
              "Wnt_Signaling", "Wnt_Signaling", "Cell_Cycle", "Cell_Cycle"),
  effect_type = c("Activation", "Activation", "Activation", "Activation", "Interaction", "Interaction",
                  "Activation", "Activation", "Activation", "Activation", "Activation", "Activation",
                  "Regulation", "Regulation", "Inhibition", "Inhibition")
)

# Create supply chain network data
supply_chain_data <- data.frame(
  supplier = c("Supplier_A", "Supplier_A", "Supplier_B", "Supplier_B", "Manufacturer_1", "Manufacturer_1",
               "Manufacturer_2", "Manufacturer_2", "Distributor_1", "Distributor_1", "Distributor_2", "Distributor_2",
               "Retailer_1", "Retailer_1", "Retailer_2", "Retailer_2", "Retailer_3", "Retailer_3"),
  customer = c("Manufacturer_1", "Manufacturer_2", "Manufacturer_1", "Manufacturer_3", "Distributor_1", "Distributor_2",
               "Distributor_2", "Distributor_3", "Retailer_1", "Retailer_2", "Retailer_3", "Retailer_4",
               "Customer_1", "Customer_2", "Customer_3", "Customer_4", "Customer_5", "Customer_6"),
  volume = c(1500, 1200, 1800, 900, 2200, 1900, 1600, 1300, 2500, 2100, 1800, 2000, 800, 950, 1100, 850, 1200, 950),
  industry = c("Raw_Materials", "Raw_Materials", "Raw_Materials", "Raw_Materials", "Manufacturing", "Manufacturing",
               "Manufacturing", "Manufacturing", "Distribution", "Distribution", "Distribution", "Distribution",
               "Retail", "Retail", "Retail", "Retail", "Retail", "Retail"),
  relationship_duration = c(5, 3, 7, 2, 8, 6, 4, 3, 9, 5, 6, 4, 3, 7, 2, 5, 4, 6)
)

# Create minimal test data for edge cases
minimal_network_data <- data.frame(
  from = c("A", "B", "C"),
  to = c("B", "C", "A"),
  strength = c(1, 2, 3),
  category = c("Type1", "Type2", "Type1")
)

# Create self-loop test data
selfloop_network_data <- data.frame(
  node1 = c("A", "A", "B", "B", "C", "C"),
  node2 = c("A", "B", "B", "C", "C", "A"),  # A->A, B->B, C->C are self-loops
  weight = c(5, 8, 6, 7, 4, 9),
  type = c("Self", "Connection", "Self", "Connection", "Self", "Connection")
)

# Create large network test data
set.seed(456)
large_network_nodes <- paste0("Node_", sprintf("%03d", 1:50))
n_edges <- 100

large_network_data <- data.frame(
  source = sample(large_network_nodes, n_edges, replace = TRUE),
  target = sample(large_network_nodes, n_edges, replace = TRUE),
  weight = round(runif(n_edges, 0.1, 10), 2),
  cluster = sample(c("Cluster_A", "Cluster_B", "Cluster_C", "Cluster_D"), n_edges, replace = TRUE),
  edge_type = sample(c("Strong", "Medium", "Weak"), n_edges, replace = TRUE, prob = c(0.3, 0.5, 0.2))
)

# Remove potential self-loops from large network for testing
large_network_data <- large_network_data[large_network_data$source != large_network_data$target, ]

# Create weighted network without groups for testing
simple_weighted_network <- data.frame(
  from_node = c("Central", "Central", "Node_A", "Node_A", "Node_B", "Node_B",
                "Node_C", "Node_C", "Node_D", "Node_D"),
  to_node = c("Node_A", "Node_B", "Node_B", "Node_C", "Node_C", "Node_D",
              "Node_D", "Central", "Central", "Node_A"),
  connection_weight = c(10.5, 8.3, 6.7, 9.2, 7.8, 5.4, 8.9, 6.1, 7.5, 4.9)
)

# Save all datasets as .rda files
save(social_network_data, file = "../data/jjarcdiagram_social_network.rda")
save(academic_network_data, file = "../data/jjarcdiagram_academic_network.rda")
save(org_hierarchy_data, file = "../data/jjarcdiagram_org_hierarchy.rda")
save(gene_network_data, file = "../data/jjarcdiagram_gene_network.rda")
save(supply_chain_data, file = "../data/jjarcdiagram_supply_chain.rda")
save(minimal_network_data, file = "../data/jjarcdiagram_minimal_network.rda")
save(selfloop_network_data, file = "../data/jjarcdiagram_selfloop_network.rda")
save(large_network_data, file = "../data/jjarcdiagram_large_network.rda")
save(simple_weighted_network, file = "../data/jjarcdiagram_simple_weighted.rda")

# Export all datasets as CSV files
write.csv(social_network_data, file = "../data/jjarcdiagram_social_network.csv", row.names = FALSE)
write.csv(academic_network_data, file = "../data/jjarcdiagram_academic_network.csv", row.names = FALSE)
write.csv(org_hierarchy_data, file = "../data/jjarcdiagram_org_hierarchy.csv", row.names = FALSE)
write.csv(gene_network_data, file = "../data/jjarcdiagram_gene_network.csv", row.names = FALSE)
write.csv(supply_chain_data, file = "../data/jjarcdiagram_supply_chain.csv", row.names = FALSE)
write.csv(minimal_network_data, file = "../data/jjarcdiagram_minimal_network.csv", row.names = FALSE)
write.csv(selfloop_network_data, file = "../data/jjarcdiagram_selfloop_network.csv", row.names = FALSE)
write.csv(large_network_data, file = "../data/jjarcdiagram_large_network.csv", row.names = FALSE)
write.csv(simple_weighted_network, file = "../data/jjarcdiagram_simple_weighted.csv", row.names = FALSE)

# Display summaries
cat("=== jjarcdiagram Test Data Summary ===\n")
cat("1. Social Network Data:", nrow(social_network_data), "edges\n")
cat("2. Academic Network Data:", nrow(academic_network_data), "edges\n")
cat("3. Organizational Hierarchy Data:", nrow(org_hierarchy_data), "edges\n")
cat("4. Gene Network Data:", nrow(gene_network_data), "edges\n")
cat("5. Supply Chain Data:", nrow(supply_chain_data), "edges\n")
cat("6. Minimal Network Data:", nrow(minimal_network_data), "edges\n")
cat("7. Self-loop Network Data:", nrow(selfloop_network_data), "edges\n")
cat("8. Large Network Data:", nrow(large_network_data), "edges\n")
cat("9. Simple Weighted Network:", nrow(simple_weighted_network), "edges\n")

# Display sample structures
cat("\n=== Sample Data Structures ===\n")
cat("Social Network Data:\n")
str(social_network_data)
cat("\nAcademic Network Data:\n")
str(academic_network_data)
cat("\nOrganizational Hierarchy Data:\n")
str(org_hierarchy_data)
