input_dir <- "Raw_Data"
output_dir <- "Result"


classify_gene <- function(logFC, padj) {
  if (is.na(padj)) { 
    padj <- 1  # Replace missing padj with 1
  }
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")
result_list <- list()


for (file_name in files_to_process) {
  cat("\nProcessing:", file_name, "\n")
  
  input_file_path <- file.path(input_dir, file_name)
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported, handling missing values...\n")
  
  data$padj[is.na(data$padj)] <- 1
  
  
  
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  
  output_file_path <- file.path(output_dir, paste0("Processed_", file_name))
  write.csv(data, output_file_path, row.names = FALSE)
  
  summary_table <- table(data$status)
  print(summary_table)
  result_list[[file_name]] <- summary_table
}

cat("\nSummary of all processed datasets:\n")
print(result_list)






