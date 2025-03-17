library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)

copy_number <- read.table('TCGA-BRCA.gistic.tsv', header = T, fill = T, sep = '\t', check.names = F)
m <- readRDS('all_nncorr_0603.rds')
nn <- readRDS('all_nn_0301.rds')
nn <- rbind(nn, as.data.frame(m))

TCGA_patient <- read_excel("TCGA_patient.xlsx", col_names = FALSE)

tcga_clinical_full <- read.delim("/Users/bbia0648/SST/SST/SST/BRCA.clin.merged.txt", header=FALSE) %>%
  t() %>%
  data.table::as.data.table() %>%
  row_to_names(1)


bc_pat_luminal <- tcga_clinical_full %>%
  filter( (patient.breast_carcinoma_estrogen_receptor_status=="positive" &
             patient.breast_carcinoma_progesterone_receptor_status=="positive") |
            (patient.breast_carcinoma_estrogen_receptor_status=="positive" &
               patient.breast_carcinoma_progesterone_receptor_status=="negative")) %>%
  pull(patient.bcr_patient_barcode) %>%
  toupper()

bc_pat_her2 <- tcga_clinical_full %>%
  filter( `patient.lab_proc_her2_neu_immunohistochemistry_receptor_status`=="positive"
  ) %>%
  pull(patient.bcr_patient_barcode) %>%
  toupper()

a <- intersect(TCGA_patient$...1, rownames(nn))
nn <- nn[match(a, rownames(nn)), ]

sample_id <- colnames(copy_number)[-1]
sample_id <- sub("\\-01.*", "", sample_id)

a <- intersect(sample_id, rownames(nn))
a <- intersect(a, bc_pat_luminal)
a <- intersect(a, bc_pat_her2)
a1 <- match(a, sample_id)

copy_number <- copy_number[, c(1, (a1+1))]

colnames(copy_number)[2:ncol(copy_number)] <- sub("\\-01.*", "", colnames(copy_number)[2:ncol(copy_number)])

### NN correlation

a2 <- match(a, rownames(nn))

nn <- nn[a2, ]

identical(colnames(copy_number)[-1], rownames(nn))


### gene names
library(tidyverse)
gene_names <- read.table('/Users/bbia0648/SST/SST/SST/gencode.v22.annotation.gene.probeMap', header = T)
a <- intersect(gene_names$id, copy_number$`Gene Symbol`)
a1 <- match(a, gene_names$id)
gene_names <- gene_names[a1, ]
a2 <- match(a, copy_number$`Gene Symbol`)
copy_number <- copy_number[a2, ]
copy_number <- add_column(copy_number, gene_names$gene, .after = 1)

### remove chromosome X

copy_number <- copy_number[-which(gene_names$chrom == 'chrX'), ]
gene_names <- gene_names[-which(gene_names$chrom == 'chrX'), ]


### check the number of genes in association analysis

### select genes
# gene_names
rownames(copy_number) <- copy_number$`Gene Symbol`
copy_number <- copy_number[, -c(1,2)]


s <- numeric()
for (i in 1:nrow(copy_number)) {
  aa <- as.data.frame(table(as.numeric(copy_number[i, ])))
  if (length(which(aa$Var1 == '-1')) == 0) {
    s[i] <- 0
  } else {
    s[i] <- aa[which(aa$Var1=='-1'), 2]
  }
}

s1 <- numeric()
for (i in 1:nrow(copy_number)) {
  aa=as.data.frame(table(as.numeric(copy_number[i, ])))
  if (length(which(aa$Var1 == '0')) == 0) {
    s1[i] <- 0
  } else {
    s1[i] <- aa[which(aa$Var1 == '0'), 2]
  }
}

s2 <- ncol(copy_number) - s - s1

s <- as.data.frame(cbind(s, s1, s2))

rownames(s) <- rownames(copy_number)

a <- which(s[,1]>=5 & s[,3]>=5)

### t-test
nn <- t(as.matrix(nn))

t_test_function <- function(cnv_row) {
  group1_idx <- which(cnv_row == -1)
  group2_idx <- which(cnv_row == 1)
  
  # Check if both groups exist, otherwise return NA
  if (length(group1_idx) >= 5 & length(group2_idx) >= 5) {
    result <- sapply(1:nrow(nn), function(i) {
      gene_row <- nn[i, ]  # Extract one row
      
      # Filter non-NA values
      t1 <- gene_row[group1_idx][!is.na(gene_row[group1_idx])]
      t2 <- gene_row[group2_idx][!is.na(gene_row[group2_idx])]
      
      # print(length(t1))
      # print(length(t2))
      
      # If both groups have enough samples, perform t-test, otherwise return NA
      if (length(t1) >= 5 & length(t2) >= 5) {
        t.test(t1, t2)$p.value
      } else {
        NA
      }
    })
  } else {
    rep(NA, nrow(nn))  # Return NA if not enough samples
  }
}


p_value_matrix <- t(apply(copy_number, 1, t_test_function))
dim(p_value_matrix)

### positions
gene_names$chrom1 <- str_replace(gene_names$chrom, 'chr', '')
gene_names$chrom1=as.numeric(gene_names$chrom1)

don <- gene_names %>% 
  
  # Compute chromosome size
  group_by(chrom1) %>% 
  mutate(chr_len=max(chromEnd)) %>% 
  ungroup() %>%
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(as.numeric(chr_len))-chr_len+chromEnd) %>%
  mutate(is_odd = as.numeric(chrom1) %% 2)

axisdf = don %>%
  group_by(chrom1) %>%
  summarize(center=( max(tot) + min(tot) ) / 2 )



### generate genomic regions
chrom_length <- unique(don$chr_len)

# Define bin size (e.g., 1 million bases)
bin_size <- 1000000  

# Create start and end positions

logp_matrix <- -log10(p_value_matrix)

res_logp <- tmp

### using i=1 to generate the initial res_logp and then loop from 2 to 22

for (i in 2:22) {
  start_positions <- seq(1, chrom_length[i], by = bin_size)
  end_positions <- c(start_positions[-1] - 1, chrom_length[i])
  bins <- data.frame(Chromosome = paste0('chr', i), Start = start_positions, End = end_positions)
  bins_log <- numeric(length = nrow(bins))
  tot_log <- numeric(length = nrow(bins))
  for (j in 1:nrow(bins)) {
    b1 <- which(don$chrom == paste0('chr', i) & don$chromStart > bins$Start[j] & don$chromStart < bins$End[j])
    b <- as.numeric(logp_matrix[b1, ])
    b <- b[-which(is.na(b))]
    if (length(b1) == 0) {
      bins_log [j] <- 0
    } else {
      bins_log[j] <- sum(b) / length(b1)
    }
    tot_log[j] <- median(don$tot[b1])
  }
  tmp <- data.frame(chr = i, row_tot=tot_log, logp = bins_log)
  res_logp <- rbind(res_logp, tmp)
}

res_logp <- res_logp[-which(is.na(res_logp$row_tot)), ]


d1 <- ggplot(data = res_logp, color=as.factor(chr)) +
  
  # Show all points
  geom_segment(aes(x = as.numeric(row_tot), xend = as.numeric(row_tot), 
                   y = 0, yend = logp, color=as.factor(chr)),alpha = 0.2, size = 0.8) +
  scale_color_manual(values = c('1'='#F8766D', '4'='#9590FF', '5'='#C77CFF', '6'='#E76BF3','7'='#FA62DB',
                                '8'='#FF62BC', '10'='#EA8331','11'='#D89000','16'='#00BB4E','17'='#00BF7D',
                                '19'='#00C1A3','20'='#00BAE0','21'='#00B0F6')) +
  #scale_color_gradientn(colors = rep(c('blue', 'red'), 11))+
  # custom X axis:
  scale_x_continuous(label = axisdf$chrom1, breaks= axisdf$center, guide = guide_axis(n.dodge = 2), limits=c(0, 2.766559e+12)) +   
  scale_y_continuous(limits = c(0, 400), breaks = c(0, 100, 200, 300, 400))+
  labs(title = "",
       x = "",
       y = "sum(-logP)")+
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold", color = "black", family = "Times"),
    axis.title.y = element_text(size = 15, face = "bold", color = "black", family = "Times"),
    axis.text.y = element_text(size=12, face = 'bold'),
    axis.text.x = element_text(size=12, face = 'bold', vjust = 0)
    
  )

### example of volcano plot

### 17q11-21, 27294076-48575513

tt17.1 <- which(gene_names$chrom=='chr17'&gene_names$chromStart>=27294076&gene_names$chromEnd<=48575513)

FC_chr17 <- FC[tt17.1, ]
p_chr17 <- as.matrix(p_value_matrix[tt17.1, ])

generate_name <- character()
for (i in 1:280) {
  generate_name <- c(generate_name, paste0(gene_names$gene[tt17.1], '-', colnames(m)[i]))
}

ds <- cbind(as.vector(FC_chr17), as.vector(p_chr17), generate_name)
ds <- as.data.frame(ds)
ds$V1 <- as.numeric(ds$V1)
ds$V2 <- as.numeric(ds$V2)
ds <- ds[-which(is.na(ds$V1)), ]
annotate_genes <- ds[ds$V2 <= 0.05 / length(a), ]

#### volcano plot

plot <- ggplot(ds, aes(x = V1, y = -log10(V2), color=color)) +
  geom_point(aes(color=-log10(V2)), alpha = 0.4, size = 1.75) +
  xlim(-1, 1)+
  #scale_color_manual(values = c("down-regulated" = "grey", "up-regulated" = "grey"))+
  scale_colour_gradient(low="blue",high="red")+
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, face = "bold", color = "black", family = "Times"))+
  theme(axis.text.x = element_text(size = 14, face = 'bold'))+
  theme(axis.title.y = element_text(size = 14, face = "bold", color = "black", family = "Times"))+
  theme(axis.text.y = element_text(size = 14, face = 'bold'))+
  theme(plot.title = element_text(size = 16, face = 'bold'))+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA))+  # Remove background panel
  labs(title = "chr17",
       x = "log2 FC",
       y = "-log10(P-value)") +
  geom_text_repel(data = annotate_genes, aes(label = generate_name),
                  size = 4, box.padding = 0.3, point.padding = 0.5, color='black', fontface='bold',
                  segment.color = 'grey50')+theme(legend.position = "none")
