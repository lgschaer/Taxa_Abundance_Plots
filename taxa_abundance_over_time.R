#Phyloseq object:
#saveRDS(justbacteria, "/home/lgschaer/old/Plastic_Deg/TA_TPA_Enrichment_02052021/justbacteria_TATPA.rds")
justbacteria <- readRDS("/home/lgschaer/old/Plastic_Deg/TA_TPA_Enrichment_02052021/justbacteria_TATPA.rds")

#Convert phyloseq object into a data frame with relative abundance counts
phylumabundance <- justbacteria %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  arrange(Phylum) 
head(phylumabundance)


phylum_colors <- c(
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
)

abundanceplot <- phylumabundance %>%
  select(Phylum, Abundance, Inocula, Time) %>%
  group_by(Phylum, Inocula, Time) %>%
  summarize(
    avg_abundance = mean(Abundance)
  ) %>%
  filter(avg_abundance > 0.10) %>% #filter everything less than 10% relative abundance
  filter(Inocula != "Blank") 
head(abundanceplot)

#dot plot, you might be able to use geom_segment to get a line to connect the dots between time points, but its tricky since not all time points have the same phyla.
ggplot(abundanceplot, mapping = aes(x = Time, y = avg_abundance, fill = Phylum), color = "black", show.legend = TRUE)+
  geom_point(position = "fill", shape = 21, size = 3, show.legend = TRUE)+
  facet_grid(rows = vars(Inocula))+
  ylab("Proportion of Community") +
  scale_fill_manual(values = phylum_colors) +
  xlab(NULL)+
  theme_minimal()+
  theme(axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 0, vjust = 1, hjust = 0.5),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 25))

#You could also plot the same data in a bar chart
ggplot(abundanceplot, mapping = aes(x = Time, y = avg_abundance, fill = Phylum), color = "black", show.legend = TRUE)+
  geom_col(shape = 21, size = 3, show.legend = TRUE, position = position_dodge2(width = 0.9, preserve = "single"))+
  facet_grid(rows = vars(Inocula))+
  ylab("Proportion of Community") +
  scale_fill_manual(values = phylum_colors) +
  xlab(NULL)+
  theme_minimal()+
  theme(axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 0, vjust = 1, hjust = 0.5),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 25))
