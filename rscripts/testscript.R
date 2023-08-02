
g <- ggplot(sales_total_db, 
              aes(y = `Total Sales`, 
                  x = release_year, 
                  color= factor(Region),
                  group = factor(Region),
            text = paste("Region: ", Region,
                         "<br>Date: ", release_year,
                         "<br>", " Sales: ", `Total Sales`))) +
  geom_line(size = .8) + 
  labs(x="Year",
       y="Millions of Copies Sold",
       color = "Regions") +
  ggtitle(paste("Video Game Units Sold Across Regions")) +
  theme_bw() + 
  theme(plot.title = element_text(size=16, face="bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_text(size = 12, vjust = 1.3),
        axis.title.y = element_text(size = 12, vjust = 1.3),
        axis.text = element_text(size =10))

ggplotly(g, tooltip = "text")
