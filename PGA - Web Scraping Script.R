# ======================
# SETUP
# ======================
# ----------------------
# R PACKAGES
# ---------------------
if(!require(pacman))install.packages("pacman")
pacman::p_load('rvest', 'stringi', 'dplyr', 'tidyr', 'measurements', 'reshape2', 'sqldf')

# ----------------------
# GET HTML LINKS
# ----------------------
# A.1) PGA Tour Website
PGA_url <- "https://www.pgatour.com"
base_url <- "https://www.pgatour.com/stats/"

# A.2) Get Category Links
Categories <- html_nodes(read_html(paste0(PGA_url, "/stats.html")), '.tabbable') %>% 
                  html_nodes("a")   %>% 
                  html_attr("href") %>%
                  data.frame()      %>%
                  mutate(Name = html_nodes(read_html("https://www.pgatour.com/stats.html"), '.tabbable') %>% 
                                    html_nodes("a") %>% 
                                    html_text()
                        ) %>%
                  rename(Link = names(.)[1]) %>% 
                  slice(2:(n()-1))

# A.3) Get Statistics HTML Links
Statistics <- data.frame("Link"     = NA,
                         "Name"     = NA,
                         "Category" = NA
                        )

for (i in Categories$Link)
{   Statistics_i <- html_nodes(read_html(paste0(PGA_url, i)), "[class='table-content clearfix']") %>% 
                        html_nodes("a")   %>% 
                        html_attr("href") %>%
                        data.frame()      %>%
                        mutate(Name = html_nodes(read_html(paste0(PGA_url, i)), "[class='table-content clearfix']") %>% 
                                          html_nodes("a") %>% 
                                          html_text()
                              ) %>% 
                        rename(Link = names(.)[1]) %>%
                        mutate(Link = stri_sub(Link, 1, -6),
                               Category = Categories$Name[Categories$Link==i])
    
    Statistics <- rbind(na.omit(Statistics), Statistics_i)
}
Statistics <- sqldf("SELECT DISTINCT * FROM Statistics")

# **********************
# SET PARAMETERS
# **********************
# B.1) Set the year we want to grab data for
YYYY <- c(2019)

# B.2) Stat no. to collect
#stat_num <- c("stat.339")
stat_num <- Statistics$Link[1]

# B.3) Tournament Table
Tournaments <- data.frame("season"     = NA,
                          "tournament" = NA,
                          "name"       = NA
                         )

# B.4) Master Table
Master <- data.frame("PLAYER.NAME" = NA,
                     "Season"      = NA,
                     "Tournament"  = NA,
                     "Category"    = NA,
                     "Statistic"   = NA,
                     "variable"    = NA,
                     "value"       = NA
                    )


# ======================
# WEB SCAPING LOOP
# ======================
for (stat_no in stat_num)
{   for (year in YYYY)
    {   cat("\n ------------------------------------------------------------- \n",
            "SCRAPING STAT", stri_sub(stat_no, 13, nchar(stat_no)), "(", Statistics$Name[Statistics$Link == stat_no], ")", year, "\n",
            "------------------------------------------------------------- \n")
        
        # --------------------------------------
        # 1) EXTRACT STAT NAME AND TOURNAMENT NO
        # --------------------------------------
        # 1.1) Set the URL of the stats page for this year
        stat_url <- paste(PGA_url, stat_no, ".y", year, ".html", sep = "")
        webpage <- read_html(stat_url)
        
        # 1.2) Get Stat Name
        stat_name <- html_nodes(webpage, '.breadcrumbs') %>% 
                         html_nodes("[class='current']") %>% 
                         html_text()
        
        # 1.3) Get a dataframe of allowed values in the dropdown
        values <- html_nodes(webpage, '[value]')
        values2 <- data.frame(t(sapply(xml_attrs(values),c)))
        
        # 1.4) Transpose the dataframe
        values3 <- data.frame(t(values2))
        names(values3)[1] <- "attribute"
        
        # 1.5) Extract the tournament labels and filter out other attributes
        values3$tournament <- stri_extract(values3$attribute,  regex="(t)([0-9]+)")
        values3$name <- values %>% html_text()
        
        tournament_list <- subset(values3[,2:3], tournament != "")
        tournament_list$season <- year
        tournament_list <- tournament_list[, c("season", "tournament", "name")]
        rownames(tournament_list) <- NULL
        
        # 1.6) Append to Tournament List
        Tournaments <- rbind(na.omit(Tournaments), tournament_list)
        
        # 1.7) Distinct Tournament List
        Tournaments <- sqldf("SELECT DISTINCT * FROM Tournaments")
        
        
        # -------------
        # 2) WEB SCRAPE
        # -------------
        # Loop through the list of tournaments and get stats
        for (i in 1:length(tournament_list$tournament))
        {   # 2.1) Get URL
            stat_tour_url <- paste(PGA_url, stat_no, ".y", year, ".eon.", tournament_list$tournament[i], ".html", sep = "")
            cat("\n Scraping Tournament", tournament_list$tournament[i], "\n",
                "|---", stat_tour_url, "\n")
            
            # 2.2) Read the html
            webpage <- read_html(stat_tour_url)
            
            # 2.3) Find all the tables in the data
            tbls <- html_nodes(webpage, "table")
            
            # 2.4) Set the path to the stats table
            res <- try(tbls_ls <- webpage %>% html_nodes("table") %>%
                                              .[2] %>%
                                              html_table(fill = TRUE)
                      )
            if(inherits(res, "try-error"))
            {   # Next tournament_label if error
                cat("|--- No Data \n")
                next
            }
            
            # 2.5) Turn the list into a dataframe
            df <- data.frame(t(sapply(tbls_ls[[1]],c)))
            
            # 2.6) Transpose the dataframe
            df2 <- data.frame(t(df))
            
            # 2.7) Reshape to Long
            df3 <- melt(df2, id.vars = c("PLAYER.NAME") , measure.vars = names(df2)[c(1, 2, 5:length(names(df2)))])
            df3$Season     <- year
            df3$Tournament <- tournament_list$tournament[i] #tournament_list$name[i]
            df3$Category   <- Statistics$Category[Statistics$Link==stat_no]
            df3$Statistic  <- stat_name
            df3 <- df3[, c("PLAYER.NAME", "Season", "Tournament", "Category", "Statistic", "variable", "value")]
            
            # 2.8) Append it to Master
            Master <- rbind(Master, df3)
            
            cat("\n |--- Success", "\n")
        }
        
        # Select Distinct Tournaments
        Tournaments <- sqldf("SELECT DISTINCT season, tournament, name FROM Tournaments")
    }
}


# ======================
# CLEAN DATASET
# ======================
Master_Clean <- Master %>% slice(2:n()) %>%
                           # Convert " '
                           mutate(value = gsub("\"", "", value)) %>% 
                           separate(value, c("value", "inches"), "' ", convert = TRUE) %>%
                           mutate(value = ifelse(is.na(inches)==TRUE, value, as.numeric(value) + conv_unit(as.numeric(inches), "inch", "ft"))) %>%
                           # Remove Ties
                           mutate(value = gsub("T", "", value)) %>%
                           # Remove Letters
                           mutate(value = gsub("[A-Z]", NA, value)) %>%
                           # Convert to Numeric
                           mutate(value = as.numeric(value))

Master_Clean <- Master_Clean[, 1:7]


# ======================
# SAVE OUTPUT
# ======================
write.csv(Master_Clean, "PGA Stats.csv", row.names = F)
