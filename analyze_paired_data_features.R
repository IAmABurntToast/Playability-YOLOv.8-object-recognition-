
library("gmodels")
library("ggplot2")
library("qqplotr")
library(DescTools)
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)

folders <- c('111', "462", "535", "825", "933")

# Import the paired image data
paired_data <- read_csv("miniconda3/minicoco/results/paired_data.csv")

# save images to all_images folder
for(i in folders){
    pathname <- paste0("miniconda3/minicoco/data/Images 2/",i)
    templist <- list.files(pathname)
  for(ii in templist){
    if(ii %in% all){
      file.copy(paste0(pathname, "/", ii), paste0("miniconda3/minicoco/data/all_images/", ii))
    }
  }
}


#Import the prediction data
data <-read.csv(paste0("miniconda3/minicoco/results/all_predictions/", PREDICTIONS_FILENAME, ".csv"))

# Clean and summarize prediction data for each image
colnames(data) <- c("image", "class_number", "class_name", "confidence")

# fix row that was saved as column name for cars
#data[155577, "image"] <- "J3G6N1_nR_Ct_326.99.jpg"
#data[155577, "class_number"] <- 2
#data[155577, 'class_name'] <- "car"
#data[155577, 'confidence'] <- 0.8389518857002258

#fix row that was saved as column name for all_other prediction file
#data[7575, "image"] <- "H1X2C3_Mtjoi_197.45.jpg"
#data[7575, "class_number"] <- 7
#data[7575, 'class_name'] <- "truck"
#data[7575, 'confidence'] <- 0.740244805812835

data$image <- str_replace(data$image, "/Users/emilygemmell/miniconda3/minicoco/data/all_images/", "") 

data <- distinct(data)

data$count <- 1

# for all_other prediction file, make an empty list to store each prediction class dataframe
all_other_list <- list()

for(i in unique(data$class_name)){
  tempdata <- data[data$class_name == i, ] 
    tempdata <- tempdata %>%
      group_by(image, class_name) %>%
    summarize(total = sum(count),
              conf_avg = mean(confidence))
  temp_w <- spread(tempdata,                       # Applying spread function
                   key = class_name,
                   value = total)
  
files <- list.files(path = "/Users/emilygemmell/miniconda3/minicoco/data/all_images/")
    
    datalist <- list()
    for (ii in files){
      if(ii %in% temp_w$image){
        next
      }else{
        datalist <- c(datalist, ii)
      }
    }
    
    missing <-do.call(rbind, datalist)
    missing <- as.data.frame(missing)
    colnames(missing) <- c("image")
    
    cnames <- as.list(names(temp_w))
    
    missing <- as.data.frame(missing) %>%
      mutate(conf_avg = NA,
             classnm = NA)
    colnames(missing) <- c("image", "conf_avg", cnames[3])
    missing <- missing %>% 
      mutate_at("image", str_replace, "/Users/emilygemmell/miniconda3/minicoco/data/all_images/", "") 
    
    all_data <- rbind(temp_w, missing)
    
    # replace nas with 0s for class column
    all_data <- all_data %>% 
      mutate_at(3, ~replace_na(.,0))
    
 # for all_other data, append files for each class to a list  
    all_other_list[[i]] <- all_data
    
}

# all_other prediction data is saved in the all_other list - use this script to 
# retrieve data for different classes by position in list, then rbind datasets to group by
# feature type. (kid-debris type is illustrated here)
   kid_debris <- all_other_list[c(1,7,16)]
   kid_debris <- do.call(rbind, kid_debris)
   
   kid_debris <- kid_debris %>% 
     mutate_at(c(3:8), ~replace_na(.,0))
   
   kid_debris$kd_total <- rowSums(kid_debris[,3:8])
   
   kid_debris <- kid_debris %>%
     group_by(image) %>%
     summarize(total = sum(kd_total))
   all_data <- kid_debris
    
# link prediction data to paired_data winner
    pred_pairs <- merge(paired_data, all_data, by.x = "winner_id", by.y = "image", all.x = TRUE)
    colnames(pred_pairs) <- c("winner_id", "loser_id", "winner_conf_avg", 
                              "winner_count")
    
    # link prediction data to paired data loser
    pred_pairs <- merge(pred_pairs, all_data, by.x = "loser_id", by.y = "image", all.x = TRUE)
    colnames(pred_pairs) <- c( "loser_id", "winner_id", "winner_conf_avg", 
                               "winner_count", "loser_conf_avg", 
                               "loser_count")
    
 # remove missing data
    pred_pairs <- filter(pred_pairs, (!is.na(pred_pairs$winner_count) & (!is.na(pred_pairs$loser_count))))
    
    average_winner_count <- mean(pred_pairs$winner_count)
    average_winner_confidence <- mean(pred_pairs$winner_conf_avg)
    average_winner_count
    average_winner_confidence
    
    average_loser_count <- mean(pred_pairs$loser_count)
    average_loser_confidence <- mean(pred_pairs$loser_conf_avg)
    average_loser_count
    average_loser_confidence

# convert data to long format    
    long_datw <- pred_pairs[, c(2,3,4)]
    colnames(long_datw) <- c("image", "conf_avg", 
                             "count")
    long_datw$group <- "winner"
    
    long_datl <- pred_pairs[, c(1,5,6)]
    colnames(long_datl) <- c("image", "conf_avg", 
                             "count")
    long_datl$group <- "loser"
    
    long_dat <- rbind(long_datw, long_datl)
    
# remove rows where both winner and loser have 0 cars
    pred_pairs2 <- pred_pairs[((pred_pairs$winner_count != 0) & (pred_pairs$loser_count !=0)) | 
                                ((pred_pairs$winner_count != 0) | (pred_pairs$loser_count !=0)), ]
    
    long_dat2w <- pred_pairs2[, c(2,3,4)]
    colnames(long_dat2w) <- c("image", "conf_avg", 
                              "count")
    long_dat2w$group <- "winner"
    
    long_dat2l <- pred_pairs2[, c(1,5,6)]
    colnames(long_dat2l) <- c("image", "conf_avg", 
                              "count")
    long_dat2l$group <- "loser"
    
    long_dat2 <- rbind(long_dat2w, long_dat2l)
   
    ##################################################################################################
    
    # T-test 
    
    library("gmodels")
    library("ggplot2")
    library("qqplotr")
    library("dplyr")
    library(tidyverse)
    library(hrbrthemes)
    library(viridis)
    
    #The following assumptions must be met in order to run a Mann-Whitney U test:
    
    # 1. Treatment groups are independent of one another. Experimental units only receive one treatment and they do not overlap.
    # 2. The response variable of interest is ordinal or continuous.
    # 3. Both samples are random.
    
    #Perform the Shapiro-Wilk Test for Normality on each group
    #cars %>%
    #  group_by(group) %>%
    #  summarise(`W Statistic` = shapiro.test(car)$statistic,
    #            `p-value` = shapiro.test(car)$p.value)
    
    hist(pred_pairs$winner_count)
    hist(pred_pairs$loser_count)
    
    # Find differences in raw data
    pred_pairs$diffcount <- pred_pairs$winner_count - pred_pairs$loser_count
    
    # Test if average difference is significantly different than zero
    # raw differences
    t.test(pred_pairs$diffcount)
    
    
    # windsorize data to deal with outliers
    pred_pairs2$windswinner <- Winsorize(pred_pairs2$winner_count, minval = NULL, 
                                        maxval = NULL, 
                                        probs = c(0.05, 0.95),
                                        na.rm = FALSE, type = 7)
    
    pred_pairs2$windsloser <- Winsorize(pred_pairs2$loser_count, minval = NULL, 
                                        maxval = NULL, 
                                        probs = c(0.05, 0.95),
                                        na.rm = FALSE, type = 7)
    
    pred_pairs2$diffwindcount <- pred_pairs2$windswinner - pred_pairs2$windsloser
  
    # Test if average difference is significantly different than zero
    # difference on winsorized data
   t.test(pred_pairs2$diffwindcount)
   
   #Produce visualizations

# Boxplots          
  long_dat2 %>%
            ggplot( aes(x= group, y=count, fill=group)) +
            stat_boxplot(geom ="errorbar", width = 0.5) +
            geom_boxplot() +
            scale_fill_brewer(palette= "Dark2") +
            theme_classic () +
            theme(
              legend.position="none",
              plot.title = element_text(size=14)) +
            ggtitle("Boxplot of all kid debris in winning and losing images") +
            ylab("number of items in image")
          
# Violin basic
          long_dat2 %>%
            ggplot( aes(x=group, y=count, fill=group)) +
            geom_violin() +
            scale_fill_brewer(palette= "Dark2") +
            theme_classic() +
            theme(
              legend.position="none",
              plot.title = element_text(size=14)
            ) +
            ggtitle("Violin plot of bicycle count in winning and losing images") +
            ylab("number of bicycles in image")
          
    

