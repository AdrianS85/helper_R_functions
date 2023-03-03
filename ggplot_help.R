p <- palmerpenguins::penguins
p <- p[complete.cases(p),]
pl <- tidyr::pivot_longer(data = p, cols = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"))

f <- openxlsx::read.xlsx(xlsxFile = "Annex  1 - REACH Assessment Test Database_DataAnalyst_v2.xlsx")
fh <- f
fh <- fh[!is.na(f$house_type),]



# continuous histogram count
ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_histogram()

ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_bar(stat = "bin")

ggplot(data = p, mapp = aes(x = body_mass_g))+
  stat_bin()

ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_freqpoly()

ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_point(stat = "bin")+
  geom_line(stat = "bin")



# continuous histogram density
ggplot(data = p, mapp = aes(x = body_mass_g, y = after_stat(density)))+
  geom_histogram()

ggplot(data = p, mapp = aes(x = body_mass_g))+
  stat_density()

ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_freqpoly()



# continuous 1-d boxplot
ggplot(data = p, mapp = aes(x = 1, y = body_mass_g))+
  geom_boxplot()



# continuous ecdf
ggplot(data = p)+
  stat_ecdf(mapping = aes(x = bill_length_mm), color = "red")



# continuous 1-d quantile plot
ggplot(data = p, mapp = aes(x = body_mass_g))+
  stat_bin(
    geom = "bar", 
    breaks = quantile(p$body_mass_g, type = 1),
    color = "red",
    closed = "right")+
  stat_count(geom = "point")



# continuous mean-sd
ggplot(data = p, mapping = aes(x = 1, y = body_mass_g))+
  stat_summary(fun = "mean", fun.max = ~mean(.)+sd(.), fun.min = ~mean(.)-sd(.), geom = "errorbar")+
  stat_summary(fun = "mean", geom = "bar", alpha = 0.5)



# discrete bar plot
ggplot(data = fh, mapping = aes(x = house_type))+
  geom_bar()



# discrete bar plot ordered
names(sort(table(fh$house_type)))
fh$house_type2 <- factor(fh$house_type, levels = names(sort(table(fh$house_type))))
ggplot(data = fh, mapping = aes(x = house_type2, group = 1))+
  stat_count(geom = "point")+
  stat_count(geom = "line")



# 2x categorical bar plot
ggplot(data = fh, mapping = aes(x = house_type))+
  # geom_bar(mapping = aes(fill = drinking_water_source), position = "dodge")
  # geom_bar(mapping = aes(fill = diarrhea_under_5), position = "fill")
  geom_bar(mapping = aes(fill = diarrhea_under_5), position = "stack")



# partial counts/densities plot
ggplot()+
  geom_density(data = p, mapping = aes(x = body_mass_g, y = after_stat(count)), fill = "grey", alpha = 0.2, trim = T)+
  geom_density(data = p[p$species == "Gentoo",], mapping = aes(x = body_mass_g, y = after_stat(count)), fill = "red", alpha = 0.2, trim = T)+
  geom_density(data = p[p$species == "Adelie",], mapping = aes(x = body_mass_g, y = after_stat(count)), fill = "green", alpha = 0.2, trim = T)+
  geom_density(data = p[p$species == "Chinstrap",], mapping = aes(x = body_mass_g, y = after_stat(count)), fill = "yellow", alpha = 0.2, trim = T)

ggplot()+
  stat_bin(data = p, mapping = aes(x = body_mass_g), geom = "area", binwidth = 200, fill = "grey")+
  stat_bin(data = p[p$species == "Gentoo",], mapping = aes(x = body_mass_g), geom = "area", binwidth = 200, fill = "red", alpha = 0.2)+
  stat_bin(data = p[p$species == "Adelie",], mapping = aes(x = body_mass_g), geom = "area", binwidth = 200, fill = "green", alpha = 0.2)+
  stat_bin(data = p[p$species == "Chinstrap",], mapping = aes(x = body_mass_g), geom = "area", binwidth = 200, fill = "yellow", alpha = 0.2)



# mosaic plot
library(ggmosaic)

ggplot(data = p)+
  ggmosaic::geom_mosaic(aes(x = ggmosaic::product(species, island, sex), fill = species))


  


# treemap
library(treemapify)

p_temp <- p %>%
  dplyr::group_by(species, island) %>%
  dplyr::summarise(mean = mean(bill_length_mm), mean_body = mean(body_mass_g))

ggplot(data = p_temp, mapping = aes(area = mean, fill = island, label = paste0(species, "_", mean)))+
  treemapify::geom_treemap()+
  treemapify::geom_treemap_text()



# slopegraph - paired data
p_temp_l <- p_temp
p_temp_l$mean <- scale(p_temp_l$mean, scale = T)
p_temp_l$mean_body <- scale(p_temp_l$mean_body, scale = T)
p_temp_l$group <- stringr::str_c(p_temp_l$species, "_", p_temp_l$island)
p_temp_l <- tidyr::pivot_longer(p_temp_l, cols = c("mean", "mean_body"), values_to = "value")

ggplot(p_temp_l, aes(x = name, y = value, group = group, color = group))+
  geom_point()+
  geom_line()
  


# parallel sets plot - https://datascience.blog.wzb.eu/2016/09/27/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
library(ggparallel)
GGally::ggparcoord(
  data = p[,colnames(p) %in% c("species", "bill_length_mm", "bill_depth_mm", "sex")],
  columns = 2:4,
  groupColumn = 1) ### For categorical/continous

ggparallel::ggparallel(vars = list("species", "sex"), data = as.data.frame(p)) ### For categorical


p1 <- p
p1$species <- as.character(p1$species)
p1$sex <- as.character(p1$sex)

ggplot(p1, aes(y = bill_length_mm, axis1 = species, axis2 = sex, fill = island)) +
  ggalluvial::geom_alluvium() +
  ggalluvial::geom_stratum()+
  scale_x_discrete(limits = c("species", "sex"))



# multiple distributions
ggplot(data = p)+
  geom_density(mapping = aes(x = scale(bill_length_mm)), color = "red", fill = "red", alpha = 0.1)+
  geom_density(mapping = aes(x = scale(bill_depth_mm)), color = "blue", fill = "blue", alpha = 0.1)+
  geom_density(mapping = aes(x = scale(flipper_length_mm)), color = "green", fill = "green", alpha = 0.1)



# multiple distributions - ridgeplot
ggplot(data = p, mapping = aes(x = bill_length_mm, y = island, fill = after_stat(x)))+
  ggridges::geom_density_ridges_gradient(rel_min_height = 0.001)



# qqplot
ggplot(data = p, mapping = aes(sample = bill_length_mm))+
  geom_qq()+
  geom_qq_line()



# using continous colors
ggplot(data = p, mapp = aes(x = body_mass_g, fill = after_stat(x)))+
  stat_bin(bins = 20)
  # scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2])
  # scale_fill_viridis_c()
  # scale_fill_continuous(type = "viridis")
  # scale_fill_distiller()
  # scale_fill_steps(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], n.breaks = 20)
  # scale_fill_steps2(low = scico::scico(n = 3, palette = "berlin")[1], mid = scico::scico(n = 3, palette = "berlin")[2], high = scico::scico(n = 3, palette = "berlin")[3], nice.breaks = T)
  


# using discrete colors 
temp <- viridis::viridis(n = 5)
names(temp) <- c("Timber and concrete", "Makeshift shelter", "Hut", "Timber frame", "Concrete")

ggplot(data = fh, mapping = aes(x = house_type, fill = house_type))+
  geom_bar()
  # scale_fill_brewer()
  # scale_fill_discrete(type = viridis::viridis(n = 5))
  # scale_fill_manual(values = temp)



# scatter plot matrix
ggplot(p, aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species))+
  geom_point()+
  geom_smooth(method ="lm")

GGally::ggpairs(data = p, mapping = aes(color = species))

gpairs::gpairs()

Hmisc::ggfreqScatter(p$bill_length_mm, p$body_mass_g)



# correlogram
GGally::ggcorr(data = p, label = T, palette = "viridis")



# error bars



# survival plots
GGally::ggsurv()
"butt" "butthole" "bum-bum" "poopers" "doodoomaster" "eldoopa" "vondoopa"


vtree::vtree(p, "species")
sjmisc::flat_table(p, species, island, sex)
sjmisc::descr(p)

# limits
# minor_breaks may be overriden by theme which does not show them
# break generally also return reference lines on the drawing itself
ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_histogram()+
  theme_dark()
  # scale_x_continuous(n.breaks = 5, minor_breaks = NULL, trans = "log10") # trans for reverse transformations
  scale_x_continuous(
    name = "Body mass",
    limits = c(2500,6500),
    breaks = seq(2500,6500,1000),
    minor_breaks = function(x){return(seq(x[1],x[2], 200))},
    labels = stringr::str_c(as.character(seq(2500,6500,1000)), " g"), # function with limits as input
    expand = ggplot2::expansion(add = 500),
    position = "bottom",
    guide = guide_axis(),
    oob = scales::censor, na.value = NA)+ 
  coord_cartesian(xlim = c(2500, 6500), ylim = c(0, 25), clip = "off") # zoom


# value may be continous, but it has only few values, so You dont need to bin the values, its enough to just count them
test1 <- ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_histogram()+
  theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, debug = T),
    panel.background = element_rect(fill = "green"),
    plot.background = element_rect(fill = "red"))


test2 <- ggplot(data = p, mapp = aes(x = body_mass_g))+
  geom_histogram()+
  geom_hline(yintercept = 25)+
  annotate(geom = "text", x = 5575, y = 20, label = "0", size = 10) # these are aestetics from geom text


test3 <- ggplot()+
  annotate(geom = "text", x = 1, y = 1, label = "ass", size = 10)+
  theme_void()

(test1 + test2) / inset_element(test3, left = 0.5, bottom = 0.5, right = 1, top = 1)+
  plot_annotation(
    title = 'The surprising truth about mtcars',
    subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = 'Disclaimer: None of these plots are insightful',
    tag_levels = 'A')
  

patchwork::wrap_plots(test1, test, nrow = 1)



test <- layer_data(
  
  ggplot(data = p, mapp = aes(x = body_mass_g))+
    geom_histogram()+
    theme_dark()
  
  
  
)

test2 <- layer_data(
  ggplot()+
    geom_density(data = p[p$species == "Gentoo",], mapping = aes(x = body_mass_g, y = after_stat(count)), fill = "red", stat = "density", alpha = 0.2)
  
)
