# install.packages("devtools")
#devtools::install_github("nickhaf/eatPlot")

library(eatPlot)

# Linienplot 1 Gruppe -----------------------------------------------------

trend_dat <- readRDS("I:/Methoden/02_IQB-interne_eat_Workshops/eatRep_2024/Beispieloutputs/04_meansAufbereitet.rds")

dat_lineplot_1 <- prep_lineplot(
  trend_dat
)

## For correctly displaying the state names:
dat_lineplot_1 <- process_bundesland(dat_lineplot_1)

lineplot_1 <- plot_lineplot(dat_lineplot_1,
                            years_lines = list(c(2009, 2015), c(2015, 2022)),
                            years_braces = list(c(2009, 2015), c(2015, 2022)),
                            #brace_label_sig_superscript = "sig_comparison_trend_crossDiffTotal",
                            plot_settings = plotsettings_lineplot(default_list = lineplot_4x4)
)


save_plot(lineplot_1, filename = "C:/Users/hafiznij/Downloads/lineplot_1_group.pdf")


# Linienplots 2 Gruppen ---------------------------------------------------
trend_dat_geschlecht <- readRDS("I:/Methoden/02_IQB-interne_eat_Workshops/eatRep_2024/Beispieloutputs/disp_geschlecht.rds")

dat_lineplot_2 <- prep_lineplot(
  trend_dat_geschlecht,
  subgroup_var = "Kgender"
)
dat_lineplot_2 <- process_bundesland(dat_lineplot_2)

## Here we have to set the order of the brace labels manually:
dat_lineplot_2$subgroup_var <- factor(dat_lineplot_2$subgroup_var, ordered = TRUE, levels = c("weiblich", "maennlich", "total"))

lineplot_2 <- plot_lineplot(
  dat_lineplot_2,
  years_lines = list(c(2009, 2015), c(2015, 2022)),
  years_braces = list(c(2009, 2015), c(2015, 2022)),
  background_subgroup = "total",
  plot_settings = plotsettings_lineplot(subgroup_colours = c(weiblich = "#9B1B34",
                                                             maennlich = "#F4BA02"),
                                        axis_x_background_colour = "#00e0e0",
                                        background_line_colour = "#01364C",
                                        default_list = lineplot_4x4)
)

save_plot(lineplot_2, filename = "C:/Users/hafiznij/Downloads/lineplot_2_groups.pdf")



# Tabelle/Barplot Geschlechterkapitel 6.6 ---------------------------------

dat_6.6 <- prep_tablebarplot(
  trend_dat_geschlecht,
  subgroup_var = "Kgender",
  par = c("mean", "sd"), ## We need both mean and sd for the plot
  comparisons = c("none", "trend", "trend_crossDiff") ## Set for smaller output-table
)

dat_6.6 <- subset(dat_6.6, Kgender %in% c("maennlich", "weiblich"))
dat_6.6$Kgender <- gsub("maennlich", "Jungen", dat_6.6$Kgender)
dat_6.6$Kgender <- gsub("weiblich", "Mädchen", dat_6.6$Kgender)

dat_6.6_a <- dat_6.6[order(dat_6.6$TR_BUNDESLAND), ]

dat_6.6_a$TR_BUNDESLAND[duplicated(dat_6.6_a$TR_BUNDESLAND)] <- " "
dat_6.6_a$TR_BUNDESLAND <- gsub("total", "Deutschland", dat_6.6_a$TR_BUNDESLAND)
dat_6.6_a <- process_bundesland(dat_6.6_a)

column_widths_stand <- standardize_column_width(
  column_widths = list(
    p1 = c(0.1, rep(0.04, 7), 0.05, rep(0.04, 2), NA),
    p2 = c(0.05, rep(0.04, 2), NA)
  ),
  plot_ranges = c(80, 80) # Range of the x-axes of both plots set in 'axis_x_lims'.
)


p_1 <- plot_tablebarplot(
  dat = dat_6.6_a,
  bar_est = "est_2015 - 2009_trend_mean",
  bar_label = NULL,
  bar_sig = "sig_2015 - 2009_trend_mean",
  bar_fill = "Kgender",
  column_spanners = list(
    "**2009**<sup>a</sup>" = c(3, 4),
    "**2015**<sup>a</sup>" = c(5, 6),
    "**2022**<sup>a</sup>" = c(7, 8),
    "**Differenz 2015-2009<sup>a</sup>**" = c(9, 12)
  ),
  columns_table_se = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "se_2015 - 2009_trend_mean", NULL),
  headers = list("**Land**", " ", "*M*", "*SD*", "*M*", "*SD*", "*M*", "*SD*", "*M<sub>2015</sub> - M<sub>2009</sub>*", "*(SE)*", "*d*", " "),
  columns_table = c("TR_BUNDESLAND", "Kgender", "est_2009_none_mean", "est_2009_none_sd", "est_2015_none_mean", "est_2015_none_sd", "est_2022_none_mean", "est_2022_none_sd", "est_2015 - 2009_trend_mean", "se_2015 - 2009_trend_mean", "es_2015 - 2009_trend_mean"),
  columns_table_sig_bold = list(NULL, NULL, "sig_2009_none_mean",
                                NULL, "sig_2015_none_mean", NULL, "sig_2022_none_mean", NULL, "sig_2015 - 2009_trend_mean", NULL, "sig_2015 - 2009_trend_mean"),
  columns_table_sig_superscript = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "sig_2015 - 2009_trend_crossDiffTotal_mean", NULL,NULL),
  y_axis = "y_axis",
  columns_round = c(rep(0, 10), 2),
  plot_settings = plotsettings_tablebarplot(
    axis_x_lims = c(-40, 40),
    bar_pattern_spacing = 0.0154, ## We calculated this below in standardize_pattern_spacing()
    background_stripes_colour = c(rep(c("white", "white", "#EBFDF3", "#EBFDF3"), 8), "grey", "grey"),
    bar_fill_colour = c("#20D479", "#8DEBBC"),
    columns_alignment = c(0, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2),
    columns_width = column_widths_stand$p1, ## This is the column-width object we set above
    headers_alignment = c(0, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0),
    default_list = barplot_table_plot_pattern
  )
)

p_2 <- plot_tablebarplot(
  dat = dat_6.6_a,
  bar_est = "est_2022 - 2015_trend_mean",
  bar_label = NULL,
  bar_sig = "sig_2022 - 2015_trend_mean",
  bar_fill = "Kgender",
  column_spanners = list(
    "**Differenz 2022-2015**" = c(1, 4)
  ),
  headers = list(
    "*M<sub>2022</sub> - M<sub>2015</sub>*",
    "*(SE)*",
    "*d*",
    " "
  ),
  columns_table = c(
    "est_2022 - 2015_trend_mean",
    "se_2022 - 2015_trend_mean",
    "es_2022 - 2015_trend_mean"
  ),
  columns_table_se = list(NULL, "se_2022 - 2015_trend_mean", NULL),
  columns_table_sig_bold = list("sig_2022 - 2015_trend_mean", NULL, "sig_2022 - 2015_trend_mean"),
  columns_table_sig_superscript = list("sig_2022 - 2015_trend_crossDiffTotal_mean", NULL, NULL),
  y_axis = "y_axis",
  columns_round = c(0, 0, 2),
  plot_settings = plotsettings_tablebarplot(
    axis_x_lims = c(-40, 40),
    bar_pattern_spacing = 0.0346, ## We calculated this below in standardize_pattern_spacing()
    background_stripes_colour = c(rep(c("white", "white", "#EBFDF3", "#EBFDF3"), 8), "grey", "grey"),
    bar_fill_colour = c("#20D479", "#8DEBBC"),
    columns_alignment = c(rep(0.5, 2), 2),
    columns_width = column_widths_stand$p2, ## This is the column-width object we set above
    headers_alignment = c(0.5, 0.5, 0.5, 0),
    default_list = barplot_table_plot_pattern
  )
)

bar_pattern_spacing_stand <- standardize_pattern_spacing(list(p_1, p_2), pattern_spacing = 0.05)

tableplot_6.6 <- combine_plots(list(p_1, p_2))

save_plot(tableplot_6.6, filename = "C:/Users/hafiznij/Downloads/abb_6.6.pdf", width = 320)



# Tabelle 6.6 mit Nested output -------------------------------------------
kap_06 <- readRDS("i:/Methoden/02_IQB-interne_eat_Workshops/eatRep_2024/Beispieloutputs/05_disparitaeten.rds")

## Let's take a look:
str(kap_06)

## Wir haben mehrere kompetenzbereiche:
names(kap_06)

## Ich würde empfehlen, den Plot am Anfang anhand eines Bereichs zu bauen, und dann einfach in eine Schleife zu packen. Let's start with "hoeren":
kap_06$hoeren

## 1. Zuerst suche ich mir eine Passende Voralge von der Webseite aus: https://nickhaf.github.io/eatPlot/articles/tableplots.html
## Wir haben Glück, Abbildung 6.6 gibts schon! Ich kann mir also die Datenaufbereitung einfach herunterkopieren und durch meine Daten austauschen.
## Dafür muss ich zuerst herausfinden, wie meine Gruppierungsvariable heißt. Am einfachsten kann ich das über den group oder den plain Data.frame herausfinden:
str(kap_06$hoeren$group)

## Meine Gruppierungsvariable heißt "Kgender".

dat_6.6 <- prep_tablebarplot(
  kap_06$hoeren,
  subgroup_var = "Kgender",
  par = c("mean", "sd") ## We need both mean and sd for the plot
)



