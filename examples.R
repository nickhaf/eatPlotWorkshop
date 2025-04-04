# install.packages("devtools")
# devtools::install_github("nickhaf/eatPlot")

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
  # brace_label_sig_superscript = "sig_comparison_trend_crossDiffTotal",
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
  plot_settings = plotsettings_lineplot(
    subgroup_colours = c(
      weiblich = "#9B1B34",
      maennlich = "#F4BA02"
    ),
    axis_x_background_colour = "#00e0e0",
    background_line_colour = "#01364C",
    default_list = lineplot_4x4
  )
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
  columns_table_sig_bold = list(
    NULL, NULL, "sig_2009_none_mean",
    NULL, "sig_2015_none_mean", NULL, "sig_2022_none_mean", NULL, "sig_2015 - 2009_trend_mean", NULL, "sig_2015 - 2009_trend_mean"
  ),
  columns_table_sig_superscript = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "sig_2015 - 2009_trend_crossDiffTotal_mean", NULL, NULL),
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

# install.packages("devtools")
devtools::install_github("nickhaf/eatPlot")

library(eatPlot)
library(tidyverse)

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


## Jetzt müssen wir die Daten noch ein wenig für unseren spezifischen Plot umformen
## Wir wollen nur männlich und weiblich plotten:
gender_hoeren <- subset(dat_6.6, subgroup_var %in% c("maennlich", "weiblich"))

## Mädchen sollen nach oben, d.h. wir sortieren zuerst absteigend alphabetisch, und dann nach den Bundesländern, um diese wieder in die richtige Reihenfolge zu bekommen
gender_hoeren <- gender_hoeren[order(gender_hoeren$subgroup_var, decreasing = TRUE), ]
gender_hoeren <- gender_hoeren[order(gender_hoeren$state_var), ]

## Dann soll nur in jeder zweiten Zeile das Bundesland geplotted werden, wir entfernen also Duplikate:
gender_hoeren$state_var[duplicated(gender_hoeren$state_var)] <- " "

## Mit process_bundesland() können wir Bindestriche einfügen und Umlaute austauschen.
## linebreak = TRUE damit Bundesländer die aus zwei Wörten bestehen umgebrochen werden
gender_hoeren$state_var <- process_bundesland(gender_hoeren$state_var, linebreak = TRUE)

## Dann müssen wir noch die Gruppen in Jungen und Mädchen umbenennen
gender_hoeren$subgroup_var <- gsub("maennlich", "Jungen", gender_hoeren$subgroup_var)
gender_hoeren$subgroup_var <- gsub("weiblich", "Mädchen", gender_hoeren$subgroup_var)

## Und eine leere Spalte erzeugen, die später als Trenner dient
gender_hoeren$empty <- ""


## 2. Spaltenweiten
## Da wir gleich mehrere Plots aneinanderfügen müssen wir noch die Spaltenbreiten harmonisieren:

column_widths_stand <- standardize_column_width(
  column_widths = list(
    p1 = c(0.085, 0.05, rep(0.035, 6), 0.015, rep(0.035, 3), NA),
    p2 = c(rep(0.035, 3), NA)
  ),
  plot_ranges = c(140, 142) # Ranges of the x-axes of both plots set in 'axis_x_lims'.
)


## 3. Tabellen erzeugen:
## Dafür copy-pasten wir einfach von der Vorlage:

p_1 <- plot_tablebarplot(
  dat = gender_hoeren,
  bar_est = "est_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
  bar_label = NULL,
  bar_sig = "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
  bar_fill = "subgroup_var",
  column_spanners = list(
    "**2009**" = c(3, 4),
    "**2015**" = c(5, 6),
    "**2022**" = c(7, 8),
    "**Differenz 2015-2009**" = c(10, 13)
  ),
  columns_table_se = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "se_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", NULL),
  headers = list("**Land**", " ", "*M*", "*SD*", "*M*", "*SD*", "*M*", "*SD*", "", "*M<sub>2015</sub>-<br>M<sub>2009</sub>* ", "*(SE)*", "*d*", " "),
  columns_table = c("state_var", "subgroup_var", "est_mean_comp_none_2009", "est_sd_comp_none_2009", "est_mean_comp_none_2015", "est_sd_comp_none_2015", "est_mean_comp_none_2022", "est_sd_comp_none_2022", "empty", "est_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", "se_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", "es_mean_comp_trend_sameFacet_sameSubgroup_2009_2015"),
  columns_table_sig_bold = list(
    NULL, NULL, "sig_mean_comp_none_2009",
    NULL, "sig_mean_comp_none_2015", NULL, "sig_mean_comp_none_2022", NULL, NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015"
  ),
  columns_table_sig_superscript = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "sig_mean_comp_trend_crossDiff_totalFacet_sameSubgroup_2009_2015", NULL, NULL),
  y_axis = "y_axis",
  columns_round = c(rep(0, 11), 2),
  plot_settings = plotsettings_tablebarplot(
    axis_x_lims = c(-70, 70),
    bar_pattern_spacing = 0.016, ## We calculated this below in standardize_pattern_spacing()
    columns_alignment = c(0, 0, rep(2, 10)),
    columns_width = column_widths_stand$p1, ## This is the column-width object we set above
    columns_table_sig_superscript_letter = "a",
    columns_table_sig_superscript_letter_nudge_x = 5.5,
    columns_nudge_y = c(-0.5, rep(0, 11)),
    headers_alignment = c(0, 0, rep(0.5, 7), 0, 0.5, 0.5, 0),
    headers_row_height = 1.75,
    headers_nudge_x = c(rep(0, 9), 2, rep(0, 3)),
    default_list = abb_6.6
  )
)

p_1

p_2 <- plot_tablebarplot(
  dat = gender_hoeren,
  bar_est = "est_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
  bar_label = NULL,
  bar_sig = "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
  bar_fill = "subgroup_var",
  column_spanners = list(
    "**Differenz 2022-2015**" = c(1, 4)
  ),
  headers = list(
    "*M<sub>2022</sub>-<br>M<sub>2015</sub>* ",
    "*(SE)*",
    "*d*",
    " "
  ),
  columns_table = c(
    "est_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
    "se_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
    "es_mean_comp_trend_sameFacet_sameSubgroup_2015_2022"
  ),
  columns_table_se = list(NULL, "se_mean_comp_trend_sameFacet_sameSubgroup_2015_2022", NULL),
  columns_table_sig_bold = list("sig_mean_comp_trend_sameFacet_sameSubgroup_2015_2022", NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2015_2022"),
  columns_table_sig_superscript = list("sig_mean_comp_trend_crossDiff_totalFacet_sameSubgroup_2015_2022", NULL, NULL),
  y_axis = "y_axis",
  columns_round = c(0, 0, 2),
  plot_settings = plotsettings_tablebarplot(
    axis_x_lims = c(-70, 72),
    bar_pattern_spacing = 0.034, ## We calculated this below in standardize_pattern_spacing()
    columns_alignment = c(2, 2, 2),
    columns_width = column_widths_stand$p2, ## This is the column-width object we set above
    columns_table_sig_superscript_letter = "a",
    columns_table_sig_superscript_letter_nudge_x = 5.5,
    headers_nudge_x = c(2, 0, 0, 0),
    headers_alignment = c(0, 0.5, 0.5, 0),
    headers_row_height = 1.75,
    default_list = abb_6.6
  )
)

p_2

## 3. Damit die Muster in den Balken gleich-breit sind müssen wir sie noch standardisieren und
## oben anpassen:
bar_pattern_spacing_stand <- standardize_pattern_spacing(list(p_1, p_2), pattern_spacing = 0.05)

## (Die Plots dann nochmal ausführen)


## 4. Schlussendlich kombinieren:
tableplot_6.6 <- combine_plots(list(p_1, p_2))

## 5. Zum Anschauen kann ich das Paket ggview verwenden, damit die margins gleich richtig angezeigt werden:
p <- tableplot_6.6 +
  ggview::canvas(
    235, 130,
    units = "mm"
  )
p

## 6. Wenn alles passt, abspeichern: (Bitte die width und height so setzen wie in der Vorlage, damit es einheitlich ist)
save_plot(tableplot_6.6, filename = "C:/Users/hafiznij/Downloads/abb_6.6.pdf", width = 235, height = 130, scaling = 1)


#################
## In Schleife ##
#################
## Wenn das Grundgerüst steht, einfach in lapply() packen für alle Kompetenzbereiche. Am schönsten siehts in einer eigenen Funktion aus:

prepare_6.6_data <- function(dat_kb) {
  dat_6.6 <- prep_tablebarplot(
    dat_kb,
    subgroup_var = "Kgender",
    par = c("mean", "sd") ## We need both mean and sd for the plot
  )


  ## Jetzt müssen wir die Daten noch ein wenig für unseren spezifischen Plot umformen
  ## Wir wollen nur männlich und weiblich plotten:
  dat_6.6 <- subset(dat_6.6, subgroup_var %in% c("maennlich", "weiblich"))

  ## Mädchen sollen nach oben, d.h. wir sortieren zuerst absteigend alphabetisch, und dann nach den Bundesländern, um diese wieder in die richtige Reihenfolge zu bekommen
  dat_6.6 <- dat_6.6[order(dat_6.6$subgroup_var, decreasing = TRUE), ]
  dat_6.6 <- dat_6.6[order(dat_6.6$state_var), ]

  ## Dann soll nur in jeder zweiten Zeile das Bundesland geplotted werden, wir entfernen also Duplikate:
  dat_6.6$state_var[duplicated(dat_6.6$state_var)] <- " "

  ## Mit process_bundesland() können wir Bindestriche einfügen und Umlaute austauschen.
  ## linebreak = TRUE damit Bundesländer die aus zwei Wörten bestehen umgebrochen werden
  dat_6.6$state_var <- process_bundesland(dat_6.6$state_var, linebreak = TRUE)

  ## Dann müssen wir noch die Gruppen in Jungen und Mädchen umbenennen
  dat_6.6$subgroup_var <- gsub("maennlich", "Jungen", dat_6.6$subgroup_var)
  dat_6.6$subgroup_var <- gsub("weiblich", "Mädchen", dat_6.6$subgroup_var)

  ## Und eine leere Spalte erzeugen, die später als Trenner dient
  dat_6.6$empty <- ""

  return(dat_6.6)
}
plot_6.6 <- function(dat_prepped) {
  column_widths_stand <- standardize_column_width(
    column_widths = list(
      p1 = c(0.085, 0.05, rep(0.035, 6), 0.015, rep(0.035, 3), NA),
      p2 = c(rep(0.035, 3), NA)
    ),
    plot_ranges = c(140, 142) # Ranges of the x-axes of both plots set in 'axis_x_lims'.
  )


  ## 3. Tabellen erzeugen:
  ## Dafür copy-pasten wir einfach von der Vorlage:

  p_1 <- plot_tablebarplot(
    dat = dat_prepped,
    bar_est = "est_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
    bar_label = NULL,
    bar_sig = "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
    bar_fill = "subgroup_var",
    column_spanners = list(
      "**2009**" = c(3, 4),
      "**2015**" = c(5, 6),
      "**2022**" = c(7, 8),
      "**Differenz 2015-2009**" = c(10, 13)
    ),
    columns_table_se = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "se_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", NULL),
    headers = list("**Land**", " ", "*M*", "*SD*", "*M*", "*SD*", "*M*", "*SD*", "", "*M<sub>2015</sub>-<br>M<sub>2009</sub>* ", "*(SE)*", "*d*", " "),
    columns_table = c("state_var", "subgroup_var", "est_mean_comp_none_2009", "est_sd_comp_none_2009", "est_mean_comp_none_2015", "est_sd_comp_none_2015", "est_mean_comp_none_2022", "est_sd_comp_none_2022", "empty", "est_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", "se_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", "es_mean_comp_trend_sameFacet_sameSubgroup_2009_2015"),
    columns_table_sig_bold = list(
      NULL, NULL, "sig_mean_comp_none_2009",
      NULL, "sig_mean_comp_none_2015", NULL, "sig_mean_comp_none_2022", NULL, NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015", NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015"
    ),
    columns_table_sig_superscript = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "sig_mean_comp_trend_crossDiff_totalFacet_sameSubgroup_2009_2015", NULL, NULL),
    y_axis = "y_axis",
    columns_round = c(rep(0, 11), 2),
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(-70, 70),
      bar_pattern_spacing = 0.016, ## We calculated this below in standardize_pattern_spacing()
      columns_alignment = c(0, 0, rep(2, 10)),
      columns_width = column_widths_stand$p1, ## This is the column-width object we set above
      columns_table_sig_superscript_letter = "a",
      columns_table_sig_superscript_letter_nudge_x = 5.5,
      columns_nudge_y = c(-0.5, rep(0, 11)),
      headers_alignment = c(0, 0, rep(0.5, 7), 0, 0.5, 0.5, 0),
      headers_row_height = 1.75,
      headers_nudge_x = c(rep(0, 9), 2, rep(0, 3)),
      default_list = abb_6.6
    )
  )



  p_2 <- plot_tablebarplot(
    dat = dat_prepped,
    bar_est = "est_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
    bar_label = NULL,
    bar_sig = "sig_mean_comp_trend_sameFacet_sameSubgroup_2009_2015",
    bar_fill = "subgroup_var",
    column_spanners = list(
      "**Differenz 2022-2015**" = c(1, 4)
    ),
    headers = list(
      "*M<sub>2022</sub>-<br>M<sub>2015</sub>* ",
      "*(SE)*",
      "*d*",
      " "
    ),
    columns_table = c(
      "est_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
      "se_mean_comp_trend_sameFacet_sameSubgroup_2015_2022",
      "es_mean_comp_trend_sameFacet_sameSubgroup_2015_2022"
    ),
    columns_table_se = list(NULL, "se_mean_comp_trend_sameFacet_sameSubgroup_2015_2022", NULL),
    columns_table_sig_bold = list("sig_mean_comp_trend_sameFacet_sameSubgroup_2015_2022", NULL, "sig_mean_comp_trend_sameFacet_sameSubgroup_2015_2022"),
    columns_table_sig_superscript = list("sig_mean_comp_trend_crossDiff_totalFacet_sameSubgroup_2015_2022", NULL, NULL),
    y_axis = "y_axis",
    columns_round = c(0, 0, 2),
    plot_settings = plotsettings_tablebarplot(
      axis_x_lims = c(-70, 72),
      bar_pattern_spacing = 0.034, ## We calculated this below in standardize_pattern_spacing()
      columns_alignment = c(2, 2, 2),
      columns_width = column_widths_stand$p2, ## This is the column-width object we set above
      columns_table_sig_superscript_letter = "a",
      columns_table_sig_superscript_letter_nudge_x = 5.5,
      headers_nudge_x = c(2, 0, 0, 0),
      headers_alignment = c(0, 0.5, 0.5, 0),
      headers_row_height = 1.75,
      default_list = abb_6.6
    )
  )

  tableplot_6.6 <- combine_plots(list(p_1, p_2))

  save_plot(tableplot_6.6,
    filename = paste0(
      "C:/Users/hafiznij/Downloads/abb_6.6_",
      unique(dat_prepped$kb),
      ".pdf"
    ),
    width = 235,
    height = 130,
    scaling = 1
  )

  return(tableplot_6.6)
}

## Die Funktionen machen jetzt einfach das gleiche wie wir vorher gemacht haben für einen kb:
prepped_hoeren <- prepare_6.6_data(kap_06$hoeren)
plot_hoeren <- plot_6.6(prepped_hoeren)


## Let's try the loop:
list_prepped <- lapply(kap_06, prepare_6.6_data)
list_plots <- lapply(list_prepped, plot_6.6)
