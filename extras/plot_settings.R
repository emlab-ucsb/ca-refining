theme_line = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 10, 
                         subtitle_size = 9,
                         axis_title_just = 'center',
                         axis_title_size = 9, 
                         axis_text_size = 9,
                         strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.position = 'bottom',
        strip.text = element_text(hjust = 0.5, face = 'bold'),
        plot.margin = unit(c(1,3,1,1), 'lines'),
        plot.background = element_rect(color = 'white'))

pal_fuel = c('HDV Hydrogen'	=	'#fbb4ae',
             'LDV Hydrogen'	=	'#b3cde3',
             'HDV Electricity'	=	'#ccebc5',
             'LDV Electricity'	=	'#decbe4',
             'Renewable Natural Gas'	=	'#fed9a6',
             'Biodiesel'	=	'#ffffcc',
             'Ethanol'	=	'#e5d8bd',
             'Jet Fuel (Interstate + Military)' = '#A75D5D',
             'Sustainable Aviation Fuel'	=	'#FFC3A1',
             'Jet Fuel (Intrastate)'	=	'#D3756B',
             'Jet Fuel' = '#D3756B',
             'Renewable Diesel'	=	'#ABC7E3',
             'Diesel'	=	'#366BA1',
             'Renewable Gasoline'	=	'#A6BB8D',
             'Gasoline'	=	'#3C6255')

pal_fuel_title = c('Sustainable Aviation Fuel'	=	'#FFC3A1',
                   'Jet Fuel'	=	'#D3756B',
                   'Renewable Diesel'	=	'#ABC7E3',
                   'Diesel'	=	'#366BA1',
                   'Renewable Gasoline'	=	'#A6BB8D',
                   'Gasoline'	=	'#3C6255',
                   'Exports' = '#dadbdf')
pal_label = c('GHG emissions' = '#000000')

race_col_pal <- c("Black" = "#002147",
                  "Hispanic" = "#721817",
                  "Asian" = "#40826D",
                  "white" = "#FFBA00")


race_shape_ptc <- c("forgone_wages_h" = 19,
                    "forgone_wages_l" = 21)

high_low_labs <- c("forgone_wages_h" = "high estimate",
                    "forgone_wages_l" = "low estimate")

poverty_lty <- c("Above poverty line" = "dashed",
                 "Below poverty line" = "solid")

race_shape_ptc <- c("forgone_wages_h" = 19,
                    "forgone_wages_l" = 21)


poverty_ptc_h <- c("Above poverty line" = 19,
                   "Below poverty line" = 17)


poverty_pt_share_l <- c("Above poverty line_forgone_wages_h" = 19,
                 "Above poverty line_forgone_wages_l" = 21,
                 "Below poverty line_forgone_wages_h" = 17,
                 "Below poverty line_forgone_wages_l" = 24)

poverty_ptc_l <- c("total_above_poverty_forgone_wages_h" = 19,
                   "total_above_poverty_forgone_wages_l" = 21,
                   "total_below_poverty_forgone_wages_h" = 17,
                   "total_below_poverty_forgone_wages_l" = 24)

poverty_hl_share_labs <- c("Above poverty line_forgone_wages_h" = "Above poverty line - high esimate",
                     "Above poverty line_forgone_wages_l" = "Above poverty line - low estimate",
                     "Below poverty line_forgone_wages_h" = "Below poverty line - high estimate",
                     "Below poverty line_forgone_wages_l" = "Below poverty line - low estimate")

poverty_hl_labs <- c("total_above_poverty_forgone_wages_h" = "Above poverty line - high esimate",
                     "total_above_poverty_forgone_wages_l" = "Above poverty line - low estimate",
                     "total_below_poverty_forgone_wages_h" = "Below poverty line - high estimate",
                     "total_below_poverty_forgone_wages_l" = "Below poverty line - low estimate")

dac_lty <- c("DAC" = "solid",
             "Non-DAC" = "dotted")

dac_ptc <- c("DAC" = 15,
             "Non-DAC" = 18)

dac_hl_ptc <- c("dac_forgone_wages_h" = 15,
                 "dac_forgone_wages_l" = 22,
                 "non_dac_forgone_wages_h" = 18,
                 "non_dac_forgone_wages_l" = 23)

dac_hl_labs <- c("dac_forgone_wages_h" = "DAC - high esimate",
                 "dac_forgone_wages_l" = "DAC - low estimate",
                 "non_dac_forgone_wages_h" = "Non-DAC - high estimate",
                 "non_dac_forgone_wages_l" = "Non-DAC - low estimate")

dac_hl_ptc_share <- c("DAC_forgone_wages_h" = 15,
                "DAC_forgone_wages_l" = 22,
                "Non-DAC_forgone_wages_h" = 18,
                "Non-DAC_forgone_wages_l" = 23)

dac_hl_labs_share <- c("DAC_forgone_wages_h" = "DAC - high esimate",
                 "DAC_forgone_wages_l" = "DAC - low estimate",
                 "Non-DAC_forgone_wages_h" = "Non-DAC - high estimate",
                 "Non-DAC_forgone_wages_l" = "Non-DAC - low estimate")


