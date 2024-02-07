# PREP DATA ----

# Set colors ----
m_color = "#00C4A9"
f_color = "#8600FC"

set_colors <- c("#8600FC", "#00C4A9")

names(set_colors) <- c("F", "M")

options(scipen = 999)



# Data ----

# original data
babynames <- babynames::babynames


## Sum of proportions all data ----

# calculate the total sum of prop value for each name
# assign percentile

dat_sum_prop <- babynames |>
    
    # group by name
    group_by(name) |>
    
    # get sum of prop & n for each name
    dplyr::summarise(
        sum_prop = sum(prop), 
        count_n = sum(n)) |> 
    
    # put in descending order
    arrange(desc(sum_prop)) |>
    
    # ungroup
    ungroup() |>
    
    # assign percentile to each name based on sum_prop value
    mutate(percentile = ntile(sum_prop, 100))

# calculate total n, n Male, n Female, percent M, percent F
dat_calc <- dat_sum_prop |>
    left_join(babynames, by = join_by(name), 
              relationship = "many-to-many") |>
    dplyr::group_by(name) |>
    summarise(total_n = sum(n),
              total_n_M = sum(n[sex == "M"]),
              total_n_F = sum(n[sex == "F"])) |>
    mutate(prop_M = total_n_M / total_n * 100,
           prop_F = total_n_F / total_n * 100)

# put dat_calc together with dat_sum_prop
dat_all <- dat_sum_prop |>
    full_join(dat_calc, by = join_by(name))



# Function to select random sample ----
# input data, percentile to filter, and n samples to return

random_sample_names_percentile <- function(dat, n_percentile, n_samples) {
    random_sample <- dat |>
        dplyr::filter(percentile == n_percentile) |>
        slice_sample(n = n_samples)
    return(random_sample)
}


random_sample_names <- function(dat, n_samples) {
    random_sample <- dat |>
        dplyr::slice_sample(n = n_samples)
    return(random_sample)
}


# Line plot

line_plot_by_time_by_sex <- function(dat, y_var, title_text, y_text) {
    g <- ggplot(dat, 
                aes(x = year, 
                    y = y_var, 
                    color = sex)) + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = set_colors) +
        coord_cartesian(xlim = c(1875, 2020)) +
        labs(
            title = title_text,
            x = "Year",
            y = y_text
        ) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.title = ggtext::element_markdown(),
            axis.text = element_text(size = 11)
        )
    
    g
}



empty_plotly_msg <- function(msg_text) {
    
        empty_plot <- plot_ly(
            x = 0.5,
            y = 0.5,
            type = "scatter",
            mode = "text",
            text = msg_text,
            textfont = list(size = 15),
            showlegend = FALSE
        ) %>%
            plotly::layout(
                xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, showline = FALSE, visible = FALSE),
                yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, showline = FALSE, visible = FALSE)
            ) |>
            plotly::config(displayModeBar = FALSE)
        
        # Display the empty plot
        empty_plot
  
}
