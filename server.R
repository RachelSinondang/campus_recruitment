#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot_composition <- renderPlotly({
        
        if(input$numvar == "degree_t") {by_gen <- p_status %>%
            filter(status == "Placed", specialisation == input$spec) %>%
            group_by(gender, degree_t) %>%
            summarise(total = n())
        
        per <- by_gen %>%
            group_by(degree_t) %>% 
            mutate(proportion = round(total/sum(total),2)*100)
        
        plot_composition <- per %>% 
            ggplot(aes(x = total, 
                       y = degree_t,
                       text = glue("Gender: {gender}
                         Proportion: {proportion}%"))) +
            geom_col(aes(fill = gender), position = "fill") +
            geom_vline(xintercept = 0.5, col = "white", lty = 2, lwd = 1.5) + #membuat vertical line
            labs(title = "Composition of Male and Female",
                 x = NULL,
                 y = NULL) +
            scale_fill_manual(values = c("black", "firebrick")) +
            scale_y_discrete(labels = wrap_format(30)) + # wrap tulisan agar tidak terlalu panjang
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            theme_algoritma + #harus dirun chunk atas untuk custom theme yg disediakan
            theme(legend.position = "none") #untuk hapus legend
        
        ggplotly(plot_composition, tooltip = "text")}
        else{by_gen <- p_status %>%
            filter(status == "Placed", specialisation == input$spec) %>%
            group_by(gender, hsc_s) %>%
            summarise(total = n())
        
        per <- by_gen %>%
            group_by(hsc_s) %>% 
            mutate(proportion = round(total/sum(total),2)*100)
        
        plot_composition <- per %>% 
            ggplot(aes(x = total, 
                       y = hsc_s,
                       text = glue("Gender: {gender}
                         Proportion: {proportion}%"))) +
            geom_col(aes(fill = gender), position = "fill") +
            geom_vline(xintercept = 0.5, col = "white", lty = 2, lwd = 1.5) + #membuat vertical line
            labs(title = "Composition of Male and Female",
                 x = NULL,
                 y = NULL) +
            scale_fill_manual(values = c("black", "firebrick")) +
            scale_y_discrete(labels = wrap_format(30)) + # wrap tulisan agar tidak terlalu panjang
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            theme_algoritma + #harus dirun chunk atas untuk custom theme yg disediakan
            theme(legend.position = "none") #untuk hapus legend
        
        ggplotly(plot_composition, tooltip = "text")}
        
    })
    
    output$prediction <- renderValueBox({
        
        gender <- input$gen
        ssc_p <- input$ssc_p
        ssc_b <- input$ssc_b
        hsc_p <- input$hsc_p
        hsc_b <- input$hsc_b
        hsc_s <- input$hsc_s
        degree_p <- input$degree_p
        degree_t <- input$degree_t
        workex <- input$workex
        etest_p <- input$etest_p
        specialisation <- input$specialisation
        mba_p <- input$mba_p
        
        status_test <- data.frame(gender,ssc_p,ssc_b,hsc_p,hsc_b,hsc_s,degree_p,degree_t,workex,etest_p,specialisation,mba_p)
        
        model_forest <- readRDS("model_forest.RDS")
        
        forest_class <- predict(model_forest, status_test, type = "raw")
        
        valueBox(value = forest_class,
                 subtitle = "Prediction",
                 color = "purple",
                 icon = icon("money-bill-alt"))
        
    })
    
    output$mean_hscs <- renderTable({
        
        if(input$var == "ssc_p"){mean_hsc <- data.frame(p_status %>%
                filter(status == "Placed",specialisation == input$spec1) %>%
                group_by(hsc_s) %>%
                summarise(rerata = mean(ssc_p)))}
        
        if(input$var == "hsc_p"){mean_hsc <- data.frame(p_status %>%
                filter(status == "Placed",specialisation == input$spec1) %>%
                group_by(hsc_s) %>%
                summarise(rerata = mean(hsc_p)))}
        
        if(input$var == "degree_p"){mean_hsc <- data.frame(p_status %>%
                filter(status == "Placed",specialisation == input$spec1) %>%
                group_by(hsc_s) %>%
                summarise(rerata = mean(degree_p)))}
        
        if(input$var == "etest_p"){mean_hsc <- data.frame(p_status %>%
                filter(status == "Placed",specialisation == input$spec1) %>%
                group_by(hsc_s) %>%
                summarise(rerata = mean(etest_p)))}
        
        if(input$var == "mba_p"){mean_hsc <- data.frame(p_status %>%
                filter(status == "Placed",specialisation == input$spec1) %>%
                group_by(hsc_s) %>%
                summarise(rerata = mean(mba_p)))}
        
        mean_hsc
        
    })
    
    output$mean_degree_t <- renderTable({
        
        if(input$var2 == "ssc_p"){mean_degree <- data.frame(p_status %>%
                                                            filter(status == "Placed",specialisation == input$spec1) %>%
                                                            group_by(degree_t) %>%
                                                            summarise(rerata = mean(ssc_p)))}
        
        if(input$var2 == "hsc_p"){mean_degree <- data.frame(p_status %>%
                                                            filter(status == "Placed",specialisation == input$spec1) %>%
                                                            group_by(degree_t) %>%
                                                            summarise(rerata = mean(hsc_p)))}
        
        if(input$var2 == "degree_p"){mean_degree <- data.frame(p_status %>%
                                                               filter(status == "Placed",specialisation == input$spec1) %>%
                                                               group_by(degree_t) %>%
                                                               summarise(rerata = mean(degree_p)))}
        
        if(input$var2 == "etest_p"){mean_degree <- data.frame(p_status %>%
                                                              filter(status == "Placed",specialisation == input$spec1) %>%
                                                              group_by(degree_t) %>%
                                                              summarise(rerata = mean(etest_p)))}
        
        if(input$var2 == "mba_p"){mean_degree <- data.frame(p_status %>%
                                                            filter(status == "Placed",specialisation == input$spec1) %>%
                                                            group_by(degree_t) %>%
                                                            summarise(rerata = mean(mba_p)))}
        
        mean_degree
        
    })
    
    

})
