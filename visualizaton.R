
plot <- function (data,....,.value,fct_rorder=TRUE,fct_order_rev=FALSE,include_lbl=TRUE,color = palette_light()[[1]]) {
    group_vars_expr <- quos(...)
    if (length(group_vars_expr) == 0) 
        group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
    value_expr <- enquo(.value)
    value_name <- quo_name(value_expr)
    
}

#### Age Group Plot ----
p <- ageClassGroup %>% 
    mutate(Status = case_when(
        XLABEL == 1 ~ "LAPSED",
        XLABEL == 0 ~ "IN-FORCE",
    ))  %>% 
    arrange(desc(n))  %>% 
    ggplot(aes(x = reorder(AgeClass,n), y = n,fill = Status)) + ggtitle("Percetage of Lapse Rate Across Age Groups") + geom_bar(stat = "identity", position = "stack") + xlab("Age Group") + ylab("Percentage of Lapse") + geom_text(aes(x = AgeClass, y = n -15000,label=paste(round(LapsePct*100,digits = 0),'%')),position = position_dodge(0.5),size=3) + coord_flip()
p <- ggplotly(p)
p


#### Survival analysis

glimpse(uniquePolicy)

