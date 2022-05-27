# find the aggregates for 1_ Total sales 2_ total profit, 3) total quantity and 4) total Discount

find_totals<-function(grp.var=dplyr::quo(Sales),df=df.filter){
# options are:"t.sales", "t.profit",t.quantity, t.discount
  print(dplyr::quo_name(grp.var))
  
  t<-df %>% dplyr::select(!!grp.var)%>% dplyr::summarise(total=sum(.,na.rm = TRUE)) %>% dplyr::pull()
  t
  #huxtable::fmt_pretty(t,big.mark =",",scientific = FALSE  )
}

# Find the top 20 of customers or sold products

top_n_var<-function(grp.var=dplyr::quo(`Customer Name`),df=df.filter){
  
  df %>% dplyr::select(!!grp.var,Country,State,Region,City,`Product Name`,Sales) %>%
    dplyr::group_by(!!grp.var,State,Region,City) %>%
    dplyr::summarise(Total=sum(Sales)) %>%
    dplyr::arrange(desc(Total)) %>%
    dplyr::ungroup()%>%
    dplyr::slice_max(Total,n=20)
}