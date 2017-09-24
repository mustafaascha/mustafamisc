#75 through 106
five_names <- function(bc_type){
  five_meetings <- c("Pren", "Adm", "Del", "Inpt", "Post")
  paste(five_meetings, bc_type, sep = "_")
}


#credit here: https://stackoverflow.com/questions/30460410/plot-glmnet-increase-size-of-variable-labels
myPlot <- glmnet::plotCoef

# replace relevant part
body(myPlot)[[14]] <- quote(if (label) {
  nnz = length(which)
  xpos = max(index)
  pos = 4
  if (xvar == "lambda") {
    xpos = min(index)
    pos = 2
  }
  xpos = rep(xpos, nnz)
  ypos = beta[, ncol(beta)]
  text(xpos, ypos, paste(which), pos = pos, ...) # only changed this with ...
})

# copy first level of plot and replace plotCoef  with myPlot
newplotter <- plot.glmnet

body(newplotter)[[3]] <- quote(myPlot(x$beta, lambda = x$lambda, 
                                      df = x$df, dev = x$dev.ratio, 
                                      label = label, xvar = xvar, ...))

tidy_glm <- function(mdl, keep_clohi = FALSE){
  mdl_df <- cbind(broom::tidy(mdl), confint(mdl))  
  names(mdl_df) <- 
    c("term", "estimate", "std.error", "statistic", 
      "p.value", "conf.low", "conf.high")
  rownames(mdl_df) <- NULL
  mdl_df$estimate <- exp(as.numeric(mdl_df$estimate))	
  mdl_df$conf.high <- exp(as.numeric(mdl_df$conf.high))  
  mdl_df$conf.low <- exp(as.numeric(mdl_df$conf.low))
  mdl_df <- mdl_df %>% 	
    select(term, odds_ratio = estimate, conf.low, conf.high, p.value)	
  mdl_df$Confidence_Interval <- 	
    with(mdl_df, 	
         paste(sprintf("%.2f", conf.low), 	
               sprintf("%.2f", conf.high), 	
               sep = " - "))
  if(!keep_clohi){
    mdl_df <- 	
      mdl_df %>% 	
      select(-conf.low, -conf.high) %>% 	
      select(term, odds_ratio, Confidence_Interval, p.value)
  }
  mdl_df$odds_ratio <- 	
    sprintf("%.4f", mdl_df$odds_ratio)	
  mdl_df$p.value <- sprintf("%.4f", mdl_df$p.value)	
  mdl_df
}



#This function accepts a visit name (Pren, Adm, etc) and returns TRUE 
# if there is a row that is checked. 
meeting_check <- function(visit){
  vars <- 
    c("_OCP",       "_MiniPill",       "_Patch",       "_Vaginal_Ring",
      "_Depo",      "_Nexplanon",      "_Mirena",      "_Paragard",
      "_IUD_Gen",   "_Sterilization",  "_Condoms",     "_Abstinence",
      "_Declines",  "_Other")   
  df <- elect_st %>% select_(paste(visit, vars, sep = ""))
  apply(df, 1, function(x) any(x == 1))
}

show_names <- function(string, df){
  sprintf("%s is located at position %d", 
          names(df)[grep(string, names(df))], 
          as.numeric(grep(string, names(df))))
}

test_for_bridge <- function(ppcp) (elect_st[,ppcp] == 1) & (elect_st$Plan_Tier_1 == 1)

make_bridge_factor <- function(bridge_var, bridge_name){
  factor(ifelse(bridge_var, bridge_name, paste("No", bridge_name)))
}

fix_names <- function(stuff) {
  gsub(pattern = "Delivery_Type|Race|Yes|\\ \\(forceps,\\ vacuum\\)|__|Adequate_", 
       replacement = "", stuff)
}

reference_no <- function(fctr, is_char = TRUE){
  if(is_char) { fctr <- factor(fctr) }
  fct_levels <- levels(fctr)
  which_is_no <- grep("Not", fct_levels)
  relevel(fctr, ref = fct_levels[which_is_no])
}

grep_raw <- function(pattern, x){
  x[grep(pattern = pattern, x = x)]
}

make_or_epi <- function(predictor, df) {
  Epi::twoby2(predictor, as.character(df$AP1))
}

switch_levels <- function(predictor){
  prvs_levels <- levels(predictor)
  factor(predictor, levels = c(prvs_levels[2], prvs_levels[1]))
}

tta_diff <- function(df, comparator) {
  comparator <- enquo(comparator)
  comparator_values <- df %>% select(!!comparator) %>% unlist
  comp_levels <- unique(as.character(comparator_values))
  first_name <- paste0("Median_", comp_levels[1])
  second_name <- paste0("Median_", comp_levels[2])
  wilcox_test_results <-
    wilcox.test(df$Time_to_Achieve ~ comparator_values, 
                simulate.p.values = TRUE, conf.int = TRUE)
  summarise(df,
            !!first_name := median(.data$Time_to_Achieve[
              which(comparator_values == comp_levels[1])
              ], 
              na.rm = TRUE),
            !!second_name := median(.data$Time_to_Achieve[
              which(comparator_values == comp_levels[2])
              ], 
              na.rm = TRUE),
            Difference = wilcox_test_results$estimate, 
            Lower_Conf = wilcox_test_results$conf.int[1],
            Upper_Conf = wilcox_test_results$conf.int[2],
            P_Value = wilcox_test_results$p.value
  )
}


show_surv_diff <- function(df, comparator) {
  comparator <- enquo(comparator)
  if(grepl("Survival", expr_label(comparator))) {
    return(NA)
  }
  surv_diff <- survdiff(Survival ~ eval_tidy(comparator, data = df), data = df)
  names(surv_diff$n) <- gsub("eval_tidy\\(comparator\\,\\ data\\ =\\ df\\)\\=", 
                             "", unlist(dimnames(surv_diff$n)))
  data.frame(Bridge = names(surv_diff$n), 
             N = unname(surv_diff$n), 
             Observed = surv_diff$obs, 
             Expected = surv_diff$exp, 
             P = pchisq(surv_diff$chisq, 
                        length(surv_diff$n)-1, 
                        lower.tail = FALSE), 
             stringsAsFactors = FALSE)
}
#f_rhs(glm_model_formula)
# I just copied and pasted...really disappointed, I wanted to use
# the right hand side of the formula. I've thought about manipulating
# formulas more than once, I definitely need to think about it again :( 

new_glm_by_outcome <- function(df, comparator) {
  comparator <- enquo(comparator)
  model_formula <- 
    eval_tidy(comparator, data = df) ~ 
    Bridging_OCP + Bridging_MiniPill + Bridging_Depo + Insurance + 
    Age___Delivery + Adequate_PNC + Parity + GestAge_Del + Delivery_Type + 
    Race + Married + Education_Level
  glm(model_formula, data = df)
}

