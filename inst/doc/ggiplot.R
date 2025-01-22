## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggfixest)

## ----est_did------------------------------------------------------------------
data(base_did)

est_did = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)

## ----est_did_plot_defaults----------------------------------------------------
iplot(est_did)
ggiplot(est_did)

## ----est_did_ebar-------------------------------------------------------------
ggiplot(est_did, geom = 'errorbar')

## ----est_did_pt_join----------------------------------------------------------
iplot(est_did, pt.join = TRUE)
ggiplot(est_did, pt.join = TRUE, geom_style = 'errorbar')

## ----est_did_ribbon-----------------------------------------------------------
# iplot(est_did, pt.join = TRUE, ci.lty = 0, ci.width = 0, ci.fill = TRUE)
iplot(
	est_did, pt.join = TRUE, ci.lty = 0, ci.width = 0, ci.fill = TRUE,
	ci.fill.par = list(col = 'black', alpha = 0.3)
	)
ggiplot(est_did, geom_style = 'ribbon')
ggiplot(est_did, geom_style = 'ribbon', pt.pch = NA, col = 'orange')

## ----est_did_ci_multi---------------------------------------------------------
ggiplot(est_did, ci_level = c(.8, .95))

## ----est_did_aggr_eff---------------------------------------------------------
ggiplot(
	est_did, ci_level = c(.8, .95),
	aggr_eff = "post", aggr_eff.par = list(col = "orange") # default col is grey
	)

## ----base_stagg---------------------------------------------------------------
data(base_stagg)

est_twfe = feols(
	y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
	data = base_stagg
	)

est_sa20 = feols(
	y ~ x1 + sunab(year_treated, year) | id + year,
	data = base_stagg
	)

## ----stagg_iplot--------------------------------------------------------------
iplot(
	list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
	main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
	)
legend(
	'topleft', col = c(1, 2), pch = c(20, 17),
	legend = c('TWFE', 'Sun & Abraham (2020)')
	)

## ----stagg_ggiplot------------------------------------------------------------
ggiplot(
	list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
	main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
	)

## ----stagg_ggiplot_noname-----------------------------------------------------
ggiplot(
	list(est_twfe, est_sa20),
	main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
	)

## ----stagg_ggiplot_facet------------------------------------------------------
ggiplot(
	list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
	main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
	multi_style = 'facet'
	)

## ----base_stagg_grp-----------------------------------------------------------
base_stagg_grp = base_stagg
base_stagg_grp$grp = ifelse(base_stagg_grp$id %% 2 == 0, 'Evens', 'Odds')

## ----stagg_grp----------------------------------------------------------------
est_twfe_grp = feols(
	y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
	data = base_stagg_grp, split = ~ grp
	)
est_sa20_grp = feols(
	y ~ x1 + sunab(year_treated, year) | id + year,
	base_stagg_grp, split = ~ grp
	)

## ----stagg_grp_single---------------------------------------------------------
iplot(est_twfe_grp, ref.line = -1, main = 'Staggered treatment: TWFE')
legend('topleft', col = c(1, 2), pch = c(20, 17), legend = c('Evens', 'Odds'))
ggiplot(est_twfe_grp, ref.line = -1, main = 'Staggered treatment: TWFE')

## ----stagg_grp_multi_iplot, error = TRUE--------------------------------------
try({
iplot(
	list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
	ref.line = -1, main = 'Staggered treatment: Split mutli-sample'
	)
})

## ----stagg_grp_multi_ggiplot--------------------------------------------------
ggiplot(
	list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
	ref.line = -1, main = 'Staggered treatment: Split mutli-sample'
	)

## ----stagg_grp_multi_ggiplot_fancy--------------------------------------------
ggiplot(
    list("TWFE" = est_twfe_grp, "Sun & Abraham (2020)" = est_sa20_grp),
    ref.line = -1,
    main = "Staggered treatment: Split mutli-sample",
    xlab = "Time to treatment",
    multi_style = "facet",
    geom_style = "ribbon",
    facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
    theme = theme_minimal() +
        theme(
            text = element_text(family = "HersheySans"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none"
        )
)


## ----theme_update-------------------------------------------------------------
last_plot() +
	labs(caption = 'Note: Super fancy plot brought to you by ggiplot')
last_plot() + 
	theme_grey() + 
	theme(legend.position = 'none') +
	scale_fill_brewer(palette = 'Set1', aesthetics = c('colour', 'fill'))

## ----dict---------------------------------------------------------------------
base_did$letter = letters[base_did$period]
est_letters = feols(y ~ x1 + i(letter, treat, 'e') | id+letter, base_did)

# Dictionary for capitalising the letters
dict = LETTERS[1:10]; names(dict) = letters[1:10]

ggiplot(est_letters) # No dictionary

## ----dict_direct--------------------------------------------------------------
ggiplot(est_letters, dict = dict)

## ----dict_global--------------------------------------------------------------
setFixest_dict(dict)
ggiplot(est_letters)

setFixest_dict() # reset

