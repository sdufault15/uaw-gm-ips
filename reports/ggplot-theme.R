mytheme <- theme(
		axis.text = element_text(size = 7),
		axis.title = element_text(size = 8),
		legend.box.background = element_rect(colour = "black", size = 0.2),
		legend.title = element_blank(),
		legend.text = element_text(size = 7),
		legend.justification = "left",
		# Legend position is relative to plot window:
		#   c(0,0) is bottom left, c(1,1) is top right
		legend.position = c(0.305, 0.935),
		legend.margin = margin(0, 5, 5, 5, "pt"),
		legend.box.margin = margin(1, 1, 1, 1, "pt"),
		legend.key.size = unit(6, "pt"),
		panel.border = element_rect(colour = "black", size = 0.75)
	)

# Tikz/lualatex options
options(
	tikzDefaultEngine = 'luatex',
	tikzLualatexPackages = c(
		"\\usepackage{amssymb}",
		"\\usepackage[no-math]{fontspec}\n",
		paste0(
			"\\setmainfont{Arial}",
			ifelse(Sys.info()["sysname"] == "Darwin",
						 "\n",
						 "[Extension = .ttf,
			UprightFont = *,
			BoldFont = *bd,
			talicFont = *i,
			BoldItalicFont = *bi]\n")),
		"\\usepackage[italic]{mathastext}",
		"\\usepackage{tikz}\n",
		"\\usepackage[active,tightpage,psfixbb]{preview}\n",
		"\\PreviewEnvironment{pgfpicture}\n",
		"\\setlength\\PreviewBorder{0pt}\n"
	)
)