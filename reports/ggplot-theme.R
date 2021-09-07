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
		panel.border = element_rect(colour = "black", size = 0.75),
		panel.grid = element_blank(),
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
# Run LuaLaTex
latex <- function(
	pattern = ".*\\.tex",
	directory = here::here("reports"),
	magick = T,
	break_after = 30,
	png_density = 400,
	compiler = "latex",
	compilation.options = NULL) {

	dir.create(directory, F, T)
	if (magick) {dir.create(paste0(gsub("/$", "", directory), "/images"), F, T)}

	if (!grepl("\\.tex$", pattern, ignore.case = T)) {
		pattern <- paste0(pattern, "\\.tex")
	}

	if (grepl("Windows", Sys.info()['sysname'], ignore.case = T)) {
		# Windows
		library(stringr)
		directory <- gsub("/", "\\", directory)
		invisible(sapply(
			grep(pattern, list.files(directory), value = T), function(
				file.tex) {system((paste(
					paste0("cd \"", directory, "\"\n"),
					paste0("for %i in (", file.tex, ") do"),
					paste0(compiler, " ",
								 paste(compilation.options, collapse = " "),
								 " \"$i\""),
					"; del \"%~ni.log\"; del \"%~ni.aux\";",
					if (magick) {
						paste0("magick -density ", png_density, " \"%~ni.pdf\" \".\\images\\%~ni.png\"")
					} else {NULL},
					sep = " ")), timeout = break_after)}
		))
	}
	if (grepl("Darwin", Sys.info()['sysname'], ignore.case = T)) {
		# OS X
		invisible(sapply(
			grep(pattern, list.files(directory), value = T), function(
				file.tex) {system((paste(
					paste0("cd \"", directory, "\";"),
					paste0("for i in \"", file.tex,"\"; do {"),
					paste0(compiler, " ", paste(compilation.options, collapse = " "),
								 "\"$i\";"),
					"rm \"${i%.tex}.aux\"; rm \"${i%.tex}.log\"; rm \"${i%.tex}.out\";",
					if (magick) {
						paste0("magick -density ", png_density, " \"${i%.tex}.pdf\" \"./images/${i%.tex}.png\";")
					} else {NULL},
					"} done", sep = "\n")), timeout = break_after)}
		))
	}
}

lualatex <- function(
	pattern = ".*\\.tex",
	directory = here::here("reports"),
	magick = T,
	break_after = 30,
	png_density = 400,
	compilation.options = NULL) {
	return(latex(pattern, directory, magick, break_after, png_density, compiler = "lualatex", compilation.options))
}