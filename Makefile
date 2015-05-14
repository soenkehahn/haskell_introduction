all: slides.html
	ack-grep todo slides.md && false

slides.html: template.html slides.md mkSlides.hs
	# doctoc slides.md
	./mkSlides.hs
