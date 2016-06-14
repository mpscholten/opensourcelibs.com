.PHONY: all clean

FILES=landing-page.hs result.hs

all: ${FILES}

clean:
	rm ${FILES}

landing-page.hs: landing-page.html
	blaze-from-html $< > $@

result.hs: result.html
	blaze-from-html $< > $@
