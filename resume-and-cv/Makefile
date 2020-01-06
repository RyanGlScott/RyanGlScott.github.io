RESUME=resume
CV=cv

LATEXMK_OPTS= -pdf
LATEXMK=latexmk ${LATEXMK_OPTS}

all: resume cv

resume:
	${LATEXMK} ${RESUME}.tex

cv:
	${LATEXMK} ${CV}.tex

clean:
	latexmk -C; rm -f *.out *.aux *.bbl *.log *.blg *.fls *.fdb_latexmk \
		${RESUME}.pdf ${CV}.pdf
