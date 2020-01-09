---
title: FLPKG CHECKLIST
author: Iago Mosqueira, EC JRC G03
rights:  Creative Commons Share Alike 4.0
---

# Package structure

- README.md
- NEWS.md
- .travis.yml
- .Rbuildignore
- Makefile
- CITATION

# Vignette

# /data

# WORKFLOWS

## Create new package (or make existing one follow this style)

- EDIT DESCRIPTION
- ADD NAMESPACE
- ADD R/*.R files

- EDIT README.md
- EDIT inst/CITATION

## Add new code / fix a bug

- CHANGE code in R/*.R
- ADD entry in NEWS.md
- RUN 'make test' and check test passes
- git add -A .
- git commit -m 'Message used in NEWS.md'
- EDIT Version and Date in DESCRIPTION
- RUN 'make README.md'
- git add .
- git commit -m 'Version `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`'
- git push

## Add or alter roxygen documentation

- CHANGE entry in R/*.R file(s)
- Run 'make roxygen'
- Run 'make gh-pages'
- git add -A .
- git commit -m 'Updated documentation for ...'
- git push


# HOW TO convert pkgs to use gh-pages using staticdocs

1. ADD README.md following template
2. COPY Makefile
3. CREATE an empty gh-pages branch

	git checkout --orphan gh-pages
	git rm -rf .

4. ADD a README.md to the branch

	touch README.md
	git add README.md

5. COMMIT and PUSH branch
	
	git commit -a -m "First gh-pages commit"
	git push origin gh-pages
