PKGNAME := lexer-listings
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))

default : setup

-include local.mk

setup :
	raco setup $(PKGNAME)

install :
	raco pkg install --name $(PKGNAME)

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

check-pkg-deps :
	raco setup --check-pkg-deps $(PKGNAME)

gh-homepage :
	( cd gh-pages && git clean -d -f && git rm --ignore-unmatch -rf . )
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest gh-pages --dest-name gh-pages/index.html scribblings/lexer-listings.scrbl
	( cd gh-pages && git add . && git status )

gh-upload :
	( cd gh-pages && git commit -m "update $$(date)" && git push --set-upstream github gh-pages:gh-pages )
