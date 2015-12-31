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
