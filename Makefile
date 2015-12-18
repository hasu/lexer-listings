PKGNAME := cxx-lexer
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))

default : setup

-include local.mk

setup :
	raco setup $(PKGNAME)

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r
