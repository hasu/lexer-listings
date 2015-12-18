PKGNAME := cxx-lexer
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))

default : setup

-include local.mk

setup :
	raco setup $(PKGNAME)
