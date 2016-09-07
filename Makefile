
# Just some shortcuts to the Stack build

GHC	  :=	stack-ghc.yaml
GHCJS	:=	stack-ghc-js.yaml

ghc:
	stack --stack-yaml=$(GHC)   build
ghcjs:
	stack --stack-yaml=$(GHCJS) build
test-run:
	stack --stack-yaml=$(GHC)   test
test-generate:
	stack --stack-yaml=$(GHC)   test --test-arguments --generate
test-report:
	stack --stack-yaml=$(GHC)   test --test-arguments --report

ghc-prof:
	stack --stack-yaml=$(GHC) build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"


		# && stack exec bench-ad-dashboard-calc -- config.json.local +RTS -p
