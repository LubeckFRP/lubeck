

import Servant
import Servant.Utils.StaticFiles (serveDirectory)

type GhcJsTestServer = Raw

server :: Server GhcJsTestServer
-- TODO how to get this from Stac
server = serveDirectory ".stack-work/install/x86_64-osx/nightly-2015-12-14/ghcjs-0.2.0_ghc-7.10.2/bin/ghcjs-test.jsexe"

main = print "Server!"
