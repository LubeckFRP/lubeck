

import Servant
import Servant.Utils.StaticFiles (serveDirectory)
import qualified Network.Wai.Handler.Warp

type GhcJsTestServer = Raw

server :: Server GhcJsTestServer
-- TODO how to get this from Stack
server = serveDirectory "/Users/Hoglund/Code/hs/ghcjs-test/.stack-work/install/x86_64-osx/nightly-2015-12-14/ghcjs-0.2.0_ghc-7.10.2/bin/ghcjs-test.jsexe"

main2 = serve (Proxy::Proxy GhcJsTestServer) server

main = print "Server!" >> Network.Wai.Handler.Warp.run 8080 main2
