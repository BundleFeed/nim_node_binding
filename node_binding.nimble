# Package

version       = "0.1.0"
author        = "Geoffrey Picron"
description   = "node binding generator"
license       = "Apache-2.0"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["node_binding"]


# Dependencies

requires "nim >= 1.6.10"
requires "cligen >= 1.5.42"