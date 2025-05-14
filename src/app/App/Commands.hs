module App.Commands where

import App.Commands.Compile (Compile)
import App.Commands.GenerateSignatures (GenerateSignatures)
import App.Frameworks.CommandCliApp
import Base.Prelude

compile :: Command
compile = modelCommand @Compile Proxy

generateSignatures :: Command
generateSignatures = modelCommand @GenerateSignatures Proxy
