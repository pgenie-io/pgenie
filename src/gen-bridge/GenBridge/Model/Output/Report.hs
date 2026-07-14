-- | Re-export of the latest gen-contract rung's report rendering helpers. See
-- "GenContractV5.Output".
module GenBridge.Model.Output.Report
  ( Report (..),
    toErrorYamlText,
    toWarningYamlText,
  )
where

import GenContractV5.Output (Report (..), toErrorYamlText, toWarningYamlText)
