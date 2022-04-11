import           Prelude
import           Test.Tasty                                (defaultMain,
                                                            testGroup)
import qualified Tests.Contranomy.Core.ALU
import           Tests.Contranomy.FirmwareIntegrationTests (generateTests)

main :: IO ()
main = do
  integ <- generateTests "firmware-integration-tests/"

  let tests = testGroup "Contranomy Tests"
        [ Tests.Contranomy.Core.ALU.tests
        , integ
        ]

  defaultMain tests
