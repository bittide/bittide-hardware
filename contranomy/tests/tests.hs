import           Prelude
import           Test.Tasty                                (defaultMain,
                                                            testGroup)
import qualified Tests.Contranomy.Core.ALU
import           Tests.Contranomy.FirmwareIntegrationTests (generateTests)

main :: IO ()
main = do
  integ <- generateTests "../firmware/tests/target/riscv32imc-unknown-none-elf/release"

  let tests = testGroup "Contranomy Tests"
        [ Tests.Contranomy.Core.ALU.tests
        , integ
        ]

  defaultMain tests
