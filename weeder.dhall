{ roots =
  [ "^Main\\.main\$"
  , "^Restore\\.main\$"
  , "^Latency\\.main\$"
  , "^Paths_.*"
  , "^Cardano.Startup.Windows\\..*\$"
  , "^Cardano\\.CLI\\.(getDataDir|requireFilePath|stateDirOption)\$"
  , "^Cardano\\.CLI\\.cmdByronWalletCreate"
  , "^Cardano\\.Wallet\\.Api\\.Client\\.byron(Address|Transaction|Wallet)Client\$"
  , "^Cardano\\.Wallet\\.Api\\.Link\\.(mintBurnAssets|patchSharedWallet|postExternalTransaction)\$"
  , "^Cardano\\.Wallet\\.DB\\.Sqlite\\.Types\\.sqlSettings'\$"
  , "^Cardano\\.Wallet\\.Primitive\\.Types.\\stabilityWindow(Byron|Shelley)\$"
  , "^Cardano\\.Wallet\\.Unsafe\\."
  , "^Cardano\\.Wallet\\.Version\\.TH\\.gitRevFromGit\$"
  , "^Data\\.Set\\.Strict\\.NonEmptySet\\."
  , "^UnliftIO\\.Compat\\.mkRetryHandler\$"
  , "^Spec\\.main\$"
  , "^Test\\..*\\.spec\$"
  , "^Test\\.Utils\\.Paths\\.getTestData"
  , "^Cardano\\.Wallet\\.Api\\.Malformed\\."
  , "^Cardano\\.Wallet\\.DB\\.StateMachine\\.showLabelledExamples\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Launch\\.Cluster\\.genMonetaryPolicyScript\$"
  , "^Test\\.Integration\\.Faucet\\."
  , "^Test\\.Integration\\.Framework\\.(TestData|DSL)\\."
  , "^Cardano\\.Wallet\\.Shelley\\.nullTracers\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Compatibility\\.emptyGenesis\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Compatibility\\.interval0\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Compatibility\\.interval1\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Compatibility\\.isInternalError\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Compatibility\\.toCardanoHash\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Launch\\.Cluster\\.singleNodeParams\$"
  , "^Cardano\\.Wallet\\.Shelley\\.Launch\\.Cluster\\.tokenMetadataServerFromEnv\$"
  ]
, type-class-roots = True
}
