import Data.Char (toLower)

-- Define a function to convert a string to lowercase
toLowerCase :: String -> String
toLowerCase s = toLower s

-- Define a function to create a TerracottaMemorySystemInterfaceConfig object
createTerracottaMemorySystemInterfaceConfig :: TerracottaMemorySystemInterfaceConfig
createTerracottaMemorySystemInterfaceConfig = TerracottaMemorySystemInterfaceConfig "default" "default"
