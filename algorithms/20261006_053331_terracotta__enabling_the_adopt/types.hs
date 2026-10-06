module Types where

import Data.Char (toLower)

-- Define data types for Terracotta elements
data TerracottaElement = TerracottaElement {
  terracottaElementType :: String,
  terracottaElementValue :: Double
} deriving (Show, Eq)

-- Define data type for Terracotta configuration
data TerracottaConfig = TerracottaConfig {
  terracottaConfigMemoryType :: String,
  terracottaConfigMemorySize :: Double
} deriving (Show, Eq)

-- Define data type for Terracotta interface
data TerracottaInterface = TerracottaInterface {
  terracottaInterfaceType :: String,
  terracottaInterfaceConfig :: TerracottaConfig
} deriving (Show, Eq)

-- Define data type for Terracotta memory
data TerracottaMemory = TerracottaMemory {
  terracottaMemoryElements :: [TerracottaElement]
} deriving (Show, Eq)

-- Define data type for Terracotta memory configuration
data TerracottaMemoryConfig = TerracottaMemoryConfig {
  terracottaMemoryType :: String,
  terracottaMemorySize :: Double
} deriving (Show, Eq)

-- Define data type for Terracotta memory interface
data TerracottaMemoryInterface = TerracottaMemoryInterface {
  terracottaMemoryInterfaceType :: String,
  terracottaMemoryInterfaceConfig :: TerracottaMemoryConfig
} deriving (Show, Eq)

-- Define data type for Terracotta memory system
data TerracottaMemorySystem = TerracottaMemorySystem {
  terracottaMemory :: TerracottaMemory
} deriving (Show, Eq)

-- Define data type for Terracotta memory system configuration
data TerracottaMemorySystemConfig = TerracottaMemorySystemConfig {
  terracottaMemoryType :: String,
  terracottaMemorySize :: Double
} deriving (Show, Eq)

-- Define data type for Terracotta memory system interface
data TerracottaMemorySystemInterface = TerracottaMemorySystemInterface {
  terracottaMemorySystemInterfaceType :: String,
  terracottaMemorySystemInterfaceConfig :: TerracottaMemorySystemConfig
} deriving (Show, Eq)

-- Define data type for Terracotta memory system
data TerracottaMemorySystem = TerracottaMemorySystem {
  terracottaMemorySystem :: TerracottaMemorySystemInterface
} deriving (Show, Eq)

-- Define data type for Terracotta memory system configuration
data TerracottaMemorySystemConfig = TerracottaMemorySystemConfig {
  terracottaMemorySystemType :: String,
  terracottaMemorySystemInterfaceConfig :: TerracottaMemorySystemInterfaceConfig
} deriving (Show, Eq)

-- Define data type for Terracotta memory system interface configuration
data TerracottaMemorySystemInterfaceConfig = TerracottaMemorySystemInterfaceConfig {
  terracottaMemorySystemInterfaceType :: String,
  terracottaMemorySystemInterfaceConfig :: TerracottaMemorySystemInterfaceConfig
} deriving (Show, Eq)
