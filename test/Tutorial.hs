{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Control.Exception.Lifted (bracket_)
import Control.Monad ((<=<))
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, begin, close, connectPostgreSQL, rollback)
import Database.Postgres.Temp (toConnectionString, with)
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main (defaultMain)
import qualified Hedgehog.Range as Range
import Lib

prop_tables :: Pool Connection -> Property
prop_tables pool =
  property $ do
    withResource pool $ \conn -> do
      abort conn $
        evalIO $
          createTables conn

data CreateUser (v :: Type -> Type) = CreateUser Text Text
  deriving (Eq, Ord, Show, Generic, FunctorB, TraversableB)

newtype DeleteUser (v :: Type -> Type) = DeleteUser (Var User v)
  deriving (Eq, Ord, Show, Generic, FunctorB, TraversableB)

data CreatePost (v :: Type -> Type) = CreatePost (Var User v) Text Text
  deriving (Eq, Ord, Show, Generic, FunctorB, TraversableB)

genCreateUser :: MonadGen gen => gen (CreateUser v)
genCreateUser = do
  name <- Gen.element ["stephanie", "lennart", "simon"]
  pure $
    CreateUser name (name <> "@haskell.land")

genDeleteUser :: MonadGen gen => [Var User v] -> gen (DeleteUser v)
genDeleteUser users =
  DeleteUser
    <$> Gen.element users

genCreatePost :: MonadGen gen => [Var User v] -> gen (CreatePost v)
genCreatePost users =
  CreatePost
    <$> Gen.element users
    <*> Gen.element ["C", "C++", "Haskell", "Rust", "JavaScript"]
    <*> Gen.element ["fast", "slow", "best", "worst"]

data Model (v :: Type -> Type) = Model
  { modelUsers :: [Var User v],
    modelPosts :: [(Var User v, Var Post v)]
  }
  deriving (Eq, Ord, Show)

modelAddUser :: Var User v -> Model v -> Model v
modelAddUser user x =
  x {modelUsers = modelUsers x <> [user]}

modelRemoveUser :: Eq1 v => Var User v -> Model v -> Model v
modelRemoveUser user x =
  x {modelUsers = filter (/= user) (modelUsers x)}

modelAddPost :: Var User v -> Var Post v -> Model v -> Model v
modelAddPost user post x =
  x {modelPosts = modelPosts x <> [(user, post)]}

modelUserHasPosts :: Eq1 v => Var User v -> Model v -> Bool
modelUserHasPosts user x =
  any ((user ==) . fst) (modelPosts x)

execCreateUser ::
  ( MonadIO m,
    MonadTest m
  ) =>
  Connection ->
  Text ->
  Text ->
  m User
execCreateUser conn name email = do
  let new = NewUser name email
  uid <- evalIO $ createUser conn new
  mgot <- evalIO $ readUser conn uid
  got <- eval $ fromJust mgot

  let want = packUser uid (userCreatedAt got) new
  want === got

  label "CreateUser"

  return want

cCreateUser :: (MonadTest m, MonadIO m, MonadGen gen) => Connection -> Command gen m Model
cCreateUser conn =
  Command
    gen
    exec
    [ Update $ \model _input output -> modelAddUser output model,
      Ensure $ \(Model users _) (Model users' _) (CreateUser name email) output -> do
        assert $ output `notElem` map concrete users
        assert $ output `elem` map concrete users'
        userName output === name
        userEmail output === email
    ]
  where
    gen _ = Just genCreateUser
    exec (CreateUser name email) = execCreateUser conn name email

execDeleteUser ::
  ( MonadIO m,
    MonadTest m
  ) =>
  Connection ->
  User ->
  m ()
execDeleteUser conn user = do
  evalIO $ deleteUser conn (userId user)
  label "DeleteUser"

cDeleteUser :: (MonadTest m, MonadIO m, MonadGen gen) => Connection -> Command gen m Model
cDeleteUser conn =
  Command
    gen
    exec
    [ Require $ \model (DeleteUser user) -> not (modelUserHasPosts user model),
      Update $ \model (DeleteUser user) _output -> modelRemoveUser user model
    ]
  where
    gen (Model users _) = if null users then Nothing else Just (genDeleteUser users)
    exec (DeleteUser user) = execDeleteUser conn (concrete user)

execCreatePost ::
  ( MonadIO m,
    MonadTest m
  ) =>
  Connection ->
  User ->
  Text ->
  Text ->
  m Post
execCreatePost conn user title body = do
  let new = NewPost (userId user) title body
  pid <- evalIO $ createPost conn new
  mgot <- evalIO $ readPost conn pid
  got <- eval $ fromJust mgot

  let want = packPost pid (postCreatedAt got) new
  want === got

  label "CreatePost"

  return want

cCreatePost :: (MonadTest m, MonadIO m, MonadGen gen) => Connection -> Command gen m Model
cCreatePost conn =
  Command
    gen
    exec
    [ Update $ \model (CreatePost user _ _) output -> modelAddPost user output model
    ]
  where
    gen (Model users _) = if null users then Nothing else Just (genCreatePost users)
    exec (CreatePost user title body) = execCreatePost conn (concrete user) title body

prop_commands :: Pool Connection -> Property
prop_commands pool =
  property $ do
    withResource pool $ \conn -> do
      let commands = ($ conn) <$> [cCreateUser, cDeleteUser, cCreatePost]
          initialState = Model [] []

      actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands

      abort conn $ do
        evalIO $ createTables conn
        executeSequential initialState actions

abort :: MonadBaseControl IO m => Connection -> m a -> m a
abort conn =
  bracket_
    (liftBase (begin conn))
    (liftBase (rollback conn))

withPool :: (Pool Connection -> IO a) -> IO a
withPool io =
  either throwIO pure <=< with $ \db -> do
    let connect = connectPostgreSQL (toConnectionString db)
    pool <- createPool connect close 2 60 10
    io pool

-- don't forget to add prop_commands to your tests function
tests :: IO Bool
tests =
  withPool $ \pool ->
    checkParallel $
      Group
        "Tutorial"
        [ ("prop_tables", prop_tables pool),
          ("prop_commands", prop_commands pool)
        ]

main :: IO ()
main = defaultMain [tests]
