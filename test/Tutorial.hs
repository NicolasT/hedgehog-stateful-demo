{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial where

import           Control.Exception (Exception, throwIO, try, catch)
import           Control.Exception.Lifted (bracket_)
import           Control.Monad (when)
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Class (MonadState(..), modify, gets)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.State (execStateT)

import           Data.Foldable (for_)
import           Data.Function (on)
import qualified Data.List as List
import           Data.Maybe (listToMaybe, fromJust)
import           Data.Pool (Pool, createPool, withResource)
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..))

import           Database.PostgreSQL.Simple (Connection, Only(..))
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.PostgreSQL.Simple (execute, execute_)
import           Database.PostgreSQL.Simple (query, close)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.Postgres.Temp (with, toConnectionString)

import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Hedgehog hiding (Command)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Text.Printf (printf)

data DbError = DbError Text
  deriving (Eq, Ord, Show)
instance Exception DbError where

newtype UserId =
  UserId {
      unUserId :: Int
    } deriving (Eq, Ord, Show)

data NewUser =
  NewUser {
      newuserName :: Text
    , newuserEmail :: Text
    } deriving (Eq, Ord, Show)

data User =
  User {
      userId :: UserId
    , userName :: Text
    , userEmail :: Text
    , userCreatedAt :: UTCTime
    } deriving (Eq, Ord, Show)

packUser :: UserId -> UTCTime -> NewUser -> User
packUser uid ctime x =
  User uid
    (newuserName x)
    (newuserEmail x)
    ctime

newtype PostId =
  PostId {
      unPostId :: Int
    } deriving (Eq, Ord, Show)

data NewPost =
  NewPost {
      newpostUserId :: UserId
    , newpostTitle :: Text
    , newpostBody :: Text
    } deriving (Eq, Ord, Show)

data Post =
  Post {
      postId :: PostId
    , postUserId :: UserId
    , postTitle :: Text
    , postBody :: Text
    , postCreatedAt :: UTCTime
    } deriving (Eq, Ord, Show)

packPost :: PostId -> UTCTime -> NewPost -> Post
packPost pid ctime x =
  Post pid
    (newpostUserId x)
    (newpostTitle x)
    (newpostBody x)
    ctime

createTables :: Connection -> IO ()
createTables conn = do
  _ <- execute_ conn [sql|
    CREATE TABLE users (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT NOT NULL,
      created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
    );
    CREATE TABLE posts (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id),
      title TEXT NOT NULL,
      body TEXT NOT NULL,
      created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
    );
    |]
  pure ()

createUser :: Connection -> NewUser -> IO UserId
createUser conn user  = do
  rows <- query conn [sql|
    INSERT INTO users (name, email)
    VALUES (?, ?)
    RETURNING id
    |] (newuserName user, newuserEmail user)
  case rows of
    [] ->
      throwIO $ DbError "failed to create user"
    Only uid : _ ->
      pure (UserId uid)

deleteUser :: Connection -> UserId -> IO ()
deleteUser conn uid  = do
  n <- execute conn [sql|
    DELETE FROM users
    WHERE id = ?
    |] (Only (unUserId uid))
  if n == 0 then
    throwIO $ DbError "user did not exist"
  else
    pure ()

readUser :: Connection -> UserId -> IO (Maybe User)
readUser conn uid = do
  rows <- query conn [sql|
    SELECT name, email, created_at
    FROM users
    WHERE id = ?
    |] (Only (unUserId uid))
  case rows of
    [] ->
      pure Nothing
    (name, email, ctime) : _ ->
      pure (Just (User uid name email ctime))

createPost :: Connection -> NewPost -> IO PostId
createPost conn post  = do
  rows <- query conn [sql|
    INSERT INTO posts (user_id, title, body)
    VALUES (?, ?, ?)
    RETURNING id
    |] (unUserId (newpostUserId post), newpostTitle post, newpostBody post)
  case rows of
    [] ->
      throwIO $ DbError "failed to create user"
    Only pid : _ ->
      pure (PostId pid)

readPost :: Connection -> PostId -> IO (Maybe Post)
readPost conn pid = do
  rows <- query conn [sql|
    SELECT user_id, title, body, created_at
    FROM posts
    WHERE id = ?
    |] (Only (unPostId pid))
  case rows of
    [] ->
      pure Nothing
    (uid, title, body, ctime) : _ ->
      pure (Just (Post pid (UserId uid) title body ctime))

prop_tables :: Pool Connection -> Property
prop_tables pool =
  property $ do
    withResource pool . abort $ \conn -> do
      evalIO $ createTables conn

-- Make sure your Hedgehog import is `hiding (Command)`
-- You're building something much simpler that serves the same purpose.
data Command =
    CreateUser Text Text     -- name / email
  | DeleteUser Int           -- user-index
  | CreatePost Int Text Text -- user-index / title / body
    deriving (Eq, Ord, Show)

genCreateUser :: Gen Command
genCreateUser = do
  name <- Gen.element ["stephanie", "lennart", "simon"]
  pure $
    CreateUser name (name <> "@haskell.land")

-- You can generate just about anything
-- here, you'll see why later.
genUserIx :: Gen Int
genUserIx =
  Gen.int (Range.constant 0 50)

genCreatePost :: Gen Command
genCreatePost =
  CreatePost
    <$> genUserIx
    <*> Gen.element ["C", "C++", "Haskell", "Rust", "JavaScript"]
    <*> Gen.element ["fast", "slow", "best", "worst"]

genDeleteUser :: Gen Command
genDeleteUser = do
  DeleteUser
    <$> Gen.int (Range.constant 0 50)

genCommand :: Gen Command
genCommand =
  Gen.choice [
      genCreateUser
    , genDeleteUser
    , genCreatePost
    ]

data Model =
  Model {
      modelUsers :: [User]
    } deriving (Eq, Ord, Show)

modelAddUser :: User -> Model -> Model
modelAddUser user x =
  x { modelUsers = modelUsers x <> [user] }

modelRemoveUser :: UserId -> Model -> Model
modelRemoveUser uid x =
  x { modelUsers = List.filter ((/= uid) . userId) (modelUsers x) }

execCreateUser :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Text
  -> Text
  -> m ()
execCreateUser conn name email = do
  let new = NewUser name email
  uid <- evalIO $ createUser conn new
  mgot <- evalIO $ readUser conn uid
  got <- eval $ fromJust mgot

  let want = packUser uid (userCreatedAt got) new
  want === got

  -- Track in the model that a user was created.
  -- Importantly, this means their UserId is known.
  modify (modelAddUser want)

execDeleteUser :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Int
  -> m ()
execDeleteUser conn userN = do
  muser <- gets (lookupIx userN . modelUsers)
  case muser of
    Nothing ->
      -- no users created yet, failed precondition, skip
      pure ()
    Just user -> do
      evalIO $ deleteUser conn (userId user)
      modify (modelRemoveUser (userId user))

-- Lookup an element at the specified index
-- or a modulo thereof if past the end.
lookupIx :: Int -> [a] -> Maybe a
lookupIx ix = \case
  [] ->
    Nothing
  xs ->
    listToMaybe (drop (ix `mod` length xs) (reverse xs))

execCreatePost :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Int
  -> Text
  -> Text
  -> m ()
execCreatePost conn userIx title body = do
  muser <- gets (lookupIx userIx . modelUsers)
  case muser of
    Nothing ->
      -- failed precondition, skip
      pure ()
    Just user -> do
      let new = NewPost (userId user) title body
      pid <- evalIO $ createPost conn new
      mgot <- evalIO $ readPost conn pid
      got <- eval $ fromJust mgot

      let want = packPost pid (postCreatedAt got) new
      want === got

execCommands :: (
    MonadIO m
  , MonadTest m
  )
  => Connection
  -> [Command]
  -> m Model
execCommands conn xs =
  flip execStateT (Model []) . for_ xs $ \case
    CreateUser name email ->
      execCreateUser conn name email
    DeleteUser userIx ->
      execDeleteUser conn userIx
    CreatePost userIx title body ->
      execCreatePost conn userIx title body

prop_commands :: Pool Connection -> Property
prop_commands pool =
  property $ do
    commands <- forAll $ Gen.list (Range.constant 0 100) genCommand
    withResource pool . abort $ \conn -> do
      evalIO $ createTables conn
      _model <- execCommands conn commands
      pure ()

abort :: MonadBaseControl IO m => (Connection -> m a) -> Connection -> m a
abort f conn =
  bracket_
    (liftBase (execute_ conn "BEGIN"))
    (liftBase (execute_ conn "ROLLBACK"))
    (f conn)

withPool :: (Pool Connection -> IO a) -> IO a
withPool io =
  (either throwIO pure =<<) .
  with $ \db -> do
    let connect = connectPostgreSQL (toConnectionString db)
    pool <- createPool connect close 2 60 10
    io pool

-- don't forget to add prop_commands to your tests function
tests :: IO Bool
tests =
  withPool $ \pool ->
  checkParallel $ Group "Tutorial" [
      ("prop_tables", prop_tables pool)
    , ("prop_commands", prop_commands pool)
    ]
