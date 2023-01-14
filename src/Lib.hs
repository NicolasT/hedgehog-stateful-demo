{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import           Control.Exception (Exception, throwIO)

import           Data.Time.Clock (UTCTime(..))
import           Data.Text (Text)

import           Database.PostgreSQL.Simple (Connection, Only(..))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple (execute, execute_)
import           Database.PostgreSQL.Simple (query)

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
