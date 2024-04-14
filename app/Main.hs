{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad ((>=>), void)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Char (toLower)
import Data.Functor (($>), (<&>))
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

newtype Environ m i o = Environ
  { runEnviron :: (m i, o -> m ()) }


class Scene s where
  type SceneRunner s (m :: * -> *)

  runScene :: (Monad m) => SceneRunner s m -> s -> m (Maybe GameScene)


data GameSceneRunner m = GameSceneRunner
  { titleSceneGameRunner :: TitleSceneRunner m
  , playSceneGameRunner :: PlaySceneRunner m
  }

data GameScene
  = TitleGameScene TitleScene
  | PlayGameScene PlayScene

instance Scene GameScene where
  type SceneRunner GameScene m = GameSceneRunner m

  runScene :: (Monad m) => GameSceneRunner m -> GameScene -> m (Maybe GameScene)
  runScene runner = \case
    TitleGameScene s ->
      runScene (titleSceneGameRunner runner) s

    PlayGameScene s ->
      runScene (playSceneGameRunner runner) s


data TitleSceneRunner m = TitleSceneRunner
  { introTitleScreen :: (Monad m) => Maybe String -> m ()
  , promptNameTitleScreen :: (Monad m) => Maybe String -> m String
  , confirmNameTitleScreen :: (Monad m) => String -> m Bool
  }

data TitleScene
  = IntroTitleScene (Maybe String)
  | InputNameTitleScene (Maybe String)
  | ConfirmNameTitleScene String

instance Scene TitleScene where
  type SceneRunner TitleScene m = TitleSceneRunner m

  runScene :: (Monad m) => TitleSceneRunner m -> TitleScene -> m (Maybe GameScene)
  runScene runner = \case
    IntroTitleScene maybeName ->
      introTitleScreen runner maybeName $>
        Just (TitleGameScene $ InputNameTitleScene maybeName)

    InputNameTitleScene maybeName ->
      promptNameTitleScreen runner maybeName <&>
        (Just . TitleGameScene . ConfirmNameTitleScene)

    ConfirmNameTitleScene name ->
      confirmNameTitleScreen runner name <&> \case
        True -> Just $ PlayGameScene $ GreetPlayerPlayScene name
        False -> Just $ TitleGameScene $ InputNameTitleScene $ Just name


data PlaySceneRunner m = PlaySceneRunner
  { greetPlayerPlayScreen :: (Monad m) => String -> m ()
  , promptReturnPlayScreen :: (Monad m) => String -> m (Either String Bool)
  , invalidResponsePlayScreen :: (Monad m) => String -> String -> m ()
  }

data PlayScene
  = GreetPlayerPlayScene String
  | ReturnPromptPlayScene String
  | InvalidResponsePlayScene String String

instance Scene PlayScene where
  type SceneRunner PlayScene m = PlaySceneRunner m

  runScene :: (Monad m) => PlaySceneRunner m -> PlayScene -> m (Maybe GameScene)
  runScene runner = \case
    GreetPlayerPlayScene name ->
      greetPlayerPlayScreen runner name $>
        Just (PlayGameScene $ ReturnPromptPlayScene name)

    ReturnPromptPlayScene name ->
      promptReturnPlayScreen runner name <&> \case
        Right True -> Just $ TitleGameScene $ IntroTitleScene $ Just name
        Right False -> Nothing
        Left response -> Just $ PlayGameScene $ InvalidResponsePlayScene name response

    InvalidResponsePlayScene name response ->
      invalidResponsePlayScreen runner name response $>
        Just (PlayGameScene $ ReturnPromptPlayScene name)


iterMaybeM :: (Monad m) => (a -> m (Maybe a)) -> a -> m (Maybe a)
iterMaybeM f = runMaybeT . (MaybeT . f >=> MaybeT . iterMaybeM f)


program :: (Monad m) => GameSceneRunner m -> GameScene -> m ()
program runner = void . iterMaybeM (runScene runner)


testEnviron :: Environ IO String String
testEnviron = Environ (getLine, putStr >=> const (hFlush stdout))

testInitScene :: GameScene
testInitScene = TitleGameScene $ IntroTitleScene Nothing


testGameRunner :: (Monad m) => Environ m String String -> GameSceneRunner m
testGameRunner env = GameSceneRunner
  { titleSceneGameRunner = testTitleRunner env
  , playSceneGameRunner = testPlayRunner env
  }

testTitleRunner :: (Monad m) => Environ m String String -> TitleSceneRunner m
testTitleRunner (Environ (input, output)) = TitleSceneRunner
  { introTitleScreen
  , promptNameTitleScreen
  , confirmNameTitleScreen
  }
    where
      introTitleScreen _ = do
        output "Welcome!\n"

      promptNameTitleScreen maybeName = do
        output $ "Input Name (" ++ maybe "No supplied name"
          ("Default - " ++) maybeName ++ "): "
        input >>= \case
          "" -> maybe (promptNameTitleScreen Nothing) return maybeName
          name -> return name

      confirmNameTitleScreen name = do
        output $ "You're okay with the name \"" ++ name ++ "\"? (Y/N): "
        input >>= (\case
          "y" -> return True
          "n" -> return False
          _ -> confirmNameTitleScreen name) . fmap toLower

testPlayRunner :: (Monad m) => Environ m String String -> PlaySceneRunner m
testPlayRunner (Environ (input, output)) = PlaySceneRunner
  { greetPlayerPlayScreen
  , promptReturnPlayScreen
  , invalidResponsePlayScreen
  }
    where
      -- greetPlayerPlayScreen :: (Monad m) => String -> m ()
      greetPlayerPlayScreen name = do
        output $ "\nHello, " ++ name ++ "!\n"

      -- promptReturnPlayScreen :: (Monad m) => String -> m (Either String Bool)
      promptReturnPlayScreen name = do
        output "Would you like to play again? (Y/N): "
        input <&> (\case
          "y" -> Right True
          "n" -> Right False
          response -> Left response) . fmap toLower

      -- invalidResponsePlayScreen :: (Monad m) => String -> String -> m ()
      invalidResponsePlayScreen _ response = do
        output $ "Invalid response - \"" ++ response ++ "\"\n"


main :: IO ()
main = let
  runner = testGameRunner testEnviron
  scene = testInitScene
  in program runner scene
