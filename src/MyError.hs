--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- Error
--


module MyError (TerminationException (TE), withExit, throwMyError) where

import System.Environment.Blank (getArgs)
import System.Exit
    ( ExitCode(ExitFailure),
      exitWith,
      ExitCode,
      ExitCode(ExitFailure) )
import Control.Exception ( throw, catch, Exception, throw )
import Type (Defined(..))

data TerminationException = TE ExitCode String
    deriving Show
instance Exception TerminationException where


myExit :: String -> IO a
myExit message = putStrLn ("*** ERROR : " ++ message ++ ".")
                    >> exitWith (ExitFailure 84)

withExit :: IO a -> IO a
withExit action = action `catch` (\(TE code message) -> myExit message)

throwMyError :: String -> a
throwMyError string = throw (TE (ExitFailure 84) string)