module Demodulator where

import Invaluator (Data(..))
import Debug.Trace (traceShowId)

demodulate :: String -> Data
demodulate input =
  let (value, _) = demodulateInternal input in
  value

demodulateInternal :: String -> (Data, String)
demodulateInternal input =
  case input of
    '0':'0':tail -> (DNil, tail)
    '1':'1':tail ->
      let (first, tail') = demodulateInternal tail
          (second, tail'') = demodulateInternal tail' in
      (DCons first second, tail'')
    '0':'1':tail -> demodulateNumber tail 1
    '1':'0':tail -> demodulateNumber tail (-1)
    _ -> error $ "WTF encoding: " ++ input

demodulateNumber :: String -> Integer -> (Data, String)
demodulateNumber input sign =
  let (number, tail) = demodulateNumberInternal input in
  (DNum $ sign * number, tail)

demodulateNumberInternal :: String -> (Integer, String)
demodulateNumberInternal input =
  case input of
    '0':tail -> (0, tail)
    '1':_ ->
      let (bound, tail') = traceShowId $ demodulateNumberBound input in
      demodulateBoundedNumber (bound `div` 2) tail'
  where
    demodulateNumberBound input =
      case input of
        '1':tail ->
          let (b, tail') = demodulateNumberBound tail in
          (16*b, tail')
        '0':tail -> (1, tail)

demodulateBoundedNumber :: Integer -> String -> (Integer, String)
demodulateBoundedNumber bound input =
  case bound of
    0 -> (0, input)
    _ -> case input of
          '1':tail ->
            let (result, tail') = demodulateBoundedNumber (bound `div` 2) tail in
            (bound + result, tail')
          '0':tail -> demodulateBoundedNumber (bound `div` 2) tail
