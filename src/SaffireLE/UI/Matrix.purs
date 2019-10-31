module SaffireLE.UI.Matrix where

import SPrelude

import Data.Int (floor, toNumber)
import SaffireLE.Mixer (MixerState)

matrix :: Dynamic MixerState -> Widget Unit
matrix state = do
  el "div" [class_ "mdc-data-table", attr "style" "width: 100%"] do
    el "table" [class_ "mdc-data-table__table"] do
      el_ "thead" do
        el "tr" [class_ "mdc-data-table__header-row"] do
          for_ inputs $ \(Tuple name _) -> el "th" headAttrs $ text name
      el "tbody" [class_ "mdc-data-table__content"] do
        mixParams "Out 1" $ _.out1 <$> lowResMatrix
        mixParams "Out 2" $ _.out2 <$> lowResMatrix
        mixParams "Out 3" $ _.out3 <$> lowResMatrix
        mixParams "Out 4" $ _.out4 <$> lowResMatrix
  where
  lowResMatrix = _.lowResMixer ∘ unwrap <$> state
  mixParams name mix = do
    el "tr" [class_ "mdc-data-table__content"] do
      for_ inputs $ \(Tuple _ getter) -> el "td" cellAttrs $ dynText $ formatNumber ∘ getter <$> mix
  formatNumber = show ∘ floor ∘ (_ * toNumber 0x7fff)
  cellAttrs = [classes ["mdc-data-table__cell", "mdc-data-table__cell--numeric"]]
  headAttrs = [classes ["mdc-data-table__header-cell", "mdc-data-table__header-cell--numeric"]]
  inputs =
    [ Tuple "DAC 1" _.dac1
    , Tuple "DAC 2" _.dac2
    , Tuple "DAC 3" _.dac3
    , Tuple "DAC 4" _.dac4
    , Tuple "DAC 5" _.dac5
    , Tuple "DAC 6" _.dac6
    , Tuple "DAC 7" _.dac7
    , Tuple "DAC 8" _.dac8
    , Tuple "In 1" _.in1
    , Tuple "In 2" _.in2
    , Tuple "In 3" _.in3
    , Tuple "In 4" _.in4
    , Tuple "SPDIF 1" _.spdif1
    , Tuple "SPDIF 2" _.spdif2
    ]
