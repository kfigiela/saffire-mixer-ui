module SaffireLE.UI.VUMeter where

import SPrelude

import Data.Array ((:))
import Data.Array as Array
import Effect.Uncurried (mkEffectFn2)
import Math (pow, round)
import Specular.Dom.Browser (Node)
import Specular.Dom.Element (Prop(..))
import Specular.Dom.Widget (emptyWidget)
import Specular.FRP (filterMapEvent, foldDyn, uniqDyn)
import Specular.Internal.Effect (pushDelayed)

vuMeter :: String -> Dynamic (Maybe Number) -> Widget Unit
vuMeter chName value = do
    lastMesurements <- foldDyn addNewMeasurement [] $ (fromMaybe (negate meterRange)) <$> changed value
    currentScale <- uniqDyn $ roundCents ∘ dbToFraction <$> value
    peakScale <- uniqDyn $ roundCents ∘ dbToFraction ∘ maximum <$> lastMesurements
    clipping <- uniqDyn $ (_ == 1.0) <$> peakScale

    el "div" [class_ "vu"] do
        el "div" [classes ["vu__label", "mdc-typography--caption"]] $ text chName
        el "div" [class_ "vu__meter"] do
            el "div" [class_ "vu__scale", class_ "mdc-typography--caption"] do
                el "div" [attr "style" "left: 0%;"] $ text "-96 dB"
                el "div" [attr "style" "left: 25%;"] $ text "-54"
                el "div" [attr "style" "left: 50%;"] $ text "-12"
                el "div" [attr "style" "left: 75%;"] $ text "-6"
                el "div" [attr "style" "left: 87.5%;"] $ text "-3"
                el "div" [attr "style" "left: 100%;"] emptyWidget
            el "div" [class_ "mdc-linear-progress", attrs ("role":="progressbar")] do
                el "div" [class_ "mdc-linear-progress__buffer"] emptyWidget
                el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__secondary-bar", "vu__peak"], transform peakScale, classWhenD clipping "vu__peak--clipping"] do
                    el "span" [class_ "mdc-linear-progress__bar-inner"] emptyWidget
                el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__primary-bar", "vu__average"], transform currentScale] do
                    el "span" [class_ "mdc-linear-progress__bar-inner"] emptyWidget
    where
        dbToFraction = maybe 0.0 $ \db -> -- pow 10.0 (db/20.0)
            if db > -12.0 then
                0.5 + 0.5* (db+12.0)/12.0
            else
                0.5 * (db+96.0)/(96.0-12.0)
        roundCents v = (round (v * 500.0))/500.0
        meterRange = 96.0
        addNewMeasurement v [] = [v]
        addNewMeasurement v acc = v : (Array.take 9 acc)


transform :: Dynamic Number -> Prop
transform valueD = Prop $ mkEffectFn2 \node cleanups -> do
    setScaleX <- runEffectFn1 _setScaleX node
    unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 setScaleX) (changed valueD)
    pushDelayed cleanups unsub
    initialClasses <- readDynamic valueD
    runEffectFn1 setScaleX initialClasses

foreign import _setScaleX :: EffectFn1 Node (EffectFn1 Number Unit)
