module SaffireLE.UI.VUMeter where

import SPrelude

import Effect.Uncurried (mkEffectFn2)
import Specular.Dom.Browser (Node)
import Specular.Dom.Element (Prop(..))
import Specular.Internal.Effect (pushDelayed)

vuMeter :: String -> Dynamic (Maybe Number) -> Widget Unit
vuMeter chName value = do
    el "div" [class_ "vu"] do
        el "h3" [classes ["vu__label", "mdc-typography--caption"]] $ text chName
        el "div" [class_ "mdc-linear-progress", attrs ("role":="progressbar")] do
            el_ "div" [class_ "mdc-linear-progress__buffer"]
            el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__secondary-bar", "vu__peak"], transform scaleFraction] do
                el_ "span" [class_ "mdc-linear-progress__bar-inner"]
            el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__primary-bar", "vu__average"], transform scaleFraction] do
                el_ "span" [class_ "mdc-linear-progress__bar-inner"]

    where
        scaleFraction = dbToFraction <$> value
        dbToFraction = maybe 0.0 $ \db -> ((db + meterRange)/meterRange)
        meterRange = 100.0

transform :: Dynamic Number -> Prop
transform valueD = Prop $ mkEffectFn2 \node cleanups -> do
    setScaleX <- runEffectFn1 _setScaleX node
    unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 setScaleX) (changed valueD)
    pushDelayed cleanups unsub
    initialClasses <- readDynamic valueD
    runEffectFn1 setScaleX initialClasses

foreign import _setScaleX :: EffectFn1 Node (EffectFn1 Number Unit)
