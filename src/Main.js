// _initClasses :: EffectFn1 Node (EffectFn1 (Array ClassName) Unit)
exports._setScaleX = function(node) {
    return function(scale) {
      node.style.transform = "scaleX(" + scale + ")";
    }
};

exports._initSlider = function(node) {
  window.slider =  new mdc.slider.MDCSlider(node);
  return slider;
};

exports._subscribeSlider = function(slider, callback) {
  slider.listen('MDCSlider:change', function() { callback(slider.value/100.0)() });
};

exports._setSliderValue = function(slider, value) {
  slider.value = value*100.0;
};


// getCheckboxChecked :: Node -> IOSync Boolean
exports._getCheckboxChecked = function(node) {
  return function() {
    return node.checked;
  };
};

// setCheckboxChecked :: Node -> Boolean -> IOSync Unit
exports._setCheckboxChecked = function(node) {
  return function(value) {
    return function() {
      return node.checked = value;
    };
  };
};
