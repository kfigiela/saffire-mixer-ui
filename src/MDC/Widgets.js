
exports._initSlider = function(node) {
  window.slider =  new mdc.slider.MDCSlider(node);
  return slider;
};

exports._subscribeSlider = function(slider, callback) {
  slider.listen('MDCSlider:change', function() { callback(slider.value)() });
};

exports._setSliderValue = function(slider, value) {
  slider.value = value;
};


// getCheckboxChecked :: Node -> IOSync Boolean
exports._getCheckboxChecked = function(node) {
  return function() {
    console.log("read")
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
