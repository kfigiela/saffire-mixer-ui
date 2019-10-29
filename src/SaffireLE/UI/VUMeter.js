// _initClasses :: EffectFn1 Node (EffectFn1 (Array ClassName) Unit)
exports._setScaleX = function(node) {
    return function(scale) {
      node.style.transform = "scaleX(" + scale + ")";
    }
};
