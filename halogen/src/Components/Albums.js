"use strict";

exports.scrollToId = function (identity) {
    var el = document.getElementById(identity);  
    el.scrollIntoView();
};
