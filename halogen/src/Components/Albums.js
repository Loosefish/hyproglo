"use strict";

exports.scrollToId = function (identity) {
    return function () {
        var el = document.getElementById(identity);  
        el.scrollIntoView();
    };
};
