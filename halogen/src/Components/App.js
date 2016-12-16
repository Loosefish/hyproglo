"use strict";

exports.setTitle = function (title) {
    return function () {
        if (document.title !== title) {
          document.title = title;
        }
    };
};
