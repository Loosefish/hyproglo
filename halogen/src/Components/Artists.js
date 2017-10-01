"use strict";

exports.getSearchQuery = function (x) {
  return document.getElementById("query").value;  
};

exports.setSearchQuery = function (value) {
  document.getElementById("query").value = value;  
};
