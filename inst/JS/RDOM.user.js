// ==UserScript==
// @name        RDOM
// @namespace   test
// @description open a web socket to R and handle messages from R
// @version     1
// @include     http://pmur002.neocities.org/*
// @include     file:///*
// @require     https://www.stat.auckland.ac.nz/~paul/R/DOM/0.6.0/JS/css-selector-generator.min.js
// @require     https://www.stat.auckland.ac.nz/~paul/R/DOM/0.6.0/JS/RDOM.js
// @grant       none
// ==/UserScript==
RDOM.init("127.0.0.1", "52000", "-1");
window.RDOM = RDOM;
