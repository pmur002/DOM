// ==UserScript==
// @name        RDOM
// @namespace   test
// @description open a web socket to R and handle messages from R
// @version     1
// @include     http://pmur002.neocities.org/*
// @include     file:///*
// @require     https://www.stat.auckland.ac.nz/~paul/R/DOM/JS/DOM.js
// @require     https://www.stat.auckland.ac.nz/~paul/R/DOM/JS/socket.js
// @grant       none
// ==/UserScript==

(function() {
    var ws = new WebSocket("ws://localhost:52000");
    initSocket(ws);
})();
