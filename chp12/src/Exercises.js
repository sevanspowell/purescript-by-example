"use strict";

exports.timeoutImpl = function(time, onFinish) {
    return function() {
        return setTimeout(onFinish, time);
    };
}
