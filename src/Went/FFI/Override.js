export const override2 = function (f) {
    return function (a, b) {
        return f(this)(a)(b)()
    }
}


export const override1 = function (f) {
    return function (a) {
        return f(this)(a)()
    }
}

export const override0 = function (f) {
    return function () {
        return f(this)()
    }
}