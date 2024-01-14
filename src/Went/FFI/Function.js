export const call4 = (curried) => {
    return function (a, b, c, d) {
        return curried(a)(b)(c)(d)()
    }
}

export const call3 = (curried) => {
    return function (a, b, c) {
        return curried(a)(b)(c)()
    }
}

export const call2 = (curried) => {
    return function (a, b) {
        return curried(a)(b)()
    }
}

export const call1 = (curried) => {
    return function (a) {
        return curried(a)()
    }
}

export const call0 = (curried) => {
    return curried()
}