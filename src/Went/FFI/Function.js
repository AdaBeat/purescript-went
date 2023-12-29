export const call2 = (curried) => {
    return function (e, port) {
        return curried(e)(port)()
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