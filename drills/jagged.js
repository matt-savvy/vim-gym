/*
 * Append a semicolon to each line.
 */
function associative(f, x, y) {
    const forwards = f(x, y)
    const backwards = f(y, x)
    return forwards === backwards
}
