/*
 * Delete all lines starting with a comment.
 * Use :global.
 */

// returns "Fizz", "Buzz", "FizzBuzz" or a stringified version of the number
function fizzBuzz(n) {
    // true when the number is a fizz
    const fizz = n % 3 == 0;
    // true when the number is a buzz
    const buzz = n % 5 == 0;

    // string that is going to get built up and returned;
    let accumulator = "";

    // add "Fizz" if number is a fizz
    if (fizz) {
        accumulator += "Fizz";
    }
    // add "Buzz" if number is a buzz
    if (buzz) {
        accumulator += "Buzz";
    }
    // if string is still empty, set it stringified n
    if (!accumulator) {
        accumulator = n.toString();
    }

    // return accumulated string
    return accumulator;
}

