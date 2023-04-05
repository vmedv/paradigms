"use strict";

const variables = ['x', 'y', 'z']
let operations = []

const cnst = value => () => value;
const variable = name => (...args) => args[variables.indexOf(name)];
const operation = (func, token) => {
    operations[token] = [(...expr) => (...args) => func(...expr.map(ex => ex(...args))), func.length];
    return operations[token][0];
}

const pi = operation(() => Math.PI, 'pi')()
const e = operation(() => Math.E, 'e')()
const add = operation((a, b) => a + b, '+');
const subtract = operation((a, b) => a - b, '-');
const multiply = operation((a, b) => a * b, '*');
const divide = operation((a, b) => a / b, '/');
const negate = operation(a => -a, 'negate');
const avg3 = operation((a, b, c) => (a + b + c) / 3, 'avg3');
const med5 = operation((a, b, c, d, e) => [a, b, c, d, e].sort((a, b) => a - b)[2], 'med5');

const getExpr = input => {
    let stack = [];
    let tokens = input.split(' ').filter(tok => tok !== '');
    for (let tok of tokens) {
        if (!isNaN(parseInt(tok))) {
            stack.push(cnst(parseInt(tok)));
        } else if (variables.indexOf(tok) !== -1) {
            stack.push(variable(tok));
        } else {
            stack.push(operations[tok][0](...stack.splice(stack.length - operations[tok][1])));
        }
    }
    return stack.pop();
}

const parse = input => (...args) => getExpr(input)(...args)
