"use strict";

const ZERO = new Const(0);
const ONE = new Const(1);
const TWO = new Const(2);

const variables = ['x', 'y', 'z'];
let operations = {};

function Element (evaluate, toString, prefix, postfix, diff) {
    this.evaluate = evaluate;
    this.toString = toString;
    this.prefix = prefix;
    this.postfix = postfix;
    this.diff = diff
}

function createElement (prototype, evaluate, toString, prefix, postfix, diff) {
    prototype.evaluate = evaluate;
    prototype.toString = toString;
    prototype.prefix = prefix;
    prototype.postfix = postfix;
    prototype.diff = diff
}

function AbstractOperation (func, token, diffFunc, ...terms) {
    createElement(this,
         (...args) => func(...terms.map(t => t.evaluate(...args))),
         () => terms.join(" ") + (token !== "" ? " " + token : ""),
         () => "(" + (token !== "" ? token + " " : "") + terms.map(v => v.prefix()).join(" ") + ")",
         () => "(" + terms.map(v => v.postfix()).join(" ") + (token !== "" ? " " + token : "") + ")",
         (name) => diffFunc(...terms, ...terms.map(o => o.diff(name)))
    );
    this.argsNumber = func.length !== 0 ? func.length : terms.length;
    this.terms = terms;
}

function createOperation(func, token, diffFunc) {
    let Operation = function (...args) {
        AbstractOperation.call(this, func, token, diffFunc, ...args)
    };
    Operation.prototype = Object.create(AbstractOperation.prototype);
    Operation.prototype.constructor = Operation;
    operations[token] = Operation;
    return Operation
}

function Const(value) {
    createElement(this,
        () => value,
        () => value.toString(),
        () => value.toString(),
        () => value.toString(),
        () => ZERO)
}

function Variable(name) {
    createElement(this,
        function (...args) {
            return args[variables.indexOf(name)]
        },
        function () {
            return name.toString()
        },
        function () {
            return name.toString()
        },
        function () {
            return name.toString()
        },
        function (diffName) {
            if (name === diffName) return ONE;
            else return ZERO
    })
}

let Negate = createOperation(a => -a, "negate", (a, da) => new Negate(da));
let Exponent = createOperation(a => Math.exp(a), "exp", (a, da) => new Multiply(a, da));
let Add = createOperation((a, b) => a + b, "+", (a, b, da, db) => new Add(da, db));
let Subtract = createOperation((a, b) => a - b, "-", (a, b, da, db) => new Subtract(da, db));
let Multiply = createOperation((a, b) => a * b, "*",
    (a, b, da, db) => new Add(new Multiply(a, db), new Multiply(b, da)));
let Divide = createOperation((a, b) => a / b, "/",
    (a, b, da, db) => new Divide(new Subtract(new Multiply(da, b), new Multiply(a, db)), new Multiply(b, b)));
let Gauss = createOperation((a, b, c, x) => a * Math.exp(-((x-b)*(x-b)/(2 * c * c))),
    "gauss",
    (a, b, c, x, da, db, dc, dx) => new Negate(new Divide(new Multiply(new Subtract(x, b), this)), new Multiply(c, c)));
let Sumexp = createOperation((...args) => args.map(v => Math.exp(v)).reduce((a, b) => a + b, 0),
    "sumexp",
    function (...terms) {
        let res = new Const(0);
        for (let i = 0; i < terms.length / 2; i++) {
            res = new Add (res, new Multiply(terms[i + terms.length / 2], new Exponent(terms[i])))
        }
        return res
    });
let Softmax = createOperation(
    (...args) => Math.exp(args[0]) / args.map(v => Math.exp(v)).reduce((a, b) => a + b, 0),
    "softmax",
    function (...terms) {
        let denom = new Const(0);
        for (let i = 0; i < terms.length / 2; i++) {
            denom = new Add (denom, new Exponent(terms[i]))
        }
        let ddenom = new Const(0);
        for (let i = 0; i < terms.length / 2; i++) {
            ddenom = new Add (ddenom, new Multiply(terms[i + terms.length / 2], new Exponent(terms[i])))
        }
         return new Divide(
             new Subtract(
                 new Multiply(new Multiply(new Exponent(terms[0]), terms[terms.length / 2]), denom),
                 new Multiply(new Exponent(terms[0]), ddenom)),
             new Multiply(denom, denom))
    });

const parse = input => {
    let stack = [];
    let tokens = input.split(' ').filter(tok => tok !== '');
    for (let tok of tokens) {
        if (operations[tok] !== undefined) {
            stack.push(new operations[tok](...stack.splice(stack.length - (new operations[tok]).argsNumber)));
        } else if (variables.indexOf(tok) !== -1) {
            stack.push(new Variable(tok));
        } else {
            stack.push(new Const(parseInt(tok)));
        }
    }
    return stack.pop();
};

let WrongBracketsError = createCustomError("WrongBracketsError");
let UnknownTokenError = createCustomError("UnknownTokenError");
let WrongAmountsOfArgs = createCustomError("WrongAmountsOfArgs");


function smartSplit (input, post) {
    if (input[0] === '(' ^ input[input.length - 1] === ')')
        throw new WrongBracketsError("Brackets expected in the begin and the end of expression");
    let inputArr = input.split("");
    let res = [];
    let covered = false;
    if (input[0] === '(' && input[input.length - 1] === ')') {
        inputArr = inputArr.slice(1, input.length - 1);
        covered = true
    }
    inputArr[inputArr.length] = " ";
    let accum = "";
    let accumT = "";
    let collecting = true;
    let balance = 0;
    for (let i of inputArr) {
        if (i === '(') {
            if (accum.trim() !== "") {
                res.push(accum);
                accum = ""
            }
            balance++
        }
        if (i === ' ' && balance === 0) {
            collecting = true
        }
        if (balance > 0) accumT += i;
        if (collecting && balance === 0) accum += i;
        if (i === ')') {
            balance--;
            if (balance < 0) throw new WrongBracketsError("Too many closing brackets");
            if (balance === 0) {
                res.push(accumT);
                accumT = "";
                accum = ""
            }
        }
        if (i === ' ' && accum.trim() !== "" && balance === 0) {
            if (accumT === "") res.push(accum);
            accum = ""
        }
    }
    if (balance !== 0) throw new WrongBracketsError("Wrong amount");
    res = res.map(v => v.trim());
    if (covered && operations[res[post ? res.length - 1 : 0]] === undefined)
        throw new WrongBracketsError("No brackets expected here");
    return res
}

function parseVar (post) {
    return function (input) {
        let tokenized = smartSplit(input.trim(), post)
        if (operations[tokenized[post ? tokenized.length - 1 : 0]] !== undefined) {
            if ((new operations[tokenized[post ? tokenized.length - 1 : 0]]).argsNumber !== 0 &&
                tokenized.length !== (new operations[tokenized[post ? tokenized.length - 1 : 0]]).argsNumber + 1)
                throw new WrongAmountsOfArgs("op: " + tokenized[post ? tokenized.length - 1 : 0])
            return new operations[tokenized[post ? tokenized.length - 1 : 0]](...tokenized.slice(
                post ? 0 : 1,
                post ? tokenized.length - 1 : tokenized.length
            ).map(v => post ? parsePostfix(v) : parsePrefix(v)))
        } else if (variables.indexOf(tokenized[post ? tokenized.length - 1 : 0]) !== -1) {
            if (tokenized.length !== 1)
                throw new WrongAmountsOfArgs(
                    "expected: " +
                    (new operations[tokenized[post ? tokenized.length - 1 : 0]]).argsNumber + ", got: " + (tokenized.length + 1))
            return new Variable(tokenized[post ? tokenized.length - 1 : 0])
        } else {
            if (isNaN(parseInt(tokenized[post ? tokenized.length - 1 : 0])) ||
                parseInt(tokenized[post ? tokenized.length - 1 : 0]).toString() !== tokenized[post ? tokenized.length - 1 : 0])
                throw new UnknownTokenError("unknown token:" + tokenized[post ? tokenized.length - 1 : 0])
            if (tokenized.length !== 1) throw new WrongAmountsOfArgs("wrong expression")
            else return new Const(parseInt(tokenized[post ? tokenized.length - 1 : 0]))
        }
    }
}

const parsePrefix = parseVar(false)
const parsePostfix = parseVar(true)

function createCustomError (name) {
    let Err = function (msg) {
        this.message = msg
    };
    Err.prototype = Object.create(Error.prototype);
    Err.prototype.constructor = Err;
    Err.prototype.name = name;
    return Err
}
