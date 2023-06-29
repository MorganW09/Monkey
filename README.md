# Monkey

A F# implementation of the Monkey interpreter built by following [Writing An Interpreter in Go](https://interpreterbook.com/). 

There are slight differences due to the implementation language, but it is very similar as the Go implementation.


## Testing Monkey

There are two ways to run Monkey code. Both leverage the `Program.fs` file situated in [./src/MonkeyLang.Runner](./src/MonkeyLang.Runner/) directory.

* Repl 
  * Running `dotnet run repl` will start the repl.
* Running .mon files
  * Put any Monkey code you desire into a file with a .mon extension.
  * Run `dotnet run file <filePath>` will read in that file, and output the result

In the `MonkeyLang.Runner` directory are two sample files (`Basec.mon` and `Complex.mon`) to get you started.

## Monkey Language Features

As a language, Monkey is a (mostly)* dynamically typed language.

It has the following features:

### Integer Expressions

Monkey supports using integers:

```
>> 1 + 1
2
>> let x = 1;
>> let y = 2;
>> x + y
3
>> x * y
2
>> x * y * 100
200
>> 100 / 2
50
>> 100 - 2
98
```

### Boolean Expressions

Monkey supports using Booleans:

```
>> true
true
>> false
false
>> !true
false
>> 1 == 1
true
>> 1 == 2
false
>> 1 != 2
true
>> 1 > 1
false
>> 1 > 0
true
>> 1 < 0
false
```

### String Expressions

Monkey suppors using Strings:

```
>> "Hello world"
Hello world
>> "Hello" + " " + "world"
Hello world
>> let h = "Hello";
>> let w = "world";
>> h + " " + w
Hello world
```

### Functions

Monkey supports functions!:

```
>> let double = fn (x) { x * x; };
>> double(10)
100
```

### If/Else Expressions

Monkey supports if/else expressions:

```
>> let positiveNum = fn (x) { if (x > 0) { "Yes" } else { "Nope" } };
>> positiveNum(3)
Yes
>> positiveNum(-3)
Nope
>> positiveNum(0)
Nope
```

### Arrays

Monkey supports using Arrays:

```
>> [1,2,3,4]
[1, 2, 3, 4]
>> let nums = [1,2,3,4];
>> nums[0]
1
>> nums[100]
null
```

As well as some functions to help operate on arrays:

#### len

Returns the len of the array:

```
>> let nums = [1,2,3,4];
>> len(nums)
4
>> len([])
0
```

#### first

Gets first element of array:

```
>> let nums = [1,2,3,4];
>> first(nums)
1
```

#### last

Get last element of array:

```
>> let nums = [1,2,3,4];
>> last(nums)
4
>> last([1])
1
>> last([])
null
```

#### push

Adds an element to an array and returns a new array:

```
>> let nums = [1,2,3,4];
>> let newNums = push(nums, 2);
>> newNums
[1, 2, 3, 4, 2]
```

### Hash

Monkey supports a Hash (Map/Dictionary) as long as the Key is an integer

```
>> let hash = {1: "Total", 5: "Fishing", 100: "Garbage"};
>> hash
{1:Total, 5:Fishing, 100:Garbage}
>> hash[1]
Total
>> hash[5]
Fishing
>> hash[8]
null
```

### puts

And finally, the only IO operation, the ability to print something:

```
>> puts(23)
23
null
>> puts("This is a cool things")
This is a cool things
null
>> puts(2,3,4,5)
2
3
4
5
null
>> puts(nums)
[1, 2, 3, 4]
null
>> puts(hash)
{1:Total, 5:Fishing, 100:Garbage}
null
```

The puts method does return null, which is a little odd.

### Fibonacci

Putting it all together, here is a naive definition for calculating Fibonacci.

```
let fib = fn (x) {
    if (x < 1) {
        return 0;
    }
    
    if (x == 1) {
        return 1;
    }

    return fib(x - 1) + fib(x - 2);
};
```

On my local machine, I was able to calculate `fib(35)` in a resonable amount of time.

> Using a hash would make this more efficient, but currently there's not way to add to a hash after creation. I guess that what happens when you're using a half-baked programming language. lol


\* The Hash (Map/Dictionary) is the notable exception, since I'm using F# to write the compiler, the key requires an integer value.