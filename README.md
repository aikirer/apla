# apla
Apla is a simple interpreted programming language. It's not suited for any practical use.
## syntax
Code blocks will use the following convention here:
```js
code
-----------------------------------------------------------
result
```
### Variables
Variables are declared with the `var` keyword, and can be prefixed with `mut` to make them mutable. Semicolons are mandatory after each statement.
```js
var foo = 2; 
foo = 7; # error
mut var bar = 5;
bar = 12;
```
Optionally, after the variable's name you can add the type.
```js
var foo: str = "Hello, world!";
```
If the types do not match, the program won't compile:
```js
var foo: bool = 5;
-----------------------------------------------------------
 | [error] Mismatched types: 'bool', 'int'! ['apla.apla', line 1]
 | var foo: bool = 5;
 |                 ^
```
### Scopes
Scopes use curly braces - `{}`.
```js
var foo = 15;
{
  var foo = "asd";
  std(print(foo));
}
std(print(foo));
-----------------------------------------------------------
asd
15
```
### If statements
The syntax for an if statement is `if (expression) statement`.
```js
var foo = std(str_to_int(std(read())));

if (foo % 2 == 0) std(print('foo'));
else std(print('bar'));
-----------------------------------------------------------
2
foo

4
foo

3
bar
```
### While statements
While statements follow the some rules as if statements.
```js
mut var input = std(str_to_int(std(read())));
while (input > 0) {
	std(print(input * input));
	input = std(str_to_int(std(read())));
}
-----------------------------------------------------------
5
25
3
9
9
81
0
```
### Functions
Functions are declared with the `func` keyword, the arguments follow the same rules as variables except the `var` keyword is ommited and type annotation are neccessary.
```js
func magic_number(n: int) int {
	return (n + 3) * 5;
}

func read_int() int {
	return std(str_to_int(std(read())));
}

mut var input = read_int();
while (input > 0) {
	std(print(magic_number(input)));
	input = read_int();
}
-----------------------------------------------------------
12
75
13
80
5
40
1
20
```
### Pointers
Use `^` or `^mut` as a prefix to annotate a type, `@` or `@mut` to take a pointer from a variable, and finally `*` to dereference the pointer.
```go
func foo(bar: ^mut int) {
	*bar = 54;
}
# To take a mutable pointer to a variable, it needs to be declared as mutable.
mut var n = 15; 
std(print(n));
foo(@mut n);
std(print(n));
-----------------------------------------------------------
15
54
```
### Classes
Use the `class` keyword to declare a class, and then declare functions or classes inside the class' scope.
```js
class String {
	var object: str;
	
	func print() {
		std(print(this.object));
	}
}

func StringInit(str: str) String {
	var object = String();
	object.object = str;
	return object;
}

var string = StringInit("Hello, world!");
string.print();
-----------------------------------------------------------
Hello, world!
```
### Using other files
Let's say there is a file called `apla2.apla` containing the function `StringInit` and class `String` from the previous example. It's possible to use them in another file using the `import` keyword:

```js
import "apla2";

var string = StringInit("Hello, world! again");
string.print();
-----------------------------------------------------------
Hello, world! again
```
## std functions
The std is really small and it consists of a few small native functions:
* `print(any)`
* `rust_dbg_print(any)`
* `read() [reads input] -> str`
* `str_to_int(str) -> int`
* `strlen(str) -> int`

the *any* type cannot be used in normal apla programs and is special to the std functions
## Known issues
* It's not checked whether a return statement is present at the end of a function.
* Displaying errors might fail since introducing the `import` keyword, only for syntax errors (parsing phase)
* Errors might be displayed for wrong files when using the `import` keyword
* (the whole thing might be a bit 'unstable')
