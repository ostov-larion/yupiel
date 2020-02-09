# yupiel
Yupiel is a reactive framework for SWI-Prolog.

Synopsis:
```prolog
data(foo).
data(Data) => writeln(Data).
:- data(_) $> data(foobar).

span{id:"text",text:Text} => writeln(Text).
button{text:Text} <=> span{text:Text}.

object button{id:"MyButton",text:"Text in button"}.
object span{id:"MySpan"}.
```

Yupiel add new *Data Transforation Clauses*(DTC), which declares when data mutates.
Syntax: `Head => Body`.
When `Head` changes, execute `Body`.

Example:
```prolog
data(Data) => write("Hello, "),writeln(Data). 
```

For change term is used interfix operantor `$>`(RDT - *Reactive Data Transformation*).
Syntax: `From $> To`.
For all terms, who unifies with `From`, transform to `To`.

Example:
```prolog
?- data(_) $> data(world). 
Hello, world
?- data(Who).
Who = world
true.
```

Yupiel friends with SWI-Prolog's dicts. If in DTC the `Head` is dict, then Yupiel uses partial unification (`:<`) for it.
Example:
```prolog
point{x:X,y:Y} => format('Point coordinates now is:\nX=~w\nY=~w.',[X,Y]).
object point{id:point1,x:1,y:1}. % Prints "Point coordinates now is:\nX=1\nY=1."

:- point{id:point1} $> point{x:3,y:2} % Prints "Point coordinates now is:\nX=3\nY=2."
```

> Ok, but if I want get **full** dict?
Use `:<` in `Head`:
```prolog
point{x:X,y:Y} :< This => writeln(x=X),writeln(y=Y),writeln(full:This).

object point{id:0,x:1,y:1,z:0}. % Prints "x=1","y=1", and "point{id:0,x:1,y:1,z:0}"

button{html:Inner} :< This => format(Outer,'<button>~w</button>',[Inner]), This $> button{outer:Outer}.

object button{id:"btn",html:"test"}. % Now it is button{btn:"btn",html:"test",outer:"<button>test</button>"}
```
