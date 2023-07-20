# While Abstract Interpreter

Software Verification project developed in the year 2022-2023 by Davide Albiero and Damiano Mason.

The abstract Interpreter implements 3 types of domains:
- Interval Domain
- Sign Domain
- Congruence Domain

It is also possible to specify the widening delay as a parameter in the constructor of the abstract state.

## How it works?

```mathematica
x := 0;

while x < 40 do {
    x++;
};

while x != 0 do {
    x := x - 1;
}
```

### With Interval Domain:
![Analysis done with the interval domain](img/interval.png?raw=true)

### With Sign Domain:
![Analysis done with the sign domain](img/sign.png?raw=true)

### With Congruence Domain:
![Analysis done with the congruence domain](img/congruence.png?raw=true)

More examples are available in the Examples folder.
