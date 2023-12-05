## Possibly a WL $\rightarrow$ C-like languages translator

**In development. just a prototype**

__Main goals__
- types detection
- language independent representation (sort of IR)
- complex numbers support + casting

__How it is implemented__
- any operator is typed, and all arguments carry their types as well and estimated value of the result
- one operation - one line, data transferring is performed using temporal variables (virtual registers)

### Example
Define a function and translate it to C-like language
```mathematica
Mirage`IR`Translate[
    
  Function[{x,t}, Sum[Exp[I w x - I w t], {w,0.1,10., 0.1} ]]

, {"x"->0.1, "t"->2.}] // IR // MakeCL // ToCCodeString // Print
```

The output firstly goes to sort of intermediate representation, and then, considering types it is converted to C

```c
cfloat kernel(float x, float t) {
  float reg3;
  reg3 = 0.1;

  float reg4;
  reg4 = 10.;

  float reg5;
  reg5 = 0.1;

  float w;
  w = reg3;

  cfloat reg6;
  reg6.re = reg2.re;
  reg6.im = reg2.im;

  while (w <= reg4) {
    float reg27;
    reg27 = w;

    float reg28;
    reg28 = 0.1;

    float reg29;
    reg29 = reg27 + reg28;

    w = reg29;
    float reg15;
    reg15 = 1;

    float reg16;
    reg16 = w;

    cfloat reg17;
    reg17.re = 0.;
    reg17.im = reg15 * reg16;

    cfloat reg12;
    reg12.re = reg17.re;
    reg12.im = reg17.im;

    float reg13;
    reg13 = x;

    cfloat reg14;
    reg14.re = reg12.re * reg13;
    reg14.im = reg12.im * reg13;

    cfloat reg9;
    reg9.re = reg14.re;
    reg9.im = reg14.im;

    int reg21;
    reg21 = -1;

    float reg22;
    reg22 = 1;

    float reg23;
    reg23 = 0.;

    float reg18;
    reg18 = reg23;

    float reg24;
    reg24 = w;

    float reg25;
    reg25 = t;

    float reg26;
    reg26 = reg24 * reg25;

    float reg19;
    reg19 = reg26;

    cfloat reg20;
    reg20.re = reg18 * reg19;
    reg20.im = 0.;

    cfloat reg10;
    reg10.re = reg20.re;
    reg10.im = reg20.im;

    cfloat reg11;
    reg11.re = reg9.re + reg10.re;
    reg11.im = reg9.im + reg10.im;

    cfloat reg7;
    reg7.re = reg11.re;
    reg7.im = reg11.im;

    cfloat reg8;
    reg8.re = cos(reg7.im) * exp(reg7.re);
    reg8.im = sin(reg7.im) * exp(reg7.re);

    cfloat reg2;
    reg2.re = reg8.re;
    reg2.im = reg8.im;

    reg6.re = reg6.re + reg2.re;
    reg6.im = reg6.im + reg2.im;
  }
  cfloat latterly1;
  latterly1.re = reg6.re;
  latterly1.im = reg6.im; 

  return latterly1;
}
```

There is a lot of extra variables generated, but hopefully they will be removed by the compiler

## Contribution
Feel free
