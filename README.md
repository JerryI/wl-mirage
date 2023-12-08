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
f =   Function[{x,t}, 
  
  With[{r = 0.8, d = 0.3}, Sum[

  Exp[- I w t + I w x]
  
, {w, 5,10+5, 0.1}]]];

Mirage`IR`Translate[
    
 f

, {"x"->0.1, "t"->2.}] // IR // MakeCL // ToCCodeString // Print
```

The output firstly goes to sort of intermediate representation, and then, considering types it is converted to C

```c
cfloat kernel(float x, float t) {
  float r;
  r = 0.8;

  float d;
  d = 0.3;

  int reg3;
  reg3 = 5;

  int reg7;
  reg7 = 10;

  int reg8;
  reg8 = 5;

  int reg9;
  reg9 = reg7 + reg8;

  int reg4;
  reg4 = reg9;

  float reg5;
  reg5 = 0.1;

  int w;
  w = reg3;

  cfloat reg6;
  reg6.re = reg2.re;
  reg6.im = reg2.im;

  while (w <= (float) reg4) {
    int reg30;
    reg30 = w;

    float reg31;
    reg31 = 0.1;

    float reg32;
    reg32 = (float) reg30 + reg31;

    w = reg32;
    int reg21;
    reg21 = -1;

    float reg22;
    reg22 = 1;

    float reg23;
    reg23 = 0.;

    float reg18;
    reg18 = reg23;

    int reg19;
    reg19 = w;

    float reg20;
    reg20 = reg18 * (float) reg19;

    float reg15;
    reg15 = reg20;

    float reg16;
    reg16 = t;

    cfloat reg17;
    reg17.re = reg15 * reg16;
    reg17.im = 0.;

    cfloat reg12;
    reg12.re = reg17.re;
    reg12.im = reg17.im;

    float reg27;
    reg27 = 1;

    int reg28;
    reg28 = w;

    float reg29;
    reg29 = 0.;

    float reg24;
    reg24 = reg29;

    float reg25;
    reg25 = x;

    cfloat reg26;
    reg26.re = reg24 * reg25;
    reg26.im = 0.;

    cfloat reg13;
    reg13.re = reg26.re;
    reg13.im = reg26.im;

    cfloat reg14;
    reg14.re = reg12.re + reg13.re;
    reg14.im = reg12.im + reg13.im;

    cfloat reg10;
    reg10.re = reg14.re;
    reg10.im = reg14.im;

    cfloat reg11;
    reg11.re = cos(reg10.im) * exp(reg10.re);
    reg11.im = sin(reg10.im) * exp(reg10.re);

    cfloat reg2;
    reg2.re = reg11.re;
    reg2.im = reg11.im;

    reg6.re = reg6.re + reg2.re;
    reg6.im = reg6.im + reg2.im;
  }
  cfloat opaquely1;
  opaquely1.re = reg6.re;
  opaquely1.im = reg6.im;

  return opaquely1;
}
```

There is a lot of extra variables generated, but hopefully they will be removed by the compiler

## Contribution
Feel free
