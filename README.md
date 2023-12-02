## Possibly a WL $\rightarrow$ C-like languages translator

**In development.**

### Example
Define an anonumous function
```mathematica
f = Function[{x,t}, Sum[Sin[w x - w t], {w,0.0,10.0,0.01}] ];
```

Convert to intermediate representation
```mathematica
ir = Mirage`IR`Translate[f, {"x"->1.0, "t"->3.6}];
```
The second argument contains __probe values for arguments__ to automatically detect types of variables

```mathematica
IR`Entity[IR`Function[IR`Type`Real, kernel, {IR`Entity[x, 1., IR`Type`Real], IR`Entity[t, 3.6, IR`Type`Real]}, 
 
    IR`Block[{IR`Entity[IR`Block[{IR`Define[IR`Entity[sum2, 0., IR`Type`Real]], 
 
         IR`Entity[IR`Block[{IR`Define[IR`Entity[w, 0., IR`Type`Real]], 
 
            IR`Assign[IR`Entity[w, 0., IR`Type`Real], IR`Entity[IR`Constant, 0., IR`Type`Real]]}], 0., IR`Type`Real], 
 
         IR`While[IR`Operator[LessEqual, IR`Entity[w, 0., IR`Type`Real], IR`Entity[IR`Constant, 10., IR`Type`Real]], 
 
...
```

then using IR's representation convert it to OpenCL C code

```mathematica
Mirage`OpenCL`Translate[ir]
```

the output will be

```c
float kernel(float x, float t)
{
    float sum2;
    float w;
    w = 0.;

    while( w <= 10.)
    {
        sum2 = w * x + ((float) -1) * (w * t);
        w = w + 0.01;
    }
    float paeony1;
    paeony1 = sum2;

    return paeony1;
}
```

## Contribution
Feel free