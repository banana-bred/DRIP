# Namelists

This namelist files contain several Fortran namelists.
A Fortran namelist is essentially a group of variables that fall under one group name.
Such a file is supplied to the program via stdin redirection (<), i.e., the program reads stdin and expects the contents of a namelists file.
Each namelist is defined by a namelist statement in the code; something as follows

`namelist / group_name / var1, var2, var3, var4`

The above namelist would be represented as follows in a nameist file

```
$group_name

  var1 = val1
  var2 = val2
  var3 = val3
  var4 = val4

/
```

The `$` declares the start of the namelist given by `group_name`.
The `/` declares the end of the namelist.
The variables var1, var2, var3, and var4 take on the values val1, val2, val3, and val4, respectively.
The values on the right-hand side in this example must be constants, so something like `"hello"` for a character array, `1.325e0` for a real, or even `17, 23, 31` for an integer array.
Variables appearing in a `namelist` statement but not in a namelist file probably take an intentional default value and may even remain unused, but can be overwritten by including them in a namelist file.

### Variable documentation

The following convention is used to document variables in the code:

```
type(variable_type) [, attributes] :: variable_name
  !! description comment
```

A similar one will be used in the template namelist.


```
var1 = val1
  !! description comment
```
