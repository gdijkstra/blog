So one has to manually write these file names or is there some neat
workflow? Okay, time to try out the syntax highlighting:

```python
def hello(x):
    print("Hello,", x)
```

Did that work? Okay, so that works, but we do not get any fancy
colours. Let us try to set that up. Maybe force a rebuild? Okay, so I
had to adapt the template.

$$ \ln x = \int_{-\infty}^x \frac 1 y \, dy  $$

Let's celebrate.

$$\require{AMScd}
\begin{CD}
F\mu F @>{in}>> \mu F\\
@V{F (fold\, \theta)}VV @VV{fold\, \theta}V \\
FX @>{\theta}>> X
\end{CD}$$

Let's try this. Now on to some nifty Agda stuff.

\begin{code}
module BlogPost where
\end{code}

So it seems that things do not exactly work yet, so let us just use
booleans for now:

\begin{code}
open import Agda.Primitive public

data Bool : Set where
  True : Bool
  False : Bool
\end{code}

Some more *text* and, _"yow!"_, some more Agda code:


\begin{code}
foo : Bool -> Bool
foo x = x
\end{code}

Now let's see if we can use this definition somewhere else.

\begin{code}
bar : Bool -> Bool -> Bool
bar x = foo
\end{code}

Time to rejoice?
