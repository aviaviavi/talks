## Monadic Parsers for Imperative Programmers
### Avi Press

---

### What we're used to

For simple things, just checking a few conditions directly works fine

```python
if s.startswith("[") and s.endswith("]"):
   return s.replace("[", "").replace("]", "")
``` 

---

### What we're used to

For simple things, just checking a few conditions directly works fine

```python
if s.startswith("[") and s.endswith("]"):
   return s.replace("[", "").replace("]", "")
``` 

It's often compelling to reach for a regex

```
\[(\S+)\]
```
