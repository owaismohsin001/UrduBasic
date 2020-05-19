# UrduBASIC
The language Basic taught in many schools as their first language of students, in now implemented in Urdu(ish).

## BASIC
BASIC is a language that's still taught as first language to many students around the globe and especially in countries like Pakistan and India. It is a very old language implementation more specifically the formal implementation is GW-BASIC which doesn't have multi-line functions, and even has GOTO statements.

## Urdu
In Pakistan it's often hard for students to grasp the concepts of programming in english but most programming languages today, are made in english and the first Urdu based programming language was "UrduScript", which partially was my inspiration for creating UrduBASIC. That language is largely an extremely beneficial language but since, it compiled to JS, it ought to give errors in english, and ought not to have direct I/O, but here comes in UrduBASIC. It's an interpreted language which provides it with the power of providing errors in Urdu.

## Environment
This language is interpreted and both it's interpreter and parser are written entireley from scratch using python. It should be ran using PyPy(to make it fast, but if you don't care enough then CPython is good too), if one wants to run it directly from source but if you don't then I will try to make an exe package using Nuitka for it. Now another implementation has been released [here](https://github.com/ameerwasi001/UrduBasic-JsCompiler), that compiles it to JavaScript for gaining performance.

## Complexity
It's predecessor UrduScript was a language that compiled to JavaScript so it naturally had all the complexity of JavaScript, while UrduBASIC tries to be simple while providing enough features to be useable, such as "list generation".

## Why not "real" Urdu?
There are many reasons as to why I decided to use Urduish instead of real Urdu and excluding all the reasons provided in UrduScript's page but they obviously was included in design process. First of all, today's generation is moving more towards Urduish than traditional Urdu because smart phones and stuff and then, it must be said that writing Urdu for most keyboards, for most people is horendous, innatural, and isn't native.

## Examples
This is a little example written in UrduBASIC.
```
RAKHO naam = PUCHO("Ap ka naam kya hai? ")
AGAR naam == "Ameer" PHIR;
	LINE_LIKHO("Salam developer, Ameer")
WARNAAGAR naam == "User" PHIR
	LINE_LIKHO("User koi nam nahi hota")
WARNA
	LINE_LIKHO("Assalam-o-Alikum "+naam)
KHATAM
```
I should also point out that I will produce the most important examples from 10th class' book of BISE, in UrduBASIC.

# Documentation
Well, I am working on it and it should be avalible pretty soon.

# Goals
This section is where, I ask for help. My current goal is to find a word for "FOR" because currently "FOR" keywords is used in language because of a lack of a better term in my vocabulary but I want your help to find an appropriate keyword for "FOR".
