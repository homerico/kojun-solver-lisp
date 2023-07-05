# Kojun-Solver-Lisp

Solucao com base no resolvedor de Sudoku presente em:

https://gist.github.com/GOFAI/6103d01049ae83175c09

## Usage



## Installation
Para instalar o Common Lisp no Ubuntu, basta executar o seguinte comando:
```bash
sudo apt-get update
sudo apt-get install clisp
```

Após isso, instalar o pacote Quicklisp, basta seguir as instruções presentes em 
[aqui](https://www.quicklisp.org/beta/).

Por fim, para instalar a dependência, deve entrar no ambiente 
REPL do Common Lisp e executar o seguinte comando:
```lisp
(ql:quickload "iterate")
```