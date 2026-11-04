# Comandos de esta versión de PROC. Se corren desde esta carpeta.
#
#   make install     instala el paquete, que da #lang proc
#   make uninstall   lo quita, para poder instalar otra versión
#   make test        corre las pruebas
#   make run         abre el REPL

# El nombre del paquete, que es el de la carpeta
PKG := proc-varios-azucar

.PHONY: install uninstall test run

install:
	raco pkg install --auto --link ../$(PKG)

uninstall:
	raco pkg remove $(PKG)

test:
	raco test --make tests

run:
	racket -y main.rkt
