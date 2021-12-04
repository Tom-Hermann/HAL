##
## EPITECH PROJECT, 2021
## B-FUN-501-RUN-5-1-HAL-tom.hermann
## File description:
## Makefile
##

BINARY_PATH	:=	$(shell stack path --local-install-root)

UNITTEST_PATH := $(shell stack path --project-root)

NAME	=	hal

all:	$(NAME)

$(NAME): fclean
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

test:
	cabal test

clean:
	stack clean

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY: all test clean fclean re
