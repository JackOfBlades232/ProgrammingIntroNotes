#!/bin/bash

g++ -g -Wall -c utils.cpp
g++ -g -Wall -c constants.cpp
g++ -g -Wall -c fifteen_board.cpp
g++ -g -Wall -c fifteen_button.cpp
g++ -g -Wall -c fifteen_history.cpp
g++ -g -Wall main.cpp utils.o constants.o fifteen_board.o fifteen_button.o fifteen_history.o -lfltk -o main
