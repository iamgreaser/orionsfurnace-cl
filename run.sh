#!/bin/sh
sbcl --disable-debugger --eval "(ql:quickload 'orions-furnace)" --eval "(orions-furnace:run)" --quit

