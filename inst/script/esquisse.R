#!/usr/bin/env r

args<-commandArgs(TRUE)

library(esquisse)
esquisse::esquisser(data = args[1], viewer = 'browser')
