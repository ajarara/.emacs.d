# Config

This directory holds all the base configuration. Init.el simply loads all files in this directory. Everything stored in the aux directory within this folder is only loaded when called from the config files in this level. This is mostly so that I can easily disable packages without having to hide files (ie evil-leader, when I'm feeling more holy).