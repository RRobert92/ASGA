# Report issues
If you have any issue with The ASGA, sorry about that, I will do what I
can to fix that ASAP. Actually, it may already be fixed, so first thing to do 
is to use the ASGA runing on the shinyapp.io, or update the ASGA and 
see if the bug is still there.

If it is (sorry again :sweat_smile:), have a look if the problem has not already been reported 
and if not, just open an issue on [GitHub](https://github.com/RRobert92/ASGA) with
the following basic information:
  - the output of `ASGA --version` (something like `ASGA 0.69`);
  - your system (Debian 7, ArchLinux, Windows, etc.);
  - how to reproduce the bug and if possible which data you used;
  - if relavent what was the error message or when ASGA crahed;
  - anything else you think is relevant.

It's only with enough information that I can do something to fix the problem.

# Make a pull request

We gladly accept pull request on the [official
repository](https://github.com/RRobert92/ASGA) for new tools, new features, bug
fixes, etc.

# Developing

[Create and activate a Python 3 virtual environment.](https://docs.python.org/3/tutorial/venv.html)

Install `ASGA` for development:

```bash
# install.packages("devtools")
library(devtools)
install_github("ropensci/git2r")

git clone https://github.com/RRobert92/ASGA.git
cd ASGA
make install

renv::restore()
```

Run code style checks:

```bash
# install.packages("styler")
library("styler")

```
