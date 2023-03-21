# LambdaLock
A password manager inspired by pass, written in Haskell.

# Usage 
Run ``lambdalock --help`` to display the help page


# Dependencies
- Requires GPG to be set up and installed 

# Installation
In the project folder: 

```bash
stack build 
stack install 
```

# Notes
You cannot have ``HOME_FOLDER_PLACEHOLDER`` as a ``--path`` argument because of default values, will be fixed later hopefully
