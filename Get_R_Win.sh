#Resource: https://github.com/dirkschumacher/r-shiny-electron/
# Download and extract the Windows binary install
# Requires innoextract installed in the Dockerfile
mkdir R-Portable-Win
wget https://cloud.r-project.org/bin/windows/base/R-4.0.4-win.exe \
  --output-document R-Portable-Win/latest_r.exe
cd R-Portable-Win
innoextract -e latest_r.exe
mv app/* ../R-Portable-Win
rm -r app latest_r.exe 
# Remove unneccessary files TODO: What else
rm -r doc tests