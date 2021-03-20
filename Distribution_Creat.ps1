# Download and extract the Windows binary install
if(Test-Path R-Portable-Win){
	rm -r R-Portable-Win
}

if(Test-Path Dist){
	rm -r Dist
}

mkdir R-Portable-Win

wget https://cloud.r-project.org/bin/windows/base/R-4.0.4-win.exe -OutFile R-Portable-Win/latest_r.exe

wget https://constexpr.org/innoextract/files/innoextract-1.9-windows.zip -OutFile R-Portable-Win/innoextract.zip
Expand-Archive -Force .\R-Portable-Win\innoextract.zip .\R-Portable-Win\innoextract

rm -r R-Portable-Win/innoextract.zip

cd R-Portable-Win
.\innoextract\innoextract.exe -e latest_r.exe

mv app/* ../R-Portable-Win

# Remove unneccessary files TODO: What else
rm -r app
rm -r innoextract 
rm -r latest_r.exe 
rm -r doc
rm -r tests

#Install all packages
cd ../
.\R-Portable-Win\bin\Rscript.exe .\Install_Packages_Deploy.R

#Deploy distributions 
npm install electron-packager
npm install

npm run package-win
mkdir .\Dist\ASGA-win32-x64\resources\app\R\Data
Compress-Archive -Force .\Dist\ASGA-win32-x64 .\Dist\ASGA-win32-x64.zip
rm -r .\Dist\ASGA-win32-x64

npm run package-linux
mkdir .\Dist\ASGA-linux-x64\resources\app\R\Data
Compress-Archive -Force .\Dist\ASGA-linux-x64 .\Dist\ASGA-linux-x64.zip
rm -r .\Dist\ASGA-linux-x64

npm run package-mac
mkdir .\Dist\ASGA-mac-x64\resources\app\R\Data
Compress-Archive -Force .\Dist\ASGA-mac-x64 .\Dist\ASGA-mac-x64.zip
rm -r .\Dist\ASGA-mac-x64