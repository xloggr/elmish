#!/bin/bash

set -e

PKGVERSION=3.1.0-buf

dotnet fake build
dotnet pack --nologo --no-build -c Release /p:PackageVersion="$PKGVERSION" netstandard/Elmish.fsproj
DIRNAME=$(basename $(pwd))
pushd ..
dotnet nuget push $DIRNAME/netstandard/bin/Release/Elmish.$PKGVERSION.nupkg -s nuget-local
popd
