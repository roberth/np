{ mkDerivation, aeson, base, bytestring, directory, filepath
, process, stdenv, unix, yaml
}:
mkDerivation {
  pname = "np";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory filepath process unix yaml
  ];
  homepage = "https://roberthensing.nl/np";
  description = "Nix Project development tool";
  license = stdenv.lib.licenses.bsd3;
}
