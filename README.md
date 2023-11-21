Delphi-JsonToDelphiClass
========================

Generates Delphi Classes based on JSON string. Just like XML Data Binding, but for JSON.

![screen1](https://github.com/HemulGM/Delphi-JsonToDelphiClass/blob/master/media/screen1.png?raw=true)
![screen1](https://github.com/HemulGM/Delphi-JsonToDelphiClass/blob/master/media/screen2.png?raw=true)

Main features:

- Build entirely on the RTL (no external dependencies) so it's cross-platform;
- Accepts any valid JSON string, no matter how complex the object is;
- Visualizes the structure of the JSON objects in a treeview;
- Generates complete delphi unit (declaration and implementation), based on the JSON string input;
- Automatically prefixes reserved Delphi words with "&" (ampersand);
- * Blocks unit generation if the JSON string contains empty Array;
- Adds support code to automatically destroy complex sub types. So you don't have to manage subobject's lifetime manually;
- ** Uses TArray<T> to represent lists;
- Adds helper serialization/deserialization functions;
- Serialization and deserialization results in the same JSON structure!
- Automatically detects date/datetime parts and maps them to TDate/TDateTime (as long as dates are ISO8601 compliant);
- Maps all numbers to Double;
- Maps true/false values to Boolean;
- Allows you to change property names (keys);
- Allows you to change the names of the stub classes;
- Supports JSON pretty print to format the input string;
- Simple and responsive GUI;
- It's open source! You can find the source code and binary releases on GitHub.

* If the JSON array is empty the contained type is unknown. Unit generation works only with known and supported types.

*** The releases of JsonToDelphiClass (source and binaries) are public and reside on GitHub.

Report any problems/suggestions using GitHub's facilities.

<hr>
<p align="center">
<img src="https://dtffvb2501i0o.cloudfront.net/images/logos/delphi-logo-128.webp" alt="Delphi">
</p>
<h5 align="center">
Made with :heart: on Delphi
</h5>
