# AS3-Project

## Setup

### Setup Haskell

The official haskell-website has an installation guide for GHCup, which is the main installer for the general purpose language Haskell.
<https://www.haskell.org/ghcup/install/#how-to-install>

*TL;DR*

For **Linux, macOS, FreeBSD or Windows Subsystem 2 for Linux**, run this in a terminal:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

For **Windows**, run this in a PowerShell session:

```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

*Note: You might want to disable your antivirus temporarily, if you experience problems  during the installation.*

### Install Stack (Required)

**Make sure to install Stack as well, as it is needed for setting up the project**
You have two options for installing Stack:
1. During GHCup Installaion:
   - When prompted during the installation process of GHCup, simply type and enter "Y"
2. Post GHCup Installation:
   - If you've already completed GHCup installation, you can install Stack by running
     
   ```bash
   ghcup tui
   ```
   
   this command opens the GHCup Text User Interface where you can navigate and select Stack for installation.

### Setup Project

1. Clone this repository: Open a terminal and run the following command
   
   ```bash
   git clone https://github.com/kauii/AS3-Project.git
   ```
   
3. Go to a terminal and type in the following command:
   
   ```bash
   stack build
   ```
   
   *If you make any changes to your project, don't forget to first run stack build again before executing the main.*
   
5. Now try to run the project using the command:
   
     ```bash
   stack exec AS3-Project-exe
   ```
     
   This should execute the ```main.hs```.If everything is set up correctly.
   Have Fun!

### IDE

As editor I would recommend using **Visual Studio Code** with the Haskell extension (extensionId: ```haskell.haskell```). You are free to use any other IDE that supports Haskell though.
