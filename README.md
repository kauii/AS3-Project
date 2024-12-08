# Text-Based Adventure Game in Haskell
Welcome to the Text-Based Adventure Game – a thrilling command-driven exploration through a medieval world filled with danger, puzzles, and treasures! This game challenges players to explore various rooms, solve puzzles, engage in combat with randomized enemies, and collect useful items to advance through the game.

## Overview
In this game you can:
- **Explore Rooms**: Navigate through predefined rooms, each with its own unique descriptions, difficulties, and potential dangers.
- **Solve Puzzles**: Interact with objects like switches and chests to solve puzzles and unlock hidden paths or rewards.
- **Combat Enemies**: Encounter randomized enemies that match the difficulty of the room. Each enemy may drop loot upon defeat.
- **Collect Items**: Find and use randomized consumables, weapons, and armor to aid your journey. Items are generated with different quantities and effects.
- **Interact with NPCs and Objects**: Engage in conversations, inspect objects, and manipulate the environment to uncover secrets and progress in the story.
- **End the Game**: By defeating the final boss or getting defeated during your adventure.

## Key Features:
- **Randomization**: Items and enemies are randomized to provide a different experience each playthrough.
- **Combat System**: Engage in battles, attack enemies, use items, or try to flee.
- **Interactive Puzzles**: Solve puzzles, like flipping switches in the correct order and using passphrases to unlock new areas.
- **Inventory Management**: Equip weapons and armor, use consumables, and manage your inventory effectively.

  
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
      
5. Now try to run the project using the command:
   
     ```bash
   stack exec AS3-Project-exe
   ```
     
   This should execute the ```main.hs```.If everything is set up correctly.
   Have Fun!
